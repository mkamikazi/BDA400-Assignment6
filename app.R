# BDA400 Assignment 6 | Mireille Kamikazi
# Step 1: Data collection and setup. Run install_packages.R first.
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

fetch_prices <- function(symbol, start, end, provider = 'Yahoo Finance') {
  # Fetch extra history to initialize monthly and weekly indicators.
  from <- as.Date(start) - 3650
  if (provider == 'Yahoo Finance') {
    x <- getSymbols(symbol, src='yahoo', from=from, to=as.Date(end)+1,
                    auto.assign=FALSE)
    d <- data.frame(Date=as.Date(index(x)), Open=as.numeric(Op(x)),
                    High=as.numeric(Hi(x)), Low=as.numeric(Lo(x)), Close=as.numeric(Cl(x)))
  } else {
    # Stooq is an optional independent source for US-listed equities.
    ticker <- paste0(tolower(symbol), '.us')
    url <- sprintf('https://stooq.com/q/d/l/?s=%s&i=d&d1=%s&d2=%s',
                   ticker, format(from,'%Y%m%d'), format(as.Date(end),'%Y%m%d'))
    d <- read.csv(url)
    if (!all(c('Date','Open','High','Low','Close') %in% names(d))) stop('Stooq returned no usable prices.')
    d <- d[,c('Date','Open','High','Low','Close')]
    d$Date <- as.Date(d$Date)
  }
  d <- d[complete.cases(d) & d$Date <= as.Date(end), ]
  d <- d[order(d$Date), ]; d <- d[!duplicated(d$Date), ]
  if (!nrow(d)) stop('No valid prices returned. Check the symbol, dates or network.')
  d
}

aggregate_prices <- function(d, frame) {
  if (frame == 'Daily') return(d)
  group <- if (frame == 'Weekly') format(d$Date - (as.integer(format(d$Date,'%u'))-1), '%Y-%m-%d') else format(d$Date,'%Y-%m')
  do.call(rbind, lapply(split(d, group), function(z) data.frame(
    Date=max(z$Date), Open=z$Open[1], High=max(z$High), Low=min(z$Low), Close=tail(z$Close,1))))
}

# Step 3 and 4 helpers: indicators and genuine crossover events.
trade_signals <- function(short, long) {
  delta <- short-long
  previous <- c(NA_real_, head(delta,-1))
  out <- rep('Hold',length(delta))
  ready <- !is.na(delta) & !is.na(previous)
  out[ready & delta > 0 & previous <= 0] <- 'Buy'
  out[ready & delta < 0 & previous >= 0] <- 'Sell'
  out[!ready] <- 'Not ready'
  out
}
add_indicators <- function(d, short=20, long=50, rsi_n=14) {
  safe <- function(n, expr) if(nrow(d) >= n) expr() else rep(NA_real_,nrow(d))
  d$Short <- safe(short,function() as.numeric(SMA(d$Close,n=short)))
  d$Long <- safe(long,function() as.numeric(SMA(d$Close,n=long)))
  d$RSI <- safe(rsi_n+1,function() as.numeric(RSI(d$Close,n=rsi_n)))
  m <- if(nrow(d)>=34) MACD(d$Close,nFast=12,nSlow=26,nSig=9,percent=FALSE) else matrix(NA_real_,nrow(d),2)
  d$MACD <- as.numeric(m[,1]); d$MACDSignal <- as.numeric(m[,2])
  d$Signal <- trade_signals(d$Short,d$Long)
  d
}

# Step 2: Shiny user interface and interactive controls.
ui <- fluidPage(
  tags$head(tags$style(HTML('body{background:#f4f7fb;color:#183047} .well{background:white;border:1px solid #dce5ed} h2{font-weight:700} .note{color:#52667a;font-size:13px}'))),
  titlePanel('Portfolio dashboard | Technical analysis'),
  p('BDA400 Assignment 6 · Mireille Kamikazi',class='note'),
  sidebarLayout(sidebarPanel(
    textInput('symbol','Stock symbol','AAPL'),
    selectInput('provider','Data source',c('Yahoo Finance','Stooq (US stocks)')),
    dateRangeInput('date_range','Date range',start='2023-01-01',end='2023-07-01'),
    actionButton('fetch','Load stock data',class='btn-primary'), hr(),
    selectInput('time_frame','Time frame',c('Daily','Weekly','Monthly')),
    selectInput('chart','Chart type',c('Line','Candlestick','Area')),
    checkboxGroupInput('indicators','Technical indicator overlays',c('Moving Averages','RSI','MACD'),selected='Moving Averages'),
    numericInput('short','Short moving average',20,min=2,max=100,step=1),
    numericInput('long','Long moving average',50,min=3,max=200,step=1),
    numericInput('rsi','RSI period',14,min=2,max=50,step=1),
    checkboxGroupInput('annotations','Show signals',c('Buy','Sell','Hold'),selected=c('Buy','Sell','Hold')),
    helpText('Periods refer to the selected time frame. Signals are educational, not investment advice.')
  ),mainPanel(
    textOutput('status'), plotOutput('stock_chart',height='560px'),
    p('RSI and MACD overlays use independent min-max scaling to fit the price panel; their original values appear in the table.',class='note'),
    h4('Recent observations and signals'), tableOutput('recent'),
    downloadButton('download','Download filtered results')
  ))
)

server <- function(input,output,session) {
  request <- eventReactive(input$fetch, {
    symbol <- toupper(trimws(input$symbol)); dates <- input$date_range
    validate(need(grepl('^[A-Z0-9.^=-]+$',symbol),'Enter a valid ticker.'),
             need(length(dates)==2 && all(!is.na(dates)) && dates[1]<=dates[2],'Choose a valid date range.'))
    result <- tryCatch(fetch_prices(symbol,dates[1],dates[2],input$provider),error=function(e)e)
    validate(need(!inherits(result,'error'),if(inherits(result,'error')) paste('Data request failed:',conditionMessage(result)) else ''))
    list(data=result,symbol=symbol,dates=dates,provider=input$provider)
  },ignoreNULL=FALSE)
  data <- reactive({
    r <- request(); req(input$short,input$long,input$rsi)
    validate(need(input$short>=2 && input$long>input$short && input$long<=200 && input$rsi>=2 && input$rsi<=50 && all(c(input$short,input$long,input$rsi) %% 1 == 0),'Use whole periods with short MA < long MA and valid RSI.'))
    # Include warm-up observations before filtering visible range.
    d <- aggregate_prices(r$data,input$time_frame)
    d <- add_indicators(d,input$short,input$long,input$rsi)
    d <- d[d$Date>=r$dates[1] & d$Date<=r$dates[2],]
    validate(need(nrow(d)>0,'No trading observations in the selected range.'))
    d
  })
  output$status <- renderText({
    d <- data(); r <- request()
    paste(r$symbol,'|',r$provider,'|',input$time_frame,'|',nrow(d),'bars |',sum(d$Signal=='Buy'),'Buy |',sum(d$Signal=='Sell'),'Sell |',sum(d$Signal=='Hold'),'Hold |',sum(d$Signal=='Not ready'),'warm-up')
  })
  output$stock_chart <- renderPlot({
    d <- data(); r <- request(); d$Direction <- ifelse(d$Close>=d$Open,'Up','Down')
    width <- if(input$time_frame=='Daily') .65 else if(input$time_frame=='Weekly') 4 else 18
    p <- ggplot(d,aes(Date,Close))
    if(input$chart=='Line') p <- p+geom_line(colour='#214e78',linewidth=.8)+geom_point(colour='#214e78',size=.7)
    if(input$chart=='Area') p <- p+geom_area(fill='#a9c9e5',alpha=.75)+geom_line(colour='#214e78')
    if(input$chart=='Candlestick') p <- p+geom_segment(aes(xend=Date,y=Low,yend=High),colour='#52667a')+geom_rect(aes(xmin=Date-width/2,xmax=Date+width/2,ymin=pmin(Open,Close),ymax=pmax(Open,Close),fill=Direction),colour='#52667a',linewidth=.15)+scale_fill_manual(values=c(Up='#18845b',Down='#c84b4b'))
    # Step 3: toggle independent overlays. Oscillators are explicitly rescaled.
    if('Moving Averages' %in% input$indicators) p <- p+geom_line(aes(y=Short,colour='Short MA'),na.rm=TRUE)+geom_line(aes(y=Long,colour='Long MA'),na.rm=TRUE)
    yr <- range(c(d$Low,d$High)); if(diff(yr)==0) yr <- yr+c(-1,1)
    rescale <- function(x,limits) { if(any(!is.finite(limits))) return(rep(NA_real_,length(x))); if(diff(limits)==0) return(ifelse(is.na(x),NA_real_,mean(yr))); yr[1]+(x-limits[1])/diff(limits)*diff(yr) }
    if('RSI' %in% input$indicators) { d$RSIplot <- rescale(d$RSI,c(0,100)); p <- p+geom_line(data=d,aes(y=RSIplot,colour='RSI (scaled)'),linetype=2,na.rm=TRUE) }
    if('MACD' %in% input$indicators) { vals <- c(d$MACD,d$MACDSignal); lim <- if(any(is.finite(vals))) range(vals,na.rm=TRUE) else c(NA,NA); d$Mplot <- rescale(d$MACD,lim); d$Splot <- rescale(d$MACDSignal,lim); p <- p+geom_line(data=d,aes(y=Mplot,colour='MACD (scaled)'),na.rm=TRUE)+geom_line(data=d,aes(y=Splot,colour='MACD signal (scaled)'),linetype=3,na.rm=TRUE) }
    # Step 4: annotate every selected signal; compact dots indicate Hold.
    h <- d[d$Signal=='Hold' & 'Hold' %in% input$annotations,]
    s <- d[d$Signal %in% intersect(input$annotations,c('Buy','Sell')),]
    p <- p+geom_point(data=h,aes(shape='Hold'),colour='#82909e',size=1.4)+scale_shape_manual(values=c(Hold=16),name='Signal')+
      geom_point(data=s,aes(y=Close,colour=Signal),size=2.8)+geom_text(data=s,aes(label=Signal,colour=Signal),vjust=-1,size=3.4,check_overlap=TRUE)+
      scale_colour_manual(name='Overlays / signals',values=c('Short MA'='#d17b00','Long MA'='#7149a4','RSI (scaled)'='#008c95','MACD (scaled)'='#4758bd','MACD signal (scaled)'='#ad5389',Buy='#00804b',Sell='#b52935'))+
      labs(title=paste(r$symbol,'|',input$time_frame,input$chart),x=NULL,y='Closing price / scaled indicators',caption='Grey dots = Hold. Buy/Sell markers = moving-average crossover events.')+theme_minimal(base_size=12)+theme(legend.position='bottom',legend.box='vertical',plot.title=element_text(face='bold'),panel.grid.minor=element_blank())
    print(p)
  })
  output$recent <- renderTable({
    recent <- tail(data()[,c('Date','Close','Short','Long','RSI','MACD','MACDSignal','Signal')],10)
    recent$Date <- format(recent$Date, '%Y-%m-%d')
    recent
  },digits=2)
  output$download <- downloadHandler(filename=function() paste0(request()$symbol,'_indicators.csv'),content=function(file) write.csv(data(),file,row.names=FALSE))
}
shinyApp(ui,server)
