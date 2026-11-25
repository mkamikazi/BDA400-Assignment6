# Run from the assignment folder after installing packages.
source('app.R',local=TRUE)
# Deterministic unit checks, independent of Yahoo connectivity.
stopifnot(identical(trade_signals(c(NA,1,3,4,1,1,3),rep(2,7)),
                    c('Not ready','Not ready','Buy','Hold','Sell','Hold','Buy')))
d <- data.frame(Date=as.Date('2023-01-01')+0:119,Open=100+0:119,High=102+0:119,Low=99+0:119,Close=101+0:119)
a <- add_indicators(d)
stopifnot(isTRUE(all.equal(a$Short[20],mean(d$Close[1:20]))),
          isTRUE(all.equal(a$Long[50],mean(d$Close[1:50]))),
          all(a$Signal[1:50]=='Not ready'),a$Signal[51]=='Hold')
w <- aggregate_prices(d,'Weekly'); m <- aggregate_prices(d,'Monthly')
stopifnot(nrow(w)<nrow(d),nrow(m)<nrow(w),w$Open[1]==d$Open[1],tail(m$Close,1)==tail(d$Close,1))
cat('PASS: crossover, moving averages, warm-up and aggregation checks.\n')
# Live integration check: errors are reported, not falsely marked passed.
tryCatch({live <- fetch_prices('AAPL','2023-01-01','2023-07-01'); stopifnot(nrow(live)>0); cat('PASS: Yahoo historical data request.\n')},error=function(e) cat('LIVE CHECK FAILED:',conditionMessage(e),'\n'))
