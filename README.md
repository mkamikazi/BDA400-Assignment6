# BDA400 Assignment 6 — Technical Analysis using R, Visualization Phase
**Student:** Mireille Kamikazi | CDI College

## Run the dashboard
1. Open this folder in RStudio and set it as the working directory (Session > Set Working Directory > Choose Directory).
2. Run `source('install_packages.R')` in the Console.
3. Open app.R and click Run App, or run `shiny::runApp('.')`.
4. The initial selection is AAPL, January 1–July 1, 2023, from Yahoo Finance. Change the symbol, dates or provider and click Load stock data. Other chart controls update immediately.

## Implementation
- Step 1: Yahoo Finance via quantmod; optional Stooq CSV source for US equities; network errors and missing observations handled explicitly. Extra historical bars initialize indicators.
- Step 2: Shiny UI/server; daily, weekly and monthly OHLC aggregation; line, candlestick and area charts; results table and CSV download. First/last periods may be partial.
- Step 3: SMA (20/50 by default), RSI (14), MACD (12/26/9); independently toggled overlays. RSI maps 0–100 to the price range, and MACD and its signal share a min-max mapping. Original indicator units appear in the table and CSV.
- Step 4: Buy when the short SMA crosses above the long SMA; Sell when it crosses below; Hold otherwise. Missing current/previous SMA values are Not ready. This follows the written crossover rule rather than the assignment's example code, which labels every above/below observation as Buy/Sell. Parameters and signal visibility are customizable. Buy/Sell markers and labels and grey Hold dots annotate the chart. No future values are used. Signals are not financial recommendations.

## Verification
Run `source('verify.R')` in the RStudio Console. It checks crossover logic, SMA arithmetic, aggregation and Yahoo connectivity. Then run the app and manually verify all chart types, time frames, individual indicator toggles, signal filters, custom MA lengths and invalid ticker handling. Try Stooq separately. Save screenshots and console output if desired. The code was prepared with AI assistance; runtime and live data tests have not been executed in the authoring environment.

## Submission
Create a public GitHub repository named BDA400-Assignment6 (or choose another name). Upload app.R, install_packages.R, verify.R, README.md and the completed cover PDF. Replace the repository URL placeholder in the cover with the actual public URL; check the link in a private browser window. Commit changes. Submit the cover PDF and repository link in the LMS. Do not submit the unfilled cover.
