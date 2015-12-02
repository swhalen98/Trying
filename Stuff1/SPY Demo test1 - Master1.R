

require(quantstrat)
Sys.setenv(TZ="UTC")


suppressWarnings(rm("order_book.bbands",pos=.strategy))
suppressWarnings(rm("account.bbands","portfolio.bbands",pos=.blotter))
suppressWarnings(rm("account.st","portfolio.st","stock.str","stratBBands","initDate","initEq",'start_t','end_t'))

#1) set up environment values - dates, global variables(fast=10), etc
#set initial Values
initDate='1993-02-02'
initEq=10000


#2) Get Symbols
currency('USD')
symbols <- c("SPY")
getSymbols(symbols, from="1993-02-02",index.class=c('POSIXt','POSIXct'))
stock.str='SPY'
stock(stock.str,currency='USD',multiplier=1)


#3) Initialize portfolio and account

rm(list=ls(.blotter), envir=.blotter)

portfolio.st='bbands'
account.st='bbands'
strategy.st='bbands'

initPortf(portfolio.st, symbols=stock.str, initDate=initDate, currency='USD')
initAcct(account.st, portfolios=portfolio.st, initDate=initDate, initEq=initEq, currency='USD')
initOrders(portfolio = portfolio.st, initDate=initDate)
addPosLimit(portfolio.st, stock.str, initDate, 200, 2 )

#4) define strategy
stratBBands <- strategy("bbands")

  #a) indicators (quick inspecting of indicator output) [SMA etc]

stratBBands <- add.indicator(strategy = 'stratBBands', name = "BBands", 
                             arguments = list(HLC = quote(HLC(mktdata)), 
                                              maType='SMA'), label='BBands')

  #b) signals [sigCrossover, label long or short]
stratBBands <- add.signal(stratBBands,name="sigCrossover",arguments = list(columns=c("Close","up"),relationship="gt"),label="Cl.gt.UpperBand")
stratBBands <- add.signal(stratBBands,name="sigCrossover",arguments = list(columns=c("Close","dn"),relationship="lt"),label="Cl.lt.LowerBand")
stratBBands <- add.signal(stratBBands,name="sigCrossover",arguments = list(columns=c("High","Low","mavg"),relationship="op"),label="Cross.Mid")

  #c) rules
stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(sigcol="Cl.gt.UpperBand",sigval=TRUE, orderqty=-100, ordertype='market', orderside=NULL, threshold=NULL,osFUN=osMaxPos),type='enter')
stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(sigcol="Cl.lt.LowerBand",sigval=TRUE, orderqty= 100, ordertype='market', orderside=NULL, threshold=NULL,osFUN=osMaxPos),type='enter')
stratBBands <- add.rule(stratBBands,name='ruleSignal', arguments = list(sigcol="Cross.Mid",sigval=TRUE, orderqty= 'all', ordertype='market', orderside=NULL, threshold=NULL,osFUN=osMaxPos),type='exit')


#5) Apply Strategy
start_t<-Sys.time()
applyStrategy(strategy='stratBBands', portfolios='bbands')


#6) update portfolio and view trades

end_t<-Sys.time()
print("strat execution time:")
print(end_t-start_t)

start_t<-Sys.time()
updatePortf(Portfolio='bbands',Dates=paste('::',as.Date(Sys.time()),sep=''))
dateRange <- time(getPortfolio(portfolio.st)$summary)[-1]
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)
end_t<-Sys.time()
print("updatePortf execution time:")
print(end_t-start_t)

#Update Account
updateAcct(account.st)

# View transactions
View(getTxns(Portfolio=portfolio.st, Symbol="SPY"))

#Chart position and P&L
chart.Posn(Portfolio='bbands',Symbol=stock.str)

#View trade statistics
View(t(tradeStats(Portfolios=portfolio.st)))
View((tradeOrderStats(portfolio = portfolio.st, symbol = stock.str)))

#View Order Book
View(getOrderBook(portfolio.st)[[portfolio.st]]$SPY)

#View statistics per trade
View(perTradeStats(portfolio.st))

#View annual returns using DailyEndEq
y<-PortfReturns(Account=account.st,Dates=initDate, Portfolios=portfolio.st)
equity <- y$SPY.DailyEndEq
table.AnnualizedReturns(equity)
table.CalendarReturns(equity)
VaR(equity)
charts.PerformanceSummary(equity)
Return.cumulative(equity)



