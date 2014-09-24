getSymbol = function(ticker){
  library("quantmod")  #Load the quantmod Library
  stockData <- new.env() #Make a new environment for quantmod to store data in
  
  startDate = as.Date("2008-01-13") #Specify period of time we are interested in
  endDate = Sys.Date()
  
  tickers <- c(ticker) #Define the tickers we are interested in
  
  #Download the stock history (for all tickers)
  values = getSymbols(tickers, env = stockData, src = "yahoo", from = startDate, to = endDate)
  return(get(ticker,envir=stockData))
}



getSymbols(tickers,src="yahoo",from=as.Date("2008-01-13"),to=as.Date(Sys.Date()))
lineChart(VIX)
lineChart(GSPC)