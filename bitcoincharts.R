library(quantmod)
library(ggplot2)
library(ggthemes)
library(scales)
library(quantmod)
library(RSQLite)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(tidyquant)


multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}



dbName <- "C:/Users/Avi/Downloads/coin2db/April3/cryptohist.db"
dbConn <- dbConnect(drv = SQLite(), dbname= dbName) #(dbName)
btc_daily = dbGetQuery(dbConn, "select * from crypto_history where Currency = 'bitcoin'")
total_mkt_cap = dbGetQuery(dbConn, "select * from daily_cap")

dbName1 <- "C:/Users/Avi/Downloads/coin2db/April3/ohlcv.db"
dbConn <- dbConnect(drv = SQLite(), dbname= dbName1) #(dbName)
btc_hourly = dbGetQuery(dbConn, "select * from ohlcv where exchange = 'btcusd' and symbol = 'gdax' and period_id = 3600 ")
mytime = btc_hourly$time_period_start[1:5]
as.POSIXlt(mytime, format ="%Y-%m-%dT%H:%M:%S" )
btc_hourlyXTS = xts(btc_hourly[,c("open", "high", "low", "close", "volume", "trades_count")], order.by = as.POSIXlt(btc_hourly$time_period_start ,"%Y-%m-%dT%H:%M:%S" ))
btc_hourly1 =  data.frame(Index = as.POSIXlt(btc_hourly$time_period_start ,"%Y-%m-%dT%H:%M:%S", tz='GMT' ), coredata(btc_hourlyXTS))
#chart_Series(btc_hourlyXTS[200:300,])
#.indexhour(btc_hourlyXTS)

#write.csv(all_bitcoin, "bitcoin_cm.csv")
btc_trans = read.csv(url("https://api.blockchain.info/charts/n-transactions?timespan=all&format=csv"), header = FALSE)
btc_transxts = xts(btc_trans[,2], order.by = as.Date(btc_trans[,1], format = "%Y-%m-%d"))
colnames(btc_transxts) = "Trans_Volume"
#tail(btc_transxts)

btc_daily$MarketCap = as.numeric(gsub(",", "", btc_daily$MarketCap))
btc_daily$Volume = as.numeric(gsub(",", "", btc_daily$Volume))
btc_xts1 = xts(btc_daily[,2:8], order.by = as.Date(as.character(btc_daily$RawDate), format ="%Y%m%d"))
total_mkt_cap_xts = xts(total_mkt_cap[,2], order.by = as.Date(as.character(total_mkt_cap$RawDate), format ="%Y%m%d"))
colnames(total_mkt_cap_xts)[1] = "Total_MktCap"

#btc_xts1 = cbind(btc_xts1,total_mkt_cap_xts )
btc_xts1 = cbind(btc_xts1, total_mkt_cap_xts, btc_transxts)
btc_xts1$Trans_Volume = na.locf(btc_xts1$Trans_Volume)
btc_xtsdf$black = "black"
btc_xtsdf$red = "red"
btc_xtsdf$btc_dom = btc_xtsdf$MarketCap / btc_xtsdf$Total_MktCap*100
#plot(btc_xtsdf$btc_dom, type="l")

#plot(btc_xtsdf$ema21_stretch >0)
n = nrow(btc_xts1)
lastIndex = btc_xtsdf$Index[n]
bartoUSe = (n-100): n
bartoUSe_hourly = (nrow(btc_hourlyXTS)-15): nrow(btc_hourlyXTS)
bartoUSe_Heat = (n-15): n

#colnames(btc_xtsdf)
mycolnames = c("Big_Move", "trend_1", "trend_2","breakout_UP","breakout_DOWN" )
myMat = btc_xtsdf[bartoUSe_Heat,mycolnames]
rownames(myMat) = btc_xtsdf$Index[bartoUSe_Heat]
myMat  = as.matrix(myMat)
myMat_melted = melt(myMat)
colnames(myMat_melted)[3] = "Signal"




#btc_xtsdf= btc_xtsdf[bartoUSe,]
#tail(btc_xtsdf[bartoUSe,])



p1 = ggplot()+ geom_point(aes(x = lastIndex, y = last(btc_hourlyXTS$close) ), colour = "blue", size =3) +
  geom_line(data = btc_xtsdf[bartoUSe,], aes(x = Index, y = Close)) + 
  theme(axis.title.x=element_blank()) + ylab("Bitcoin Price")

#p2 = ggplot(data = btc_xtsdf[bartoUSe,], aes(x = Index, y = myMACD))+ geom_segment(aes(x=Index, xend = Index, y = 0, yend = myMACD, colour = macdCol))+
p2 = ggplot(data = btc_xtsdf[bartoUSe,], aes(x = Index, y = myMACD))+ geom_segment(aes(x=Index, xend = Index, y = 0, yend = myMACD, color = macdCol)) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank())  + ylab("Trend Score 1")

p3 = ggplot()+ geom_ribbon(data = btc_xtsdf[bartoUSe,], aes(x = Index, ymax = btc_dom, ymin = min(btc_xtsdf[bartoUSe,"btc_dom"], na.rm = TRUE)*.9), 
                           colour = "grey80", fill = "lightblue") + 
  geom_line(data = btc_xtsdf[bartoUSe,], aes(x = Index, y = btc_dom))  +
  theme(axis.title.x=element_blank()) + ylab("Bitcoin Dominance (%)")

hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')), space='Lab')
p5 = ggplot(myMat_melted, aes(x = Var1, y = Var2, fill = Signal)) + geom_tile(colour = "black")+
  scale_fill_gradientn(colours = rev(hm.palette(100)), breaks=c(-2,0,2),labels=c("Sell",0,"Buy"),limits=c(-2,2))+
  theme(axis.title.x=element_blank()) + ylab("Daily Signals") +  theme(axis.text.x = element_blank())


#colnames(btc_xtsdf)
#btc_xtsdf[bartoUSe,c("Index", "Open","High", "Low", "Close")] 
p4=  ggplot(btc_hourly1[bartoUSe_hourly,c("Index", "open","high", "low", "close")],aes(x = as.character(gsub("2018-", "",Index)), y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  ylab("BitCoin 1h") + theme(axis.title.x=element_blank()) + theme(axis.text.x = element_text(size=4, angle=90)) 


p6 = ggplot()+ geom_line(data = btc_xtsdf[bartoUSe,], aes(x = Index, y = roc7, colour = black)) +
  geom_line(data = btc_xtsdf[bartoUSe,], aes(x = Index, y = roc7t, colour = red), size =1.1) +  
  #scale_color_manual(values=c("black", "green")) +
  scale_color_discrete(name = "Weekly Change", labels = c("Bitcoin Price", "Bitcoin Transactions")) +
  theme(axis.title.x=element_blank()) + ylab("Bitcoin Price & Transactions") +theme(legend.position = c( 0.13, .9))


multiplot(p1, p2,p3,p4, p5, p6, cols =2)
ggsave("btc_view.png", plot= multiplot(p1, p2,p3,p4, p5, p6, cols =2), width = 15, height = 7.5, units = "in" )

