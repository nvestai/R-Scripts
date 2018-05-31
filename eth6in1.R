library(ggplot2)
library(ggthemes)
library(scales)
library(quantmod)
library(RSQLite)
library(dplyr)
library(reshape2)
library(RColorBrewer)
library(tidyquant)

#suppressWarnings(as.numeric(c("1", "2", "X")))
zz <- file("D:/RFiles/sixINonepoutPut-eth.txt", open="wt")
sink(zz, type="message")
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
eth_daily = dbGetQuery(dbConn, "select * from crypto_history where Currency = 'ethereum'")
total_mkt_cap = dbGetQuery(dbConn, "select * from daily_cap")

dbName1 <- "C:/Users/Avi/Downloads/coin2db/April3/ohlcv.db"
dbConn <- dbConnect(drv = SQLite(), dbname= dbName1) #(dbName)
eth_hourly = dbGetQuery(dbConn, "select * from ohlcv where exchange = 'ethusd' and symbol = 'gdax' and period_id = 3600 ")


eth_hourlyXTS = xts(eth_hourly[,c("open", "high", "low", "close", "volume", "trades_count")], order.by = as.POSIXlt(eth_hourly$time_period_start ,"%Y-%m-%dT%H:%M:%S" , tz='GMT'))
eth_hourly1 =  data.frame(Index = as.POSIXlt(eth_hourly$time_period_start ,"%Y-%m-%dT%H:%M:%S", tz='GMT' ), coredata(eth_hourlyXTS))

eth_trans = read.csv(url("https://etherscan.io/chart/tx?output=csv"), header = TRUE)
eth_transxts = xts(eth_trans[,3], order.by = as.Date(eth_trans[,1], format = "%m/%d/%Y"))
colnames(eth_transxts) = "Trans_Volume"

eth_daily$MarketCap = as.numeric(gsub(",", "", eth_daily$MarketCap))
eth_daily$Volume = as.numeric(gsub(",", "", eth_daily$Volume))
eth_xts1 = xts(eth_daily[,2:8], order.by = as.Date(as.character(eth_daily$RawDate), format ="%Y%m%d"))
total_mkt_cap_xts = xts(total_mkt_cap[,2], order.by = as.Date(as.character(total_mkt_cap$RawDate), format ="%Y%m%d"))
colnames(total_mkt_cap_xts)[1] = "Total_MktCap"

#eth_xts1 = cbind(eth_xts1,total_mkt_cap_xts )
eth_xts1 = cbind(eth_xts1, total_mkt_cap_xts, eth_transxts)
eth_xts1$Trans_Volume = na.locf(eth_xts1$Trans_Volume)

eth_xts1$Big_Move = eth_xts1$Close ##sorry not giving this away
eth_xts1$trend_2 = eth_xts1$Close ##sorry not giving this away
eth_xts1$trend_1 = eth_xts1$Close##sorry not giving this away


eth_xtsdf = data.frame(Index = index(eth_xts1), coredata(eth_xts1))
eth_xtsdf$macdCol = ifelse(eth_xtsdf$Trend1 >0, "red", "blue")
eth_xtsdf$breakout_UP = ifelse(eth_xtsdf$HighX>0, 1,0)
eth_xtsdf$breakout_DOWN = ifelse(eth_xtsdf$LowX>0, -1,0)
  
mdf1 = data.frame(eth_xtsdf$trend_1)
eth_xtsdf$black = "black"
eth_xtsdf$red = "red"
eth_xtsdf$eth_dom = eth_xtsdf$MarketCap / eth_xtsdf$Total_MktCap*100
#plot(eth_xtsdf$eth_dom, type="l")

n = nrow(eth_xts1)
lastIndex = eth_xtsdf$Index[n]
bartoUSe = (n-100): n
bartoUSe_hourly = (nrow(eth_hourlyXTS)-15): nrow(eth_hourlyXTS)
bartoUSe_Heat = (n-15): n

#colnames(eth_xtsdf)
mycolnames = c("Big_Move", "trend_1", "trend_2","breakout_UP","breakout_DOWN" )
myMat = eth_xtsdf[bartoUSe_Heat,mycolnames]
rownames(myMat) = eth_xtsdf$Index[bartoUSe_Heat]
myMat  = as.matrix(myMat)
myMat_melted = melt(myMat)
colnames(myMat_melted)[3] = "Signal"




#eth_xtsdf= eth_xtsdf[bartoUSe,]
#tail(eth_xtsdf[bartoUSe,])



p1 = ggplot()+ geom_point(aes(x = lastIndex, y = last(eth_hourlyXTS$close) ), colour = "blue", size =3) +
  geom_line(data = eth_xtsdf[bartoUSe,], aes(x = Index, y = Close)) + 
  theme(axis.title.x=element_blank()) + ylab("Ethereum Price")

#p2 = ggplot(data = eth_xtsdf[bartoUSe,], aes(x = Index, y = myMACD))+ geom_segment(aes(x=Index, xend = Index, y = 0, yend = myMACD, colour = macdCol))+
p2 = ggplot(data = eth_xtsdf[bartoUSe,], aes(x = Index, y = myMACD))+ geom_segment(aes(x=Index, xend = Index, y = 0, yend = myMACD, color = macdCol)) +
  theme(legend.position="none") +
  theme(axis.title.x=element_blank())  + ylab("Trend Score 1")

p3 = ggplot()+ geom_ribbon(data = eth_xtsdf[bartoUSe,], aes(x = Index, ymax = eth_dom, ymin = min(eth_xtsdf[bartoUSe,"eth_dom"], na.rm = TRUE)*.9), 
                           colour = "grey80", fill = "lightblue") + 
  geom_line(data = eth_xtsdf[bartoUSe,], aes(x = Index, y = eth_dom))  +
  theme(axis.title.x=element_blank()) + ylab("Ethereum Dominance (%)")

hm.palette <- colorRampPalette(rev(brewer.pal(9, 'RdYlBu')), space='Lab')
p5 = ggplot(myMat_melted, aes(x = Var1, y = Var2, fill = Signal)) + geom_tile(colour = "black")+
  scale_fill_gradientn(colours = rev(hm.palette(100)), breaks=c(-2,0,2),labels=c("Sell",0,"Buy"),limits=c(-2,2))+
  theme(axis.title.x=element_blank()) + ylab("Daily Signals") +  theme(axis.text.x = element_blank())

#print(eth_hourly1[bartoUSe_hourly,c("Index", "open","high", "low", "close")])
#colnames(eth_xtsdf)
#eth_xtsdf[bartoUSe,c("Index", "Open","High", "Low", "Close")] 
p4=  ggplot(eth_hourly1[bartoUSe_hourly,c("Index", "open","high", "low", "close")],aes(x = as.character(gsub("2018-", "",Index)), y = close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  ylab("Ethereum 1h") + theme(axis.title.x=element_blank()) + theme(axis.text.x = element_text(size=4, angle=90)) 


p6 = ggplot()+ geom_line(data = eth_xtsdf[bartoUSe,], aes(x = Index, y = roc7, colour = black)) +
  geom_line(data = eth_xtsdf[bartoUSe,], aes(x = Index, y = roc7t, colour = red), size =1.1) +  
  #scale_color_manual(values=c("black", "green")) +
  scale_color_discrete(name = "Weekly Change", labels = c("Ethereum Price", "Ethereum Transactions")) +
  theme(axis.title.x=element_blank()) + ylab("Ethereum Price & Transactions") +theme(legend.position = c( 0.13, .9))

print(eth_hourly1[bartoUSe_hourly,c("Index", "open","high", "low", "close")])
#multiplot(p1, p2,p3, p5, p6, cols =2)

#ggsave("eth_view.png", plot= multiplot(p1, p2,p3,p4, p5, p6, cols =2), width = 15, height = 7.5, units = "in" )
ggsave("D:/Gdrive/Nvest Images/eth_view.png", plot= multiplot(p1, p2,p3,p4, p5, p6, cols =2), width = 15, height = 7.5, units = "in" )
write(paste0("SixInOne script from R ran fine ", Sys.time()),"D:/Gdrive/Nvest Images/sixinONe.txt" )
