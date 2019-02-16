
###１、データの処理###

##プロンプト上のデータの表示数を最大にする##
old.op <- options(max.print=9999999)
old.op



##ファイルの読み込み（通貨）##
AllCurrency <- read.csv("FRB_H10.csv",header=F,stringsAsFactors=F,skip=6)



##データのクリーニング（通貨）##
AllCurrency[AllCurrency == "ND"] <- NA
AllCurrency_na <- as.data.frame(na.omit(AllCurrency))
names(AllCurrency_na) <- c("Time","AUD/USD","EUR/USD","NZD/USD","GBP/USD","USD/JPY")
attach(AllCurrency_na)
Currency_list <- list()



##データを変化率に変換する関数##
Change_Rate <- function (Select_Currency) {
  Currency <- as.numeric(na.omit(Select_Currency))
  Currency_Len <- length(Currency)
  Change_Rate_Currency <- list()
  Change_Currency_list <- list()
  
  l <- 0
  for (l in 2:Currency_Len) {
    Change_Rate_Currency[l-1] <- ((Currency[l] - Currency[l-1]) / Currency[l-1]) * 100
    Change_Currency_list <- rbind(Change_Currency_list,Change_Rate_Currency[l-1])
  }
  return(Change_Currency_list)
}



##時間以外のデータを変化率に変換し、リストにまとめる
for(m in 1:length(AllCurrency)) {
  if(m == 1) {
    #日付の調整
    AllCurrency_Change_Rate_List <- data.frame()
    AllCurrency_Change_Rate_List <- AllCurrency_na[2:length(AllCurrency_na[,1]),1]
  } else {
    list <- Change_Rate(AllCurrency_na[,m])
    AllCurrency_Change_Rate_List <- cbind(AllCurrency_Change_Rate_List, list)
  }
}






###２、データの分析とグラフのプロット###

##各為替レートの基本統計量##
for(i in 2:ncol(AllCurrency_na)) {
  print(names(AllCurrency_na[i]))
  print(summary(as.numeric(AllCurrency_na[,i])))
}



##各為替レートの時系列図##
par(mfrow = cbind((ncol(AllCurrency_na))/2,2))
count1 <- 1
for(p in 1:plot_na_num+1) {
  plot(AllCurrency_na[,p],ylab = names(AllCurrency_na)[p],main = paste("図 1 -",count1))
  count1 <- 1 + count1
}



##定常化前プロット##
par(mfrow = cbind((ncol(AllCurrency_na)-2)/2,2))
plot_na_num <- ncol(AllCurrency_na)-1
count2 <- 1
for(p in 2:plot_na_num) {
  par(mfrow = cbind(2,2))
  for(q in (p+1):ncol(AllCurrency_na)) {
    Currency_na_lm <- lm(as.numeric(AllCurrency_na[,p]) ~ as.numeric(AllCurrency_na[,q]))
    plot(unlist(AllCurrency_na[,p]),unlist(AllCurrency_na[,q]),
         ylab = names(AllCurrency_na)[p],xlab = names(AllCurrency_na)[q],main = paste("図 2 -",count2))
    count2 <- 1 + count2
    abline(Currency_na_lm)
    summary(Currency_na_lm)
  }
}



##線形単回帰分析##
# 5c2の組み合わせでプロット
num_plot <- (ncol(AllCurrency_Change_Rate_List)-1) * (ncol(AllCurrency_Change_Rate_List)-2) / 2
par(mfrow = cbind((num_plot)/2,2))
plot_num <- ncol(AllCurrency_Change_Rate_List)-1
count3 <- 1

for(p in 2:plot_num) {
  par(mfrow = cbind(2,2))
  for(q in (p+1):ncol(AllCurrency_Change_Rate_List)) {
    Currency_lm <- lm(unlist(AllCurrency_Change_Rate_List[,p]) ~ unlist(AllCurrency_Change_Rate_List[,q]))
    plot(unlist(AllCurrency_Change_Rate_List[,p]),unlist(AllCurrency_Change_Rate_List[,q]),xlim = c(-5,5), ylim = c(-5,5),
         ylab = names(AllCurrency_na)[p],xlab = names(AllCurrency_na)[q],main = paste("図 3 -",count3))
    count3 <- 1 + count3
    abline(Currency_lm)
    summary(Currency_lm)
  }
}



##移動回帰分析用のデータの作成##

#AUD<- AllCurrency_Change_Rate_List[,2]
EUR <- AllCurrency_Change_Rate_List[,3]
#NZD <- AllCurrency_Change_Rate_List[,4]
#GBP <- AllCurrency_Change_Rate_List[,5]
JPY <- AllCurrency_Change_Rate_List[,6]

Currency1 <- unlist(EUR) 
Currency2 <- unlist(JPY) 
stock <- data.frame(Currency1,Currency2)
stock.lm <- lm(Currency1 ~ Currency2)



##数値の代入##
leng <- length(Currency1)
data_count <- 4800    ##為替レート　20日*12ヶ月*20年=4800のデータ数?
start.year <- 1999
end.year <- 2018
fre <- 240

i <- 1
x <- leng-data_count+1
beta <- rep(0,x)
rsq <- rep(0,x)



##時間的安定性を確認するための移動回帰分析の実行##
for(i in 1:x) {
  tsstock.lm <- lm(Currency1[i:(i+data_count-1)] ~ Currency2[i:(i+data_count-1)])
  coef.tsstock.lm <- coef(tsstock.lm)
  beta[i] <- coef.tsstock.lm[2]
  
  rss <- deviance(tsstock.lm)
  tss <- var(Currency1[i:(i+data_count-1)]) * (data_count-1)
  rsq[i] <- 1-(rss/tss)
}



##β値と決定係数をtsのフォーマットに変換する
tsbeta <- ts(beta,start=start.year,end=end.year,frequency=fre)
tsrsq <- ts(rsq,start=start.year,end=end.year,frequency=fre)



##プロットの作成##
par(mfrow=c(2,1))
plot(tsbeta,type="l",las=1,ylab=expression(beta),xlab="")
abline(h=1,lty=2)
abline(h=1.064,col="red")
title("過去20年間のβ値の推移(Currency1:1999-2018)")
plot(tsrsq,type="l",las=1,ylab=expression(R^2),xlab="")
abline(h=0.5834,col="red")
title("過去20年間の決定系数の推移(Currency1:1999-2018")

