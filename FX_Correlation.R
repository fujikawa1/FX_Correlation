
###�P�A�f�[�^�̏���###

##�v�����v�g��̃f�[�^�̕\�������ő�ɂ���##
old.op <- options(max.print=9999999)
old.op



##�t�@�C���̓ǂݍ��݁i�ʉ݁j##
AllCurrency <- read.csv("FRB_H10.csv",header=F,stringsAsFactors=F,skip=6)



##�f�[�^�̃N���[�j���O�i�ʉ݁j##
AllCurrency[AllCurrency == "ND"] <- NA
AllCurrency_na <- as.data.frame(na.omit(AllCurrency))
names(AllCurrency_na) <- c("Time","AUD/USD","EUR/USD","NZD/USD","GBP/USD","USD/JPY")
attach(AllCurrency_na)
Currency_list <- list()



##�f�[�^��ω����ɕϊ�����֐�##
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



##���ԈȊO�̃f�[�^��ω����ɕϊ����A���X�g�ɂ܂Ƃ߂�
for(m in 1:length(AllCurrency)) {
  if(m == 1) {
    #���t�̒���
    AllCurrency_Change_Rate_List <- data.frame()
    AllCurrency_Change_Rate_List <- AllCurrency_na[2:length(AllCurrency_na[,1]),1]
  } else {
    list <- Change_Rate(AllCurrency_na[,m])
    AllCurrency_Change_Rate_List <- cbind(AllCurrency_Change_Rate_List, list)
  }
}






###�Q�A�f�[�^�̕��͂ƃO���t�̃v���b�g###

##�e�בփ��[�g�̊�{���v��##
for(i in 2:ncol(AllCurrency_na)) {
  print(names(AllCurrency_na[i]))
  print(summary(as.numeric(AllCurrency_na[,i])))
}



##�e�בփ��[�g�̎��n��}##
par(mfrow = cbind((ncol(AllCurrency_na))/2,2))
count1 <- 1
for(p in 1:plot_na_num+1) {
  plot(AllCurrency_na[,p],ylab = names(AllCurrency_na)[p],main = paste("�} 1 -",count1))
  count1 <- 1 + count1
}



##��퉻�O�v���b�g##
par(mfrow = cbind((ncol(AllCurrency_na)-2)/2,2))
plot_na_num <- ncol(AllCurrency_na)-1
count2 <- 1
for(p in 2:plot_na_num) {
  par(mfrow = cbind(2,2))
  for(q in (p+1):ncol(AllCurrency_na)) {
    Currency_na_lm <- lm(as.numeric(AllCurrency_na[,p]) ~ as.numeric(AllCurrency_na[,q]))
    plot(unlist(AllCurrency_na[,p]),unlist(AllCurrency_na[,q]),
         ylab = names(AllCurrency_na)[p],xlab = names(AllCurrency_na)[q],main = paste("�} 2 -",count2))
    count2 <- 1 + count2
    abline(Currency_na_lm)
    summary(Currency_na_lm)
  }
}



##���`�P��A����##
# 5c2�̑g�ݍ��킹�Ńv���b�g
num_plot <- (ncol(AllCurrency_Change_Rate_List)-1) * (ncol(AllCurrency_Change_Rate_List)-2) / 2
par(mfrow = cbind((num_plot)/2,2))
plot_num <- ncol(AllCurrency_Change_Rate_List)-1
count3 <- 1

for(p in 2:plot_num) {
  par(mfrow = cbind(2,2))
  for(q in (p+1):ncol(AllCurrency_Change_Rate_List)) {
    Currency_lm <- lm(unlist(AllCurrency_Change_Rate_List[,p]) ~ unlist(AllCurrency_Change_Rate_List[,q]))
    plot(unlist(AllCurrency_Change_Rate_List[,p]),unlist(AllCurrency_Change_Rate_List[,q]),xlim = c(-5,5), ylim = c(-5,5),
         ylab = names(AllCurrency_na)[p],xlab = names(AllCurrency_na)[q],main = paste("�} 3 -",count3))
    count3 <- 1 + count3
    abline(Currency_lm)
    summary(Currency_lm)
  }
}



##�ړ���A���͗p�̃f�[�^�̍쐬##

#AUD<- AllCurrency_Change_Rate_List[,2]
EUR <- AllCurrency_Change_Rate_List[,3]
#NZD <- AllCurrency_Change_Rate_List[,4]
#GBP <- AllCurrency_Change_Rate_List[,5]
JPY <- AllCurrency_Change_Rate_List[,6]

Currency1 <- unlist(EUR) 
Currency2 <- unlist(JPY) 
stock <- data.frame(Currency1,Currency2)
stock.lm <- lm(Currency1 ~ Currency2)



##���l�̑��##
leng <- length(Currency1)
data_count <- 4800    ##�בփ��[�g�@20��*12����*20�N=4800�̃f�[�^��?
start.year <- 1999
end.year <- 2018
fre <- 240

i <- 1
x <- leng-data_count+1
beta <- rep(0,x)
rsq <- rep(0,x)



##���ԓI���萫���m�F���邽�߂̈ړ���A���͂̎��s##
for(i in 1:x) {
  tsstock.lm <- lm(Currency1[i:(i+data_count-1)] ~ Currency2[i:(i+data_count-1)])
  coef.tsstock.lm <- coef(tsstock.lm)
  beta[i] <- coef.tsstock.lm[2]
  
  rss <- deviance(tsstock.lm)
  tss <- var(Currency1[i:(i+data_count-1)]) * (data_count-1)
  rsq[i] <- 1-(rss/tss)
}



##���l�ƌ���W����ts�̃t�H�[�}�b�g�ɕϊ�����
tsbeta <- ts(beta,start=start.year,end=end.year,frequency=fre)
tsrsq <- ts(rsq,start=start.year,end=end.year,frequency=fre)



##�v���b�g�̍쐬##
par(mfrow=c(2,1))
plot(tsbeta,type="l",las=1,ylab=expression(beta),xlab="")
abline(h=1,lty=2)
abline(h=1.064,col="red")
title("�ߋ�20�N�Ԃ̃��l�̐���(Currency1:1999-2018)")
plot(tsrsq,type="l",las=1,ylab=expression(R^2),xlab="")
abline(h=0.5834,col="red")
title("�ߋ�20�N�Ԃ̌���n���̐���(Currency1:1999-2018")
