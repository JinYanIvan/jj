setwd("C:/Users/Administrator/Desktop/��ҵ����")
X=read.csv("BYSJ.csv",header=T)

#����ͳ����
library(plyr)
library(moments)
library(fBasics)
library(tseries)
each(mean,max,min,median,sd)(X$LNG)
each(mean,max,min,median,sd)(X$LNQ)
skewness(X$LNG)
anscombe.test(X$LNG)
skewness(X$LNQ)
anscombe.test(X$LNQ)
kurtosis(X$LNG)
agostino.test(X$LNG)
kurtosis(X$LNQ)
agostino.test(X$LNQ)
jarque.bera.test(X$LNG)
jarque.bera.test(X$LNQ)

#ʱ����������ͼ
LNGT<-ts(X$LNG)
plot(LNGT,xaxt='n',main="���ǵ�������Ʊ�۸�����",col="blue")
axis(1,at=1:731,labels=X$TIME,lwd=1,lty=1,col="blue",xlim=10)
LNQT<-ts(X$LNQ)
plot(LNQT,xaxt='n',main="���ڻ��۸�ָ������",col="blue")
axis(1,at=1:731,labels=X$TIME,lwd=1,lty=1,col="blue",xlim=10)

#����Է���
library(ggplot2)
LNXG<-cor(X$LNG,X$LNQ,method="spearman")
LNXG
cor.test(X$LNG,X$LNQ,method="spearman")

#ƽ���Լ���(ADF)
adf.test(LNGT,k = 1)#k=1/2/3
adf.test(LNQT,k = 1)#k=1/2/3
#���
library(forecast)
ndiffs(LNGT)
LNG.dif=diff(LNGT,1)
adf.test(LNG.dif,k=1)
plot(LNG.dif)
ndiffs(LNQT)
LNQ.dif=diff(LNQT,1)
adf.test(LNQ.dif.k=1)
plot(LNQ.dif)

#Э������
library(lmtest)
library(zoo)
library(urca)
reg<-lm(LNGT~LNQT)#�ع鷽��
summary(reg)
dwtest(reg)
#�в�ƽ���Լ���
error<-residuals(reg)
urt.resid<-ur.df(error,type='none',selectlags='AIC')
summary(urt.resid)

#ECM�������ģ��
error.lagged<-error[-c(731,732)]
ecm.reg1<-lm(LNG.dif~error.lagged+LNQ.dif)
summary(ecm.reg1)
dwtest(ecm.reg1)

#�������������
library(vars)
library(MASS)
library(strucchange)
library(sandwich)
Gm<-lm(LNG.dif~LNQ.dif)
AIC(Gm,k=1)
grangertest(LNG.dif~LNQ.dif,order=1)
grangertest(LNQ.dif~LNG.dif,order=1)

#VARģ��
#����ͳ�Ʒ���
Y<-read.csv("NVAR.csv",header=T)
Z<-read.csv("XVAR.csv",header=T)
each(mean,max,min,median,sd)(Y$NLNG)
each(mean,max,min,median,sd)(Z$XLNG)
skewness(Y$NLNG)
skewness(Z$XLNG)
kurtosis(Y$NLNG)
kurtosis(Z$XLNG)
jarque.bera.test(Y$NLNG)
jarque.bera.test(Z$XLNG)
#2017
library(fUnitRoots)
NLNGT<-ts(Y$NLNG)
plot(NLNGT,xaxt='n')
axis(1,at=1:243,labels=Y$TIME,lwd=1,lty=1,col="red",xlim=10)
NLNQT<-ts(Y$NLNQ)
plot(NLNQT,xaxt='n')
axis(1,at=1:243,labels=Y$TIME,lwd=1,lty=1,col="red",xlim=10)
adfTest(NLNGT)
adfTest(NLNQT)
ndiffs(NLNGT)
ndiffs(NLNQT)
NLNGT.dif=diff(NLNGT,1)
NLNQT.dif=diff(NLNQT,1)
adfTest(NLNGT.dif)
adfTest(NLNQT.dif)
Mdn<-data.frame(NLNGT.dif,NLNQT.dif)
VARselect(Mdn,lag.max = 6,type="const")
VAR1<-VAR(Mdn,p=1,type="const")
VAR1

#2018
XLNGT<-ts(Z$XLNG)
plot(XLNGT,xaxt='n')
axis(1,at=1:243,labels=Z$TIME,lwd=1,lty=1,col="green",xlim=10)
XLNQT<-ts(Z$XLNQ)
plot(XLNQT,xaxt='n')
axis(1,at=1:243,labels=Z$TIME,lwd=1,lty=1,col="green",xlim=10)
adfTest(XLNGT)
adfTest(XLNQT)
ndiffs(XLNGT)
ndiffs(XLNQT)
XLNGT.dif=diff(XLNGT,1)
XLNQT.dif=diff(XLNQT,1)
adfTest(XLNGT.dif)
adfTest(XLNQT.dif)
Mdx<-data.frame(XLNGT.dif,XLNQT.dif)
VARselect(Mdx,lag.max = 6,type="const")
VAR2<-VAR(Mdx,p=2,type="const")
VAR2
