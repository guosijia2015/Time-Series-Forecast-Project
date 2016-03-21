                            ###NOBIVAC SALES OUT FORECAST MODEL###

## Load the Sales Out data for NOBIVAC AND ACTIVYL
data<-read.csv(file="Nobivac&Activyl_Sales_2013-2015.csv",header=TRUE)

## Change the column names
colnames(data)[colnames(data)=="PRODUCT_CODE"]<-"Sku_Code"
colnames(data)[colnames(data)=="PRODUCT_NAME"]<-"Sku_Name"
colnames(data)[colnames(data)=="PRODUCT_FAMILY"]<-"GPF_Desc"

## Change the format as DATE in Month and Week
data$Date<-as.Date(data$INVOICE_DATE,"%d%b%Y")
data$Month<-as.Date(cut(data$Date,breaks="month"))
data$Week<-as.Date(cut(data$Date,breaks="week",start.on.monday=TRUE))

#Sku code as factor
data$Sku_Code<-as.factor(data$Sku_Code)

## Extract only Nobivac data
n = grep("NOBIVAC",data$Sku_Name)
Nobivacdata <- data[n,]

## Aggregate quantity on a weekly basis
Nob_SO_Qty.wk<-aggregate(Nobivacdata$QUANTITY,by=list(Nobivacdata$Week,Nobivacdata$Sku_Code),sum)
names(Nob_SO_Qty.wk)<-c("Week","Sku_Code","Qty")
library(reshape)
Nob_SO_Qty.wk<-cast(Nob_SO_Qty.wk,Week~Sku_Code,sum,value="Qty")

## Plot Sales out for all Nobivac Skus
Nob_SO_wk.mat<-as.matrix(Nob_SO_Qty.wk)[(1:152),(1:36)]
library(zoo)
z.Nob_SO_wk<-zoo(Nob_SO_wk.mat,Nob_SO_Qty.wk$Week)
library(lattice)
xyplot(z.Nob_SO_wk,type="l",main="Weekly Sales Out Qty for Nobivac Skus",
       strip = function(bg,...) 
         strip.default(bg='white',...))

## SALES OUT forecast for 6772 and partition training and validation sets
library(forecast)
Nob_SO_6772 = Nob_SO_Qty.wk[,2]
Nob_SO_6772.train = Nob_SO_6772[1:136]
Nob_SO_6772.test = Nob_SO_6772[135:152]
Nob_SO_6772.train.ts = ts(Nob_SO_6772[1:136],freq=365.25/7,start=2013-1/365.25)

## USE of Fourier transformation. h is the length of prediction (eg h=4 gives 4 weeks of prediction). K is a coefficient in the Fourier series.
Nob_SO_6772.fit<-auto.arima(Nob_SO_6772.train.ts,xreg=fourier(Nob_SO_6772.train.ts, K=26), seasonal=FALSE)
plot(forecast(Nob_SO_6772.fit,h=16,xreg=fourierf(Nob_SO_6772.train.ts,K=26,h=16)))
Nob_SO_6772.test.ts=ts(Nob_SO_6772.test,freq=365.25/7,start=2013-1/365.25+134*7/365.25)
lines(Nob_SO_6772.test.ts,col="red")

##16 fitted values
Nob_SO_6722.testpred=as.vector(forecast(Nob_SO_6772.fit,h=16,xreg=fourierf(Nob_SO_6772.train.ts,K=26,h=16))$mean)


