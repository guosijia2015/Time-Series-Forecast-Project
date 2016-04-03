# SalesOut model including historical and external GFK data


Ext_99895=external.t[,27]
SO_99895=N_SO_Mon_Dose[,c("99895")]
# Inv_99895=l[,c("99895")]
# SI_99895=N_SI_Mon_Dose[,c("99895")]
# Sku99895 = cbind(SO_99895,Ext_99895,Inv_99895,SI_99895)

library(TSA);library(forecast);library(xts)
Ext_99895.test=ts(Ext_99895[1:31],start=c(2013,1),frequency=12)
SO_99895.test=ts(SO_99895[1:31],start=c(2013,1),frequency=12)
SOExt_99895.test=ts.intersect(SO_99895.test,Ext_99895.test)
# dev.off()
# par(mar=rep(0,4))
# par(mfrow=c(2,1))
# plot(as.xts(SO_99895.test),type="o",major.format="%Y-%m",main="Monthly Sales Out (in Doses)")
# plot(as.xts(Ext_99895.test),type="o",major.format="%Y-%m",main="Monthly GFK (Dispensed in Doses)")
#plot(SOExt_99895.test,yax.flip=T,main="Monthly Sales Out and GFK Data (in Doses)")

#1.
# ignore covariate, use ARIMA for Sales Out
dev.off()
par(mar=rep(2,4))
tsdisplay(SO_99895.test)

SO_99895.arima = auto.arima(SO_99895.test,ic = "aicc", trace = TRUE)
summary(SO_99895.arima)
SOpredict = forecast(SO_99895.arima,h=4)
plot(SOpredict)
SOpred = as.matrix(SOpredict$mean)
Conversion<- convert[which(convert$Sku_Code=="99895"), ]$Dose
SOpred=SOpred/Conversion
SOpred

#2.Covariate as xreg
Ext_99895.arima = auto.arima(Ext_99895.test,ic = "aicc", trace = TRUE)
Extpredict = forecast(Ext_99895.arima,h=4)
plot(Extpredict)
Extpred = as.matrix(Extpredict$mean)

SO_99895.arimax <- auto.arima(SO_99895.test,xreg=Ext_99895.test,ic="aicc",trace=TRUE)
SO_arimax.predict = predict(SO_99895.arimax,4,newxreg=Extpred)
SO_arimax.pred = as.matrix(SO_arimax.predict$pred)
SO_arimax.pred = SO_arimax.pred/Conversion
SO_arimax.pred

# #3.Covariate as xreg
# 
# prewhiten(x=Ext_99895.test,y=SO_99895.test)
# abline(v=0,col=2,lty=3)
# 
# SO.ols=lm(SO_99895.test~Ext_99895.test)
# summary(SO.ols)
# 
# noise=ts(resid(SO.ols))
# tsdisplay(noise)
# noise.arimax <- auto.arima(noise,ic = "aicc", trace = TRUE)


library(xlsx)
write.xlsx(Sku99895,"C:/Users/angie/Desktop/Sku.xlsx")