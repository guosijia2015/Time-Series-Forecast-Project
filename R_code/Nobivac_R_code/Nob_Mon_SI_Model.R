# SalesIn model including historical, sales out and inventory



SO_6772=Nob_SO_Qty[,c("6772")]
Inv_6772=inv_frame[,c("6772")]
SI_6772=Nob_SI_Qty[,c("6772")]
Sku6772 = cbind(SO_6772,Inv_6772,SI_6772)

library(TSA);library(forecast);library(xts)
SO_6772qty.test=ts(SO_6772[1:31],start=c(2013,1),frequency=12)
Inv_6772qty.test=ts(Inv_6772[1:31],start=c(2013,1),frequency=12)
SI_6772qty.test=ts(SI_6772[1:31],start=c(2013,1),frequency=12)
Combined_6772.test=ts.intersect(SO_6772qty.test,Inv_6772qty.test,SI_6772qty.test)
# dev.off()
# par(mar=rep(0,4))
# par(mfrow=c(2,1))
plot(as.xts(SO_6772qty.test),type="o",major.format="%Y-%m",main="Monthly Sales Out (in Quantity)")
plot(as.xts(Inv_6772qty.test),type="o",major.format="%Y-%m",main="Monthly Average Inventory (in Quantity)")
plot(as.xts(SI_6772qty.test),type="o",major.format="%Y-%m",main="Monthly Sales In (in Quantity)")

#1. ignore covariate, use ARIMA for Sales In
# dev.off()
# par(mar=rep(2,4))
# tsdisplay(SO_6772.test)

SI_6772.arima = auto.arima(SI_6772qty.test,ic = "aicc", trace = TRUE)
summary(SI_6772.arima)
SIpredict = forecast(SI_6772.arima,h=4)
plot(SIpredict)
SIpred = as.matrix(SIpredict$mean)
SIpred

#2.Covariate Sales Out and Inventory as xreg
Inv_6772.arima = auto.arima(Inv_6772qty.test,ic = "aicc", trace = TRUE)
Invpredict = forecast(Inv_6772.arima,h=4)
plot(Invpredict)
Invpred = as.matrix(Invpredict$mean)

SI_6772.arimax <- auto.arima(SI_6772qty.test,xreg=cbind(SO_6772qty.test,Inv_6772qty.test),ic="aicc",trace=TRUE)
SI_arimax.predict = predict(SI_6772.arimax,4,newxreg=cbind(SO_best[,c("6772")],Invpred))
SI_arimax.pred = as.matrix(SI_arimax.predict$pred)
SI_arimax.pred

# #3.Covariate as xreg
# 
# prewhiten(x=Ext_6772.test,y=SO_6772.test)
# abline(v=0,col=2,lty=3)
# 
# SO.ols=lm(SO_6772.test~Ext_6772.test)
# summary(SO.ols)
# 
# noise=ts(resid(SO.ols))
# tsdisplay(noise)
# noise.arimax <- auto.arima(noise,ic = "aicc", trace = TRUE)


library(xlsx)
write.xlsx(Sku6772,"C:/Users/angie/Desktop/Sku.xlsx")