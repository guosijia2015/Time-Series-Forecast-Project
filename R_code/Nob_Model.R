                                  ## Dynamic regression to MODEL SALES IN of NOBIVAC Based on SALES out prediction

## Dynamic regression for 6772 
library(dynlm)
library(MASS)
error=rep(NA,16)
SO.total=c(Nob_SO_6772.train,Nob_SO_6722.testpred)

#Loop to compute the error of models with different lags
for (i in (17:32))
{ 
  Model.6772<-dynlm(Nob_SI_6772.train.ts~L(Nob_SO_6772.train.ts,1:i))
  C_Week=coredata(Model.6772$coefficients)
  Predict_Week = rep(NA,152)
  for(m in (137:152)){
    Predict_Week[m] = C_Week[1]+sum(C_Week[2:(i+1)]*rev(SO.total[1:m-1])[1:i])
  }
  error[i-16]=sqrt(mean((Predict_Week[-(1:136)]-Nob_SI_6772.test)^2))
}

## Choose the model with the lowest validation error
error

## Model wiht 20 lags is the best
Model.6772<-dynlm(Nob_SI_6772.train.ts~L(Nob_SO_6772.train.ts,1:20))
C_Week=coredata(Model.6772$coefficients)
Predict_Week = rep(NA,152)

# train
for(m in (21:136)){
  Predict_Week[m] = C_Week[1]+sum(C_Week[2:21]*rev(Nob_SO_6772.train[1:m-1])[1:20])
}

# validation
for(m in (136:152)){
  Predict_Week[m] = C_Week[1]+sum(C_Week[2:21]*rev(SO.total[1:m-1])[1:20])
}

#E stands for error
E=sum(abs(Predict_Week[-(1:136)]-Nob_SI_6772.test))/sum(Nob_SI_6772.test)

## Plot for training and validation sets
plot.ts(Nob_SI_6772,xlab="Week",ylab="Sales In Qty",main="Weekly Sales In Forecast for Nobivac Sku 6772")
lines(c(rep(NA,20),Predict_Week[21:136]),col=3)
lines(c(rep(NA,136),Predict_Week[136:152]),col=2)
legend(x=1,y=3200,legend=c("Actual","Training Fit","Validation"),cex=0.5,pt.cex=1,
       col=c("black","green","red"),pch=c(NA,NA,NA),lty=c(1,1,1))

