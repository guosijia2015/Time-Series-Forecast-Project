

## Load the Sales In data for NOBIVAC
Nob_SI<-read.csv(file="Salesin_Nobivac.csv",header=TRUE)

## Change the column names
colnames(Nob_SI)[colnames(Nob_SI)=="UIN"]<-"Sku_Code"
colnames(Nob_SI)[colnames(Nob_SI)=="UIN_DESCRIPTION"]<-"Sku_Name"

## Change the format as DATE in Month and Week
Nob_SI$Date<-as.Date(Nob_SI$INVOICE_DATE,"%d%b%Y")
Nob_SI$Week<-as.Date(cut(Nob_SI$Date,breaks="week",start.on.monday=TRUE))

#Sku code as factor
Nob_SI$Sku_Code<-as.factor(Nob_SI$Sku_Code)

## Aggregate quantity on a weekly basis
Nob_SI_Qty.wk<-aggregate(Nob_SI$QTY,by=list(Nob_SI$Week,Nob_SI$Sku_Code),sum)
names(Nob_SI_Qty.wk)<-c("Week","Sku_Code","Qty")
library(reshape)
Nob_SI_Qty.wk<-cast(Nob_SI_Qty.wk,Week~Sku_Code,sum,value="Qty")

## Plot Weekly Sales In for all Nobivac Skus
Nob_SI_wk.mat<-as.matrix(Nob_SI_Qty.wk)[(1:152),]
library(zoo)
z.Nob_SI_wk<-zoo(Nob_SI_wk.mat,Nob_SI_Qty.wk$Week)
dev.off()
library(lattice)
xyplot(z.Nob_SI_wk,type="l",main="Weekly Sales In Qty for Nobivac Skus from 2012-12-31 to 2015-12-28",
       strip = function(bg,...) 
         strip.default(bg='white',...))
       #ylim=range(z.Nob_SI_wk, na.rm=TRUE))

# Partition training and validation sets for Nobivac 6772
Nob_SI_6772 = Nob_SI_Qty.wk[,2]
Nob_SI_6772.train = Nob_SI_6772[1:136]
Nob_SI_6772.train.ts = ts(Nob_SI_6772.train,freq=365.25/7,start=2013-1/365.25)
Nob_SI_6772.test = Nob_SI_6772[137:152]




