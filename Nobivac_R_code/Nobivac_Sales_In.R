#Aggregate SalesIn data for each month in quantity

Nob_SI<-read.csv(file="Salesin_Nobivac.csv",header=TRUE)
colnames(Nob_SI)[colnames(Nob_SI)=="UIN"]<-"Sku_Code"
colnames(Nob_SI)[colnames(Nob_SI)=="UIN_DESCRIPTION"]<-"Sku_Name"
Nob_SI$Date<-as.Date(Nob_SI$INVOICE_DATE,"%d%b%Y")
#Nob_SI$Week<-as.Date(cut(Nob_SI$Date,breaks="week",start.on.monday=TRUE))
Nob_SI$Month<-as.Date(cut(Nob_SI$Date,breaks="month"))
Nob_SI$Sku_Code<-as.factor(Nob_SI$Sku_Code)
Nob_SI_Qty<-aggregate(Nob_SI$QTY,by=list(Nob_SI$Month,Nob_SI$Sku_Code),sum)
names(Nob_SI_Qty)<-c("Month","Sku_Code","Qty")
library(reshape)
Nob_SI_Qty<-cast(Nob_SI_Qty,Month~Sku_Code,sum,value="Qty")[1:35,]
excludeVars <- names(Nob_SI_Qty) %in% c("6886","26778","36204","37752","65299")
Nob_SI_Qty<-Nob_SI_Qty[!excludeVars]




