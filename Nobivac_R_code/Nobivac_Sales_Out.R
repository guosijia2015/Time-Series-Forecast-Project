#Aggregate SalesOut data for each month in quantity

data<-read.csv(file="Nobivac&Activyl_Sales_2013-2015.csv",header=TRUE)
colnames(data)[colnames(data)=="PRODUCT_CODE"]<-"Sku_Code"
colnames(data)[colnames(data)=="PRODUCT_NAME"]<-"Sku_Name"
colnames(data)[colnames(data)=="PRODUCT_FAMILY"]<-"GPF_Desc"
data$Date<-as.Date(data$INVOICE_DATE,"%d%b%Y")
data$Month<-as.Date(cut(data$Date,breaks="month"))
data$Sku_Code<-as.factor(data$Sku_Code)
n = grep("NOBIVAC",data$Sku_Name)
Nobivacdata <- data[n,]
Nob_SO_Qty<-aggregate(Nobivacdata$QUANTITY,by=list(Nobivacdata$Month,Nobivacdata$Sku_Code),sum)
names(Nob_SO_Qty)<-c("Month","Sku_Code","Qty")
library(reshape)
Nob_SO_Qty<-cast(Nob_SO_Qty,Month~Sku_Code,sum,value="Qty")[1:35,]
excludeVars <- names(Nob_SO_Qty) %in% c("6886","26778","36204","37752","65299","107701")
Nob_SO_Qty<-Nob_SO_Qty[!excludeVars]

