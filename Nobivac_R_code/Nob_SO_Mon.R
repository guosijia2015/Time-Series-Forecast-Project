#Aggregate SalesOut data for each month and convert quantity to doses.

data<-read.csv(file="Nobivac&Activyl_Sales_2013-2015.csv",header=TRUE)
colnames(data)[colnames(data)=="PRODUCT_CODE"]<-"Sku_Code"
colnames(data)[colnames(data)=="PRODUCT_NAME"]<-"Sku_Name"
colnames(data)[colnames(data)=="PRODUCT_FAMILY"]<-"GPF_Desc"
data$Date<-as.Date(data$INVOICE_DATE,"%d%b%Y")
data$Month<-as.Date(cut(data$Date,breaks="month"))
data$Week<-as.Date(cut(data$Date,breaks="week",start.on.monday=TRUE))
data$Sku_Code<-as.factor(data$Sku_Code)
n = grep("NOBIVAC",data$Sku_Name)
Nobivacdata <- data[n,]
Nob_SO_Month<-aggregate(Nobivacdata$QUANTITY,by=list(Nobivacdata$Month,Nobivacdata$Sku_Code),sum)
names(Nob_SO_Month)<-c("Month","Sku_Code","Quantity")
Nob_SO_Month<-Nob_SO_Month[which(Nob_SO_Month$Sku_Code!="6886"&Nob_SO_Month$Sku_Code!="26778"&Nob_SO_Month$Sku_Code!="36204"
                           &Nob_SO_Month$Sku_Code!="37752"&Nob_SO_Month$Sku_Code!="65299"&Nob_SO_Month$Sku_Code!="65299"&Nob_SO_Month$Sku_Code!="107701"),]

convert<-read.csv(file="DosageConversion.csv",header=TRUE)
N_SO_Mon_Dose<-merge(Nob_SO_Month,convert,by="Sku_Code")
N_SO_Mon_Dose$Doses<-N_SO_Mon_Dose$Quantity*N_SO_Mon_Dose$Dose
exVars <- names(N_SO_Mon_Dose) %in% c("Quantity","Sku_Name","Size","Dose")
N_SO_Mon_Dose<-N_SO_Mon_Dose[!exVars]
library(reshape)
N_SO_Mon_Dose<-cast(N_SO_Mon_Dose,Month~Sku_Code,sum,value="Doses")[(1:35),]




