

Nob_SI<-read.csv(file="Salesin_Nobivac.csv",header=TRUE)
colnames(Nob_SI)[colnames(Nob_SI)=="UIN"]<-"Sku_Code"
colnames(Nob_SI)[colnames(Nob_SI)=="UIN_DESCRIPTION"]<-"Sku_Name"
Nob_SI$Date<-as.Date(Nob_SI$INVOICE_DATE,"%d%b%Y")
Nob_SI$Month<-as.Date(cut(Nob_SI$Date,breaks="month"))
Nob_SI$Sku_Code<-as.factor(Nob_SI$Sku_Code)
Nob_SI_Month<-aggregate(Nob_SI$QTY,by=list(Nob_SI$Month,Nob_SI$Sku_Code),sum)
names(Nob_SI_Month)<-c("Month","Sku_Code","Quantity")
Nob_SI_Month<-Nob_SI_Month[which(Nob_SI_Month$Sku_Code!="6886"&Nob_SI_Month$Sku_Code!="26778"&Nob_SI_Month$Sku_Code!="36204"
                           &Nob_SI_Month$Sku_Code!="37752"&Nob_SI_Month$Sku_Code!="65299"&Nob_SI_Month$Sku_Code!="107701"),]


convert<-read.csv(file="DosageConversion.csv",header=TRUE)
N_SI_Mon_Dose<-merge(Nob_SI_Month,convert,by="Sku_Code")
N_SI_Mon_Dose$Doses<-N_SI_Mon_Dose$Quantity*N_SI_Mon_Dose$Dose
exVars <- names(N_SI_Mon_Dose) %in% c("Quantity","Sku_Name","Size","Dose")
N_SI_Mon_Dose<-N_SI_Mon_Dose[!exVars]
library(reshape)
N_SI_Mon_Dose<-cast(N_SI_Mon_Dose,Month~Sku_Code,sum,value="Doses")[(1:35),]
