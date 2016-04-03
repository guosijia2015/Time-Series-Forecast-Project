#upload GFK data into R 

external<-read.csv(file="External.csv",header=TRUE)
monseq = seq(as.Date("2012-12-01"), by = "month", length.out = 36)
names(external)<-monseq
colnames(external)[colnames(external)=="2012-12-01"]<-NA
external.t<-t(external[,2:ncol(external)])
colnames(external.t)<-external[,1]


