# Inventory data for each sku each month

# for each sku, sum the average inventory of each warehouse


inventory = read.csv("Inventory_Dist.csv",header = TRUE)
inventory$INV_DT = as.Date(inventory$INV_DT,"%m/%d/%Y")
inventory$Month<-as.Date(cut(inventory$INV_DT,breaks="month"))
inventory$Day<-format(inventory$INV_DT,"%d")
inv_frame=data.frame(matrix(c(0:0),nrow = 35,ncol = 31))
dateSequence = seq(as.Date("2013/01/01"), by = "month", length.out = 35)
warehouse = unique(inventory$DIST_DESCRIPTION)
skuCode = unique(convert$Sku_Code)
rownames(inv_frame) <- dateSequence
colnames(inv_frame) <- skuCode



Sku<-list()
for (k in (1:length(skuCode)))
{
  Sku[[k]]=inventory[grep(skuCode[k],inventory$MFGPARTNUMBER),]
  
  Sku_Month<-list()
  Sku_Avg_Inv = rep(0,length(dateSequence))
  for (j in (1:length(dateSequence)))
  {
    Sku_Month[[j]]=Sku[[k]][grep(dateSequence[j],Sku[[k]]$Month),]
    
    Sku_Dis<-list()
    uniqueWarehouse=unique(Sku_Month[[j]]$DIST_DESCRIPTION)
    temp=rep(0,length(uniqueWarehouse))
    for (i in (1:length(uniqueWarehouse)))
    {
      Sku_Dis[[i]]=Sku_Month[[j]][grep(uniqueWarehouse[i],Sku_Month[[j]]$DIST_DESCRIPTION),]
      temp[i]=(Sku_Dis[[i]]$SUM_QUANTITY_[which.min(Sku_Dis[[i]]$Day)]+Sku_Dis[[i]]$SUM_QUANTITY_[which.max(Sku_Dis[[i]]$Day)])/2
    }
    Sku_Avg_Inv[j]=sum(temp)
    inv_frame[,k]=Sku_Avg_Inv
  }
}
