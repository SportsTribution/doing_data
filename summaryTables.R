#### mark small groups as Other
subCont1D <- function(DF,maxRows=20,order.fac=TRUE){
  nbDim <- dim(DF)[2]
  for (i in 1:nbDim){
    iTable1<-data.frame(table(DF[,i]))
    iTable<-iTable1[order(iTable1$Freq,decreasing=TRUE),]
    if (dim(iTable)[1]>maxRows){
      minEl<-iTable$Freq[maxRows]
      otherValues<-iTable[iTable$Freq<=minEl,1]
      if (order.fac){
        inValues<-as.character(iTable[iTable$Freq>minEl,1])
      }else{
        inValues<-as.character(iTable1[iTable1$Freq>minEl,1])
      }
      DF[,i]<-as.character(DF[,i])
      DF[,i]<-ifelse(DF[,i] %in% otherValues,"Other_Fac",DF[,i])
      isNull<- is.na(DF[,i])|is.null(DF[,i])
      if (sum(isNull)>0){
        DF[isNull,i]<-"NULL"
        DF[,i]<-factor(DF[,i])
        DF[,i]<-gdata::reorder.factor(DF[,i],new.order=c(inValues,"NULL","Other_Fac"))
      } else{
        DF[,i]<-factor(DF[,i])
        DF[,i]<-gdata::reorder.factor(DF[,i],new.order=c(inValues,"Other_Fac"))
      }
    } else {
      if (order.fac){
        inValues<-as.character(iTable[,1])
      }else{
        inValues<-as.character(iTable1[,1])
      }
      DF[,i]<-as.character(DF[,i])
      isNull<- is.na(DF[,i])|is.null(DF[,i])
      if (sum(isNull)>0){
        DF[isNull,i]<-"NULL"
        DF[,i]<-factor(DF[,i])
        DF[,i]<-gdata::reorder.factor(DF[,i],new.order=c(inValues,"NULL"))
      } else{
        DF[,i]<-factor(DF[,i])
        DF[,i]<-gdata::reorder.factor(DF[,i],new.order=c(inValues))
      }
    }
  }
  return(DF)
}











