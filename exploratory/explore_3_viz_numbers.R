if (j==1){
  p<-ggplot(DFfac,aes_string(fieldName)) + geom_density(fill="white",adjust=0.25,alpha=0.5)
  if (colIsDate[i]){
    diffT<-difftime(max(DFfac[,fieldName],na.rm=TRUE),min(DFfac[,fieldName],na.rm=TRUE),units="secs")
    if (diffT<60*60){
      tString<-"%m"
      minorBreaks <- NULL
    } else if (diffT<60*60*24){
      tString<-"%h"
      minorBreaks <- "30 minutes"
    } else if (diffT<60*60*24*7){
      tString<-"%d %b %y"
      minorBreaks <- "12 hours"
    } else{
      tString<-"%b %y"
      minorBreaks <- "1 weeks"
    }
    xlim1 = quantile(as.numeric(DFfac[,fieldName]),c(0.002, 0.998),na.rm=TRUE)
    p<-p+scale_x_date(date_labels = tString, date_minor_breaks = minorBreaks,
                      limits=as.Date(xlim1+ c(-0.05, 0.05) * diff(xlim1) / 2))
  } else {
    xlim1 = quantile(DFfac[,fieldName],c(0.002, 0.998),na.rm=TRUE)
    p <- p + xlim(xlim1+ c(-0.05, 0.05) * diff(xlim1) / 2)
  }
  ggsave(paste("exploratory/Summary_",tableName,"/",i,"_",fieldName,"_INFO.png",sep=""),
         width = 6,
         height = 5,
         dpi = 150)
  ggsave(paste(folderName,"/","0_",fieldName,"_INFO.png",sep=""),
         width = 6,
         height = 5,
         dpi = 150)
  
}
if ((colIsNum[j] | colIsDate[j]) & i!=j){
  p<-ggplot(DFfac, aes_string(fieldName, fieldName2)) +
    geom_jitter(height = 0.25,width=0.25,alpha=0.05,shape=19)+
    geom_smooth()
  if (colIsDate[j]){
    diffT<-difftime(max(DFfac[,fieldName2],na.rm=TRUE),min(DFfac[,fieldName2],na.rm=TRUE),units="secs")
    if (diffT<60*60){
      tString<-"%m"
    } else if (diffT<60*60*24){
      tString<-"%h"
    } else if (diffT<60*60*24*7){
      tString<-"%d %b %y"
    } else{
      tString<-"%b %y"
    }
    ylim1 = quantile(as.numeric(DFfac[,fieldName2]),c(0.002, 0.998),na.rm=TRUE)
    p<-p+scale_y_date(date_labels = tString,
                      limits=as.Date(ylim1+ c(-0.05, 0.05) * diff(ylim1) / 2))
  } else {
    ylim1 = quantile(DFfac[,fieldName2],c(0.002, 0.998),na.rm=TRUE)
    p <- p + ylim(ylim1+ c(-0.05, 0.05) * diff(ylim1) / 2)
  }
  ggsave(paste(folderName,"/",j,"_",fieldName,"_",fieldName2,"_INFO.png",sep=""),
         width = 8,
         height = 6,
         dpi = 150)        
}
