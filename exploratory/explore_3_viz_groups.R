if (i==j){
  nbGroups <- length(unique(DF[,i]))
  p<-ggplot(DFfac,aes_string(fieldName))
  p<-p+geom_bar(width = 0.8)
  p <- p+geom_text(aes(y=..count.., label=..count..),
                   stat="count", color="black",
                   vjust=-1.0, size=2)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+
    ggtitle(paste("# of unique groups:",nbGroups))#+ 
  ggsave(paste("exploratory/Summary_",tableName,"/",i,"_",fieldName,"_INFO.png",sep=""),
         width = min(nFac+2,10),
         height = 5,
         dpi = 150)
  ggsave(paste(folderName,"/","0_",fieldName,"_INFO.png",sep=""),
         width = min(nFac+2,10),
         height = 5,
         dpi = 150)
} else if (colIsFactorChar[j] | colIsFactorNum[j] | colIsLogical[j]){
    mFac<-nlevels(DFfac[,j])
    DF.table<-table(DFfac[,c(i,j)])
    DF.table<-melt(DF.table)
    DF.table[,fieldName2]<-factor(DF.table[,fieldName2])
    c.scale <- scale_fill_gradient(low = "white", 
                                   high = "steelblue3",
                                   limits=c(0,
                                            max(DF.table$value)),
                                   trans = 'sqrt')
    p<-ggplot(DF.table, aes_string(fieldName, fieldName2)) + 
      geom_tile(aes(fill = value), colour="white") +
      geom_text(aes(fill = value,label= value),size=2)+
      theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
      ggtitle(paste(fieldName,fieldName2))
    p<-p + scale_fill_gradient(low = "white", 
                               high = "steelblue3",
                               trans = 'sqrt')
    ggsave(paste(folderName,"/",j,"_",fieldName,"_",fieldName2,"_INFO.png",sep=""),
           width = min(nFac+2,10),
           height = min(mFac+2,7),
           dpi = 150)
} else if (colIsNum[j]| colIsDate[j]){
  p<-ggplot(DFfac, aes_string(fieldName, fieldName2)) + 
    geom_violin(alpha=0.5)+ geom_jitter(height = 0,width=0.25,alpha=0.02)+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
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
         width = min(nFac+2,10),
         height = 6,
         dpi = 150)
}
