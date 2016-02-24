source("heatmap_JB.R")
# dev.off()

### General info about NAs
dir.create(paste("exploratory/Summary_",tableName,sep=""), showWarnings = FALSE)

na.DF<-is.na(DF)+is.null(DF)
nbRows<-dim(na.DF)[1]
nbCols<-dim(na.DF)[2]
percentEmpty<-data.frame(colSums(na.DF)/nbRows,table.Field=colnames(DF))
colnames(percentEmpty)<-c("percent.Empty","table.Field")

topNAs<-rownames(percentEmpty[order(percentEmpty$percent.Empty,decreasing = TRUE),])

fieldsWithEmpty <- sum(percentEmpty$percent.Empty>0)
topX<-min(50,fieldsWithEmpty)

DFfac<-DF
if (sum(colIsFactorChar)>0){
  DFtmp<-subCont1D(data.frame(DF[,colIsFactorChar]),25)
  DFfac[,colIsFactorChar]<-DFtmp
}
if (sum(colIsFactorNum + colIsLogical)>0){
DFtmp<-subCont1D(data.frame(DFfac[,colIsFactorNum + colIsLogical>0]),25,order.fac=FALSE)
DFfac[,colIsFactorNum + colIsLogical>0]<-DFtmp
}
if (sum(colIsDate)>0){
DFtmp<-data.frame(DFfac[,colIsDate])
for (i in 1:dim(DFtmp)[2]){
  DFtmp[,i]<-as.Date(DFtmp[,i])
}
DFfac[,colIsDate]<-DFtmp
}


p<-ggplot(percentEmpty[topNAs[1:topX],],aes(table.Field,percent.Empty))+geom_bar(stat="identity")+ 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))+ scale_y_continuous(labels = scales::percent) +
  ggtitle(paste(topX,"fields with highest amount of NULL values\nOverall",fieldsWithEmpty,"fields have NULL values"))+ 
  coord_cartesian(ylim = c(0, 1))+geom_text(aes(y=percent.Empty, label=round(percent.Empty,4)*100),
                                            color="black",
                                            vjust=-1.0, size=2)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))
ggsave(paste("exploratory/Summary_",tableName,"/0_NULL_1_INFO.png",sep=""),
       width = 27, height = 21, units = c("cm") ,dpi=300)

  ##use up to 2'000 random rows for a heatmap

rowSelect <- sample(1:nbRows, min(2000,nbRows),replace=FALSE)
if (sum(na.DF[rowSelect,]*1)>0){
  prepareHeatmapJB(na.DF[rowSelect,]*1,save_name = paste("exploratory/Summary_",tableName,"/0_NULL_2_HEATMAP",sep=""),
                   col_norm=F,title_name = "NAs", key_name = "NOT NA | NA", logo ="logo.png", rowsep = F)
}

subFolder<-c("Gruppe","NummerGruppe","Nummer","Datum","Text")
for (i in 1:nbCols){
  fieldName <- DF.names[i]
  print(fieldName)
  folderName <- subFolder[1*colIsFactorChar[i] + 2*colIsFactorNum[i] + 3*colIsNum[i] + 4*colIsDate[i] + 5*colIsChar[i]]
  folderName <- paste("exploratory/Summary_",tableName,"/",folderName ,"/",fieldName,sep="")
  nFac<-nlevels(DFfac[,i])
  if (colIsFactorChar[i] | colIsFactorNum[i] | colIsNum[i] | colIsDate[i] | colIsChar[i]){
    dir.create(folderName, showWarnings = FALSE,recursive = TRUE)
  }
  for (j in 1:nbCols){
    fieldName2 <- DF.names[j]
    if (colIsFactorChar[i] | colIsFactorNum[i] | colIsLogical[i]){
      source("exploratory/explore_3_viz_groups.R")
    } else if (colIsNum[i] | colIsDate[i]){
      source("exploratory/explore_3_viz_numbers.R")
    } else if (colIsChar[i]){
      source("exploratory/explore_3_viz_text.R")
    }
  }
}