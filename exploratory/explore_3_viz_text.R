if (j==1){
  DFfieldText <-  paste(as.character(DFfac[,i]), collapse=" ")
  DFfieldText <- Corpus(VectorSource(DFfieldText))
  DFfieldText <- tm_map(DFfieldText,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')), mc.cores=1)
  DFfieldText <- tm_map(DFfieldText, content_transformer(tolower))
  DFfieldText <- tm_map(DFfieldText, content_transformer(removePunctuation), mc.cores=1)
  DFfieldText <- tm_map(DFfieldText, content_transformer(removeNumbers))
  DFfieldText <- tm_map(DFfieldText, content_transformer(stripWhitespace))
  
  # create term document matrix
  dtm <- DocumentTermMatrix(DFfieldText )
  dtm2 <- data.frame(as.matrix(dtm))
  frequency <- colSums(dtm2)
  frequency <- sort(frequency, decreasing=TRUE)
  frequency <- frequency[frequency>=20]
  if (length(frequency)>0){
    Top_freq <- data.frame(frequency=frequency[1:30])
    Top_freq[,"Top_Word"] <- rownames(Top_freq)
    p<-ggplot(Top_freq,aes(Top_Word,frequency))
    p<-p+geom_bar(stat="identity",width = 0.8)
    p <- p+geom_text(aes(y=frequency, label=frequency), 
                     color="black",
                     vjust=-1.0, size=2)+
      theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=0.5))+
      ggtitle(fieldName)#+ 
    ggsave(paste("exploratory/Summary_",tableName,"/",i,"_",fieldName,"_INFO.png",sep=""),
           width = 10,
           height = 5,
           dpi = 150)
    ggsave(paste(folderName,"/","0_",fieldName,"_INFO.png",sep=""),
           width = 10,
           height = 5,
           dpi = 150)
  }
}


if ((colIsFactorChar[j] | colIsFactorNum[j] | colIsLogical[j])& length(frequency)>0 ){
  
  LL.df <- NULL
  nameGroup<-levels(DFfac[,j])
  textInfo<-data.frame(matrix(data =0 , nrow = length(nameGroup)+1, ncol = length(frequency)+1))
  textLL<-data.frame(matrix(data =0 , nrow = length(nameGroup), ncol = length(frequency)))
  colnames(textInfo)<-c(names(frequency),"All_")
  rownames(textInfo)<-c(nameGroup,"All_")
  textInfo["All_",]<-c(frequency,sum(frequency))
  
  colnames(textLL)<-c(names(frequency))
  rownames(textLL)<-c(nameGroup)
  
  for (jGroup in nameGroup){
    DFfieldText <-  paste(as.character(DFfac[DFfac[,j]==jGroup,i]), collapse=" ")
    DFfieldText <- Corpus(VectorSource(DFfieldText))
    DFfieldText <- tm_map(DFfieldText,content_transformer(function(x) iconv(x, to='UTF-8', sub='byte')), mc.cores=1)
    DFfieldText <- tm_map(DFfieldText, content_transformer(tolower))
    DFfieldText <- tm_map(DFfieldText, content_transformer(removePunctuation), mc.cores=1)
    DFfieldText <- tm_map(DFfieldText, content_transformer(removeNumbers))
    DFfieldText <- tm_map(DFfieldText, content_transformer(stripWhitespace))
    
    dtm <- DocumentTermMatrix(DFfieldText )
    dtm2 <- data.frame(as.matrix(dtm))
    dtm2 <- dtm2[names(dtm2) %in% names(textInfo)]
    if (dim(dtm2)[2]>0){
    textInfo[jGroup,names(dtm2)]<-dtm2
    textInfo[jGroup,"All_"]<-sum(dtm2)
    speakerTMP<-textInfo[jGroup,1:length(frequency)]
    otherTMP<-textInfo["All_",1:length(frequency)]-speakerTMP
    speakerTMP[speakerTMP==0]<-0.0001
    otherTMP[otherTMP==0]<-0.0001
    
    E1 <- (textInfo[jGroup,"All_"]*textInfo["All_",1:length(frequency)])/textInfo["All_","All_"]
    E2 <- ((textInfo["All_","All_"]-textInfo[jGroup,"All_"])*
             textInfo["All_",1:length(frequency)])/textInfo["All_","All_"]
    textLL[jGroup,] <- 2*(speakerTMP*log(speakerTMP/E1) + otherTMP*log(otherTMP/E2))
    tmpLL <- data.frame(textLL[jGroup,])
    tmpLL[,E1>speakerTMP]<-0
    tmpLL[,tmpLL<10]<-0
    
    if (sum(tmpLL>0)>1){
      rankTMP<-order(tmpLL,decreasing = TRUE)
      
      tmpLL[,rankTMP[1:sum(tmpLL>0)]]<-1:sum(tmpLL>0)
      
      LLmelt<-melt(tmpLL[1,tmpLL>0 & tmpLL<21])
      LLmelt[,"Group"]<-jGroup
    } else if  (sum(tmpLL>0)==1){
      LLmelt <- data.frame(variable=names(tmpLL)[tmpLL>0],value=1,Group=jGroup)
    } else {
      LLmelt <- data.frame(variable="No significant text",value=0,Group=jGroup)
    }
    } else{
      LLmelt <- data.frame(variable="No significant text",value=0,Group=jGroup)
    }
    LL.df<-rbind(LL.df,LLmelt)
    #     row <- data.frame(speaker, word, word.total, speaker.total, speaker.word, E1, E2, LL)
    #     LL.df <- rbind(LL.df, row)
    
  }
  names(LL.df)[2:3]<-c("rank",fieldName2)
  
  ggplot(LL.df,aes_string(fieldName2,"rank"))+ 
    geom_point(color="white")+ 
    geom_label(aes_string(label="variable",fill=fieldName2), color='white', fontface='bold', size=2)+ guides(fill=FALSE)
  ggsave(paste(folderName,"/",j,"_",fieldName,"_",fieldName2,"_INFO.png",sep=""),
         width = 8,
         height = 4.5,
         dpi = 150) 
  
}
