DF.names <- names(DF)
dimDF <- dim(DF)

  ##Careful with Dates!

  ##variables can have different Type. Idea is to manually detect them
  ##Empty: all rows are null or NA
colIsEmpty <- sapply(DF, function(x) sum(is.na(x)|is.null(x))==dimDF[1])
  ##Key: Does probably not make sense to use them for plots
colIsKey <- substring(DF.names, 1 ,nchar("PK_"))== "PK_" |
  substring(DF.names, 1 ,nchar("FK_"))== "FK_" & !colIsEmpty
  ##Date: string is in a specific format
colIsDate <- sapply(DF, function(x) sum(is.na(as.Date(as.character(x),format="%Y-%m-%d")))==sum(is.na(x))) &
  !colIsEmpty & !colIsKey 
  ##Character Factors: Less than 201 unique strings
colIsFactorChar <- sapply(DF, function(x) length(unique(x))<=200) & sapply(DF, function(x) is.character(x)) & 
  !colIsEmpty & !colIsKey & !colIsDate
  ## Numerical Factor: Less than 20 unique factors (e.g. 1 or 0)
colIsFactorNum <- sapply(DF, function(x) length(unique(x))<=20) & sapply(DF, function(x) is.numeric(x)) & 
  !colIsEmpty & !colIsKey
  ## Number: Things like Kilometer or similar
colIsNum <- sapply(DF, function(x) is.numeric(x)) & !colIsEmpty & !colIsKey & !colIsFactorNum
  ## Strings: IPs or similar: could probably be later used in a wordcloud
colIsChar <- sapply(DF, function(x) is.character(x)) &
  !colIsEmpty & !colIsKey & !colIsFactorChar &!colIsDate

DFtmp<-sapply(DF[,colIsFactorNum|colIsFactorChar], function(x) as.factor(x))
DF[,colIsFactorNum|colIsFactorChar]<-DFtmp