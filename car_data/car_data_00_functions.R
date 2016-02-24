## use list (allows multiple output variables) (I don't exactly know how it works)
list <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
  args <- as.list(match.call())
  args <- args[-c(1:2,length(args))]
  length(value) <- length(args)
  for(i in seq(along=args)) {
    a <- args[[i]]
    if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
  }
}



## This function does k-crossvalidation of a random forest
kCrossVal <- function (kCross,dataSet,gId, ... ){
  ## dots is probably not necessary in this slimmer version
  dots <- list(...)
  nbId <- length(gId)
  ## split trainingsSet in kCross parts
  ## sort data in a way that subgroups are equally distributed 
  ## this works because gId is an integer
  gIdRand <- gId + runif(nbId)
  orderG <- order(gIdRand)
  gId <- gId[orderG]
  dataSet <- dataSet[orderG,]
  pId <- vector(mode="integer", length=nbId)
  ## crossG contains indexes of our k groups
  crossG <- (seq(1,nbId)%%kCross)+1
  gLabel <- sort(unique(gId))
  ## Parallel computing
  nbCoresUsed <- max(ceiling(detectCores()/2),min(detectCores() - 1,3))
  cl <- makeCluster(nbCoresUsed)
  registerDoParallel(cl)
  print(paste("using", nbCoresUsed, "Cores"))
  
  ## gives us only good filters as output
  topFilters<-c()
  for (ik in 1:kCross){   
    print(paste("k is",ik))
    trainId <- gId[crossG != ik]
    trainData <- dataSet[crossG != ik,]
    testData <- dataSet[crossG == ik,]
    treeInfo <- doRandomForest(as.matrix(testData),trainData=as.matrix(trainData),trainId=trainId)
    pId[crossG == ik] <- gLabel[treeInfo$pId]
    importance <- treeInfo$importance
    importance <- importance[order(importance[,'MeanDecreaseAccuracy'],decreasing=TRUE),]
    print(importance[,'MeanDecreaseAccuracy'])
    topFilters<-append(topFilters,rownames(importance[1:min(100,dim(trainData)[2]),]))
  }
  stopCluster(cl)
  topFilters<-unique(topFilters)
  #   
  pId <- pId[order(orderG)]
  return(list(pId,topFilters))
  
}

  ## parallelizable random Forrest
doRandomForest <- function (testData, trainData, trainId) {
  clickTree <- foreach(ntree=rep(50, 8), .combine=combine, .packages='randomForest') %dopar% {
    randomForest(trainData,as.factor(trainId), ntree=ntree, importance=TRUE, do.trace=1000)
  }
  predVal <- predict(clickTree, testData, type="response",
                     norm.votes=TRUE, predict.all=FALSE, proximity=FALSE, nodes=FALSE)
  treeInfo <- list(pId = predVal,importance = clickTree$importance)
  return(treeInfo)
}