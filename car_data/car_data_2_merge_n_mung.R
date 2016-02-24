### GET THE DATA INTO R
  ## start with an empty data.frame
carData <- data.frame()
for (i in 9:16){
  ## read car data. The data is seperated by tab's and has a header.
  ## We will first load the data as strings and not factors
  ## as there are some problems with the data. (You'll see)
  carTMP <- data.frame()
  try(carTMP <- read.table(paste("car_data/data/all_alpha_",sprintf("%02d", i),".txt",sep=""),
                           sep = "\t",header = TRUE,stringsAsFactors=FALSE))
  ## We have columns that we don't need and that also lead to trouble, 
  ## so we delete them before they start creating trouble
  ## Example: In 2016, Sales.Area got renamed to Cert.Region
  ## The Code: Select all columns of carTMP for which the column names are not %in% the vector
  carTMP <- carTMP[, !names(carTMP) %in% c("Sales.Area","Stnd","Stnd.Description","Underhood.ID","SmartWay","Cert.Region")]
  ## also: let us add a year to the information, to see if there are changes between years
  carTMP$dataYear<-sprintf("%02d", i)
  if (length(carData)==0){
    carData <- carTMP
    ## The first year defines which columns we have
    carNames <- names(carData)
  } else {
    ## Delete unneccesary columns and bind the new data to the bottom
    carTMP <- carTMP[,carNames]
    carData <- rbind(carData,carTMP)
  }
}

  ## delete the unused data
rm(carTMP)


### CLEAN THE DATA
  ## First problem: we have hybrids that give us a "mixed message" for MPG (e.g. 29/66)
  ## Those cars contain a "/" in their Fuel colum. We delete those rows
areHybrids <- grep("/",carData$Fuel,value=FALSE)
carData <- carData[-areHybrids,]
  ## Sidenote: grep gives us the indexes of the rows, so we afterwards need to use "-"
  ## the %in% we used earlier gives us a true/false vector, so we need to use "!"

  ## now we turn MPG data from factors to numbers (lapply applies as.numeric column by column)
  ## we would get a warning: "In lapply(mpgData, as.numeric) : NAs introduced by coercion"
  ## Because some data points are NA. We turn the warning off shortly
options(warn=-1)
mpgData<-carData[,grep("MPG",names(carData))]
carData[,grep("MPG",names(carData))] <- lapply(mpgData,as.numeric)
rm(mpgData)

  ## turn Displacement etc. data to numbers (comma need to be period! Done by gsub while it is still a string)
carData$Displ <- as.numeric(gsub(",",".",carData$Displ))
carData$Cyl <- as.numeric(carData$Cyl)
carData$Air.Pollution.Score <- as.numeric(carData$Air.Pollution.Score)
carData$Greenhouse.Gas.Score <- as.numeric(carData$Greenhouse.Gas.Score)
options(warn=0)


###SAVE EXTRA FOR EXPLORATORY THINGS
write.table(carData,"car_data/data/car_data_exploratory.txt",
            sep = "\t",col.names=TRUE,row.names=FALSE)

  ## We have NA data, either because the information is missing or because electric cars don't have displacement
  ## let us just remove all numeric rows with missing data
  ## Note: While lapply gave us a list, sapply gives us a vector
isNumeric <- sapply(carData, is.numeric)
rowHasNaNs <- rowSums(is.na(carData))>0
carData <- carData[!rowHasNaNs,]
  ## Note: Of course we also deleted all electric cars. It's probably anyways weird to compare them 


  ## We still have duplicates from having several repeats
  ## The idea is as follows: We will later have our responses (the Y's)
  ## and our tests (the X's in a regression model)
  ## We take all unique combinations of X's and average the Y's
  ## Y's: everything that is somehow eco related
responses<-c("Air.Pollution.Score","City.MPG","Hwy.MPG","Cmb.MPG","Greenhouse.Gas.Score")
  ## X's: everything else that is not the Model
tests<-names(carData)[!(names(carData) %in% c(responses,"Model"))]
carData <- aggregate(carData[,responses], carData[,c("Model",tests)],  FUN = mean )
  ## Note: data.table way would be something like DT[, lapply(.SD,sum), by=tests]


  ## Time for some information. First about the X's
for (i in tests){
  print(i)
  print(table(carData[,i]))
}

  ## OK, we have a few problems.
  ## A) If we want to use Fuel Type, then we cannot use CNG and Ethanol, as there are not enough cars
carData <- carData[!carData$Fuel %in% c("CNG","Ethanol"),]
  ## Plus, there seems to be a year, where Diesel became diesel for a few cars
carData$Fuel <- revalue(carData$Fuel, c("diesel"="Diesel"))

  ## B) we have way too many different groups in Trans. 
  ## Let us focus on transition types (AMS, Auto, AutoMan, CVT, Man, SCV & SemiAuto)
  ## the sub command removes the number of gears and the "-"
carData$Trans<-sub('-.*', '', carData$Trans)
  ## I have no idea what "Other" Transition is, so we get rid of it
carData <- carData[!carData$Trans %in% c("Other"),]

  ## Also, we don't need the car information as detailed as it is
carData$Veh.Class <- revalue(carData$Veh.Class, c("large car"="large",
                                                  "midsize car"="medium",
                                                  "small car"="small",
                                                  "small SUV"="SUV",
                                                  "standard SUV"="SUV",
                                                  "station wagon"="large",
                                                  "special purpose"="large",
                                                  "minivan" = "van"
                                                  ))
  ## I think that's better
