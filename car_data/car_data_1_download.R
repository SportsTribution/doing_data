## DOWNLOAD THE DATA
  ## paste concatenates all following commands, sprintf is for leading zeros
  ## try makes sure that it doesn#t crash if a file is missing
for (i in 9:16){
try(download.file(paste("http://www.fueleconomy.gov/feg/EPAGreenGuide/txt/all_alpha_",sprintf("%02d", i),".txt",sep=""),
                  paste("car_data/data/all_alpha_",sprintf("%02d", i),".txt",sep="")))}