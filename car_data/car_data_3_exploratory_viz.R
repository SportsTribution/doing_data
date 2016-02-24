### EXPLORATORY DATA ANALYSIS
dir.create("car_data/car_output/", showWarnings = FALSE)

  ## Let us first look at our two (more or less) numeric X values: Displacement and Cylinders
p<- ggplot(carData, aes(Displ,Cmb.MPG))+geom_point()
ggsave("car_data/car_output/exploratory_1_displ.png")
  
  ## For the Cylinders, let's go unneccesarily fancy, because we can
  ## We cannot use ggsave (I think), because ggsave would only save the last element
png("car_data/car_output/exploratory_2_cyl.png")
  ## scatter plot
scatter <- ggplot(data=carData,aes(Cyl,Cmb.MPG))+geom_point()
  ## Surrounding Histogram
p<-ggMarginal(scatter + theme_gray(), type = "histogram",
           fill = "steelblue", col = "darkblue",bins=100)
plot(p)
dev.off()

  ## Now for the categorical variables
catVar<-c("Trans","Drive","Fuel","Veh.Class","dataYear")
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
  ## we'll just use a foor loop
for (i in 1:length(catVar)){
    ## density plot (aes_string because we use the colnames as strings)
  p<- ggplot(carData, aes(Cmb.MPG))+geom_density(aes_string(fill=catVar[i]),alpha = 0.2,size=1.2)+
    geom_density(aes_string(colour=catVar[i]),alpha = 0.8,size=1.2)+
    scale_colour_manual(values=cbbPalette)+scale_fill_manual(values=cbbPalette)
  ggsave(paste("car_data/car_output/exploratory_",i+2,"_",catVar[i],"_Density.png",sep=""))
    
    ## for couriosity: Is there a difference between the city and highway?
    ## We use jitter, to better see overlapping points
  p<- ggplot(carData, aes(Hwy.MPG,City.MPG))+geom_jitter(aes_string(colour=catVar[i]),width = 0.5)+
    geom_smooth(aes_string(colour=catVar[i]))+
    scale_colour_manual(values=cbbPalette)
  ggsave(paste("car_data/car_output/exploratory_",i+2,"_",catVar[i],"_Scatter.png",sep=""))
}

  ## Wow, that's already a lot of information. Interesting to see that mpg slightly increased over the years
  ## (Must avoid VW joke...)
  ## Let's do some modeling