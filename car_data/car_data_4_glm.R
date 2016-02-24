### REGRESSION MODEL
  ## Let us rename ur data frame, so that we have the "original" one still handy
carDataGLM <- carData
  ## one important Problem: We have to take away one Factor from each Class, otherwise the glm will complain

  ## the following turns all character´information into factors by turning the data frame into an unclassed list and back
carDataGLM<-as.data.frame(unclass(carDataGLM))

  ## we do not need the Model Name for the glm
carDataGLM<-carDataGLM[,!(colnames(carData)%in% "Model")]
  
  ## now we turn each factor into dummy variables
isFactor <- sapply(carDataGLM, is.factor)
carDataGLM<-as.data.frame(model.matrix(~ .+1, data=carDataGLM, 
             contrasts.arg = lapply(carDataGLM[,isFactor], contrasts, contrasts=TRUE)))

  ## You will see that one group dissappeared from each Factor and we have the group intercept instead
print("***Model with Factors***")
print(names(carDataGLM))
  ## To get the as.formula to work, the intercept needs to be renamed
colnames(carDataGLM)[1]<-"Intercept"

  ##So, we can almost start to glm around.
  ##Smog, the mpg columns and the greenhouse column is our output.
  ##Solution: get only the valid terms and the valid responses
responses<-c("Air.Pollution.Score","City.MPG","Hwy.MPG","Cmb.MPG","Greenhouse.Gas.Score")
tests<-names(carDataGLM)[!(names(carDataGLM) %in% c(responses))]
  ##I love the %in% command. 

  ##Now we can use this as formula command
  ##let's start a test with Cmb.MPG
frm <- as.formula(paste("Cmb.MPG"," ~", paste(tests,collapse ="+"),"-1"))
myglm.All.Lin <- glm(formula = frm, family = gaussian(link = "identity"), data = carDataGLM, 
             na.action = na.exclude)
print("***Linear GLM All tests***")
print(summary(myglm.All.Lin))

   ##Not bad.
   ##But... do we really need the Years?
testsNoYears <- tests[-c(grep("Years*",tests,value=FALSE))]
frmNY <- as.formula(paste("Cmb.MPG"," ~", paste(testsNoYears,collapse ="+"),"-1"))
myglm.Lin <- glm(formula = frmNY, family = gaussian(link = "identity"), data = carDataGLM, 
             na.action = na.exclude)
print("***Linear GLM no Years***")
print(summary(myglm.Lin))

  ## AIC got slightly worse, so let us keep years. But... our displacement plots showed that we do not have a linear relation ship
  ## So, let's try some other families
  ## Inverse function
myglm.Inv <- glm(formula = frm, family = Gamma(link = "inverse"), data = carDataGLM, 
             na.action = na.exclude)
print("***inverse GLM***")
print(summary(myglm.Inv))

  ## inverse 2nd order
myglm.Inv2 <- glm(formula = frm, family = inverse.gaussian(link = "1/mu^2"), data = carDataGLM, 
             na.action = na.exclude)
print("***inverse GLM second order***")
print(summary(myglm.Inv2))

  ## Nonsurprisingly, those two look better, as MPG space goes from 0 to (in theory) infinity 
  ## Comparing the two last, in one is TransAutoMan a positive variable and in the other one a negative 
  ## (in comparison to TransAMS). CVT seems to be pretty good for fuel transmission
  ## Other link functions won't work for MPG

### GLM FIT COMPARISONS
  ## We will use a plot of actual MPG values against residual values
  ## The response argument is needed to recieve an answer in MPG units
carDataGLM$Res.Lin<-carDataGLM$Cmb.MPG-predict(myglm.Lin, newdata=carDataGLM, type="response")
carDataGLM$Res.Inv<-carDataGLM$Cmb.MPG-predict(myglm.Inv, newdata=carDataGLM, type="response")
carDataGLM$Res.Inv2<-carDataGLM$Cmb.MPG-predict(myglm.Inv2, newdata=carDataGLM, type="response")

  ## We can now plot each Residual individually
  ## We use geom_points and alpgha to allow us to see point density
  ## and geom_quantile with smooth to estimate our standard deviation
p<- ggplot(carDataGLM, aes(Cmb.MPG,Res.Lin))+geom_point(alpha = 0.1)+
  geom_quantile(method = "rqss", lambda = 10)
ggsave("car_data/car_output/glm_1_Lin_Residuals.png")
p<- ggplot(carDataGLM, aes(Cmb.MPG,Res.Inv))+geom_point(alpha = 0.1)+
  geom_quantile(method = "rqss", lambda = 10)
ggsave("car_data/car_output/glm_2_Inv_Residuals.png")
p<- ggplot(carDataGLM, aes(Cmb.MPG,Res.Inv2))+geom_point(alpha = 0.1)+
  geom_quantile(method = "rqss", lambda = 10)
ggsave("car_data/car_output/glm_3_Inv2_Residuals.png")

  ## Problem: We have trouble to compare the models, as they are not in the same plot
  ## Solution: We melt the 3 residuals into one column 
carDataGLMmelt <- carDataGLM[,c("Cmb.MPG","Res.Lin","Res.Inv","Res.Inv2")]
carDataGLMmelt <- melt(carDataGLMmelt,id="Cmb.MPG", variable.name = "GLM.Type", value.name="Residual")
  ## Focus is on the quantiles, especially the median
p<- ggplot(carDataGLMmelt, aes(Cmb.MPG,Residual))+
  geom_quantile(aes(colour=GLM.Type), method = "rqss", lambda = 10,quantiles=c(0.25,0.75),size=0.5,alpha=0.5)+
  geom_quantile(aes(colour=GLM.Type), method = "rqss", lambda = 10,quantiles=c(0.5),size=1.5)
ggsave("car_data/car_output/glm_4_All_Residuals.png")

  ## Okay, we see that the inverse functions are much less skewed for high MPG 
  ## There is slight skewdness for low MPG in the inverse functions, 
  ## but especially the inverse fit from around 18 to 28 MPG has a residual very close to zero

  ## Idea for future work: Take a look at the outliers




