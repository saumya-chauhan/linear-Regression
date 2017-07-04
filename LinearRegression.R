############
setwd("D:/R datasets")

library(ggplot2)
#------------- Simple Regression (AlbumSales) -----------

Album1 <- read.delim ("Album Sales.dat", header=TRUE) 
head(Album1)
Scatterplot<-ggplot (Album1, aes (adverts, sales)) + geom_point()
Scatterplot+ geom_smooth (method=lm, alpha=0.4) 

AlbumSales <- lm(sales~adverts, data=Album1, na.action=na.omit)
summary (AlbumSales)
anova(AlbumSales)

sqrt (0.3346)
cor (Album1 [, c("sales", "adverts")])

#------ Difference B/W Residual and Influencial cases-------------
  
pubs <- c(10,20,30,40,50,60,70, 500)
mortality <- c( 1000, 2000, 3000, 4000, 5000, 6000, 7000, 10000)
PubsData <- data.frame (pubs, mortality)
write.table (PubsData, "Pubs.dat", sep="/t", row.names=FALSE)

pub <- read.delim ("Pubs.dat", header=TRUE)
PubSReg<- lm(mortality~pubs, data=pub, na.action=na.exclude)
summary (PubSReg)
scatter<- ggplot (pub, aes (pubs, mortality))+ geom_point ()
scatter+ geom_smooth (method= "lm", se=F)+ labs (x= "No. of Pubs", y=" No. of Death")
SaveInImageDirectory ("07 PubsData.png") 

#--------------Multiple Regression (Album Sales2)----------------
  
Album2 <- read.delim ("Album Sales2.dat", header=TRUE)
head(Album2)
AlbumSReg <- lm (sales~adverts, data=Album2, na.action=na.omit)
summary(AlbumSReg)

AlbumMReg<- lm (sales~ adverts + airplay+ attract, data=Album2, na.action=na.omit)
summary (AlbumMReg)
anova(AlbumSReg,AlbumMReg)

fit<- lm (scale(sales)~ scale(adverts) + scale (airplay)+ scale (attract), data=Album2, na.action=na.omit)
summary (fit)

AlbumMRegUpdate <- update (AlbumSReg, .~. + airplay + attract)
summary (AlbumMReg)

sqrt (.66647)

#-----Standardized Beta values -----------------
  
library (QuantPsyc)
library (psych)
library (pastecs)

lm.beta (AlbumMReg)
round (stat.desc (Album2, basic=FALSE, norm=TRUE), digit=3)

#----------Multiple Regression C.I------
  
confint (AlbumMReg)

#.....Comparing Model---------
  
anova (AlbumSReg, AlbumMReg)

#---------Check AIC---------------
  
AIC (AlbumSReg)
AIC (AlbumMReg)

#--------Stepwise Model---------
  
stepwise(AlbumMReg, direction='forward', criterion='AIC')
step (AlbumMReg, direction='backward', criterion='AIC')


#--------- Outlier and Influencial Case----------

#Album2$Residual <- round (resid (AlbumMReg), digit=3)
Album2$Standard.Residual <- round (rstandard (AlbumMReg), digit=3)
Album2$Cooks.Distance <- round (cooks.distance (AlbumMReg), digit=3)
Album2$DfBeta <- round (dfbeta (AlbumMReg), digit=3)
Album2$DFFIT <- round (dffits (AlbumMReg), digit=3)
Album2$leverage <- round (hatvalues (AlbumMReg), digit=3)
Album2$Covariance.Ratio <- round (covratio (AlbumMReg), digit=3)
#Album2$Studentized.residual <- round (rstudent (AlbumMReg), digit=3)
Album2$Predicted.value <- round (fitted (AlbumMReg), digit=3) 
Album2

write.table (Album2, " AlbumSales with Diagnosist.dat", sep="\t", row.names=FALSE)

Album2$large.residual<- abs (Album2$Standard.Residual)>=2
sum (Album2$large.residual)

Album2$large.residual <- Album2$Standard.Residual > 2 | Album2$Standard.Residual < -2

#--------Casewise Diagnosist---------------

Album2 [ Album2$large.residual, c("sales", "adverts", "airplay", "attract","Standard.Residual")] 

Album2 [ Album2$large.residual, c("Cooks.Distance", "leverage", "Covariance.Ratio")]

hist (Album2$Standard.Residual)
hist (Album2$Studentized.residual)
summary (AlbumMReg)

Album2$fitted.values

## Case study walmart
library(sas7bdat)
walmart <- read.sas7bdat(file.choose())
head(walmart)
names(walmart)
#which variables have an impact on customer satisfaction; analysing the situation and create a model
#to be able to predict customer satifaction
#Divide the data into training and validation set. Carry out the initial predictive modeling based on
#the training data and then use the hold out set to compare the performance of the predictive model.
#Also please check the result with that of the Adjusted R square criteria.
#How do you define Multicollinearity?
#What is VIF?
#How do you check the goodness of fit of the model?
#How do you interpret the explanatory variable in the model?
#Please carry out a validity check for the model using the hold out data set.
dim(walmart)
set.seed(25) # setting the random number seed for splitting the dataset
ranuni <- sample(x=c("Training","Testing"),
                 size=nrow(walmart),replace=T,
                 prob=c(0.7,0.3)) # creating a vector with texts "Training" (approx. 70% )& "Testing" (approx. 30%)
ranuni
#cbind(ranuni)
TrainingData <- walmart[ranuni=="Training", ] # generating the training data
View(TrainingData)
TestingData <- walmart[ranuni=="Testing",] # generating the testing data

TrainingModel <- lm(Employee_Performance~., data= TrainingData)
TrainingModel1 <- lm(Employee_Performance~., data= TrainingData[, -14])
TrainingModel


summary(TrainingModel)
summary(TrainingModel1)

TrainingModel2 <- step(object= TrainingModel, direction="backward")
summary(TrainingModel2)    #ALL VAR(s) mentioned ARE IMPACTING THE Y VARIABLE
TrainingModel2$fitted.values
#cor(walmart)
#pairs.panels(walmart)
# install.packages("usdm")
# library(usdm)
library(car)
vif(TrainingModel1)  #to check for the Multicollinearity   #VIFs less than 5 so no problem

dwt(TrainingModel2)     #Null Hyp: rho = 0

qqnorm(TrainingModel1$residuals)  #qq plot
qqline(TrainingModel1$residuals)#will draw a straight line

par(mfrow=c(2,2))
plot(TrainingModel1)

shapiro.test(TrainingModel2$residuals)    #checking normALITY OF RESIDUALS. SAMPLE SIZE IS LESS. Works good when n > 2000.
#first do stat.desc() to decrease skewness...take log of all varialble.
#.for kurtosis do inverse of all varialble...if swkeness in negative then do square..if then also not then do box cox
#tranformation this will establize both skewness and kurtosis

residuals <- resid(TrainingModel) #getting the residuals
plot(x= TrainingData$Employee_Performance, y = residuals)

TrainingModel2$fitted.values
TrainingModel2$residuals
anova(TrainingModel1)
#by rms e...we check...if least.... model best 
newdata <- data.frame()

# TestPred <- predict.lm(object= TrainingModel2, newdata = )
# TestPred    #estimated values of y 

cor(x= TrainingModel2$fitted.values, y = TrainingData$Employee_Performance)  #corr high means good
plot(x= TrainingData$Employee_Performance,
     y= TrainingModel2$fitted.values, type= "p", col= "red")   #plot looks linear then good

#  

TestPred <- predict.lm(object= TrainingModel2, newdata = TestingData)
TestPred
summary(TestPred)
cor(x= TestPred, y = TestingData$Employee_Performance)
residuals<-res(TestingData$Employee_Performance,TestPred)
res<-residuals*residuals
mean<-res/40
mean
sqrt(mean)

Album2$Standard.Residual <- round (rstandard (AlbumMReg), digit=3)
a <- Album2$Standard.Residual > 3
Album2[a,]




