### Loading Libraries ----
require(pacman) # package management p_load function
p_load(readr,install=TRUE,update=getOption("par_update")) # reading csv files
p_load(DataExplorer,install=TRUE,update=getOption("par_update")) # for exploratory data analysis
p_load(ggplot2,install=TRUE,update=getOption("par_update")) # Visualization
p_load(psych,install=TRUE,update=getOption("par_update")) #panel.pairs for bivariate analysis
p_load(corrplot,install=TRUE,update=getOption("par_update")) # correlations of variables
p_load(nFactors,install=TRUE,update=getOption("par_update")) #for PCA and Factor analysis
p_load(car,install=TRUE,update=getOption("par_update")) #VIF function for multicollinearity

### Working Directory Setup ----
setwd("C:/DSBA_Course/Proper Learning/Module 3 [Advanced Statistics]/M3 W4 [Project 3]/")

### Loading Dataset ----
mydata <- read.csv("Factor-Hair-Revised.csv",header = TRUE)

### Explore Dataset ----
dim(mydata) #100 rows and 13 columns
plot_intro(mydata) # all are continuous variables
names(mydata) # standard format names of variables
plot_str(mydata, max_level = 1) # all numeric variables
head(mydata)
tail(mydata) 
plot_missing(mydata) #no missing values
summary(mydata) #same scale of variables apart from ID
boxplot(mydata[2:13],pch=16,outcol="red",cex=1.25,lty=3)

Outliers = data.frame(Ecom=mydata[,3],SalesFImage=mydata[,8],OrdBilling=mydata[,11],DelSpeed=mydata[,12])
boxplot(Outliers ,pch=16,outcol="red",cex=1.25,lty=3, horizontal = TRUE,labels=TRUE)
abline(v=seq(1,9,0.25),col="lightgray")
abline(h=seq(1.5,4,1),col="lightgray")

boxplot.stats(mydata[,3])$out
boxplot.stats(mydata[,8])$out
boxplot.stats(mydata[,11])$out
boxplot.stats(mydata[,12])$out

### UNIVARIATE ANALYSIS ---- 
par(mfrow=c(1,2),mai = c(0.5, 0.5, 0.5, 0.5))

for(indexI in 2:13){

h <- hist(mydata[,indexI], main = paste0("Centrality of ",names(mydata[indexI]),"\n [St.Dev.=",round(sd(mydata[,indexI]),2),"]"), ylim = c(0,50), labels = TRUE, col = "dark grey",xlab = NULL, cex.main=0.75,cex=0.5,cex.labels=0.5,breaks = 10)
mode <- h$mids[h$counts == max(h$counts)]
abline(v = mean(mydata[,indexI]),col = "royalblue",lwd = 2)
abline(v = median(mydata[,indexI]),col = "red",lwd = 2)
abline(v = mode,col = "green",lwd = 2)
legend("topright", inset=.01,c("Mean","Median","Mode"), fill=c("royalblue","red","green"), horiz=TRUE, cex=0.8)

boxplot(mydata[,indexI], horizontal = TRUE, main = paste0("Dispersion of ",names(mydata[indexI]),"\n [St.Dev.=",round(sd(mydata[,indexI]),2),"]"), col="grey",xlab=NULL,cex.main=0.75)
text(fivenum(mydata[,indexI]), labels =fivenum(mydata[,indexI]), y=1.3, col = "blue", cex = 0.6)
rug(mydata[,indexI], side = 1, ticksize = 0.08, lwd = 0.9, col = "grey");
}



### BIVARIATE ANALYSIS ----
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5))
pairs.panels(mydata[2:13])
# Strong Correlations CompRes vs DelSpeed, TechSupport vs WartyClaim strong correlation
# Moderate Correlation in DelSpeed Vs OrdBilling, CompRes vs OrdBilling, Ecom vs SalesFImage

#DelSpeed vs CompRes
temp1 <- data.frame(DelSpeed=mydata$DelSpeed,CompRes=mydata$CompRes)
pairs.panels(temp1,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#TechSup vs WartyClaim
temp2 <- data.frame(TechSup=mydata$TechSup,WartyClaim=mydata$WartyClaim)
pairs.panels(temp2,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#SalesFImage vs Ecom
temp3 <- data.frame(SalesFImage=mydata$SalesFImage,Ecom=mydata$Ecom)
pairs.panels(temp3,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#OrdBilling vs CompRes
temp4 <- data.frame(OrdBilling=mydata$OrdBilling,CompRes=mydata$CompRes)
pairs.panels(temp4,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#OrdBilling vs DelSpeed
temp5 <- data.frame(OrdBilling=mydata$OrdBilling,DelSpeed=mydata$DelSpeed)
pairs.panels(temp5,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#CompRes vs Satisfaction
temp6 <- data.frame(CompRes=mydata$CompRes,Satisfaction=mydata$Satisfaction)
pairs.panels(temp6,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#ProdLine vs DelSpeed
temp7 <- data.frame(ProdLine=mydata$ProdLine,DelSpeed=mydata$DelSpeed)
pairs.panels(temp7,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#DelSpeed vs Satisfaction
temp8 <- data.frame(DelSpeed=mydata$DelSpeed,Satisfaction=mydata$Satisfaction)
pairs.panels(temp8,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#ProdLine vs CompRes
temp9 <- data.frame(ProdLine=mydata$ProdLine,CompRes=mydata$CompRes)
pairs.panels(temp9,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#ProdLine vs Satisfaction
temp10 <- data.frame(ProdLine=mydata$ProdLine,Satisfaction=mydata$Satisfaction)
pairs.panels(temp10,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#SalesFImage vs Advertising
temp11 <- data.frame(SalesFImage=mydata$SalesFImage,Advertising=mydata$Advertising)
pairs.panels(temp11,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#OrdBilling vs Satisfaction
temp12 <- data.frame(OrdBilling=mydata$OrdBilling,Satisfaction=mydata$Satisfaction)
pairs.panels(temp12,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

#SalesFImage vs Satisfaction
temp13 <- data.frame(SalesFImage=mydata$SalesFImage,Satisfaction=mydata$Satisfaction)
pairs.panels(temp13,gap=0,cex.cor = 0.5,cex=0.75,cex.labels=0.85)

### SIMPLE LINEAR REGRESSION ----
## Lets make SLM models of all the significant correlated pairs identified above

### DelSpeed Vs CompRes SLM Model
SLM_Model_One <- lm(temp1$DelSpeed~temp1$CompRes)
summary(SLM_Model_One)

plot(temp1$DelSpeed~temp1$CompRes,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model_One,col="red",lwd=2)
abline(v=median(temp1$CompRes),col="blue")
abline(h=median(temp1$DelSpeed),col="green")
text(median(temp1$CompRes),0,paste0("Median = ",median(temp1$CompRes)),pos=4)
text(0.5,median(temp1$DelSpeed),paste0("Median = ",median(temp1$DelSpeed)),pos=3)
text(0.6,SLM_Model_One$coefficients[1],paste0("Intercept = ",round(SLM_Model_One$coefficients[1],digits=2)),pos=1)

Prediction <- predict(SLM_Model_One,interval = "confidence", level = 0.99)
Actual <- temp1$DelSpeed
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(1.5,5.5), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=1,col="lightgray",lty="dotted")
abline(h=2,col="lightgray",lty="dotted")
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")


### TechSup Vs WartyClaim SLM Model
Dependant_Variable = temp2$WartyClaim
Independant_Variable = temp2$TechSup

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0.6,SLM_Model$coefficients[1],paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=1)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(3.5,8.0), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")


### SalesFImage Vs Ecom SLM Model
Dependant_Variable = temp3$SalesFImage
Independant_Variable = temp3$Ecom

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0.6,SLM_Model$coefficients[1],paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(3.0,8.0), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")


### OrdBilling Vs CompRes SLM Model
Dependant_Variable = temp4$OrdBilling
Independant_Variable = temp4$CompRes

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0.6,SLM_Model$coefficients[1],paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(9,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=3)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(2.0,7.0), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")


### OrdBilling Vs DelSpeed SLM Model
Dependant_Variable = temp5$OrdBilling
Independant_Variable = temp5$DelSpeed

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0.6,SLM_Model$coefficients[1],paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(9,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=3)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(2.0,7.0), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")

### Satisfaction Vs CompRes SLM Model
Dependant_Variable = temp6$Satisfaction
Independant_Variable = temp6$CompRes

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0.6,SLM_Model$coefficients[1],paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(4.0,10), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=9,col="lightgray",lty="dotted")



### DelSpeed Vs ProdLine SLM Model
Dependant_Variable = temp7$DelSpeed
Independant_Variable = temp7$ProdLine

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(2.0,6), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=2,col="lightgray",lty="dotted")
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")

### Satisfaction Vs DelSpeed SLM Model
Dependant_Variable = temp8$Satisfaction
Independant_Variable = temp8$DelSpeed

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(4.0,10), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=9,col="lightgray",lty="dotted")


### ProdLine Vs CompRes SLM Model
Dependant_Variable = temp9$ProdLine
Independant_Variable = temp9$CompRes

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=3)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(2.0,9), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=3,col="lightgray",lty="dotted")
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")


### Satisfaction Vs ProdLine SLM Model
Dependant_Variable = temp10$Satisfaction
Independant_Variable = temp10$ProdLine

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8.5,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(4.0,10), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=9,col="lightgray",lty="dotted")


### SalesFImage Vs Advertising SLM Model
Dependant_Variable = temp11$SalesFImage
Independant_Variable = temp11$Advertising

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8.5,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=3)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(2.5,8.5), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=3,col="lightgray",lty="dotted")



### Satisfaction Vs OrdBilling SLM Model
Dependant_Variable = temp12$Satisfaction
Independant_Variable = temp12$OrdBilling

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8.5,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(4,10.0), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=9,col="lightgray",lty="dotted")


### Satisfaction Vs SalesFImage SLM Model
Dependant_Variable = temp13$Satisfaction
Independant_Variable = temp13$SalesFImage

SLM_Model <- lm(Dependant_Variable~Independant_Variable)
summary(SLM_Model)

plot(Dependant_Variable~Independant_Variable,pch=20,xlim=c(0,10),ylim=c(0,10))
abline(SLM_Model,col="red",lwd=2)
abline(v=median(Independant_Variable),col="blue")
abline(h=median(Dependant_Variable),col="green")
text(median(Independant_Variable),0.5,paste0("Median = ",median(Independant_Variable)),pos=4)
text(0.5,median(Dependant_Variable),paste0("Median = ",median(Dependant_Variable)),pos=3)
text(0,1,paste0("Intercept = ",round(SLM_Model$coefficients[1],digits=2)),pos=4)
text(8.5,8,paste0("Slope = ",round(SLM_Model$coefficients[2],digits=2)),pos=4)

Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
Actual <- Dependant_Variable
BackTrack <- data.frame(Actual,Prediction)

plot(BackTrack$Actual, col = "blue", 
     pch=20, xlab="Number of Predictions", ylab="DelSpeed", 
     ylim = c(4,10.0), xlim=c(1,50), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)

lines(BackTrack$Actual, col = "blue", lwd=1)
lines(BackTrack$fit, col = "red", lwd=2)
abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=9,col="lightgray",lty="dotted")

## specialized the insignificant value according to the significant level
#Satisfaction based dataset with about 50% correlated 
names(mydata)
Satisfaction_Pairs = mydata
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5))
Satisfaction_Pairs = Satisfaction_Pairs[,-1]
Satisfaction_Pairs = Satisfaction_Pairs[,-2]
Satisfaction_Pairs = Satisfaction_Pairs[,-2]
Satisfaction_Pairs = Satisfaction_Pairs[,-3]
Satisfaction_Pairs = Satisfaction_Pairs[,-5]
Satisfaction_Pairs = Satisfaction_Pairs[,-5]
pairs.panels(Satisfaction_Pairs)

Satisfaction_Pairs[7]

par(mfrow=c(1,3),mai = c(0.3, 0.25, 0.8, 0.25))

for(indexLoop in 1:6){

  Sub1 = 7
  Sub2 = indexLoop
  
  SLM_Model <- lm(Satisfaction_Pairs[,Sub1]~Satisfaction_Pairs[,Sub2])
  Summary_Model <- summary(SLM_Model)
  Equation <- 
    paste0("Linear Model: Y =",round(SLM_Model$coefficients[1],digits=3),
           "+",round(SLM_Model$coefficients[2],digits=3),
           "X     R-Square=",round(Summary_Model$r.squared,digits=3),"       \nP-Value =",round(Summary_Model$coefficients[2,4],digits = 11))
  
  Prediction <- predict(SLM_Model,interval = "confidence", level = 0.99)
  Actual <- Satisfaction_Pairs[,Sub1]
  BackTrack <- data.frame(Actual,Prediction)
  
  plot(BackTrack$Actual, col = "blue", 
       pch=20, xlab="Number of Predictions", ylab="Satisfaction", 
       main=paste("",names(Satisfaction_Pairs[Sub1])," ~ ", 
                  names(Satisfaction_Pairs[Sub2])," ","\n",Equation)  , 
       ylim = c(4,10), xlim=c(1,25), cex.main=1.0, cex.axis=0.8,cex.lab=0.8)
  
  lines(BackTrack$Actual, col = "blue", lwd=1)
  lines(BackTrack$fit, col = "red", lwd=2)
  abline(h=4,col="lightgray",lty="dotted")
  abline(h=5,col="lightgray",lty="dotted")
  abline(h=6,col="lightgray",lty="dotted")
  abline(h=7,col="lightgray",lty="dotted")
  abline(h=8,col="lightgray",lty="dotted")
  abline(h=9,col="lightgray",lty="dotted")
  abline(h=10,col="lightgray",lty="dotted")
  
}

### MultiCollinearity ----

#making model to employ VIF to check multicollinearity
Multi_Colinear_Model <- lm(Satisfaction~.,data = mydata)
summary(Multi_Colinear_Model)

Prediction <- predict(Multi_Colinear_Model,interval = "confidence", level = 0.95)
Actual <- mydata$Satisfaction
BackTrack2 <- data.frame(Actual,Prediction)
BackTrack2

plot(BackTrack2$Actual, col = "blue", pch=20, xlab="Number of Predictions", ylab="Revenue", main = NULL, ylim = c(4,10), xlim=c(1,50))
lines(BackTrack2$Actual, col = "blue", lwd=1)
points(BackTrack2$fit, col = "red",pch=20,lwd=2)
lines(BackTrack2$fit, col = "red", lwd=2)

# Variance Inflation factor to check multicollinearity if more than 5 threshold
my_VIF <- vif(Multi_Colinear_Model)
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5))
plot(my_VIF,type="b",pch=20,col="blue", main="Variance Inflation Factor (VIF)",lwd=3)
text(2,unname(my_VIF[2]),names(my_VIF[2]),pos=4,cex=0.8,col="black")
text(3,unname(my_VIF[3]),names(my_VIF[3]),pos=2,cex=0.8,col="black")
text(4,unname(my_VIF[4]),names(my_VIF[4]),pos=4,cex=0.8,col="black")
text(5,unname(my_VIF[5]),names(my_VIF[5]),pos=4,cex=0.8,col="black")
text(6,unname(my_VIF[6]),names(my_VIF[6]),pos=1,cex=0.8,col="black")
text(7,unname(my_VIF[7]),names(my_VIF[7]),pos=2,cex=0.8,col="black")
text(8,unname(my_VIF[8]),names(my_VIF[8]),pos=3,cex=0.8,col="black")
text(9,unname(my_VIF[9]),names(my_VIF[9]),pos=1,cex=0.8,col="black")
text(10,unname(my_VIF[10]),names(my_VIF[10]),pos=3,cex=0.8,col="black")
text(11,unname(my_VIF[11]),names(my_VIF[11]),pos=1,cex=0.8,col="black")
text(12,unname(my_VIF[12]),names(my_VIF[12]),pos=2,cex=0.8,col="black")

abline(h=5,col="red",lwd=2)
text(0.8,5.2,"Multicollinearity Region",pos=4,cex=0.9,col="red")
text(0.8,4.8,"Normal Region",pos=4,cex=0.9,col="blue")

Temp_Data_Df <- mydata
Temp_Data_Df <- Temp_Data_Df[2:13]
Temp_Data_Df <- Temp_Data_Df[-11]

Non_Multi_Colinear_Model <- lm(Satisfaction~.,data = Temp_Data_Df)

str(Temp_Data_Df)

Prediction <- predict(Non_Multi_Colinear_Model,interval = "confidence", level = 0.95)
Actual <- Temp_Data_Df$Satisfaction
BackTrack2 <- data.frame(Actual,Prediction)
BackTrack2

plot(BackTrack2$Actual, col = "blue", pch=20, xlab="Number of Predictions", ylab="Revenue", main = NULL, ylim = c(4,10), xlim=c(1,50))
lines(BackTrack2$Actual, col = "blue", lwd=1)
points(BackTrack2$fit, col = "red",pch=20,lwd=2)
lines(BackTrack2$fit, col = "red", lwd=2)

abline(h=4,col="lightgray",lty="dotted")
abline(h=5,col="lightgray",lty="dotted")
abline(h=6,col="lightgray",lty="dotted")
abline(h=7,col="lightgray",lty="dotted")
abline(h=8,col="lightgray",lty="dotted")
abline(h=9,col="lightgray",lty="dotted")
abline(h=10,col="lightgray",lty="dotted")

# Variance Inflation factor to check multicollinearity if more than 5 threshold
# after deleting DELSPEED from the regression model
my_VIF <- vif(Non_Multi_Colinear_Model)
par(mfrow=c(1,1),mai = c(0.5, 0.5, 0.5, 0.5))
plot(my_VIF,type="b",pch=20,col="blue", main="Variance Inflation Factor (VIF)",lwd=3, ylim=c(0,10))
text(1,unname(my_VIF[1]),names(my_VIF[1]),pos=1,cex=0.8,col="black")
text(2,unname(my_VIF[2]),names(my_VIF[2]),pos=1,cex=0.8,col="black")
text(3,unname(my_VIF[3]),names(my_VIF[3]),pos=1,cex=0.8,col="black")
text(4,unname(my_VIF[4]),names(my_VIF[4]),pos=3,cex=0.8,col="black")
text(5,unname(my_VIF[5]),names(my_VIF[5]),pos=1,cex=0.8,col="black")
text(6,unname(my_VIF[6]),names(my_VIF[6]),pos=3,cex=0.8,col="black")
text(7,unname(my_VIF[7]),names(my_VIF[7]),pos=3,cex=0.8,col="black")
text(8,unname(my_VIF[8]),names(my_VIF[8]),pos=1,cex=0.8,col="black")
text(9,unname(my_VIF[9]),names(my_VIF[9]),pos=3,cex=0.8,col="black")
text(10,unname(my_VIF[10]),names(my_VIF[10]),pos=2,cex=0.8,col="black")
abline(h=5,col="red",lwd=2)
text(0.8,5.5,"Multicollinearity Region",pos=4,cex=0.9,col="red")
text(0.8,4.5,"Normal Region",pos=4,cex=0.9,col="blue")



### Principal Component Factor ----

# Eigen Values apart from ID and Satisfaction variables
ev <- eigen(cor(mydata[2:12]))
ev
EigenValues <- ev$values
Factor <- c(1,2,3,4,5,6,7,8,9,10,11)

#Number of factors for PCA
Scree <- data.frame(Factor,EigenValues)
plot(Scree,main="Scree Plot", col="Blue",ylim=c(0,4),ylab="Eigen Values", xlab="Variables", pch=20, type="b", lwd=2)
abline(h=1,col="red")
text(Scree, labels = round(EigenValues,3), pos=4)
text(9,1.1,labels = "Kaiser's Criterion (Eigen Value > 1)", cex=1, col="red")

Unrotate=principal(mydata[2:12], nfactors=4, rotate="none")
print(Unrotate,digits=3)
UnrotatedProfile=plot(Unrotate,row.names(Unrotate$loadings))

#verimax rotation to help identifying factors and naming them
Rotate=principal(mydata[2:12],nfactors=4,rotate="varimax")
print(Rotate,digits=3)
RotatedProfile=plot(Rotate,cex=1.0)

temp = mydata[2:12]
RFA <- factanal(temp,factors=4,rotation = "varimax")
print(RFA,digits = 3)
FAVars <- RFA$rotmat
FA_One <- FAVars[,1]
FA_Two <- FAVars[,2]

fData <- Rotate$scores
SLA_Factor <- fData[,1]
Marketing_Factor <- fData[,2]
Support_Factor <- fData[,3]
Quality_Factor <- fData[,4]

CorrDS <- data.frame(SLA_Factor,Marketing_Factor,Support_Factor,Quality_Factor,mydata$Satisfaction)
corrplot(cor(CorrDS),col=c("orange","darkblue"),tl.col = c("orange","darkblue"),tl.cex = 0.9,method = "number",type="upper")

### MULTIPLE LINEAR REGRESSION ----

# Preparing dataset

MLR_Model <- lm(mydata.Satisfaction~.,data = CorrDS)
summary(MLR_Model)

# Satisfaction = 6.918 + 0.618*SLA_Factor + 0.509*Marketing_factor + 0.067*Support_Factor + 0.540*Product_Factor

# backtracking to see effectiveness visually

Prediction <- predict(MLR_Model,interval = "confidence", level = 0.95)
Actual <- mydata$Satisfaction
BackTrack <- data.frame(Actual,Prediction)
BackTrack

par(mfrow=c(1,1),mai = c(0.25, 0.25, 0.25, 0.25))
  plot(BackTrack$Actual, col = "gray", pch=20, xlab="Number of Predictions", ylab="Revenue", main = NULL, ylim = c(4.5,9.0), xlim=c(1,100),lwd=5)
  lines(BackTrack$Actual, col = "gray", lwd=5)
  points(BackTrack$fit, col = "red",pch=20,lwd=5)
  lines(BackTrack$fit, col = "red", lwd=5)
  
  plot(BackTrack$Actual, col = "gray", pch=20, xlab="Number of Predictions", ylab="Revenue", main = NULL, ylim = c(4.5,9.0), xlim=c(1,100),lwd=5)
  lines(BackTrack$Actual, col = "gray", lwd=5)
  points(BackTrack2$fit, col = "blue",pch=20,lwd=5)
  lines(BackTrack2$fit, col = "blue", lwd=5)
  
  grid(20,col="lightgray")

#Validity of the model
par(mfrow=c(2,2),mai = c(0.35, 0.35, 0.35, 0.35))
plot(MLR_Model)

### CONCLUSION ----
plot(Multi_Colinear_Model)
#Comparing PCA based MLR with NORMAL MultiColinear Model 

