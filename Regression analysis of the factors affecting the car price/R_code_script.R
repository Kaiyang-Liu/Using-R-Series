#1. Packages

install.packages("GGally")
install.packages("car")
install.packages ("tidyverse")
install.packages ("caret")
install.packages ("MASS")
install.packages("moments")
install.packages("ggplot2")
install.packages("hrbrthemes")

library(GGally)
library(car)
library(tidyverse)
library(caret)
library(MASS)
library(moments)
library(ggplot2)
library(hrbrthemes)


#2. Load dataset and Data Preprocessing

df = read.csv(file.choose(),header=T)
names(df)
head(df)

#Find if there are any NaN
CarNaN = sum(is.na(df))

#Find if there are any Duplicates
CarDuplicates = unique(df$Car_ID)

#Delete useless features
df_new = df[,-c(1,3)]

###For observing categorical data ONLY
#keep all the categorical data
df_cato = df_new[,c(1:7,13,14,16)]
names(df_cato)
attach(df_cato)
str(df_cato)
df_cato$symboling=as.factor(symboling)
str(df_cato)
summary(df_cato)
summary(fuelsystem)

#delete the useless rows in df_new
which(symboling=="-2")
which(enginetype=="dohcv")
which(cylindernumber=="three")
which(cylindernumber=="twelve")
which(fuelsystem=="4bbl")
which(fuelsystem=="mfi")
which(fuelsystem=="spfi")
detach(df_cato)
df_new = df_new[-c(19,30,47,50,56:58,130,195,197,199),]



#3. Basic Statistics
##keep all the numerical data
df_num = df_new[,-c(1:7,13,14,16)]
Model_num=lm(price~.,data=df_num)

#Correlation map/ Heat map
ggcorr(df_new,label=TRUE,lable_size = 2.9, hjust = 1, layout.exp=2)

for(i in 1:(dim(df_num)[2]-1)){
  eval(parse(text = paste("p",as.character(i)," <- ggplot(df_num, aes(x = price, y = ",names(df_num)[i],")) + geom_point() + geom_smooth(method=lm , color=\"red\", se=FALSE) + theme_ipsum()",sep="")))
}
par(mfrow=c(3,3),pch=16)
hist(wheelbase,col="lightblue",main='range of wheelbase')
hist(carlength,col="pink",main='range of carlength')
hist(carwidth,col="red",main='range of carwidth')
hist(carheight,col="light blue",main='range of carheight')
hist(curbweight,col="pink",main='range of curbweight')
hist(enginesize,col="red",main='range of enginesize')
hist(boreratio,col="light blue",main='range of boreratio')
hist(stroke,col="pink",main='range of stroke')
hist(compressionratio,col="red",main='range of compressionratio')
par(mfrow=c(3,2),pch=16)
hist(horsepower,col="light blue",main='range of horsepower',ylab='horsepower')
hist(peakrpm,col="pink",main='range of peakrpm',ylab='peakrpm')
hist(citympg,col="light blue",main='range of citympg',ylab='citympg')
hist(highwaympg,col="pink",main='range of citympg',ylab='highwaympg')
hist(price,col="red",main='range of price',ylab='price')
summary(df_num)
apply(df_num,2,sd)
summary(df_cato)
skewness(df_num)
kurtosis(df_num)



#4. Hypothesis Test

attach(df_new)
#Analysis of variance for categorical variables with only two categories
#Are there difference in aspiration? 
var.test(price~aspiration,alt='two.side')

#Analysis of variance for categorical variables with multiple categories
#Are there different mean in carbody?
oneway.test(price~carbody)


#Which types in carbody have significant different mean?
pairwise.t.test(price,carbody)


#Is there a significant interaction between two categorical variables?
summary(aov(price~carbody*aspiration))
interaction.plot(carbody,aspiration,price,col=2:5)
summary(aov(price~fuelsystem*aspiration))
interaction.plot(fuelsystem,aspiration,price,col=2:5)



#VIF to find if there are muticollinearity
##keep all the numerical data
df_num = df_new[,-c(1:7,13,14,16)]
Model_num=lm(price~.,data=df_num)
vif(Model_num)
max(vif(Model_num))
mean(vif(Model_num))

#5. Model Selection and Comparison

#Stepwise to find the variable
Model1=lm(price~.,data=df_new)
step = stepAIC(Model1, direction="both")


#Input the Model from stepwise
Model2=lm(price ~ symboling + aspiration + doornumber + carbody + drivewheel + 
    enginelocation + wheelbase + carlength + carwidth + carheight + 
    curbweight + enginetype + cylindernumber + enginesize + fuelsystem + 
    boreratio + stroke + compressionratio + horsepower + peakrpm + 
citympg + highwaympg,data=df_new)

Model3=lm(price ~ symboling + aspiration + doornumber + carbody + drivewheel + 
    enginelocation + carlength + carwidth + carheight + curbweight + 
    enginetype + cylindernumber + enginesize + fuelsystem + boreratio + 
    stroke + compressionratio + horsepower + peakrpm + citympg + 
    highwaympg,data=df_new)
Model4=lm(price ~ symboling + aspiration + doornumber + carbody + drivewheel + 
    enginelocation + carlength + carwidth + carheight + curbweight + 
    enginetype + cylindernumber + enginesize + fuelsystem + boreratio + 
    stroke + compressionratio + peakrpm + citympg + highwaympg,data=df_new)
Model5=lm(price ~ aspiration + doornumber + carbody + drivewheel +enginelocation + carlength + carwidth + carheight + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + boreratio + stroke +compressionratio + peakrpm + citympg + highwaympg,data=df_new)
Model6=lm(price ~ aspiration + carbody + drivewheel + enginelocation + 
    carlength + carwidth + carheight + curbweight + enginetype + 
    cylindernumber + enginesize + fuelsystem + boreratio + stroke + 
    compressionratio + peakrpm + citympg + highwaympg,data=df_new)
Model7=lm(price ~ aspiration + carbody + drivewheel + enginelocation + 
    carlength + carwidth + carheight + curbweight + enginetype + 
    cylindernumber + enginesize + fuelsystem + boreratio + stroke + 
    compressionratio + peakrpm + highwaympg,data=df_new)
Model8=lm(price ~ aspiration + carbody + drivewheel + enginelocation + 
    carwidth + carheight + curbweight + enginetype + cylindernumber + 
    enginesize + fuelsystem + boreratio + stroke + compressionratio + 
    peakrpm + highwaympg,data=df_new)
Model9=lm(price ~ aspiration + carbody + drivewheel + enginelocation + 
    carwidth + curbweight + enginetype + cylindernumber + enginesize + 
    fuelsystem + boreratio + stroke + compressionratio + peakrpm + 
    highwaympg,data=df_new)
Model10=lm(price ~ aspiration + carbody + drivewheel + enginelocation + 
    carwidth + curbweight + enginetype + cylindernumber + enginesize + 
    fuelsystem + stroke + compressionratio + peakrpm + highwaympg,data=df_new)
Model11=lm(price ~ aspiration + carbody + enginelocation + carwidth + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + stroke +compressionratio + peakrpm + highwaympg,data=df_new)


#Significance test
Model1=lm(price~.,data=df_new)
Model12=lm(price~aspiration+carbody+enginelocation+carwidth+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+stroke+peakrpm,data=df_new)

#ANOVA Table
anovaModel1=anova(Model1)
SS.reg1=sum(anovaModel1[1:23,2])
df1=sum(anovaModel1[1:23,1])
F1=(SS.reg1/df1)/4799722

anovaModel12=anova(Model12)
SS.reg12=sum(anovaModel12[1:11,2])
df12=sum(anovaModel12[1:11,1])
F12=(SS.reg12/df12)/4730726

# Criteria for Model choose
reg1=summary(Model1)
reg2=summary(Model2)
reg3=summary(Model3)
reg4=summary(Model4)
reg5=summary(Model5)
reg6=summary(Model6)
reg7=summary(Model7)
reg8=summary(Model8)
reg9=summary(Model9)
reg10=summary(Model10)
reg11=summary(Model11)
reg12=summary(Model12)

reg1$sigma^2
reg2$sigma^2
reg3$sigma^2
reg4$sigma^2
reg5$sigma^2
reg6$sigma^2
reg7$sigma^2
reg8$sigma^2
reg9$sigma^2
reg10$sigma^2
reg11$sigma^2
reg12$sigma^2

reg1$r.squared
reg2$r.squared
reg3$r.squared
reg4$r.squared
reg5$r.squared
reg6$r.squared
reg7$r.squared
reg8$r.squared
reg9$r.squared
reg10$r.squared
reg11$r.squared
reg12$r.squared

reg1$adj.r.squared
reg2$adj.r.squared
reg3$adj.r.squared
reg4$adj.r.squared
reg5$adj.r.squared
reg6$adj.r.squared
reg7$adj.r.squared
reg8$adj.r.squared
reg9$adj.r.squared
reg10$adj.r.squared
reg11$adj.r.squared
reg12$adj.r.squared

x1 = model.matrix(Model1)
h1 = hat(x1)
Press1 = sum((reg1$residuals/(1-h1))^2)
x2 = model.matrix(Model2)
h2 = hat(x2)
Press2 = sum((reg2$residuals/(1-h2))^2)
x3 = model.matrix(Model3)
h3 = hat(x3)
Press3 = sum((reg3$residuals/(1-h3))^2)
x4 = model.matrix(Model4)
h4 = hat(x4)
Press4 = sum((reg4$residuals/(1-h4))^2)
x5 = model.matrix(Model5)
h5 = hat(x5)
Press5 = sum((reg2$residuals/(1-h5))^2)
x6 = model.matrix(Model6)
h6 = hat(x6)
Press6 = sum((reg3$residuals/(1-h6))^2)
x7 = model.matrix(Model7)
h7 = hat(x7)
Press7 = sum((reg1$residuals/(1-h7))^2)
x8 = model.matrix(Model8)
h8 = hat(x8)
Press8 = sum((reg2$residuals/(1-h8))^2)
x9 = model.matrix(Model9)
h9 = hat(x9)
Press9 = sum((reg3$residuals/(1-h9))^2)
x10 = model.matrix(Model10)
h10 = hat(x10)
Press10 = sum((reg1$residuals/(1-h10))^2)
x11 = model.matrix(Model11)
h11 = hat(x11)
Press11 = sum((reg2$residuals/(1-h11))^2)
x12 = model.matrix(Model12)
h12 = hat(x12)
Press12 = sum((reg3$residuals/(1-h12))^2)


Cp1=36
Cp2=36+(reg2$sigma^2-reg1$sigma^2)*(194-36)/(reg1$sigma^2)
Cp3=35+(reg3$sigma^2-reg1$sigma^2)*(194-35)/(reg1$sigma^2)
Cp4=34+(reg4$sigma^2-reg1$sigma^2)*(194-34)/(reg1$sigma^2)
Cp5=33+(reg5$sigma^2-reg1$sigma^2)*(194-33)/(reg1$sigma^2)
Cp6=32+(reg6$sigma^2-reg1$sigma^2)*(194-32)/(reg1$sigma^2)
Cp7=31+(reg7$sigma^2-reg1$sigma^2)*(194-31)/(reg1$sigma^2)
Cp8=30+(reg8$sigma^2-reg1$sigma^2)*(194-30)/(reg1$sigma^2)
Cp9=29+(reg9$sigma^2-reg1$sigma^2)*(194-29)/(reg1$sigma^2)
Cp10=28+(reg10$sigma^2-reg1$sigma^2)*(194-28)/(reg1$sigma^2)
Cp11=26+(reg11$sigma^2-reg1$sigma^2)*(194-26)/(reg1$sigma^2)
Cp12=24+(reg12$sigma^2-reg1$sigma^2)*(194-24)/(reg1$sigma^2)


#6. Cross validation

# Split the data into training and test set
set.seed(123)
a = df_new$price %>% createDataPartition(p = 0.8, list = FALSE)
df_train = df_new[a, ]
df_test = df_new[-a, ]

# Build the model
Model10=lm(price ~ aspiration + carbody + drivewheel + enginelocation + 
    carwidth + curbweight + enginetype + cylindernumber + enginesize + 
    fuelsystem + stroke + compressionratio + peakrpm + highwaympg,data=df_train)
Model11=lm(price ~ aspiration + carbody + enginelocation + carwidth + curbweight + enginetype + cylindernumber + enginesize + fuelsystem + stroke +compressionratio + peakrpm + highwaympg,data=df_train)
Model12=lm(price~aspiration+carbody+enginelocation+carwidth+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+stroke+peakrpm,data=df_train)

# Make predictions and compute the R2, RMSE and MAE
predictions10 = Model10 %>% predict(df_test)
predictions11 = Model11 %>% predict(df_test)
predictions12 = Model12 %>% predict(df_test)

CV1 = data.frame( R2 = R2(predictions10, df_test$price),
            RMSE = RMSE(predictions10, df_test$price),
            MAE = MAE(predictions10, df_test$price))

CV2 = data.frame( R2 = R2(predictions11, df_test$price),
            RMSE = RMSE(predictions11, df_test$price),
            MAE = MAE(predictions11, df_test$price))

CV3 = data.frame( R2 = R2(predictions12, df_test$price),
            RMSE = RMSE(predictions12, df_test$price),
            MAE = MAE(predictions12, df_test$price))

#7. Statistics Diagnosis
#Correlation matrix
cor(df_new[,-c(1:7,13,14,16)])
Model12num=lm(price~carwidth+curbweight+enginesize+stroke+peakrpm,data=df_new)
vif(Model12num)#Variance inflation factors

reg=summary(Model12)#Residuals Plot
hat1=lm.influence(Model12)$hat
res=reg$residuals/(reg$sigma*sqrt(1-hat1))
plot(res)
abline(h=0)

#Model12 fit&residuals
fit=fitted(Model12)
residuals=resid(Model12)
plot(fit,residuals)
abline(h=0)

#qqplot
residuals=resid(Model12)#qqplots-diagnostic plots
qqnorm(residuals)
qqline(residuals)

#crPlots
Model12new=lm(price~aspiration+carbody+enginelocation+carwidth+curbweight+enginetype +enginesize+fuelsystem+stroke+peakrpm,data=df_new)
crPlots(Model12new)

#Outliers Test
library(car)
outlierTest(Model12)#test outlier
fit=fitted(Model12)
residuals=resid(Model12)
plot(fit,residuals)
abline(h=0)
identify(fit,residuals,row.names(df_new))

df_new=df_new[-17,]#delet outlier
Model12=lm(price~aspiration+carbody+enginelocation+carwidth+curbweight+enginetype+cylindernumber+enginesize+fuelsystem+stroke+peakrpm,data=df_new)

outlierTest(Model12)#test outlier again

influence.measures(Model12)#test which is influential points
