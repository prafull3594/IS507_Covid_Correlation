
install.packages("tidyverse")
install.packages("caret")
install.packages("plotmo")
install.packages("alr3")
install.packages("Metrics")

library(Metrics)
library(tidyverse)
library(caret)
library(glmnet)
require(psych)


# for the covid dataset
library(dplyr)
library(ggplot2)
library(readr)


######Script to handle percentage values and store as decimal######


data=read.csv("Documents/Spring 2021/Data Stats/Project/covid_analytics_clinical_data.csv")

data = data.frame(lapply(data, as.character), stringsAsFactors=FALSE)

# convert percents -- count which ones have %'s
# This can take a moment & you might get a few NA warnings
columnsInPercent = c()
for (i in 1:length(colnames(data))){
  # check for "%" in this column
  percentColumns = FALSE
  percentCount = c()
  for (j in 1:nrow(data)){
    if(grepl("%",data[colnames(data)[i]][j,],fixed=TRUE)){
      percentColumns = TRUE
      #if (percentColumns){break} # break if we find one
      # track individual
      percentCount = c(percentCount,TRUE)
    } else {
      percentCount = c(percentCount,FALSE)
    }
  }
  # save answer for later
  columnsInPercent = c(columnsInPercent, percentColumns)
  # if we found some percentages, convert this column
  if (percentColumns){
    dataC = c()
    xx = pull(data[colnames(data)[i]])
    for (j in 1:nrow(data)){
      if (percentCount[j]){
        dataC = c(dataC,as.numeric(sub("%","",xx[j]))/100.)
      } else {
        dataC = c(dataC,as.numeric(xx[j]))
      }
    }
    
    data[colnames(data)[i]] = dataC
    
  } else { 
    # check for and update numerical columns
    xx = pull(data[colnames(data)[i]])
    nNum = !is.na(as.numeric(xx))
    if (length(nNum[nNum]) > 10){
      data[colnames(data)[i]] = as.numeric(xx)
    }
  }
}
# look at which columns are in percentages:
print(subset(colnames(data), columnsInPercent))

################################################################

##PART 1 and 2###

#Nominal variable 1 - Country barplot

c = na.exclude(data$Country)
c[c==""]<-"Unknown"
barplot(table(c))


#Nominal variable 2 - ICU vs Non ICU barplot

#fixed the issue of case sensitivity (for Non ICU and non ICU) and handled blank and special character values, combined into one NA bucket
i = gsub("[^[:alnum:]]", " ", data$ICU.vs..non.ICU)
i[i==""]<-NA
barplot(table(i))



#Numerical variable 1 - diabetes histogram

d = data$Diabetes
hist(d, main = "Diabetes histogram")
summary(d)
#after removing null
m1 = mean(d, na.rm=TRUE)
m2 = median(d,na.rm=TRUE)
print(c(m1,m2))

#Not a lot of difference after handling null values 

#Mean, Median and Skew

abline(v=m1, col="blue", lwd=2)
abline(v=m2, col="red", lwd=2)
legend("topright", c("Mean","Median"), col = c("blue", "red"), lwd=2)

#Skewness - mean and median - typical observation 
"The histogram is skewed to the right. Median is less than mean. 
This is clearly because of the value towards 1 which relatively have high frequency 
and acting as outlier. Hence in this case, median represents 
a typical observation in a better way" 

#SD and IQR

sd = sd(d,na.rm=TRUE)
iqr = IQR(d, na.rm=TRUE)
print(c(sd,iqr))

lines(c(m1-0.5*sd,m1+0.5*sd),c(20,20),col="green",lwd=3)
lines(c(m2-0.5*iqr,m2+0.5*iqr), c(1.5,1.5), col="magenta",lwd=3)
legend("right",c("STDDEV","Quartiles"),col=c("green","magenta"),lwd=4)

#SD and IQR - variability 
"Due to the high frequency and high value outliers and right skew, SD is also high along with mean. 
Hence, IQR is better to show the variability of observations."


#Normality

qqnorm(d)
qqline(d)


library(MASS)
d=na.exclude(d)
fit = fitdistr(d,'normal')
parameters = fit$estimate
par(mfrow=c(1,1))
hist(d, prob=TRUE)

# Normal sequence curve overplot 

x = seq(0,1,length = 200)

curve(dnorm(x,mean = parameters[1], sd = parameters[2]),col='red', add=TRUE)


"The upper and lower end of the data dont fall on the theoretical normal line
and there are a lot of outliers as well towards the higher end of the spectrum.
We dont even need to fit a model as diabetes distribution is clearly not normal."


#Numerical variable 2 - shortness of breath symptom 

b = (data$Shortness.of.Breath..dyspnoea.) 
hist(b, main="shortness of breath histogram")

#Calculating statistics 

summary(b)
#after removing null
mean = mean(b, na.rm=TRUE)
med = median(b,na.rm=TRUE)
print(c(mean,med))
#Not a lot of difference after handling null values 

#Mean, Median and Skew

abline(v=mean, col="blue", lwd=2)
abline(v=med, col="red", lwd=2)
legend("topright", c("Mean","Median"), col = c("blue", "red"), lwd=2)

#Skewness - mean and median - typical observation 
"Frequency wise majority of the values are limited within 0.6 and there are some high frequency values around 1.
The histogram is skewed to the right. Median is less than mean. 
This is clearly because of the value towards 1 which relatively have high frequency. Hence in this case, median represents 
a typical observation in a better way" 

# proof

#add more outliers

new_b = c(b,0.9999999, 0.99999999999999)

abline(v=mean(new_b, na.rm=TRUE), col="blue", lwd=4, lty=2)
abline(v=median(new_b, na.rm=TRUE), col="red", lwd=4, lty=2)
legend("topright", c("Mean","Median"), col = c("blue", "red"), lwd=2)

# Overplotting, We can very clearly see that adding outliers shifts mean to the right but median sticks to the same.
# Hence median represents a better way for typical observation

#SD and IQR

sd = sd(b,na.rm=TRUE)
iqr = IQR(b, na.rm=TRUE)
print(c(sd,iqr))


lines(c(mean-0.5*sd,mean+0.5*sd),c(20,20),col="green",lwd=3)
lines(c(med-0.5*iqr,med+0.5*iqr), c(1.5,1.5), col="magenta",lwd=3)
legend("topright",c("STDDEV","Quartiles"),col=c("green","magenta"),lwd=4)


#add more outliers

new_b = c(b,0.9999999, 0.99999999999999, 0.99)

m3 = mean(new_b, na.rm=TRUE)
m4 = median(new_b,na.rm=TRUE)

lines(c(m3-0.5*sd,m3+0.5*sd),c(20,20),col="green",lwd=4, lty=2)
lines(c(m4-0.5*iqr,m4+0.5*iqr), c(1.5,1.5), col="magenta",lwd=4, lty=2)

# We can see negligible difference with addition of outliers. But since we know that SD does increase in a right skew, 
# IQR makes more sense for variability of observations


#Normality

qqnorm(b)
qqline(b)


library(MASS)
b=na.exclude(b)
fit = fitdistr(b,'normal')
parameters2 = fit$estimate
par(mfrow=c(1,1))
hist(b, prob=TRUE)

# Normal sequence curve overplot 

x = seq(0,1,length = 200)

curve(dnorm(x,mean = parameters2[1], sd = parameters2[2]),col='red', add=TRUE)

"Both the upper and lower end of the distribution has outliers that dont
fall on the theoretical normal line. Hence, shortness of breath distribution
is not normal"



#---------------------JUST FOR FUN-----------------

###Just for fun####

library(plotly)

covid_table <- Reduce(function(x, y) merge(x, y, all=TRUE), 
                      list(aggregate(Mean.Age ~ Province.State, data, mean, na.rm=TRUE)))


covid_table=na.exclude(covid_table)
print(covid_table)



# first plot 

Age <- plot_ly(
  y = covid_table$Mean.Age,
  x = covid_table$Province.State,
  name = "Average of mean age by State",
  type = "bar"
)

print(Age)



#############################################################################################


###PART 4###

#Handle Missing data using mice#

df = data.frame(data$Mortality*data$Study.Pop.Size..N., 
                data$Cardiovascular.Disease..incl..CAD.*data$Study.Pop.Size..N., 
                data$Diabetes*data$Study.Pop.Size..N.,
                data$Liver.Disease..any.*data$Study.Pop.Size..N., 
                data$Chronic.kidney.renal.disease*data$Study.Pop.Size..N.)

names(df) <- c("mortality_num", "cardio_num", "diabetes_num", "liver_num", "kidney_num")


library(mice)
md.pattern(df)

library(VIM)
aggr_plot <- aggr(df, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, 
                  labels=names(df), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))


tempData <- mice(df,m=1,maxit=50,meth='pmm',seed=500)
summary(tempData)

completedData <- complete(tempData,1)


#Final dataset is completedData#

#Cross validation and feature selection using Lasso

y = completedData$mortality_num
x = model.matrix(mortality_num~.,data=completedData)  

#LASSO

grid=10^seq(10,-2, length =100)

set.seed (1)
train=sample (1: nrow(x), nrow(x)/2)
test=(- train )
y.test=y[test]

lasso.mod =glmnet (x[train ,],y[train],alpha =1, lambda =grid)
plot(lasso.mod)      

set.seed (1)
cv.out =cv.glmnet (x[train ,],y[train],alpha =1)
plot(cv.out)

#Finding best lambda value and MSE

bestlam =cv.out$lambda.min
bestlam

out=glmnet (x,y,alpha =1, lambda =grid)
lasso.coef=predict (out ,type ="coefficients",s=bestlam )
lasso.coef


#########



##Fitting the model###


mlr <- lm(mortality_num ~.,data = completedData)
summary(mlr)


par(mfrow=c(2,2))#, oma=c(0,0,2,0))
plot(mlr, sub.caption='Diagnostics for our MLR Fit')

##Outlier detection##

#check model fit after removing observations 521, 512, 20 

mlr_new <- lm(mortality_num ~.,data = completedData[-20,]) 
summary(mlr)


mlr_new <- lm(mortality_num ~.,data = completedData[-512,]) 
summary(mlr)


mlr_new <- lm(mortality_num ~.,data = completedData[-521,]) 
summary(mlr)


#R square same as before in all cases 

###PART 5###

#Predictions

# 80 20 split #

training_row_number <- sample(1:nrow(completedData), 0.8*nrow(completedData))
train_data = completedData[training_row_number,]
test_data = completedData[-training_row_number,]
dim(train_data)
dim(test_data)

lm_new <- lm(mortality_num ~.,data =  train_data)
# Summarize the results
summary(lm_new)
print(lm_new)

#Checking the model performance
predictions <- predict(lm_new, test_data)
R2 = R2(predictions, test_data$mortality_num)

print(R2)

summary(mlr)

library(corrplot)

corrplot(cor(completedData)) 

