#Ctrl + L; clears console window 

##......................................................................
#Assumptions of linear regression
##......................................................................

#1.There is linearity between the outcome and predictor variable
#2.The outcome variable is normally distributed across values of the predictor
#3.The variance of the outcome variable is constant across values 
#  of the predictor variable

#......................................................................
#Import dataset 
#......................................................................
COPD <- read.csv('COPD_student_dataset.csv') 

##......................................................................
#Histogram of MWT1Best
##......................................................................
hist(COPD$MWT1Best,
     main = 'Histogram of MWT1Best',
     xlab = 'MWT1Best',
     ylab = 'Frequency',
     breaks = 12) #specifies the number of bins 

#Examine the values >650..........

#subset(dataframe, variable > x), so here: 
subset(COPD, MWT1Best > 650)

#Looking for data where MWT1Best > 650 & MWT1Best < 150

subset(COPD, MWT1Best > 600 | MWT1Best < 150) #where ‘|’ stands for ‘and’  

#Histogram of FEV1
hist(COPD$FEV1, main="Histogram of FEV1", xlab="FEV1") 

##......................................................................
#Summarise data of MWT1Best
##......................................................................

list("Summary" = summary(COPD$MWT1Best), 
     "Mean" = mean(COPD$MWT1Best, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$MWT1Best, na.rm=TRUE), 
     "Range" = range(COPD$MWT1Best, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$MWT1Best, na.rm=TRUE)) 

#using na.rm=TRUE prevents errors when there are missing values i.e. NA

#Plotting FEV1 against MWT1Best

plot(COPD$FEV1,COPD$MWT1Best,ylab = 'MWT1Best',xlab = 'FEV1')

##......................................................................
#Calculating the correlation between FEV1 and MWT1Best
##......................................................................

#Command is cor.test(x,y) where the test can be specified
#To prevent errors from missing values, use the command use = “complete.obs” 

cor.test(COPD$FEV1,COPD$MWT1Best, use='complete.obs',method = 'pearson')

##......................................................................
#Calculating the correlation between MWT1Best (walking distance) 
#and age using Spearmans
##......................................................................

plot(COPD$MWT1Best,COPD$AGE,xlab = 'age', ylab = 'MWT1Best')

list("Summary" = summary(COPD$AGE), 
     "Mean" = mean(COPD$AGE, na.rm=TRUE), 
     "Standard Deviation" = sd(COPD$AGE, na.rm=TRUE), 
     "Range" = range(COPD$AGE, na.rm=TRUE), 
     "Inter-Quartile Range" = IQR(COPD$AGE, na.rm=TRUE)) 

hist(COPD$AGE, main="Histogram of AGE", xlab="AGE") 

cor.test(COPD$MWT1Best,COPD$AGE,use='complete.obs',
         method = 'spearman',exact = FALSE)
#Use exact=FALSE to prevent errors with computing p-value

##......................................................................
#Fitting a linear regression model for FEV1 and MWT1Best (walking distance)
##......................................................................

# Command is lm(outcome ~ predictor, data =dataframe)
# The model must be stored as a vector,new vector name<-linear regression model

MWT1Best_FEV1 <- lm(MWT1Best~FEV1, data = COPD)

# MWT1Best_FEV1 is the name of our model 
# MWT1Best is the outcome variable 
# FEV1 is the predictor variable 

summary(MWT1Best_FEV1) # To view the output of this model
confint(MWT1Best_FEV1) # To view 95% confidence intervals  

par(mfrow=c(2,2)) # view all 4 plots in one output 
plot(MWT1Best_FEV1)
par(mfrow=c(1,1))  # go back to a viewing format of one plot per page

##......................................................................
#Fitting a linear regression model for walking distance and age
##......................................................................
  
MWT1Best_AGE <- lm(MWT1Best~AGE, data = COPD)
summary(MWT1Best_AGE)
confint(MWT1Best_AGE)

#Assessing the conditions for linear regression

predictedVals <- predict(MWT1Best_AGE) #gets predicted values for the model
residualVals <- residuals(MWT1Best_AGE)#gets residuals between data and model
plot(MWT1Best_AGE)
hist(residualVals, main = "Histogram of residuals", xlab = "Residuals") 

#......................................................................
# Multiple regression model with lung function, age and walking distance 
#......................................................................

# Command: Model name <- lm(outcome ~ predictor1 + predictor2, data =dataframe) 

MWT1Best_FEV1_AGE <- lm(MWT1Best~FEV1+AGE, data = COPD) 

# MWT1Best_FEV1_AGE is the name of our model 
# FEV1 is the first predictor variable 
# MWT1Best is the outcome variable 
# AGE is the second predictor variable 

summary(MWT1Best_FEV1_AGE) #Output of model 
confint(MWT1Best_FEV1_AGE) #Confidence intervals of model

#......................................................................
# Multiple regression model with FVC, age and walking distance 
#......................................................................

#First find the relationship between FVC and walking distance 

hist(COPD$FVC, breaks = 10)

MWT1Best_FVC <- lm(MWT1Best ~ FVC, data =COPD)
plot(MWT1Best_FVC)

#Finding the individual relationships between the variables

summary(MWT1Best_FVC)
confint(MWT1Best_FVC)

summary(MWT1Best_AGE)
confint(MWT1Best_AGE)

#Multiple regression calculation

MWT1Best_FVC_AGE <- lm(MWT1Best~FVC+AGE, data = COPD)

summary(MWT1Best_FVC_AGE)
confint(MWT1Best_FVC_AGE)

cor.test(COPD$FVC,COPD$AGE,method='spearman',use='complete.obs', exact = FALSE)
# spearman of -0.18 shows a weak correlation betweem FVC and age thus lowering
# the chances of colinearlity 




