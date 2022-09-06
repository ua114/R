#Ctrl + L; clears console window

#......................................................................
# Import dataset 
#......................................................................

COPD <- read.csv('COPD_student_dataset.csv') 

#......................................................................
# Finding the dimensions of the data
#......................................................................

dim(COPD) #shows the number of rows and columns
head(COPD) #prints the first few rows of the data

class(COPD$AGE) #finding what type of variable is present 

#......................................................................
# Inspect the distribution of the variable CAT
#......................................................................

class(COPD$CAT)
summary(COPD$CAT)
hist(COPD$CAT)

# value at 188 appears to be an outlier 
subset(COPD, CAT > 150)

#......................................................................
# Inspect the distribution of the variable COPDSEVERITY
#......................................................................

class(COPD$COPDSEVERITY)
table(COPD$COPDSEVERITY, exclude = NULL) #finding the distribution of entries
# exclude = NULL asks R to include missing values in the output.

#......................................................................
# Assigning class type to variables
#......................................................................

#R will by default treat variables with numbers as integers, so it's good 
#practice to make sure R knows that a variable is meant to be categorical 
#with the following procedure.

class(COPD$gender)
COPD$gender <- as.factor(COPD$gender) 
class(COPD$gender) 

# use as.numeric() to change to numeric data type
# use as.charater() to change to character data type
# use as.integer() to change to integer data type 

#......................................................................
# Performing a regression between MWT1Best and copd (categorical variable)
#......................................................................

class(COPD$MWT1Best)
class(COPD$copd)

# copd is interpreted as an integer and must be changed to categorical 
COPD$copd <- as.factor(COPD$copd)
class(COPD$copd)

table(COPD$copd, exclude = NULL)

lr1 <- lm(MWT1Best ~ copd, data =COPD)
summary(lr1) # view the output of the regression

#......................................................................
# Changing the reference category of a categorical variable 
#......................................................................

COPD$copd <- relevel(COPD$copd, ref=3) # changing ref to 'severe'
lr1 <- lm(MWT1Best ~ copd, data =COPD)
summary (lr1)

# this function only works with variables saved as factors!  

#......................................................................
# Creating new variables from old ones
#......................................................................

# e.g. new variable comorbid indicates the presence of at least one comorbidity 
# or complete absence of comorbidities, based on the responses to the 
# variables: Diabetes, muscular, hypertension, AtrialFib, and IHD.
# 1 = positive, 0 = negative

# define length of new variable
comorbid <- length(COPD$Diabetes) 

comorbid[COPD$Diabetes == 1 | COPD$muscular == 1 | COPD$hypertension == 1 |
           COPD$AtrialFib ==1 | COPD$IHD == 1] <- 1
# comorbid = 1 if any condition is met

comorbid[is.na(comorbid)] <- 0
#Assign a value of 0 for comorbid values which are 'NA'

comorbid <- factor(comorbid)
# Converts comorbid to a factor value

COPD$comorbid <- comorbid
# Add new variable to existing data set

#......................................................................
# Steps in running a good analysis 
#......................................................................

# 1. Inspect the dataset for missing values and outliers 

#using Hmisc package
library(Hmisc)
describe(COPD)

# 2. Examine the relationship between your candidate predictor variables 

# e.g. correlation matrix between  AGE, PackHistory, FEV1, FEV1PRED, FVC, 
# CAT, HAD, and SGRQ

my_data <- COPD[,c("AGE","PackHistory","FEV1","FEV1PRED","FVC","CAT",
                   'HAD',"SGRQ")]








