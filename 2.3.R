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


