#Import dataset 

COPD <- read.csv('COPD_student_dataset.csv') 

#Histogram of MWT1Best

hist(COPD$MWT1Best,
     main = 'Histogram of MWT1Best',
     xlab = 'MWT1Best',
     ylab = 'Frequency',
     breaks = 12) #specifies the number of bins 

#Examine the values >650..........

#subset(dataframe, variable > x), so here: 
subset(COPD, MWT1Best > 650)