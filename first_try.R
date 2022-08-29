#Import dataset 

g <- read.csv(file="cancer data for MOOC 1.csv", header=TRUE, sep=',')

#simple data analysis

gender <- g[,'gender']

#Tabulate gender in two categories, 1 and 0
table(gender)


#calculate food and vegetable consumption

fruit <- g[,'fruit']
veg <- g[,'veg']
fruitveg = fruit + veg
hist(fruitveg)

#BMI

bmi <- g[,'bmi']
summary(bmi)
hist(bmi)

#Age and BMI

age <- g[,'age']
bmi <- g[,'bmi']
plot(age,bmi)

#Calculating how many people eat more than 5 fruit and veg.......

g$fruitveg <- g$fruit + g$veg 
g$five_a_day <- ifelse(g$fruitveg >=5, 1, 0) #if fruitveg is >5, output is 1...
#otherwise output is 0
table(g$five_a_day)

#Improving histograms.......

hist(g$fruitveg,
main = "Daily consumption of fruit and vegetables combined")
axis(side = 1, at = seq(0, 11, 1))
axis(side = 2, at = seq(0, 16, 2))

#seq(0, 11, 1) draws a sequence of numbers from 0 to 11 at intervals of 1

#Using plotting packages

#ggplot() tells R to start making a plot. You layer pieces onto the plot using +
#,and the piece you’re going to add is a histogram using the 
#function geom_histogram(). Within this, you can design the histogram we want. 
#The argument data = g tells R that the data set you want to make your plot 
#from is called g. The next argument aes() is where you describe the variables.

require(ggplot2)
ggplot() + geom_histogram(data = g, aes(x = fruitveg),bins=10,fill = "darkgreen"
                          ,col = "black")
labs(x = "Portions of fruit and vegetables", y = "Frequency") +
  
scale_x_continuous(breaks = seq(from = 0, to = 12, by = 1)) + theme_bw()

#There are some pre-made “themes” that facilitate easy aesthetic improvement 
#of your plots; theme_bw() is one of these

#Classifying BMI into normal (0) or abnormal (1)

g$bmi_state <- ifelse(g$bmi >24.9 |g$bmi <18.5,1,0)
table(g$bmi_state)

#Finding the link of high BMI and cancer.......

g$bmi_over <- ifelse(g$bmi >=25, 1, 0) #if BMI is >25, output is 1 else 0...
#otherwise output is 0
table(g$bmi_over)
chisq.test(x=g$bmi_over,y=g$cancer) 
t.test(g$bmi_over~g$cancer)
