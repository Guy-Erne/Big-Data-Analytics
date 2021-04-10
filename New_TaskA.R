library(readr)
DiamondDataComplete <- read.csv(file.choose(), header=TRUE)
View(DiamondDataComplete)



colnames(DiamondDataComplete)
attach(DiamondDataComplete)


sum(is.na(DiamondDataComplete$carat))
sum(is.na(DiamondDataComplete$cut))
sum(is.na(DiamondDataComplete$color))
sum(is.na(DiamondDataComplete$clarity))
sum(is.na(DiamondDataComplete$depth))
sum(is.na(DiamondDataComplete$table))
sum(is.na(DiamondDataComplete$price))
sum(is.na(DiamondDataComplete$x))
sum(is.na(DiamondDataComplete$y))
sum(is.na(DiamondDataComplete$z))


#hist(carat)
#Carat is not normally distributed and it is right skewed

colnames(DiamondDataComplete)

#[1] "carat"   "cut"     "color"   "clarity" "depth"  
#[6] "table"   "price"   "x"       "y"       "z"  

#hist(depth)
#depth is normally distributed

#hist(table)
#table is night skewed

#hist(price)
#price is also right skewed

#hist(x)
#x is normally distributed


#hist(y)
#y is right skewed and negatively distributed


#hist(z)
#z  is right skewed and negatively distributed


mean(carat)
median(carat)


getmode <- function(v){
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}
result_carat <- getmode(carat) #mode of carat
result_carat

mean(depth)   #mean of depth
median(depth) #median of depth
getmode(depth) #mode of depth


mean(table)    #mean of table
median(table)  #median of table
getmode(table) #mode of table



mean(price)    #mean of price
median(price)  #median of price
getmode(price)#mode of price


mean(x)        #mean of x - length
median(x)      #median of x - width
getmode(x)     #mode of x - depth



mean(y)      #mean of y - width
median(y)    #median of y - width
getmode(y)   #mode of y - width


cor(price, carat)    #[1] 0.9213562
cor(price, depth)    #[1] -0.01213396      
cor(price, x)        #[1] 0.884927
cor(price, y)        #[1] 0.8817025
cor(price, z)        #[1] 0.8607349
cor(price, table)    #[1] 0.1278579
cor(x, y)            #[1] 0.9927107
color = as.numeric(color)
cor(price, color) #[1] 0.174691
clarity = as.numeric(clarity)
cor(price, clarity) #[1] -0.07123085
cut <- as.numeric(cut)
cor(price, cut) #[1] 0.04027193


#library(corrplot)
#install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

Diamond_Data <- DiamondDataComplete[, c(1,5,6,7,8,9,10)] #10
chart.Correlation(Diamond_Data, histogram = TRUE, pch = 19)

#Get some colors
col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(Diamond_Data, col = col, symm = TRUE)

library(corrplot)
library(RColorBrewer)
M <- cor(DiamondDataComplete)
corrplot(M, type = "upper", order = "hclust",
         col = brewer.pal(n = 8, name = "RdYlBu"))

source("http://www.sthda.com/upload/rquery_cormat.r")
require("corrplot")
rquery.cormat(Diamond_Data)
Diamond_Data
View(Diamond_Data)


table(cut)
table(clarity)
table(color)

str(cut)
str(clarity)
str(color)

#cut
#Fair      Good     Ideal   Premium Very Good 
#1500      4539     20011     12770     11180 

#clarity
#I1    IF   SI1   SI2   VS1   VS2  VVS1  VVS2 
#679  1667 12110  8501  7582 11365  3404  4692 

#color
#D     E     F     G     H     I     J 
#6269  9097  8854 10463  7666  5029  2622

as.numeric(cut)
as.numeric(clarity)
as.numeric(color)
table(as.numeric(cut))
table(as.numeric(clarity))
table(as.numeric(color))



DiamondDataComplete$cut <- factor(DiamondDataComplete$cut)
DiamondDataComplete$clarity <- factor(DiamondDataComplete$clarity)
DiamondDataComplete$color <- factor(DiamondDataComplete$color)


View(DiamondDataComplete)

str(DiamondDataComplete$cut)
str(DiamondDataComplete$color)
str(DiamondDataComplete$clarity)


DiamondDataComplete$cut <- as.numeric(DiamondDataComplete$cut)
DiamondDataComplete$color <- as.numeric(DiamondDataComplete$color)
DiamondDataComplete$clarity <- as.numeric(DiamondDataComplete$clarity)



hist(DiamondDataComplete$cut)
hist(DiamondDataComplete$color)
hist(DiamondDataComplete$clarity)

#summary(DiamondDataComplete)

install.packages("nortest")
library(nortest)

ad.test(DiamondDataComplete$cut)
ad.test(DiamondDataComplete$color)
ad.test(DiamondDataComplete$clarity)


ad.test(DiamondDataComplete$price)


#Hpothesis testing - 1
#Null Hypothesis - There is no significant difference between Price and carat
#Alternate Hypothesis - There is a relationship between price and carat

ad.test(DiamondDataComplete$price) 

#data:  DiamondDataComplete$price
#A = 3214.2, p-value < 2.2e-16


ad.test(DiamondDataComplete$carat)

#data:  DiamondDataComplete$carat
#A = 1415.2, p-value < 2.2e-16

#Manwhiteney test is used because price and carat both are not normal

wilcox.test(price, carat)

#data:  price and carat
#W = 2.5e+09, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0


#conclusion - accept alternate hypothesis


#Hypothesis testing - 2
#Null hypothesis - There is no relationship between price and depth
#Alternate Hypothesis - There is a relationship between price and depth

ad.test(DiamondDataComplete$price) 
 
#data:  DiamondDataComplete$price
#A = 3214.2, p-value < 2.2e-16


ad.test(depth)
#data:  depth
#A = 455.55, p-value < 2.2e-16


#Manwhitney test is used
wilcox.test(DiamondDataComplete$price, DiamondDataComplete$depth)
#data:  DiamondDataComplete$price and DiamondDataComplete$depth

#W = 2.5e+09, p-value < 2.2e-16
#alternative hypothesis: true location shift is not equal to 0

#Accept alternate Hypothesis - there is a relationship between price and depth


#The following findings are done on complete Diamond Data. 

#In 1st Hypothesis test, the price of the diamond is related to the carat of the diamond.We reject Null Hypothesis(h0) and accept alternate hypothesis. we used wilcox.test because both price variable and carat variable are not normal. The nnormality is tested by using anderson darling test which gives normality. When data is not normal, we use, Non-Parametric test. 

#In 2nd Hypothesis, the price of the diamond and depth is related. Null hypothesis is rejected because both are having relationship which is known by wilcox.test. The normality is tested using anderson darling test to know the normality of the both variables, price and depth. 








