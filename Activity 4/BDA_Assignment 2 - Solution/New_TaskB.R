

#install.packages("readr")
library(readr)
#install.packages("rlang")
library(rlang)




DiamondDataComplete <- read.csv(file.choose(), header=TRUE)




View(DiamondDataComplete)


summary(DiamondDataComplete)

#   carat            cut                color          
#Min.   :0.2000   Length:50000       Length:50000      
#1st Qu.:0.4000   Class :character   Class :character  
#Median :0.7000   Mode  :character   Mode  :character  
#Mean   :0.7974                                        
#3rd Qu.:1.0400                                        
#Max.   :5.0100                                        
#   clarity            depth           table           price      
#Length:50000       Min.   :43.00   Min.   :43.00   Min.   :  326  
#Class :character   1st Qu.:61.00   1st Qu.:56.00   1st Qu.:  949  
#Mode  :character   Median :61.80   Median :57.00   Median : 2401  
#Mean   :61.75   Mean   :57.45   Mean   : 3925  
#3rd Qu.:62.50   3rd Qu.:59.00   3rd Qu.: 5312  
#Max.   :79.00   Max.   :95.00   Max.   :18823  
#     x               y                z         
#Min.   : 0.00   Min.   : 0.000   Min.   : 0.000  
#1st Qu.: 4.71   1st Qu.: 4.720   1st Qu.: 2.910  
#Median : 5.70   Median : 5.710   Median : 3.520  
#Mean   : 5.73   Mean   : 5.732   Mean   : 3.538  
#3rd Qu.: 6.54   3rd Qu.: 6.540   3rd Qu.: 4.030  
#Max.   :10.74   Max.   :31.800   Max.   :31.800 




attach(DiamondDataComplete)


boxplot(DiamondDataComplete$price)


hist(DiamondDataComplete$price) #Price is Right Skewed 



hist(DiamondDataComplete$depth) #Depth is Normally Distributed






hist(DiamondDataComplete$table) #Table is left skewed





hist(DiamondDataComplete$x) #Length of the diamond is right skewed





hist(DiamondDataComplete$y) #Width of the diamond is right skewed




hist(DiamondDataComplete$z) #Depth of the diamond is left skewed 




boxplot(DiamondDataComplete$z) #Complete Data is distributed left side






names(DiamondDataComplete)

#[1] "carat"   "cut"     "color"   "clarity" "depth"   "table"  
#[7] "price"   "x"       "y"       "z




table(cut)
#cut
#Fair      Good     Ideal   Premium Very Good 
#1500      4539     20011     12770     11180 




table(color)




color
#D     E     F     G     H     I     J 
#6269  9097  8854 10463  7666  5029  2622






table(clarity)





clarity
#I1    IF   SI1   SI2   VS1   VS2  VVS1  VVS2 
#679  1667 12110  8501  7582 11365  3404  4692





DiamondDataComplete$cut = factor(DiamondDataComplete$cut, levels = c('Fair','Good','Ideal', 'Premium','Very Good'), labels = c(0,1,2,3,4))




DiamondDataComplete$color = factor(DiamondDataComplete$color, levels = c('D','E','F','G','H','I','J'), labels = c(0,1,2,3,4,5,6))





DiamondDataComplete$clarity = factor(DiamondDataComplete$clarity, levels = c('I1','IF','SI1','SI2','VS1','VS2','VVS1','VVS2'), labels = c(0,1,2,3,4,5,6,7))




str(DiamondDataComplete$cut)




str(DiamondDataComplete$color)




str(DiamondDataComplete$clarity)





model <- lm(DiamondDataComplete$price ~ DiamondDataComplete$carat + factor(DiamondDataComplete$cut) + factor(DiamondDataComplete$color) + factor(DiamondDataComplete$clarity) + DiamondDataComplete$depth + DiamondDataComplete$table + DiamondDataComplete$price + DiamondDataComplete$x + DiamondDataComplete$y + DiamondDataComplete$z, data = DiamondDataComplete)


model




summary(model) #All are significant except depth(z) 

#Multiple R-squared:  0.9194,	Adjusted R-squared:  0.9193 

#Now finding the correlation between the features


cor(price, carat)  #highly correlated = 0.9213562



cor(price, table)  #0.1278579



cor(price, x) #0.884927



cor(price, y) #0.8817025




cor(price, z) #0.8607349



cor(price, depth) #-0.01213396 - Negatively correlated








require(caTools) 




library(caTools)



#Data is Divided into Training and testing Data 





data_split <- sort(sample(nrow(DiamondDataComplete), nrow(DiamondDataComplete)*.75))





train_Data <- DiamondDataComplete[data_split, ]



test_data <- DiamondDataComplete[-data_split, ]





nrow(train_Data)  #37500




nrow(test_data)   #12500




View(train_Data)




View(test_Data)





#Model built on Training data
train_model <- lm(train_Data$price ~., data = train_Data)
train_model





summary(train_model)





#y - width and z - depth are not significant, hence removed
train_model_final <- lm(train_Data$price ~ train_Data$carat + train_Data$cut + train_Data$color + train_Data$clarity + train_Data$depth + train_Data$depth + train_Data$table + train_Data$x, data = train_Data)






summary(train_model_final)  #All are significant 




#Multiple R-squared:  0.9191,	Adjusted R-squared:  0.919

#predicting the test data and findind RMSE
#new_Data <- data.frame(price_test = test_Data)
#output <- cbind(testdata, prediction)

#new_test <- data.frame(testData = test_data$price)
#prediction <- predict(train_model_final, newdata = new_test, interval = "confidence")
#summary(prediction)
#nrow(prediction)

#model bulding on test data




test_model <- lm(test_data$price ~., data = test_data)





test_model





summary(test_model)





#y - width and z - depth are not significant, hence removed






test_model_final <- lm(test_data$price ~ test_data$carat + test_data$cut + test_data$color + test_data$clarity + test_data$depth + test_data$table + test_data$price + test_data$x, data = test_data)




summary(test_model_final)





pred_train <- predict(train_model_final)






pred_test <- predict(test_model_final)



error = test_data$price - pred_test





error



summary(error)





rmse_test <- sqrt(mean(error^2))




rmse_test #1141.406 - RMSE of test data




actual_pred <- data.frame(cbind(test_data$price, pred_test))








correlation_accuracy <- cor(actual_pred)



correlation_accuracy
#              V1    pred_test
#V1        1.0000000 0.9576256
#pred_test 0.9576256 1.0000000    #Correlation accuracy of test data is = 95.7%






head(actual_pred)
#V1 pred_test
#1   957  1948.010
#2  4554  5138.437
#3 13853 10588.046
#4  4858  4796.315
#5 15814 14239.720
#6  2431  3167.320






#predict the train model and find RMSE
prediction_train <- predict(train_model_final)






error_train = train_Data$price - prediction_train







error_train





rmse_train <- sqrt(mean(error_train^2))





rmse_train  #1126.223 - RMSE of train data






plot(test_data$price, pred_test)






plot(train_Data$price, pred_train)

#Normalization of the diamond data






normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}






#Applying normalization on only numerical data
DiamondDataComplete_norm <- normalize(DiamondDataComplete[c(1,5,6,7,8,9,10)])
DiamondDataComplete_norm
View(DiamondDataComplete_norm)  #Data is normalized and removed categorical variables







#Building Multiple linear regression model on normalized diamond data

norm_model <- lm(DiamondDataComplete_norm$price ~., data = DiamondDataComplete_norm)
norm_model






summary(norm_model) #Z(depth) is not significant

#Removing the insignificant and building the model again





final_norm_model <- lm(DiamondDataComplete_norm$price ~ DiamondDataComplete_norm$carat + DiamondDataComplete_norm$depth + DiamondDataComplete_norm$table + DiamondDataComplete_norm$x + DiamondDataComplete_norm$y, data = DiamondDataComplete_norm)
final_norm_model





summary(final_norm_model)

#Multiple R-squared:  0.8588,	Adjusted R-squared:  0.8588 

#Splitting data in to train and test

data_split <- sort(sample(nrow(DiamondDataComplete_norm), nrow(DiamondDataComplete_norm)*.75))


train_Data_norm <- DiamondDataComplete_norm[data_split, ]
test_Data_norm <- DiamondDataComplete_norm[-data_split, ]



#Bulding model on train data

train_norm <- lm(train_Data_norm$price ~ train_Data_norm$carat + train_Data_norm$depth + train_Data_norm$table + train_Data_norm$x + train_Data_norm$y + train_Data_norm$z)
train_norm
summary(train_norm)

#Z(depth) is not significant
#Removing the z - depth and building final train model

train_norm_final <- lm(train_Data_norm$price ~ train_Data_norm$carat + train_Data_norm$depth + train_Data_norm$table + train_Data_norm$x + train_Data_norm$y, data = train_Data_norm)
train_norm_final
summary(train_norm_final) #All are significant
#Multiple R-squared:  0.8573,	Adjusted R-squared:  0.8573

#predicting the test data using train model

test_norm <- lm(test_Data_norm$price ~., data = test_Data_norm)
summary(test_norm)  #All are significant

pred_test_norm <- predict(test_norm)
pred_train_norm <- predict(train_norm_final)


error_train_norm <- train_Data_norm$price - pred_train_norm
error_test_norm <- test_Data_norm$price - pred_test_norm

error_test_norm
error_train_norm


rmse_test_norm <- sqrt(mean(error_test_norm^2))
rmse_test_norm #0.07909287 - RMSE of test data 


rmse_train_norm <- sqrt(mean(error_train_norm^2))
rmse_train_norm #0.07942949 - RMSE of train data

#plot(train_Data_norm$price, pred_train_norm)
#plot(test_Data_norm$price, pred_test_norm)








