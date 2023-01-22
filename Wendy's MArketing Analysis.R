#Read File
raw_data <- read.csv(file.choose(), header=T)
head(raw_data)
View(raw_data)
library(tidyverse)
library(dplyr)
library(magrittr)

####Question 1: looking at missing values in raw data and creating data.clean
w1=(raw_data$rating_wendys)

w2=(raw_data$patronage_wendys)

w3=(raw_data$consumption_rate)


##Counting unique value, missing values and median
raw_data %>%
  summarise(n = n_distinct(w1),
            na= sum(is.na(w1)),
            med= median(w1, na.rm = TRUE))
raw_data %>%
  summarise(n = n_distinct(w2),
            na= sum(is.na(w2)),
            med= median(w2, na.rm = TRUE))
raw_data %>%
  summarise(n = n_distinct(w3),
            na= sum(is.na(w3)),
            med= median(w3, na.rm = TRUE))

#mutate missing value
raw_data = raw_data %>%
  mutate(rating_wendys
         =replace(rating_wendys,
                  is.na(rating_wendys),
                  median(rating_wendys, na.rm = TRUE)))
raw_data = raw_data %>%
  mutate(consumption_rate
         =replace(consumption_rate,
                  is.na(consumption_rate),
                  median(consumption_rate, na.rm = TRUE)))
raw_data = raw_data %>%
  mutate(patronage_wendys
         =replace(patronage_wendys,
                  is.na(patronage_wendys),
                  median(patronage_wendys, na.rm = TRUE)))
data.clean = select(raw_data, rating_wendys, consumption_rate, patronage_wendys)
summary(data.clean)

###Ques 2 Linear Regression
fit1<-lm(rating_wendys ~ patronage_wendys + consumption_rate, data = data.clean)
summary(fit1)
####Ques 3 Coefficients and their significance
#Regression Equation : Yi=β0+ β1X1 + β2X2
#Here, β0 = Intercept Value in estimate column = 7.641
#β1 = Estimate Value in consumption_rate row = -0.0041
#β2 = Estimate Value in patronage_wendys row = 0.0573

#So, the equation will be:
 # Y(rating_wendys) = 7.641 - 0.0041(consumption_rate) + 0.0573 (patronage_wendys) 


### a) Statistical Significance of Coefficients-
#### i. consumption_rate: Since, the p-value for consumption_rate is 0.480 which is greater than 0.05 hence this variable does not have a significant effect on wendy's rating. 
#### 11. patronage_wendys: Since, the p-value for patronage_wendys is 0.228 which is greater than 0.05 hence this variable does not have a significant effect on wendy's rating. 
####b) Valences of Coefficient
## i. Estimate value for consumption_rate is -0.0041 which means that consumption_rate has negative effect on wendy's rating.
## ii. Estimate value for patronage_wendys is 0.0573 which means that patronage_wendys has positive effect on wendy's rating.
## overall, In the survey, we found no significant relationships between the frequency of visiting wendys and wendy's rating and the consumption rate of fast food and wendy's rating (p = 0.228 and 0.480 respectively). Specifically we found a 0.004% decrease (± 0.0058) in the wendy's rating for every 1% increase in consumption rate, and a 0.057% increase (± 0.047) in the wendy's rating for every 1% increase in patronage_wendys.

### Ques 4: Prediction
#Define new data
new <- data.frame(patronage_wendys =c(8),consumption_rate=c(1))

#use the fitted model to predict the rating_wendys
predict(fit1, newdata=new)

##Hence, As per the output, Predicted rating will be 8.09 ~ 8

###Question 5: New Variable "Loyal"
##creating new variable 'loyal'
data.clean$loyal <- rep(0,dim(data.clean)[1])
View(data.clean)

## Filling new variable as per the condition 1 and 0 desicbe in the ques.
data.clean = data.clean %>%
  mutate(loyal = case_when(patronage_wendys < 2 ~ '1',
                           patronage_wendys > 1 ~ '0'))

### Scatter plot and regression curve
library(ggplot2)

plot(data.clean$rating_wendys, data.clean$loyal,  main = "", xlab="", ylab="",
     col.axis="blue") 
title(main = "Scatter Plot of Wendy's Ratings and Customer Loyalty",
      xlab = "Rating_Wendys", ylab = "Loyal",
      cex.main = 1,   font.main= 3, col.main= "red",
      cex.sub = 0.75, font.sub = 3, col.sub = "green",
      col.lab ="darkblue",
)
abline(lm(data.clean$loyal ~ data.clean$rating_wendys), col = "red")
### from the graph, we can see that, regression curve is slightly upward trending. This means that if the rating for the Wendy given by customer increases then the frequency of visiting the store will be increased(within the four weeks).

