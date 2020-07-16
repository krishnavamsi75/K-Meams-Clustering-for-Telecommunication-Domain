library(tidyverse)
library(MASS) 
library(car) 
library(e1071) 
library(caret) 
library(cowplot) 
library(caTools) 
library(pROC) 
library(ggcorrplot)
library(repr)
library(dplyr)
library(corrplot)
library(ggplot2)
telco <- read.csv("E:/B.Tech/SEM-6/BDA/telecom.csv") 
glimpse(telco) 
telco <- telco[complete.cases(telco),] 
telco$SeniorCitizen <- as.factor(ifelse(telco$SeniorCitizen==1, 'YES', 'NO')) 
options(repr.plot.width =6, repr.plot.height = 2)
ggplot(telco, aes(y= tenure, x = "", fill = Churn)) +
  geom_boxplot()+
  theme_bw()+
  xlab(" ")
ggplot(telco, aes(y= MonthlyCharges, x = "", fill = Churn)) +
  geom_boxplot()+
  theme_bw()+
  xlab(" ")
ggplot(telco, aes(y= TotalCharges, x = "", fill = Churn)) +
  geom_boxplot()+
  theme_bw()+
  xlab(" ")
options(repr.plot.width =4, repr.plot.height = 4)
boxplot(telco$TotalCharges,data=telco, main="Total Charges")
boxplot(telco$MonthlyCharges,data=telco, main="Monthly Charges")
boxplot(telco$tenure,data=telco, main="Tenure")
b1 <- boxplot(tenure~Churn,data = telco,col = c("skyblue","violet"), xlab
              ="Churn" , ylab = "tenure")
b2 <- boxplot(MonthlyCharges~Churn,data = telco,col =
                c("skyblue","violet"), xlab ="Churn" , ylab = "MonthlyCharges")
b3 <- boxplot(TotalCharges~Churn,data = telco,col = c("skyblue","violet"),
              xlab ="Churn" , ylab = "TotalCharges")

plot(telco$TotalCharges, telco$tenure)

## corrplot 0.84 loaded
cor_data <- data.frame(telco$tenure,telco$MonthlyCharges,telco$TotalCharges)
corr <- cor(cor_data)
corrplot(corr, method = "number")
hist(telco$tenure, main="Tenure Distribution",xlab="Tenure (Months)")
hist(telco$MonthlyCharges, main="Distribution of Monthly Charges",xlab="Monthly Charges")
hist(telco$TotalCharges, main="Distribution of Monthly Charges",xlab="Monthly Charges")

telco <- data.frame(lapply(telco, function(x) {gsub("No internet service", "No", x)}))
telco <- data.frame(lapply(telco, function(x) {gsub("No phone service", "No", x)}))
num_columns <- c("tenure", "MonthlyCharges", "TotalCharges")
telco[num_columns] <- sapply(telco[num_columns], as.numeric)
telco_int <- telco[,c("tenure", "MonthlyCharges", "TotalCharges")]
telco_int <- data.frame(scale(telco_int))
telco <- mutate(telco, tenure_bin = tenure)
telco$tenure_bin[telco$tenure_bin >=0 & telco$tenure_bin <= 12] <- '0-1 year'
telco$tenure_bin[telco$tenure_bin > 12 & telco$tenure_bin <= 24] <- '1-2 years'
telco$tenure_bin[telco$tenure_bin > 24 & telco$tenure_bin <= 36] <- '2-3 years'
telco$tenure_bin[telco$tenure_bin > 36 & telco$tenure_bin <= 48] <- '3-4 years'
telco$tenure_bin[telco$tenure_bin > 48 & telco$tenure_bin <= 60] <- '4-5 years'
telco$tenure_bin[telco$tenure_bin > 60 & telco$tenure_bin <= 72] <- '5-6 years'
telco$tenure_bin <- as.factor(telco$tenure_bin)
options(repr.plot.width =6, repr.plot.height = 3)
ggplot(telco, aes(tenure_bin, fill = tenure_bin)) + geom_bar()
telco_cat <- telco[,-c(1,6,19,20)]
#Creating Dummy Variables
dummy<- data.frame(sapply(telco_cat,function(x) data.frame(model.matrix(~x-1,data =telco_cat))[,-1]))
head(dummy)
#Combining the data
telco_final <- cbind(telco_int,dummy)
head(telco_final)
#Splitting the data
set.seed(123)
indices = sample.split(telco_final$Churn, SplitRatio = 0.7)
train = telco_final[indices,]
validation = telco_final[!(indices),]
model_1 = kmeans(telco_final,centers=2, iter.max = 25, nstart=100)
model_1
summary(model_1)
model_1$cluster <- as.factor(model_1$cluster)
ggplot(telco, aes(tenure, TotalCharges, color = telco$cluster)) + geom_point()
pred <- model_1$betweenss/model_1$totss*100
pred


