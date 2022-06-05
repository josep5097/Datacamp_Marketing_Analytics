# Binary logistic regression
# 1) Probability to churn
# 2) Log odds
# 3) Removing the log by using exponential function gives us the odds
#    which is the prob to churn divided by the prob not to churn

# library
library(tidyverse)

churn_data <- read.csv("Data/churn_data.csv")
ggplot(churn_data,
       aes(x=returnCustomer))+
  geom_histogram(stat = "count")

logitModelFull <- glm(returnCustomer ~ title + newsletter + websiteDesign,
                      family = binomial,
                      churn_data)
summary(logitModelFull)

coefsExp <- coef(logitModelFull) %>%
  exp() %>%
  round(2)
coefsExp


library(MASS)

logitModelNew <- stepAIC(logitModelFull,
                         trace = 0)
summary(logitModelNew)

# Variables are dropped and added based on their significance

defaultData <- read.csv("Data/defaultData.csv",sep = ";")

# Build logistic regression model
logitModelFull <- glm(PaymentDefault ~ limitBal + sex + education + marriage +
                        age + pay1 + pay2 + pay3 + pay4 + pay5 + pay6 + billAmt1 + 
                        billAmt2 + billAmt3 + billAmt4 + billAmt5 + billAmt6 + payAmt1 + 
                        payAmt2 + payAmt3 + payAmt4 + payAmt5 + payAmt6, 
                      family = binomial, 
                      data = defaultData)

# Take a look at the model
summary(logitModelFull)

# Take a look at the odds ratios
coefsexp <- coef(logitModelFull) %>% exp() %>% round(2)
coefsexp


#Build the new model
logitModelNew <- stepAIC(logitModelFull,trace=0) 

#Look at the model
summary(logitModelNew) 

# Save the formula of the new model (it will be needed for the out-of-sample part) 
formulaLogit <- as.formula(summary(logitModelNew)$call)
formulaLogit

# For logistic regression there areseveral measures of model fit
# Pseudo R^2 statistics; these include the McFadden, the Cox & Snell
# And the Nagelkerke

# Interpretation:
# Reasonable if >0.2
# Good if >0.4
# Very Good if >0.5

library(descr)
LogRegR2(logitModelNew)

# Accuracy:
churn_data$predNew <- predict(logitModelNew,
                              type = "response",
                              na.action = na.exclude)

churn_data %>% 
  dplyr::select(returnCustomer,
         predNew) %>%
  tail()

# Confusion Matrix
remotes::install_version("SDMTools", "1.1-221")
library(SDMTools)

confMatrixNew <- confusion.matrix(churn_data$returnCustomer,
                                  churn_data$predNew,
                                  threshold = 0.5)
confMatrixNew

accuracyNew <- sum(diag(confMatrixNew))/sum(confMatrixNew)
accuracyNew

# Out-of-sample validation and cross validation ====
# Divide the dataset in training and test data

set.seed(534381)

churn_data$isTrain <- rbinom(nrow(churn_data),1,0.66)

train <- subset(churn_data, churn_data$isTrain == 1)
test <- subset(churn_data, churn_data$isTrain == 0)

# 2) build a model based on training data
logitTrainNew <- glm(returnCustomer ~ title + newsletter + websiteDesign +
                       paymentMethod + couponDiscount + purchaseValue + throughAffiliate + 
                       shippingFees + dvd + blueray + vinyl + videogameDownload +
                       prodOthers + prodRemitted,
                     family = binomial,
                     train)

test$predNew <- predict(logitTrainNew, type = "response",
                        newdata = test)

confMatrixNew <- confusion.matrix(test$returnCustomer,
                                  test$predNew,
                                  threshold = 0.3)

accuracyNew <- sum(diag(confMatrixNew))/sum(confMatrixNew)
accuracyNew

# Cross-validation
library(boot)

Acc03 <- function(r, pi=0){
  cm <- confusion.matrix(r, pi, threshold = 0.3)
  acc <- sum(diag(cm))/sum(cm)
  return(acc)
}

set.seed(534381)

cv.glm(churn_data,logitTrainNew, cost = Acc03, K = 6)$delta

