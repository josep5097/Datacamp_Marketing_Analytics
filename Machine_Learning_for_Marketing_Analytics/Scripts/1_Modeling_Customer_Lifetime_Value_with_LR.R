# Customer Lifetime Value - CLV
# Predicted future net-profit
# Identify promising customers
# Prioritze customers according to future margins 
# No furher customer segmentation

# Library ====
library(tidyverse)
library(corrplot)

# Data ====
salesData <- read.csv("Data/salesData.csv")
clvData1 <- read.csv("Data/clvData1.csv")

# Structure of dataset
str(salesData, give.attr = FALSE)

# Visualization of correlations
salesData %>% select_if(is.numeric) %>%
  select(-id) %>%
  cor() %>%
  corrplot()

# Frequent stores
ggplot(salesData) +
  geom_boxplot(aes(x = mostFreqStore, y = salesThisMon))

# Preferred brand
ggplot(salesData) +
  geom_boxplot(aes(x = preferredBrand, y = salesThisMon ))


# Simple Linear Regression
simpleLM <- lm(futureMargin ~ margin, data = clvData1)
summary(simpleLM)

ggplot(clvData1,
       aes(margin, futureMargin))+
  geom_point()+
  geom_smooth(method = lm,
              se = F)+
  xlab("Margin year 1")+
  ylab("Margin year 2")


# Model specification using lm
salesSimpleModel <- lm(salesThisMon ~ salesLast3Mon  , 
                       data = salesData)

# Looking at model summary
summary(salesSimpleModel)

# Multiple R-Squared:0.5932, which explains almost 60% of the variation


# Multiple Linear Regression ====
# Omited variable bias> When a variable not included in the regression
# is correlated with both, the explanatory var and the response var

multipleLM <- lm(
  futureMargin ~ margin + nOrders + nItems +
    daysSinceLastOrder + returnRatio + shareVoucher +
    shareSale + gender + age + marginPerOrder +
    marginPerItem + itemsPerOrder, 
  data = clvData1
)

summary(multipleLM)

# Multicollinearity is one threat to a multiple linear regression
# This occurs whenever one explanatory var can be explained by 
# the remaining explanatory variables.

# The reg coeffs become unstable and the se are under estimated

# In oreder to determine multicollinearity:
# Calculate Variance Inflation Factors (VIFs)
library(rms)
vif(multipleLM)

# This indicate the increase in the variance of an estimated coeff
# due to multicollinearity

# A VIF higher than 5 is problematic and values above 10 indicates
# poor regression estimates.
multipleLM2 <- lm(
  futureMargin ~ margin + nOrders +
    daysSinceLastOrder + returnRatio + shareVoucher +
    shareSale + gender + age + marginPerOrder +
    marginPerItem + itemsPerOrder, 
  data = clvData1
)

vif(multipleLM2)
summary(multipleLM2)

# Estimating the full model
salesModel1 <- lm(salesThisMon ~ . - id, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel1)

# Estimating new model by removing information on brand
salesModel2 <- lm(salesThisMon ~ . - id - preferredBrand - nBrands, 
                  data = salesData)

# Checking variance inflation factors
vif(salesModel2)

# Model validation, model fit and prediction ====

## Multiple R square
# R^2 = 1: model that explains 100% of the dependent variables variation

## F test: Is the test for the overall fit of the model
# This say that at least one regressor has significant explanatory power

# Methods to avoid overfitting ====
# AIC from stats 
# stepAIC() from mass
# Out of sample model validation
# cross-validation

AIC(multipleLM2)

# New dataset clvData2
salesData2_4 <- read.csv("Data/salesDataMon2to4.csv")

# getting an overview of new data
summary(salesData2_4)

# predicting sales
predSales5 <- predict(salesModel2, newdata = salesData2_4)

# calculating mean of future sales
mean(predSales5)