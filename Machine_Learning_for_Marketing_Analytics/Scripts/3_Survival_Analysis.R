# Survival Analysis: Introduction
# Advantages survival model 
# Less aggregation
# Allow us to model when an event takes place
# No arbitrary set timeframe
# Deeper insights into customer relations 

# Since customers enter our DB at different points in time,
# censoring times can vary between subjects

# Two different pieces of information necessary:
#   Time under observation 
#   Status at the end of this time

# According to the status we can conclude if an observation was
# censored or not

# Library 
library(tidyverse)

dataSurv <- read.csv("Data/survivalDataExercise.csv")

head(dataSurv)

# Plot a histogram
ggplot(dataSurv) +
  geom_histogram(aes(x = daysSinceFirstPurch,
                     fill = factor(boughtAgain))) +
  facet_grid( ~ boughtAgain) + # Separate plots for boughtAgain = 1 vs. 0
  theme(legend.position = "none") # Don't show legend


# Survival curve analysis by Kaplan Meier

# The measue of interest in a survival analysis is the survival function
#   Gives the prob that a customer will not churn in the perior leading up to the time point t

# The counterpart to the survival function is the cumulative hazard function
#   It describes the cumulative risk, or the prob that a customer will have churned up until time t

# The hazard rate, also called force of mortality or instantaneous event rate, describes the risk
# that an event occur in a small interval arount time t, given that the event has not yet happened

# For survival function: Calculate the percentage of customers who have not churned yet
# at each respective timepoint
# The kaplan-Meier analysis takes into account the number of customers whoe churned and
# the so-called "Number at risk", that is, the customers who are still under contract and might churn
# in the future.
library(survival)

datatelco <- read.csv("Data/Telco-Customer-Churn.csv")
datatelco <- datatelco %>%
  mutate(tenure = as.numeric(tenure),
         Churn = as.factor(Churn),
         Churn  = as.numeric(Churn),
         Churn = ifelse(Churn == 1,1,0))

fitKM <- survfit(Surv((datatelco$tenure), datatelco$Churn)~1,
                 type = "kaplan-meier")
fitKM$surv

print(fitKM)
plot(fitKM)

fitMstr <- survfit(Surv(tenure, Churn)~Partner,
                   data = datatelco)
print(fitMstr)

plot(fitMstr, lty = 2:3)
legend(10, .5, c("No", "Yes"), lty = 2:3)


# Exercise
# Create survival object
survObj <- Surv(dataSurv$daysSinceFirstPurch, 
                dataSurv$boughtAgain)
# daysSinceFirstPurch contains the time between first 
# and second order. 
# boughtAgain tells you if there was a second order or not.

# Look at structure
str(survObj)
# for each observation there is the time under observation,
# marked with a + if the second order has nor been placed yet


# Compute and print fit
fitKMSimple <- survfit(survObj ~ 1)
print(fitKMSimple)

# Plot fit
plot(fitKMSimple, conf.int = FALSE,
     xlab = "Time since first purchase", ylab = "Survival function", main = "Survival function")

# Compute fit with categorical covariate
fitKMCov <- survfit(survObj ~ voucher, data = dataSurv)

# Plot fit with covariate and add labels
plot(fitKMCov, lty = 2:3,
     xlab = "Time since first purchase", ylab = "Survival function", main = "Survival function \n Voucher")
legend(90, .9, c("No", "Yes"), lty = 2:3)

# Customer using voucher seem to take longer to place their second order

# Cox PH model with constant covariates ====
# In order to check the effecto of multiple customer characteristics on the 
# risk of churn.
# Cox proportional hazards model

## Assumptions ====
# * Predictors are linearly and additively related to the log hazard
# Our model has this form (lambda is the hazard funct)

# Lambda(t|x) = lambda(t)*exp(x'Beta)

# No assumption regarding the shape of the hazard function
# Proportional hazards assumptions, this says that the predictos are not allowed to interact with time

# Relative hazard function exp(x'Beta) constant over time
library(rms)
# Specify the units that time is measued in as moths
units(datatelco$tenure) <- "Month"
# Determine the distributions of the predicto variables 
dd <- datadist(datatelco)
# In order to make the results permanently accesible,
# I add to the global options
options(datadist = "dd")

# When is all set to specify the model using the cph()
fitCPH1 <- cph(Surv(tenure, Churn)~gender + SeniorCitizen +
                 Partner + Dependents + PaperlessBilling + PaymentMethod + MonthlyCharges,
               data = datatelco,
               x = T,
               y = T,
               surv = T,
               time.inc = 1)

print(fitCPH1)
# In cox proportional hazard model, coeffs are interpreted similar to
# logistic regression

# From the untransformed coeff I can only draw conclussions about the direction
# of the effect

# The model coefficients are stored in the element Coefficients
# By transforming them using the exp function, interpretation get easier
exp(fitCPH1$coefficients)

survplot(fitCPH1,
         MonthlyCharges,
         label.curves = list(keys = 1:5))

# The effect of having a partner or not is visualized
survplot(fitCPH1, Partner)

# A way to visualize the hazard ratios of the coeffs is given by the plot
plot(summary(fitCPH1), log = T)

# Determine distributions of predictor variables
dd <- datadist(dataSurv)
options(datadist = "dd")

# Compute Cox PH Model and print results
fitCPH <- cph(Surv(daysSinceFirstPurch, boughtAgain) ~ shoppingCartValue + voucher + returned + gender,
              data = dataSurv,
              x = TRUE, y = TRUE, surv = TRUE)
print(fitCPH)

# Interpret coefficients
exp(fitCPH$coefficients)

# Plot result summary
plot(summary(fitCPH), log = TRUE)

# You can see that a shopping cart value increase of 1 dollar decreases the 
# hazard to buy again by a factor of only slightly below 1 - 
# but the coefficient is significant, as are all coefficients. 
# For customers who used a voucher, the hazard is 0.74 times lower, 
# and for customers who returned any of the items, the hazard is 0.73 
# times lower. Being a man compared to a woman increases the hazard 
# of buying again by the factor 1.11.

# Checking model assumptions and making predicitions ====
# We start by validating the proportional hazard assumption
testCPH1 <- cox.zph(fitCPH1)
print(testCPH1)
# If p<0.05 we can reject the hyp thtat the given variable meets 
# the proportional hazard assumption 

# The monthlyCharges violates the prop hazard assumption, hence,
# their effect changes over time

# Visualizing the estimates of the coeff beta dependent on time gives
# futher insights
plot(testCPH1, var = "Partner")
plot(testCPH1, var = "gender")
plot(testCPH1, var = "MonthlyCharges")

# If coefficient are constant, so the line stay horizontal

# General remarks on test:
# cox.zph()-test conservative
# sensitive to number of observations
# Different gravity of violations

# If the proportional hazard assumption is violated for a certain variable
# a stratified cox model makes sense

# This model allows the shape of the underlying hazard to vary for the
# different levels of the variable 

# Categorical variables are added to the arg "stratum",
# continuous variables are classed first.

# The regression coeffs are modeled across the strata

fitCPH2 <- cph(Surv(tenure, Churn)~ + SeniorCitizen +
                 Partner + Dependents + MonthlyCharges + StreamingMovies +
                 Contract,
               stratum = "gender=Male",
               data = datatelco,
               x = T,
               y = T,
               surv = T,
               time.inc = 1)

# Another solution:
# Model time-dependet coeff by dividing the time under observation
# into different periods for which we assume the coeffs to be constant

# In order to make sure the model is not overfitted:
validate(fitCPH1,
         method = "crossvalidation",
         B = 10,
         pr = F)

# Predictions in survival analysis are unfortunately not straight forward

oneNewData <- data.frame(gender = "Female",
                         SeniorCitizen = 1,
                         Partner = "No",
                         Dependents = "Yes",
                         MonthlyCharges = 37.12,
                         StreamingMovies = "Yes",
                         PaperlessBilling = "Yes",
                         PaymentMethod = "Electronic check",
                         Contract = "Month-to-month")

# Estimates the probability that this customer has not churned
# until a certain timepoint specified by the "times" arg

# In this case 3 months
str(survest(fitCPH1,
            newdata = oneNewData,
            times = 3))

# This customer has a prob to not churn of 88%

# Estimate the survival curve for the new customer 
plot(survfit(fitCPH1,
             newdata = oneNewData))

# In order to predict the expected time until churn:
print(survfit(fitCPH1, newdata = oneNewData))

# The expected media of this customer is 24 months


# Check proportional hazard assumption and print result
testCPH <- cox.zph(fitCPH)
print(testCPH)

# Plot time-dependent beta
plot(testCPH, var = "gender")

# Validate model
validate(fitCPH, method = "crossvalidation",
         B = 10, dxy = TRUE, pr = FALSE)

# Create data with new customer
newCustomer <- data.frame(daysSinceFirstPurch = 21, shoppingCartValue = 99.90, gender = "female", voucher = 1, returned = 0, stringsAsFactors = FALSE)

# Make predictions
pred <- survfit(fitCPH, newdata = newCustomer)
print(pred)
plot(pred)

# Dataset is copied. Now correct the customer's gender there
newCustomer2 <- newCustomer
newCustomer2$gender <- "male"

# Redo prediction
pred2 <- survfit(fitCPH, newdata = newCustomer2)
print(pred2)