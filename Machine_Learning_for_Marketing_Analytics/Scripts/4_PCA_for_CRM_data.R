# PCA for CRM data ====
# CRM data can get very extensive and we can have
# a problem of multicollinearity because several variables
# were carrying similar information 

# Reduce the number of variables ====

# Library ====
library(tidyverse)
library(corrplot)

# Data ====
dataCustomers <- read.csv("Data/dataCustomers.csv",)
dataCustomers <- dataCustomers %>% 
  select(-c("X"))
str(dataCustomers, give.attr = F)
dataCustomers %>%
  cor() %>%
  corrplot()

# Variance of all variables before any data preparation
lapply(dataCustomers, var)

# Variables with high variance are overrepresented in the 
# resulting principal components
# Differences in the measument units introduce a fake
# weighting of variables

# Standardization
dataCustomers <- dataCustomers %>%
  scale() %>% as.data.frame()
# Checking variance of all variables again 
lapply(dataCustomers, var)

pcaCust <- prcomp(dataCustomers)
str(pcaCust, give.attr = F)

# First element Standard deviations of extracted components
pcaCust$sdev %>% round(2)

# The variance of the componetns, eigenvalues
pcaCust$sdev^2 %>% round(2)
# Higher eigenvalue, the more important a component is

# Proportion of explained variance
(pcaCust$sdev^2 / length(pcaCust$sdev)) %>% round(2)

# Loadings and interpretation
# Loadings(correlations between original variables and components)
round(pcaCust$rotation[,1:6],2)

# The components of a PCA can be considered as something like a weighted average
sum(dataCustomers[1,]*pcaCust$rotation[,1])

# The values for each customer and each component are stored in the element "x"
pcaCust$x

# Choosing the right number of Principal components
# One way: Set a minimun of the overall variance explained

# Proportion of variance explained 
summary(pcaCust)
# An arbitrary threshold, is to choose a proportion of about 70%
# PC5 is the maximun number which excess 70%

# Second criterion: Kaiser-Guttman criterion
# No relevant components: Eigenvalues <1
pcaCust$sdev^2
# Using this criterion 6 components must be selected

# The screeplot or # elbow
screeplot(pcaCust, type = "lines")
box()
abline(h=1, lty=2)

# How variables and the components behave to each other
# The biplot
biplot(pcaCust,
       choices = 1:2,
       cex = 0.7)


# Principla Components in a Regression Analysis ====
# PCA can be a preparation step for further analysis
# Solve a multicollinearity problem in a reg analysis
mod1 <- lm(customerSatis~.,dataCustomers)
library(car)
vif(mod1)

# Create dataframe with customer satisfaction and first 6 components
dataCustComponents <- cbind(dataCustomers[,"customerSatis"],
                            pcaCust$x[,1:6]) %>%
  as.data.frame() %>%
  rename(customerSatis = V1)

mod2 <- lm(customerSatis ~ ., dataCustComponents)
vif(mod2)

summary(mod1)$adj.r.squared
summary(mod2)$adj.r.squared

# The R2 decreade but the estimates become more stable
# The interpretation of the coefficients are less straightforward,
# because they refer to the components and not the original variables

summary(mod2)

# A method that is often confused with PCA analysis is factor analysis
# Both methods are used as dimension reduction techniques
# but the idea behind is a bit different
