library(arules)
library(tidyverse)

Online_Retail_2011_Q1 <- read.csv("Data/Online_Retail_2011_Q1.csv")


# Create dataset with basket counts and inspect results
Online_Retail_clean <- na.omit(Online_Retail_2011_Q1)

# Splitting transactions
data_list = split(Online_Retail_clean$Description,
                  Online_Retail_clean$InvoiceNo)

# Transform data into a transactional dataset
Online_trx = as(data_list, "transactions")


# Display the five most popular items
itemFrequencyPlot(Online_trx,
                  topN = 5)

# Absolute frequency count of 10 most popular items
itemFrequencyPlot(Online_trx,
                  type = "absolute",
                  topN = 10)
  
# Display items horizontally
itemFrequencyPlot(Online_trx,
                  topN = 5,
                  horiz = TRUE)

# Adjusting the color of bars (rainbow)
itemFrequencyPlot(Online_trx,
                  topN = 10,
                  col = rainbow(10),
                  type = "relative",
                  horiz = TRUE
)

# Adding a title and a label to the x axis
itemFrequencyPlot(Online_trx,
                  topN = 10,
                  col = rainbow(10),
                  type = "relative",
                  horiz = TRUE,
                  main = "Relative Item Frequency Plot",
                  xlab = "Frequency"
)
# Changing the font of the items
itemFrequencyPlot(Online_trx,
                  topN = 10,
                  col = rainbow(10),
                  type = "relative",
                  horiz = TRUE,
                  main = "Relative Item Frequency Plot",
                  xlab = "Frequency",
                  cex.names = 0.8
)

# Interactive table with metrics
library(arulesViz)

rules = apriori(Online_trx,
                parameter = list(
                  supp=3/7,
                  conf=0.6,
                  minlen=2
                ))
inspect(rules)
inspectDT(rules)

# Scatterplot
plot(rules)
# rulesObject, measure, shading, method
plot(rules, method = "two-key plot")

plot(rules, engine = "plotly")


# Visualizing rules
rules_html = plot(rules,
                  method = "graph",
                  engine = "htmlwidget")


