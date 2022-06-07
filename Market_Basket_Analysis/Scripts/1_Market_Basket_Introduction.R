# Basket:Collection of items

# Items: 
# 1. Products at the supermarket
# 2. Products on online website
# 3. DataCamp courses
# 4. Movies watched by useers
# Library ====
library(tidyverse)

# Process ====
store = c("Bread", "Butter", "Cheese", "Wine")

set.seed(1234)
n_items = 4
my_basket = data.frame(
  TID = rep(1, n_items),
  Product = sample(store, n_items,
                   replace = T)
)
my_basket

my_basket <- my_basket %>%
  add_count(Product) %>%
  unique() %>%
  rename(Quantity = n)

my_basket
# Number of distinct items
n_distinct(my_basket$Product)

# Total basket size
my_basket %>% summarize(sum(Quantity))

# Plotting items
ggplot(my_basket,
       aes(x=reorder(Product,Quantity),
           y = Quantity))+
  geom_col()+
  coord_flip()+
  xlab("Items")+
  ggtitle("Summary of items in my basket")


Online_Retail_2011_Q1 <- read.csv("Data/Online_Retail_2011_Q1.csv")

# Have a glimpse at the dataset
glimpse(Online_Retail_2011_Q1)

# Filter a single basket
One_basket = Online_Retail_2011_Q1 %>%
  filter(InvoiceNo == 540180)

print(One_basket)


# Plot the total number of items within the basket
ggplot(One_basket, aes(x=reorder(Description, Quantity,
                                 function(x) sum(x)),
                       y = Quantity)) + 
  geom_col() + coord_flip() + xlab("Items")
# Basket size
n_distinct(One_basket$StockCode)

# Total number of items purchased
One_basket %>% summarize(sum(Quantity))

# Sets and subsets

# Intersections: 
# intersect(A, B)

# Union:
# union(A, B)

# How many baskets of size k?
# > Binomial coefficient
# 2^(n_items)

# Combinatios in R
n_items = 4
basket_size = 2
choose(n_items, basket_size)

# Looping through all possible values
store = matrix(NA, nrow = 5, ncol = 2)
for (i in 0:n_items){
  store[i+1,] = c(i, choose(n_items, i))
}

n_items = 50
fun_nk = function(x) choose(n_items,x)

ggplot(data=data.frame(x=0),
       mapping = aes(x=x))+
  stat_function(fun = fun_nk)+
  xlim(0, n_items)+
  xlab("Subset size")+
  ylab("Number of subsets")


# Create dataset with basket counts and inspect results
Online_Retail_clean <- na.omit(Online_Retail_2011_Q1)
basket_size = Online_Retail_clean %>%
  group_by(InvoiceNo) %>%
  summarize( n_total = n(),
             n_items = n_distinct(StockCode))

head(basket_size)

# Calculate average values
basket_size %>% summarize(avg_total_items = mean(n_total), 
                          avg_dist_items = mean(n_items))

# Distribution of distinct items in baskets
ggplot(basket_size, aes(n_items)) +
  geom_bar() + ggtitle("Distribution of basket sizes")


# Number of total and distinct items for HERB MARKER THYME
Online_Retail_clean %>%
  filter(Description == "HERB MARKER THYME")  %>%
  summarize(n_tot_items = n(),
            n_basket_item = n_distinct(InvoiceNo))

# Number of total and distinct items for HERB MARKER ROSEMARY
Online_Retail_clean %>%
  filter(Description == "HERB MARKER ROSEMARY")  %>%
  summarize(n_tot_items = n(),
            n_basket_item = n_distinct(InvoiceNo))

# Number of baskets containing both items
Online_Retail_clean %>%
  filter(Description %in% c("HERB MARKER ROSEMARY", "HERB MARKER THYME")) %>%
  group_by(InvoiceNo) %>% 
  summarize(n = n()) %>% 
  filter(n==2) %>% 
  summarize(n_distinct(InvoiceNo))