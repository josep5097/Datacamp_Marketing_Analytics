# Metrics and techniques

# Market basket Analysis deals with data at a transactional level
# Transactional data = list of all items bought by a customer in a 
# single purchase

# Transaction-class
# Represents transction data used for minin itemsets or rules

# Coercion from: list, matrices, dataframes

# Important when considering transactional data
# Field/column used to identify a product
# Field/column used to identify a transaction 

# Transform TID into a factor
# my_transction$TID = factor(my_transaction$TID)

# Split into groups
# data_list = split(my_transaction$Prod, my_transaction$TID)

# Transform to transctional dataset
# data_trx = as(data_list, "transactions")

# Inspect transactions 
# inspect(data_trx)

# Ploting
# image(data_trx)

# Warning! Use the function on a limited number of transactions

library(arules)

# Splitting transactions
data_list = split(Online_Retail_clean$Description,
                  Online_Retail_clean$InvoiceNo)

# Transform data into a transactional dataset
Online_trx = as(data_list, "transactions")

# Summary of transactions
summary(Online_trx)

# inspect first 3 transactions
inspect(head(Online_trx,3))

# inspect last 5 transactions
inspect(tail(Online_trx,5))

# Inspect specific transactions
inspect(Online_trx[c(12,20,22)])

# Metrix ====
# 1) support: "Popularity of an itemset"
#    * supp(x)=Fraction of transactions that contain itemset x
#    * supp(X U Y) = Fraction of transactions with both X and Y
# 2) Confidence: "How often the rule is true"
#    * conf(x -> y) = supp(X U Y)/ supp(x)
#    Confidence shows the percentage in which Y is bought with X
# 3) Lift: "How strong is the association"
#    * lift(X -> Y) = supp(X U Y)/ (supp(x) supp(y))
#    * lift > 1: Y is likely to be bought with X
#    * lift < 1: Y is unlikely to be bought if X is bought

library(arules)
# Dont run
supp.cw <- apriori(trans, # dataset
                   parameter = list(
                     supp = 0.2,
                     conf = 0.4,
                     minlen =2,
                     target = "frequent itemsets"),
                   appearance = list(
                     items = c("Cheese", "Wine")
                     )
                   )

# Determine the support of both items with support 0.1
support_rosemary_thyme = 
  apriori(Online_trx,
          parameter = list(target = "frequent itemsets",
                           supp = 0.1),
          appearance = list(items = 
                              c("HERB MARKER ROSEMARY",
                                "HERB MARKER THYME"))
  )

# Summary of the object 
summary(support_rosemary_thyme)


# Determine the support of both items with support 0.01
support_rosemary_thyme = 
  apriori(Online_trx,
          parameter = list(target = "frequent itemsets",
                           supp = 0.01),
          appearance = list(items = 
                              c("HERB MARKER ROSEMARY",
                                "HERB MARKER THYME"))
  )

# Inspect the itemsets 
inspect(support_rosemary_thyme)


# Frequent itemsets for all items
support_all = 
  apriori(Online_trx,
          parameter = list(target="frequent itemsets",
                           supp = 0.01)
  )

# Inspect the 5 most frequent items
inspect(head(sort(support_all, by="support"), 5))

# Always sort the rules according to the specific metric

# Call the apriori function with apropriate parameters
rules_all = apriori(Online_trx,
                    parameter = list(supp=0.01, conf = 0.4)
)

# Call the apriori function with apropriate parameters
rules_all = apriori(Online_trx,
                    parameter = list(supp=0.01, conf = 0.4, minlen=2)
)


# Call the apriori function with apropriate parameters
rules_all = apriori(Online_trx,
                    parameter = list(supp=0.01, conf = 0.4, minlen=2)
)

# Inspect the rules with highest confidence
inspect(head(sort(rules_all, by="confidence"), 5))


# Call the apriori function with apropriate parameters
rules_all = apriori(Online_trx,
                    parameter = list(supp=0.01, conf = 0.4, minlen=2)
)

# Inspect the rules with highest lift
inspect(head(sort(rules_all, by="lift"), 5))

# Changing the appearance of rules ====
# Find the confidence and lift measures
rules_rosemary_rhs = 
  apriori(Online_trx,
          parameter = list(supp=0.01, conf=0.5, minlen=2),
          appearance = list(rhs="HERB MARKER ROSEMARY",
                            default = "lhs")
  )

# Inspect the rules
inspect(rules_rosemary_rhs)

# Find the confidence and lift measures
rules_rosemary_lhs =
  apriori(Online_trx,
          parameter = list(supp=0.01, conf=0.5, minlen=2),
          appearance = list(lhs="HERB MARKER ROSEMARY",
                            default = "rhs")
  )

# Inspect the rules
inspect(rules_rosemary_lhs)

# Create the union of the rules and inspect
rules_rosemary = arules::union(rules_rosemary_rhs,
                               rules_rosemary_lhs)
inspect(rules_rosemary)


# The apriori algorith
# Association rule mining
# Allows to discover interesting relationships between items in a 
# large transactional database

# This mining task can be divided into two subtasks
# 1) Frequent itemset generation: Determine all freq itemsets of a 
#    potentially large db of transactions
#    An itemset is said to be freq if it satisfies a minimun support threshold

# 2) Rule generation: From the above freq itemsets, generate association rules
# with confidence above a minimum conf threshold

# The apriori algo is a classic and fast mining alg belonging to the
# class of association rule mining alg.

# Apriori principle:
# If an itemset is freq, the all of its subsets must also be freq-

# For an infreq itemset, all its super-sets are infreq


# Rule generation:
# * Start with high-conf rules with a single precedent
# * Build more complex rules, with more itmes on the RHS

# Apply the apriori function to the Online retail dataset
rules_online = apriori(Online_trx,
                       parameter = list(supp = 0.01, conf = 0.8, minlen = 2))

# Inspect the first 5 rules
inspect(head(rules_online, 5))

# Inspect the first 5 rules with highest lift
inspect(head(sort(rules_online, by="lift"), 5))

# Transform the rules back to a data frame
rules_online_df = as(rules_online, "data.frame")

# Check the first records
head(rules_online_df)

# Parameters of the apriori
# Apply the apriori function to the Online retail dataset
rules_online = apriori(Online_trx,
                       parameter = list(supp = 0.01, 
                                        conf = 0.8, 
                                        minlen = 2))

# Inspect the first rules
inspect(head(rules_online))

# If this then that with apriori ====
# Support of herb markers
supp_herb_markers = 
  apriori(Online_trx, 
          parameter = list(
            target = "frequent itemsets",
            supp = 0.01),
          appearance = list(
            items = c("HERB MARKER THYME",
                      "HERB MARKER ROSEMARY"))
  )

# Inspect frequent itemsets
inspect(supp_herb_markers)

# Extract rules for HERB MARKER THYME on rhs of rule
rules_thyme_marker_rhs =
  apriori(Online_trx,
          parameter = list(supp=0.01, conf=0.8, minlen=2),
          appearance = list(rhs = "HERB MARKER THYME"),
          control = list(verbose=F))

# Inspect rules
inspect(rules_thyme_marker_rhs)

# Extract rules for HERB MARKER THYME on lhs of rule
rules_thyme_marker_lhs =
  apriori(Online_trx,
          parameter = list(supp=0.01, conf=0.8, minlen=2),
          appearance = list(lhs = "HERB MARKER THYME"),
          control = list (verbose=F))

# Inspect rules
inspect(rules_thyme_marker_lhs)

# Apply the apriori function to the Online retail dataset
rules = apriori(Online_trx,
                parameter = list(supp = 0.01, conf = 0.8, 
                                 minlen = 2))

# Inspect the first 5 rules
inspect(head(rules))

# Find redundant of rules
redundant_rules = is.redundant(rules)

# Inspect the non redundant rules
non_redundant_rules = rules[!redundant_rules]
inspect(head(non_redundant_rules))