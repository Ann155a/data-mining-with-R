#########################
# Market Basket Analysis#
##########################

# Performing association rule mining

# Installing Packages 
install.packages("arules") 
install.packages("arulesViz") 

# Loading package 
library(arules) 
library(arulesViz) 

# Fitting model 
set.seed <- 220 # Setting seed 

# Load sample dataset
data("Groceries")

# Quick overview
summary(Groceries)

# Training Apriori on the dataset 
associa_rules <- apriori(data = Groceries, 
                        parameter = list(support = 0.004, 
                                         confidence = 0.2)) 

# Plot 
itemFrequencyPlot(Groceries, topN = 10) 

# Visualising the results 
inspect(sort(associa_rules, by = 'lift')[1:10]) 
plot(associa_rules, method = "graph", 
     measure = "confidence", shading = "lift") 
