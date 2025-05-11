library(arules)
library(arulesViz)
library(tidyverse)
library(readxl)
library(knitr)
library(ggplot2)
library(lubridate)
library(plyr)
library(dplyr)

setwd("~/Downloads/data_mining_r") 

# Data pre-processing

retail <- read_excel('Online_Retail.xlsx')
retail <- retail[complete.cases(retail), ]

retail %>% mutate(Description = as.factor(Description))
retail %>% mutate(Country = as.factor(Country))

retail$Date <- as.Date(retail$InvoiceDate)

TransTime<- format(retail$InvoiceDate,"%H:%M:%S")
cbind(retail,TransTime)

InvoiceNo <- as.numeric(as.character(retail$InvoiceNo))
cbind(retail,InvoiceNo)

## get a glimpse of my data
glimpse(retail)


# Create transaction data
library(plyr)
transactionData <- ddply(retail,c("InvoiceNo","Date"),
                         function(df1)paste(df1$Description,
                                            collapse = ","))

## set column InvoiceNo of dataframe transactionData  
transactionData$InvoiceNo <- NULL

## set column Date of dataframe transactionData
transactionData$Date <- NULL
 
## Rename column to items
colnames(transactionData) <- c("items")

## Create transaction object

write.csv(transactionData,"market_basket_transactions.csv", quote = FALSE, 
          row.names = FALSE)

tr <- read.transactions('market_basket_transactions.csv', 
                        format = 'basket', sep=',')

summary(tr)

# Item frequency plot
library(RColorBrewer)
itemFrequencyPlot(tr,topN=20,type="absolute",
                  col=brewer.pal(8,'Pastel2'), 
                  main="Absolute Item Frequency Plot")

# Generating rules

## Min Support as 0.001, confidence as 0.8.
association.rules <- apriori(tr, parameter = list(supp=0.001, conf=0.8,maxlen=10))
summary(association.rules)

inspect(association.rules[1:10])

# Limit size of rules
shorter.association.rules <- apriori(tr, 
                                     parameter = list(supp=0.001, 
                                    conf=0.8,maxlen=3))

inspect(shorter.association.rules[1:10])


# Remove redundant rules
subset.rules <- which(colSums(is.subset(association.rules, association.rules)) > 1) # get subset rules in vector
length(subset.rules)

# Finding rules related to given items
metal.association.rules <- apriori(tr, 
                                   parameter = list(supp=0.001, conf=0.8),
                                   appearance = list(default="lhs",
                                                     rhs="METAL"))
inspect(head(metal.association.rules))

# Visualisations
subRules<-association.rules[quality(association.rules)$confidence>0.4]
plot(subRules)

## Graph based
top10subRules <- head(subRules, n = 10, by = "confidence")
plot(top10subRules, method = "graph",  engine = "htmlwidget")
