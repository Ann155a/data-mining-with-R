library(arules)
library(tidyverse) 
library(arulesViz)
library(knitr)

install.packages('kableExtra')
library(kableExtra)

data('Groceries')

# Summary
Groceries
summary(Groceries)

# Structure
str(Groceries)

# Top items
head(Groceries)

###############

# Data Analysis
itemFrequencyPlot(Groceries, topN = 30)

# Association rule mining

## Algorithm selection
rules <- apriori(Groceries, 
                 parameter = list(supp = 0.0002, conf = 0.9))

## Pattern in purchases 

### Support
### (s) counts the number of times an item appears in the dataset. 
shop_support = sort(rules, by = "support", decreasing = TRUE)
shop_support_df = inspect(head(shop_support), linebreak = FALSE)

shop_support_df %>%
  kable() %>%
  kable_styling()


### Confidence
### (c) measures the strength of an affiliation rule between factors.
shop_confidence = sort(rules, by = "confidence", decreasing = TRUE)
shop_confidence_df = inspect(head(shop_confidence), linebreak = FALSE)

shop_confidence_df %>%
  kable() %>%
  kable_styling()

### Lift
### how likely it is that X and Y will be bought collectively or not
shop_lift = sort(rules, by = "lift", decreasing = TRUE)
shop_lift_df = inspect(head(shop_lift), linebreak = FALSE)

shop_lift_df %>%
  kable() %>%
  kable_styling()

## Support vs Confidence with Lift
market_df <- as(rules, "data.frame")
ggplot(market_df, aes(x = support, y = confidence, size = lift)) +
  geom_point(color = "blue") +
  labs(title = "Support vs Confidence with Lift") +
  theme(plot.title = element_text(hjust = 0.1))

# Visualizations

## Scatter plot
plot(rules, jitter= 0)

## Scatter plot with confidence
plot(rules, measure = "confidence")

## Two key plots
plot(rules, method = "two-key plot")

## Interactive scatterplots
plot(rules, engine = "plotly")

## Grouped bar plot
plot(rules, method = "grouped", control = list(k = 5))

## Parallel coordinate plot
plot(rules, method="paracoord")


## Graphs
plot(rules[1:20], method="graph")
plot(rules, method="graph")
