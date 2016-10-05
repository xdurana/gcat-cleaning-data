library(arules)
library(reshape2)
library(dplyr)
library(vcd)
library(arulesViz)
library(visNetwork)
library(igraph)

medication <- read.table('output/medications/data.csv', sep = ',', header = TRUE)
conditions <- read.table('output/conditions/data.csv', sep = ',', header = TRUE)

medication$ATC_CODE_3 <- as.factor(substring(medication$ATC_CODE, 1, 3))
medication$value <- TRUE

### ATC conversion

atc_conversion <- read.table('data/medications/ATC.csv', sep = ',', header = TRUE)
medication <- merge(medication, atc_conversion, by.x='ATC_CODE_3', by.y='id')
medication <- medication[, c('entity_id', 'ATC_CODE_3', 'text', 'value')]

### Merge drugs and conditions

drugs <- data.frame(cast(medication, entity_id~text, fun.aggregate=length))
drugs[is.na(drugs)] <- FALSE

for (column in colnames(drugs)[-1]) {
  drugs[, column] <- as.logical(drugs[, column])
}

for (column in colnames(conditions)[-1]) {
  conditions[, column] <- as.logical(conditions[, column])
}

status <- merge(conditions, drugs)

transactions <- as(status, "transactions")
transactions

summary(transactions)

itemFrequencyPlot(transactions, support = 0.01, cex.names=0.8)
rules <- apriori(transactions, parameter = list(support = 0.01, confidence = 0.8))

summary(rules)
inspect(head(sort(rules, by="lift"), n=10))

write(rules, file = "output/relative-risk/rules.csv", sep = ",", col.names = NA)

plot(rules)
ig <- plot(head(sort(rules, by='lift'), n=10), method = "graph")
plotRulesNetwork(ig)

######

plotRulesNetwork <- function(ig) {
  
  ig_df <- get.data.frame(ig, what = "both")
  visNetwork(
    nodes = data.frame(
      id = ig_df$vertices$name
      ,value = ig_df$vertices$support # could change to lift or confidence
      ,title = ifelse(ig_df$vertices$label == "", ig_df$vertices$name, ig_df$vertices$label)
      ,ig_df$vertices
    )
    , edges = ig_df$edges
  ) %>%
    visEdges(arrows ="to") %>%
    visOptions(highlightNearest = T)
}

###

assoc(~ ENFERMEDADES_DIABETES + A02, data=status, shade=TRUE, abbreviate_labs=6)


