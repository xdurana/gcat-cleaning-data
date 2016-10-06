library(arules)
library(reshape)
library(dplyr)
library(vcd)
library(arulesViz)
library(visNetwork)
library(igraph)

getTransactions <- function() {
  
  medication <- read.table('output/medications/data.csv', sep = ',', header = TRUE)
  conditions <- read.table('output/conditions/data.csv', sep = ',', header = TRUE)
  
  conditions <- conditions[ , -which(names(conditions) %in% c('ENFERMEDADES_DIABETES_T1DM', 'ENFERMEDADES_DIABETES_T2DM'))]
  
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
}

plotRulesNetwork <- function(ig) {
  
  ig_df <- get.data.frame(ig, what = "both")
  nodes <- data.frame(
    id = ig_df$vertices$name,
    value = ig_df$vertices$support,
    title = ifelse(ig_df$vertices$label == "", ig_df$vertices$name, ig_df$vertices$label),
    ig_df$vertices
  )
  
  nodes$group <- 'drug'
  nodes[grepl('^ENFERMEDADES', as.character(nodes$title)), 'group'] <- 'disease'
  nodes[grepl('^r', as.character(nodes$title)), 'group'] <- 'rule'
  
  nodes[nodes$group == 'disease', 'color'] <- 'yellow'
  nodes[nodes$group == 'drug', 'color'] <- 'lightblue'
  nodes[nodes$group == 'rule', 'color'] <- 'grey'
  
  visNetwork(nodes = nodes, edges = ig_df$edges) %>%
    visIgraphLayout(layout = "layout_in_circle") %>%
    #visEdges(arrows ="to") %>%
    visEdges(shadow = TRUE,
             arrows =list(to = list(enabled = TRUE, scaleFactor = 0.5)),
             color = list(color = "lightblue", highlight = "darkblue"))
  #visOptions(selectedBy = "title", highlightNearest = TRUE)
}

rules <- function() {
  
  itemFrequencyPlot(transactions, support = 0.01, cex.names=0.8)
  
  rules <- apriori(transactions, parameter = list(support = 0.001, confidence = 0.8))
  rules <- rules[is.maximal(generatingItemsets(rules))]
  
  quality(rules) <- interestMeasure(rules, c("support", "confidence", "chiSquared", "lift", "conviction"), transactions = transactions)

  write(rules, file = "output/relative-risk/rules.csv", sep = ",", col.names = NA)
  
  inspect(head(sort(rules, by="lift"), n=10))
  summary(rules)
  
  subrules <- head(sort(subset(rules, subset = rhs %in% "ENFERMEDADES_HTA"), by='lift'), n=6)
  subrules <- rules
  
  plot(subrules)
  plot(subrules, method="grouped")
  plot(subrules, method="graph")
  plot(subrules, method="graph", control=list(type="items"))
  
  ig <- plot(subrules, method = "graph")
  plotRulesNetwork(ig)
}

itemsets <- function() {
  itemsets <- unique(generatingItemsets(rules))
  summary(itemsets)
  write(itemsets, file = "output/relative-risk/itemsets.csv", sep = ",", col.names = NA)
  
  plot(itemsets)
  plot(itemsets, method="graph")
  
  ig <- plot(itemsets, method = "graph")
  plotRulesNetwork(ig)
}

transactions <- getTransactions()
summary(transactions)
