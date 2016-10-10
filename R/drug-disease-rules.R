library(arules)
library(reshape)
library(dplyr)
library(vcd)
library(arulesViz)
library(visNetwork)
library(igraph)
library(shiny)

directory <- 'output/rules'

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
  status <- status[ , -which(names(status) %in% c('entity_id', 'ENFERMEDADES_NS_NC'))]
  
  df <- status
  names(df)[grep("^ENFERMEDADES", names(df))] <- substring(names(df)[grep("^ENFERMEDADES", names(df))], 14)
  names(df)
  df

  status <- df  
  saveRDS(status, 'R/rules/data/medsnconditions.Rds')
  
  transactions <- as(status, "transactions")
  transactions
}

#' Get rules based on transactions
#' @param transactions
#' @param support
#' @param confidence
getRules <- function(transactions, support = 0.001, confidence = 0.8) {
  
  rules <- apriori(transactions, parameter = list(support = support, confidence = confidence))
  rules <- rules[is.maximal(generatingItemsets(rules))]
  quality(rules) <- interestMeasure(rules, c("support", "confidence", "chiSquared", "lift", "conviction"), transactions = transactions)
  write(rules, file = file.path(directory, "rules.csv"), sep = ",", col.names = NA)
  rules
}

#' Get subrules based on a set of conditions
#' @param rules
#' @param conditions
#' @param n
#' @param measure
getSubrules <- function(rules, conditions, n, measure='chiSquared') {

  subrules <- head(sort(subset(rules, subset = rhs %in% conditions), by=measure), n=n)
  subrules
}

plotRulesNetwork <- function(subrules) {
  
  ig <- plot(subrules, method = "graph")
  plotRulesNetwork(ig)
  
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
rules <- getRules(transactions)
subrules <- getSubrules(rules, c('ENFERMEDADES_PSORIASIS', 'ENFERMEDADES_ALERGIA'), 6)
inspect(head(sort(subrules, by="chiSquared")))

plot(subrules, method="graph")
plotRulesNetwork(subrules)

arulesApp(transactions, supp = 0.001, conf = 0.8, bin = TRUE, vars = transactions@data@Dim[1])
