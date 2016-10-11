library(arulesCBA)
library(caret)

source('drug-disease-rules.R')

status <- getMedsnconditions()
transactions <- getTransactions(status)
rules <- getRules(transactions)
rules.sorted <- sort(rules, by=c('chiSquare'))

rulesMatchLHS <- is.subset(lhs(rules.sorted), transactions)
rulesMatchRHS <- is.subset(rhs(rules.sorted), transactions)

head(status)

classifier <- CBA(status, "DIABETES", supp = 0.05, conf = 0.9)

status
