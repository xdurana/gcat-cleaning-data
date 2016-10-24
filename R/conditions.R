library(dplyr)
library(tidyr)
library(stringi)

directory <- 'inst/ext-data'

questionari <- read.csv(file.path(directory, 'QUESTIONARI/data.csv'), sep = ',')
participants <- read.csv(file.path(directory, 'Participants/data.csv'), sep = ',')
all <- merge(participants, questionari, by='entity_id')

### MAIN CONDITIONS

enfermedades <- colnames(all)[grep('^ENFERMEDADES_', colnames(all))]
enfermedades <- enfermedades[!enfermedades %in% colnames(all)[grep('^ENFERMEDADES_E[0-9]', colnames(all))]]

enfermedades.ds <- all[, c('entity_id', enfermedades)]
enfermedades.ds.long <- gather(enfermedades.ds, condition, cardinal, 2:31, factor_key=TRUE)
enfermedades.ds.long <- enfermedades.ds.long[enfermedades.ds.long$cardinal > 0,]

### OTHER CONDITIONS

otras <- colnames(all)[grep('^ENFERMEDADES_E[0-9]$', colnames(all))]
otras.ds <- all[, c('entity_id', otras)]

colnames(otras.ds)
otras.ds.long <- gather(otras.ds, cardinal, condition, ENFERMEDADES_E1:ENFERMEDADES_E7, factor_key=TRUE)
otras.ds.long <- otras.ds.long[otras.ds.long$condition != '',]
otras.ds.long$condition <- toupper(otras.ds.long$condition)

### ALL CONDITIONS

columns <- c('entity_id', 'condition')
all.ds <- rbind(enfermedades.ds.long[, columns], otras.ds.long[, columns])
all.ds$condition <- gsub("ENFERMEDADES_", "", all.ds$condition)
all.ds <- filter(all.ds, (!(condition %in% c('NINGUNA', 'NS_NC', 'OTRAS'))))
all.ds$condition <- stri_trans_general(all.ds$condition, "Latin-ASCII")

### TRANSLATE

conditions <- read.csv2('inst/conditions.csv', sep = ',', stringsAsFactors = FALSE)

for (i in 1:nrow(conditions)) {
  all.ds$condition <- gsub(sprintf("^%s$", conditions[i,]$origen), conditions[i,]$traduccio, all.ds$condition)
}

### SUMMARY

all.summary <- arrange(data.frame(table(all.ds$condition)), desc(Freq)) %>%
  filter(Freq > 5)

write.table(all.summary, 'output/conditions/summary.csv', sep = ',', row.names = FALSE)

### FILTER BY FREQUENCY

all.ds <- all.ds %>%
  filter(condition %in% all.summary$Var1) %>%
  unique() %>%
  mutate(count = 1)

write.table(all.ds, 'output/conditions/long.csv', sep = ',', row.names = FALSE)

all.wide <- all.ds %>% spread(condition, count)
all.wide[is.na(all.wide)] <- 0

write.table(all.wide, 'output/conditions/wide.csv', sep = ',', row.names = FALSE)
