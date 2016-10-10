library(tidyr)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/'

questionari <- read.csv(file.path(directory, 'QUESTIONARI/data.csv'), sep = ',')
participants <- read.csv(file.path(directory, 'PARTICIPANTS/data.csv'), sep = ',')
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

otras.resume <- data.frame(table(otras.ds.long$condition))

