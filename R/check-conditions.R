library(dplyr)
library(tidyr)
library(xlsx)
library(stringi)
library(limma)
library(data.table)

#' @title Get conditions from GCAT
#' @param other Take into account the others conditions
#' @export
get_conditions <- function(others = FALSE) {
  
  export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
  
  gcat <- fread(file.path(export_dir, 'GCAT/data.csv'))
  participants <- fread(file.path(export_dir, 'Participants/data.csv'))
  
  all <- participants %>%
    merge(gcat)
  
  enfermedades <- colnames(all)[grep('^ENFERMEDADES\\.', colnames(all))]
  enfermedades.ds <- all %>%
    select_vars(
      c('entity_id', enfermedades)
    )
  
  enfermedades.ds.long <- gather(enfermedades.ds, condition, cardinal, 2:34, factor_key=TRUE)
  enfermedades.ds.long <- enfermedades.ds.long[enfermedades.ds.long$cardinal > 0 & !is.na(enfermedades.ds.long$cardinal),]
  
  columns <- c('entity_id', 'condition')
  
  ### OTHER CONDITIONS
  
  if (others) {
    otras <- colnames(all)[grep('^ENFERMEDADES_E[0-9]$', colnames(all))]
    otras.ds <- all[, c('entity_id', otras)]
    otras.ds.long <- gather(otras.ds, cardinal, condition, ENFERMEDADES_E1:ENFERMEDADES_E7, factor_key=TRUE)
    otras.ds.long <- otras.ds.long[otras.ds.long$condition != '',]
    otras.ds.long$condition <- toupper(otras.ds.long$condition)
    all.ds <- rbind(enfermedades.ds.long[, columns], otras.ds.long[, columns])
  } else {
    all.ds <- enfermedades.ds.long[, columns]
  }
  
  ### ALL CONDITIONS
  
  all.ds <- filter(all.ds, (!(condition %in% c('ENFERMEDADES_NINGUNA', 'ENFERMEDADES_NS_NC', 'ENFERMEDADES_OTRAS'))))
  if (others) {
    all.ds$condition <- gsub("ENFERMEDADES_", "", all.ds$condition)
  }
  #all.ds$condition <- stri_trans_general(all.ds$condition, "Latin-ASCII")
  
  ### TRANSLATE
  
  all.ds <- all.ds %>% mutate(count = 1)
  
  ### SUMMARY
  
  all.summary <- arrange(data.frame(table(all.ds$condition)), desc(Freq))
  
  ### FILTER BY FREQUENCY
  
  if (FALSE) {
    all.ds <- all.ds %>% filter(condition %in% all.summary$Var1)
  }
  
  all.ds <- all.ds %>% unique()
  
  all.wide <- all.ds %>% spread(condition, count)
  all.wide[is.na(all.wide)] <- 0
  
  if (others) {
    write.table(all.summary, 'output/conditions/others_summary.csv', sep = ',', row.names = FALSE, quote = FALSE)
    write.table(all.ds, 'output/conditions/others_long.csv', sep = ',', row.names = FALSE, quote = TRUE)
    write.table(all.wide, 'output/conditions/others_wide.csv', sep = ',', row.names = FALSE, quote = FALSE)
    save_conditions()
  } else {
    write.table(all.summary, 'output/conditions/summary.csv', sep = ',', row.names = FALSE, quote = FALSE)
    write.table(all.ds, 'output/conditions/long.csv', sep = ',', row.names = FALSE, quote = FALSE)
    write.table(all.wide, 'output/conditions/wide.csv', sep = ',', row.names = FALSE, quote = FALSE)
  }
}