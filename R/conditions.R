library(dplyr)
library(tidyr)
library(stringi)

#' @title Get conditions from GCAT
#' @param other Take into account the others conditions
#' @export
get_conditions <- function(others = FALSE) {
  
  directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
  
  questionari <- read.csv(file.path(directory, 'QUESTIONARI/data.csv'), sep = ',')
  participants <- read.csv(file.path(directory, 'Participants/data.csv'), sep = ',')
  all <- merge(participants, questionari, by='entity_id')
  
  ### DIABETES
  
  all$ENFERMEDADES_DIABETES_T1DM <- ifelse(all$DIABETES_TIPO == 1, 1, 0)
  all$ENFERMEDADES_DIABETES_T2DM <- ifelse(all$DIABETES_TIPO == 2, 1, 0)
  all$ENFERMEDADES_DIABETES_GESTACIONAL <- ifelse(all$DIABETES_TIPO == 3, 1, 0)
  
  ### MAIN CONDITIONS
  
  enfermedades <- colnames(all)[grep('^ENFERMEDADES_', colnames(all))]
  enfermedades <- enfermedades[!enfermedades %in% colnames(all)[grep('^ENFERMEDADES_E[0-9]', colnames(all))]]
  
  enfermedades.ds <- all[, c('entity_id', enfermedades)]
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

#' @title Get conditions dataset
#' @param filename Conditions file
#' @export
get_conditions_ds <- function(filename) {
  long <- read.csv2(filename, sep = ',', stringsAsFactors = FALSE)
  summary <- read.csv2(file.path('inst/extdata/conditions', 'summary-cim9.csv'), sep = ';')
  phenotype <- merge(long, summary, by.x = 'condition', by.y = 'Text')
  phenotype.icd9 <- phenotype %>%
    filter(!is.na(Codi) & Codi != '') %>%
    mutate(Descr_codi = stri_trans_general(Descr_codi, "Latin-ASCII")) %>%
    mutate(nom = gsub(' ', '_', paste(Codi, Descr_codi)),
           grup = gsub(' ', '_', paste(Gran_grup_classif, Descr_gran_grup_clasif)))
  phenotype.icd9
}

#' @title Get conditions dataset
#' @export
get_conditions_cod_3_ds <- function(filename) {
  phenotype.icd9.nom <- get_conditions_ds(filename) %>%
    mutate(Descr_codi_3 = stri_trans_general(Descr_codi_3, "Latin-ASCII")) %>%
    mutate(ICD9 = gsub(' ', '_', paste(Codi_3, Descr_codi_3))) %>%
    select(entity_id, ICD9) %>%
    dplyr::rename(id=entity_id) %>%
    gather(variable, value, ICD9) %>%
    unique()
  phenotype.icd9.nom
}

#' @title Get conditions from genotyped participants dataset
#' @export
get_conditions_genotyped_ds <- function() {
  conditions <- get_conditions_ds('output/conditions/others_long.csv') %>%
    select(-Freq)
  genotyped <- read.csv2(file.path('output/genotyped', 'data.csv'), sep = ',', stringsAsFactors = FALSE) %>%
    rename(entity_id = Sample.Id) %>%
    merge(conditions)
  genotyped
}

#' @title Create RData from phenotypes
#' @export
save_conditions <- function() {
  write.table(get_conditions_cod_3_ds('output/conditions/others_long.csv'), 'output/conditions/icd9.csv', row.names = FALSE, sep = ',')
}

save_conditions_genotyped <- function() {

  conditions_genotyped <- get_conditions_genotyped_ds()
  
  conditions_genotyped.rl <- conditions_genotyped %>%
    filter(sampleType == 'RL')
  
  grouped.rl <- group_by(conditions_genotyped.rl, condition, Codi, Descr_codi)
  grouped.genotyped <- group_by(conditions_genotyped, condition, Codi, Descr_codi)

  ds.rl <- summarise(grouped.rl, count.rl=length(condition))
  ds <- summarise(grouped.genotyped, count=length(condition))
  
  ds <- merge(
    summarise(grouped.rl, count.rl=length(condition)),
    summarise(grouped.genotyped, count=length(condition))
  ) %>%
    arrange(desc(count))

  write.table(ds, 'output/conditions/icd9_genotyped.csv', row.names = FALSE, sep = ',')
}

run <- function() {
  get_conditions(FALSE)
  get_conditions(TRUE)
}

args <- commandArgs(TRUE)
if (length(args) == 1) {
  do.call(args[1], list())
}