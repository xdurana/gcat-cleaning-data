library(dplyr)
library(reshape)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

get_questionari <- function() {
  questionari <- fread(file.path(directory, 'QUESTIONARI/data.csv'))
  questionari
}

get_antecedents <- function(questionari) {
  
  columns_prevalent <- c(
    "entity_id",
    "FAMILIA_PADRE_ENFERMEDADES_CANCER",
    "FAMILIA_PADRE_ENFERMEDADES_DIABETES",
    "FAMILIA_PADRE_ENFERMEDADES_HTA",
    "FAMILIA_PADRE_ENFERMEDADES_HIPERCOLESTEROLEMIA",
    "FAMILIA_PADRE_ENFERMEDADES_ICTUS",
    "FAMILIA_PADRE_ENFERMEDADES_INFARTO",
    "FAMILIA_PADRE_ENFERMEDADES_ANGINA",
    "FAMILIA_PADRE_ENFERMEDADES_HELICOBACTER",
    "FAMILIA_PADRE_ENFERMEDADES_CROHN",
    "FAMILIA_PADRE_ENFERMEDADES_COLITIS",
    "FAMILIA_PADRE_ENFERMEDADES_POLIPOS",
    "FAMILIA_PADRE_ENFERMEDADES_HEPATITIS",
    "FAMILIA_PADRE_ENFERMEDADES_ALERGIA",
    "FAMILIA_PADRE_ENFERMEDADES_RINITIS",
    "FAMILIA_PADRE_ENFERMEDADES_ECZEMA",
    "FAMILIA_PADRE_ENFERMEDADES_PSORIASIS",
    "FAMILIA_PADRE_ENFERMEDADES_ARTRITIS",
    "FAMILIA_PADRE_ENFERMEDADES_LUPUS",
    "FAMILIA_PADRE_ENFERMEDADES_EPOC",
    "FAMILIA_PADRE_ENFERMEDADES_ASMA",
    "FAMILIA_PADRE_ENFERMEDADES_OSTEOPOROSIS",
    "FAMILIA_PADRE_ENFERMEDADES_DEPRESION",
    "FAMILIA_PADRE_ENFERMEDADES_MIGRANA",
    "FAMILIA_PADRE_ENFERMEDADES_ESQUIZOFRENIA",
    "FAMILIA_PADRE_ENFERMEDADES_ALZHEIMER",
    "FAMILIA_PADRE_ENFERMEDADES_PARKINSON",
    "FAMILIA_PADRE_ENFERMEDADES_GENETICA_HEREDITARIA",
    "FAMILIA_MADRE_ENFERMEDADES_CANCER",
    "FAMILIA_MADRE_ENFERMEDADES_DIABETES",
    "FAMILIA_MADRE_ENFERMEDADES_HTA",
    "FAMILIA_MADRE_ENFERMEDADES_HIPERCOLESTEROLEMIA",
    "FAMILIA_MADRE_ENFERMEDADES_ICTUS",
    "FAMILIA_MADRE_ENFERMEDADES_INFARTO",
    "FAMILIA_MADRE_ENFERMEDADES_ANGINA",
    "FAMILIA_MADRE_ENFERMEDADES_HELICOBACTER",
    "FAMILIA_MADRE_ENFERMEDADES_CROHN",
    "FAMILIA_MADRE_ENFERMEDADES_COLITIS",
    "FAMILIA_MADRE_ENFERMEDADES_POLIPOS",
    "FAMILIA_MADRE_ENFERMEDADES_HEPATITIS",
    "FAMILIA_MADRE_ENFERMEDADES_ALERGIA",
    "FAMILIA_MADRE_ENFERMEDADES_RINITIS",
    "FAMILIA_MADRE_ENFERMEDADES_ECZEMA",
    "FAMILIA_MADRE_ENFERMEDADES_PSORIASIS",
    "FAMILIA_MADRE_ENFERMEDADES_ARTRITIS",
    "FAMILIA_MADRE_ENFERMEDADES_LUPUS",
    "FAMILIA_MADRE_ENFERMEDADES_EPOC",
    "FAMILIA_MADRE_ENFERMEDADES_ASMA",
    "FAMILIA_MADRE_ENFERMEDADES_OSTEOPOROSIS",
    "FAMILIA_MADRE_ENFERMEDADES_DEPRESION",
    "FAMILIA_MADRE_ENFERMEDADES_MIGRANA",
    "FAMILIA_MADRE_ENFERMEDADES_ESQUIZOFRENIA",
    "FAMILIA_MADRE_ENFERMEDADES_ALZHEIMER",
    "FAMILIA_MADRE_ENFERMEDADES_PARKINSON",
    "FAMILIA_MADRE_ENFERMEDADES_GENETICA_HEREDITARIA"
  )
  
  antecedents <- questionari[,columns_prevalent] %>%
    melt(id=c('entity_id')) %>%
    filter(value == 'true') %>%
    select(entity_id, variable) %>%
    mutate(variable = gsub('FAMILIA_PADRE_ENFERMEDADES_', '', variable)) %>%
    mutate(variable = gsub('FAMILIA_MADRE_ENFERMEDADES_', '', variable)) %>%
    mutate(condition = variable, count = 1) %>%
    select(-variable) %>%
    unique()
  
  write.table(antecedents, 'output/antecedents/long.csv', row.names = FALSE, sep = ',')
}

#' @title Get cancer antecedents
#' @export
get_antecedents_cancer <- function(questionari) {
  
  columns_cancer <- c(
    'entity_id',
    'FAMILIA_PADRE_ENFERMEDADES_CANCER_1_TIPO',
    'FAMILIA_PADRE_ENFERMEDADES_CANCER_2_TIPO',
    'FAMILIA_PADRE_ENFERMEDADES_CANCER_3_TIPO',
    'FAMILIA_MADRE_ENFERMEDADES_CANCER_1_TIPO',
    'FAMILIA_MADRE_ENFERMEDADES_CANCER_2_TIPO',
    'FAMILIA_MADRE_ENFERMEDADES_CANCER_3_TIPO'
  )
  
  antecedents <- questionari[,columns_cancer]
  antecedents_melt <- melt(antecedents, id=c('entity_id'))
  antecedents_melt <- antecedents_melt[antecedents_melt$value != '',]
  antecedents_melt <- unique(antecedents_melt[, c('entity_id', 'value')])
  
  length(unique(antecedents_melt$entity_id))
  conditions <- data.frame(table(antecedents_melt$value))
  icd <- read.csv('inst/extdata/malalties/ICD_cancer_cast.csv')
  conditions_icd <- merge(conditions, icd, by.x='Var1', by.y='ID')
  
  write.table(conditions_icd, 'output/antecedents/cancer.csv', row.names = FALSE, sep = ',')
}

#' @title Create RData from phenotypes
#' @export
save_antecedents <- function() {
  antecedents <- get_conditions_cod_3_ds('output/antecedents/long.csv') %>%
    mutate(variable = 'ICD_9_antecedents',
           value = sprintf('A_%s', value))
  write.table(antecedents, 'output/antecedents/icd9.csv', row.names = FALSE, sep = ',')
}