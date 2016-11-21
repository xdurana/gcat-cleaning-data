library(Hmisc)
library(dplyr)
library(tidyr)

#' @title GCAT dataset to analyze
#' @export
gcat_ds <- function() {
  
  minimum_observations <- 4000

  ### ANTROPOMÈTRIQUES
    
  data <- read.csv('output/data.csv', sep = ',', stringsAsFactors = TRUE)
  data <- data %>%
    transform(
      EDAD=cut2(EDAD_ANOS, m=minimum_observations),
      BMI=cut2(BMI, m=minimum_observations),
      WHR=cut2(WHR, m=minimum_observations),
      PRESSIO_SISTOLICA=cut2(CALC_AVG_SYSTOLIC_BP, m=minimum_observations),
      PRESSIO_DIASTOLICA=cut2(CALC_AVG_DIASTOLIC_BP, m=minimum_observations),
      POLS=cut2(CALC_AVG_PULSE_RATE, m=minimum_observations),
      SEXE=SEXO,
      ETNIA=ETNIA_PARTICIPANTE
    ) %>%
    select(entity_id,
           EDAD,
           BMI,
           WHR,
           PRESSIO_SISTOLICA,
           PRESSIO_DIASTOLICA,
           POLS,
           SEXE,
           ETNIA) %>% 
    lapply(factor) %>%
    as.data.frame()
  
  data <- data %>% transform(entity_id=as.character(entity_id))

  ### MALALTIES
  
  conditions <- horizontal_ds('output/conditions/icd9.csv') %>%
    transform(entity_id=as.character(entity_id))
  
  ### ANTECEDENTS
  
  antecedents <- horizontal_ds('output/antecedents/icd9.csv') %>%
    transform(entity_id=as.character(entity_id))
  
  ### MEDICACIÓ
  
  medications <- horizontal_ds('output/medications/atc.csv') %>%
    transform(entity_id=as.character(entity_id))
  
  ### Full dataset

  dataset <- data %>%
    select(entity_id) %>%
    merge(conditions, all.x=TRUE) %>%
    merge(medications, all.x=TRUE) %>%
    binarize %>%
    merge(data) %>%
    select(-entity_id)
      
  save(dataset, file = 'output/medsnconditions.RData')

  ### Dictionary

  dictionary <- bind_rows(
    data.frame(
      variable=data %>%
        select(-entity_id) %>%
        colnames,
      category=c('Antropomètriques')
    ),
    data.frame(
      variable=conditions %>%
        select(-entity_id) %>%
        colnames,
      category=c('Malalties')
    ),
    data.frame(
      variable=medications %>%
        select(-entity_id) %>%
        colnames,
      category=c('Medicaments')
    )
  )

  save(dictionary, file = 'output/dictionary.RData')
}

#' @title Binarize dataset
#' @param data dataset to convert
#' @export
binarize  <- function(data) {
  data[is.na(data)] <- 0
  for (column in colnames(data)) {
    if (is.numeric(data[,column])) {
      if (sum(unique(data[,column])) == 1) {
        data[, column] <- as.logical(data[, column])
      }
    }
  }
  data
}

#' @title Convert horizontal to vertical dataset
#' @param filename dataset to convert
#' @export
horizontal_ds <- function(filename) {
  dataset <- read.csv(filename, sep = ',') %>%
    select(-variable) %>%
    mutate(count=1) %>%
    spread(value, count) %>%
    filter(grepl('=E0025', id)) %>%
    transform(entity_id=as.character(id)) %>%
    select(-id)
  dataset
}