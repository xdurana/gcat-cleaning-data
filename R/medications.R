library(dplyr)
library(tidyr)

#' @title Get medications file
#' @export
get_medications <- function() {
  medications <- read.csv2('output/medications/data.csv', sep = ',', stringsAsFactors = FALSE)
  medications
}

#' @title Get medications dataset
#' @export
get_medications_ds <- function(medications) {
  atc <- read.table('inst/extdata/medications/ATC.csv', sep = ',', header = TRUE)
  medications <- get_medications() %>%
    mutate(ATC_CODE_3 = as.factor(substring(ATC_CODE, 1, 3)), count = 1) %>%
    merge(atc, by.x='ATC_CODE_3', by.y='id') %>%
    select(entity_id, ATC_CODE_3, text, count)
  medications
}

#' @title Get medications dataset
#' @export
get_medications_atc_ds <- function() {
  medications <- get_medications_ds() %>%
    mutate(ATC = gsub(' ', '_', paste(ATC_CODE_3, text))) %>%
    select(entity_id, ATC) %>%
    dplyr::rename(id=entity_id) %>%
    gather(variable, value, ATC) %>%
    unique()
  medications
}

#' @title Create RData from phenotypes
#' @export
save_medications <- function() {
  write.table(get_medications_atc_ds(), 'output/medications/atc.csv', row.names = FALSE, sep = ',')
}