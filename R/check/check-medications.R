library(tidyverse)

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
  medications <- get_medications_atc_ds()
  medications %>% write_csv('output/medications/atc.csv')
}

save_atc <- function() {
  medications <- read_csv('output/medications/atc.csv') %>%
    transform(
      value=substring(sprintf("atc_%s", value), 1, 7),
      count = 1
    )
  
  medications <- medications %>%
    dplyr::select(
      id,
      value,
      count
    ) %>%
    unique() %>%
    spread(value, count, fill = 0) %>%
    dplyr::rename(entity_id=id)
  
  as.data.frame(table(medications$value)) %>% arrange(desc(Freq)) %>% write_csv('output/medications/summary.csv')
  
  medications <- participants %>%
    dplyr::select(entity_id) %>%
    left_join(medications)

  medications[is.na(medications)] <- 0
    
  medications %>% write_csv('output/check/atc/data.csv')
}

save_medications()
save_atc()