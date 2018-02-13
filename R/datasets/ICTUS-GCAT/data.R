library(dplyr)

describe_var <- function(variables, data) {
  variables %>%
    select(
      -cases,
      -nas
    ) %>%
    filter(
      name %in% colnames(data)
    ) %>%
    mutate(
      nas = sapply(name, function(variable) {
        sum(is.na(data[variable]))
      }),
      cases = ifelse(type %in% c('binary'),
                     sapply(name, function(variable) {
                       table(data[variable])[2]
                     }),
                     NA
      )
    )
}