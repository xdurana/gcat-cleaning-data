library(tidyverse)
library(ggplot2)

recruitment <- function() {
  
  ds <- participants %>%
    mutate(
      year = substring(Admin.Participant.captureEndDate, 0, 4),
      centre = ifelse(grepl('TEMP', Admin.Participant.firstName), 'RECLUTAMENT TEMPORAL', Admin.Participant.firstName),
      count = 1
    ) %>%
    select(
      year,
      centre,
      count
    ) %>%
    filter(
      !(centre %in% c('ALTHAIA', 'CLINIC', 'VERGE DE LA CINTA'))
    )
  
  count <- ds %>%
    group_by(
      year,
      centre
    ) %>%
    summarise(n = n()) %>%
    arrange(year, desc(n))
  
  count %>%
    ggplot(aes(x = centre, y = n, fill = year, label = n)) +
    geom_bar(stat = "identity") +
    geom_text(size = 3, position = position_stack(vjust = 0.5)) +
    theme(axis.text.x = element_text(angle = 90, vjust=0.5, hjust=0.5))
}