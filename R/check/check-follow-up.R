library(tidyverse)
library(xlsx)
library(lubridate)

ds <- participants %>%
  select(
    entity_id,
    Admin.Interview.endDate
  ) %>%
  mutate(
    days_follow_up=as.numeric(interval(as.Date(Admin.Interview.endDate), Sys.Date()), "days")
  ) %>%
  dplyr::rename(
    interview_date=Admin.Interview.endDate
  )

ds %>% write_csv('output/check/follow-up/data.csv')