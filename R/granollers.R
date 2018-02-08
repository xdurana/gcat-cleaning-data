library(data.table)
library(dplyr)
library(xlsx)

participants <- fread('/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/Participants/data.csv', encoding = 'UTF-8')
gcat <- fread('/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/GCAT/data.csv', encoding = 'Latin-1')

granollers <- participants %>%
  select(
    entity_id,
    Admin.Participant.captureStartDate,
    Admin.Participant.gender,
    Admin.Participant.firstName,
    `Admin.Participant.Postal Code`
  ) %>%
  filter(
    Admin.Participant.firstName == 'TEMP GRANOLLERS'
  ) %>%
  left_join(
    gcat %>% select(
      entity_id,
      RESIDENCIA.MUNICIPIO.MUNICIPIO_RESIDENCIA
    )
  ) %>%
  mutate(
    Data = substring(Admin.Participant.captureStartDate, 1, 10)
  ) %>%
  select(
    -Admin.Participant.firstName,
    -Admin.Participant.captureStartDate
  )

granollers %>% write.xlsx2('output/granollers.xlsx')
  