library(tidyverse)
library(lubridate)
library(xlsx)

ds <- gcat %>%
  select(
    entity_id,
    EDAD_ANOS,
    FECHA_NACIMIENTO_DIA,
    FECHA_NACIMIENTO_MES,
    FECHA_NACIMIENTO_ANO
  ) %>%
  right_join(
    participants %>%
      select(
        entity_id,
        Admin.Interview.status,
        Admin.Participant.age,
        Admin.Participant.birthDate,
        Admin.Interview.startDate
      )
  ) %>%
  filter(
    Admin.Interview.status == 'COMPLETED'
  )

ds <- ds %>%
  mutate(
    Admin.Participant.age=as.numeric(Admin.Participant.age),
    EDAD_ANOS=as.numeric(EDAD_ANOS),
    FECHA_NACIMIENTO=as.Date(
      sprintf('%s/%s/%s',
              FECHA_NACIMIENTO_DIA,
              FECHA_NACIMIENTO_MES,
              FECHA_NACIMIENTO_ANO), '%d/%m/%Y'),
    Admin.Participant.birthDate=as.Date(Admin.Participant.birthDate),
    Admin.Interview.startDate=as.Date(Admin.Interview.startDate),
    DIFF_DAYS=FECHA_NACIMIENTO - Admin.Participant.birthDate,
    Admin.Participant.age.CALC=as.period(interval(Admin.Participant.birthDate, Admin.Interview.startDate), unit = 'year')$year,
    age=as.period(interval(FECHA_NACIMIENTO, Admin.Interview.startDate), unit = 'year')$year
  )

### Tots els errors

errors <- ds %>%
  select(
    entity_id,
    FECHA_NACIMIENTO,
    Admin.Participant.birthDate,
    Admin.Interview.startDate,
    Admin.Participant.age,
    Admin.Participant.age.CALC,
    EDAD_ANOS,
    age,
    DIFF_DAYS
  ) %>%
  filter(
    !(Admin.Participant.age == Admin.Participant.age.CALC &
        Admin.Participant.age.CALC == EDAD_ANOS &
        EDAD_ANOS == age &
        abs(DIFF_DAYS) < 10
      )
  )

ds %>%
  filter(
    EDAD_ANOS == age
  ) %>%
  select(
    entity_id,
    age
  ) %>%
  write_csv('output/check/age/data.csv')

errors %>% write_csv('output/check/age/errors.csv')

function() {
  
  ### Errors en els anys reportats pel participant
  
  errors %>%
    filter(
      DIFF_DAYS == 0 & EDAD.EDAD.ANOS != Admin.Participant.age.CALC
    ) %>%
    write.xlsx2('output/check/age/errors_participant.xlsx', row.names = FALSE)
  
  errors %>%
    filter(
      DIFF_DAYS == 0 & EDAD.EDAD.ANOS != Admin.Participant.age.CALC
    ) %>%
    mutate(
      EDAD.EDAD.ANOS=Admin.Participant.age
    ) %>%
    select(
      entity_id,
      EDAD.EDAD.ANOS
    ) %>%
    write.table(sprintf('/home/labs/dnalab/share/lims/R/gcat-cohort/output/import/E00__%s_GCAT.csv', Sys.Date()), row.names = FALSE, sep = ',')
  
  ### Errors en els anys reportats per l'adminstracio
  
  errors %>%
    filter(
      DIFF_DAYS == 0 & Admin.Participant.age != Admin.Participant.age.CALC
    ) %>%
    write.xlsx2('output/check/age/errors_administracio.xlsx', row.names = FALSE)
  
  errors %>%
    filter(
      DIFF_DAYS == 0 & Admin.Participant.age != Admin.Participant.age.CALC
    ) %>%
    mutate(
      Admin.Participant.age=EDAD.EDAD.ANOS
    ) %>%
    select(
      entity_id,
      Admin.Participant.age
    ) %>%
    write.table(sprintf('/home/labs/dnalab/share/lims/R/gcat-cohort/output/import/E00__%s_Participants.csv', Sys.Date()), row.names = FALSE, sep = ',')
  
  
  errors %>% write.xlsx2('output/check/age/errors.xlsx', row.names = FALSE)
}
