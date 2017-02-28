library(data.table)
library(xlsx)
library(dplyr)
library(plyr)

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

meses <- c(
  "Enero"="1",
  "Febrero"="2",
  "Marzo"="3",
  "Abril"="4",
  "Mayo"="5",
  "Junio"="6",
  "Julio"="7",
  "Agosto"="8",
  "Septiembre"="9",
  "Octubre"="10",
  "Noviembre"="11",
  "Diciembre"="12"
)

gcat <- fread(file.path(export_dir, 'GCAT/data.csv'))
participants <- fread(file.path(export_dir, 'Participants/data.csv'))

all <- participants %>%
  filter(
    Admin.Interview.status == 'COMPLETED'
  ) %>%
  merge(gcat) %>%
  mutate(
    Admin.Participant.age=as.numeric(Admin.Participant.age),
    EDAD.EDAD.ANOS=as.numeric(EDAD.EDAD.ANOS),
    Admin.Participant.age.CALC=as.numeric(floor(difftime(Admin.Interview.startDate, Admin.Participant.birthDate, units="days")/365.25)),
    FECHA_NACIMIENTO=as.Date(
      sprintf('%s/%s/%s',
              FECHA_NACIMIENTO.Fecha.Dia,
              revalue(FECHA_NACIMIENTO.Fecha.Mes, meses),
              FECHA_NACIMIENTO.Fecha.Ano), '%d/%m/%Y'),
    Admin.Participant.birthDate=as.Date(Admin.Participant.birthDate),
    Admin.Interview.startDate=as.Date(Admin.Interview.startDate),
    DIFF_DAYS=FECHA_NACIMIENTO - Admin.Participant.birthDate,
    EDAD.EDAD.ANOS.CALC=as.numeric(floor(difftime(Admin.Interview.startDate, FECHA_NACIMIENTO, units="days")/365.25))
  )

### Tots els errors

errors <- all %>%
  select(
    entity_id,
    FECHA_NACIMIENTO,
    Admin.Participant.birthDate,
    Admin.Interview.startDate,
    Admin.Participant.age,
    Admin.Participant.age.CALC,
    EDAD.EDAD.ANOS,
    EDAD.EDAD.ANOS.CALC,
    DIFF_DAYS
  ) %>%
  filter(
    !(Admin.Participant.age == Admin.Participant.age.CALC &
        Admin.Participant.age.CALC == EDAD.EDAD.ANOS &
        EDAD.EDAD.ANOS == EDAD.EDAD.ANOS.CALC &
        DIFF_DAYS == 0
      )
  )

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
  write.table('output/check/age/GCAT_edad_participants.csv', row.names = FALSE, sep = ',')

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
  write.table('output/check/age/GCAT_edad_administracio.csv', row.names = FALSE, sep = ',')


errors %>% write.xlsx2('output/check/age/errors.xlsx', row.names = FALSE)
