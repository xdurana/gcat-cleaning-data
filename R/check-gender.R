library(data.table)
library(xlsx)
library(dplyr)
library(plyr)

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

gcat <- fread(file.path(export_dir, 'GCAT/data.csv'))
participants <- fread(file.path(export_dir, 'Participants/data.csv'))

all <- participants %>%
  filter(
    Admin.Interview.status == 'COMPLETED'
  ) %>%
  merge(gcat)

all <- all %>%
  mutate(
    SEXO_ENG=revalue(SEXO, c("HOMBRE"="MALE", "MUJER"="FEMALE"))
  )

errors <- all %>%
  filter(
    SEXO_ENG != Admin.Participant.gender
  ) %>%
  select(
    entity_id,
    SEXO,
    Admin.Participant.gender
  )

errors %>%  write.xlsx2('output/check/gender/errors.xlsx', row.names = FALSE)
