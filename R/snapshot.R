library(data.table)
library(dplyr)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
date <- '2016-10-01'

## Load datasets

participants <- fread(file.path(directory, 'Participants/data.csv'))
questionari <- fread(file.path(directory, 'QUESTIONARI/data.csv'))
altura <- fread(file.path(directory, 'Altura/data.csv'))
cinturacadera <- fread(file.path(directory, 'CinturaCadera/data.csv'))
peso <- fread(file.path(directory, 'Peso/data.csv'))
tensionarterial <- fread(file.path(directory, 'TensionArterial/data.csv'))
seleccio <- fread(file.path(directory, 'SELECCIO/data.csv'))

## Snapshot

participants <- filter(participants,
                       participants$Admin.Interview.status == 'COMPLETED',
                       participants$Admin.Participant.captureEndDate < date)

participants <- select(participants,
                       entity_id,
                       Admin.Participant.captureStartDate,
                       Admin.Participant.captureEndDate,
                       Admin.Participant.gender,
                       Admin.Participant.firstName,
                       Admin.Participant.birthDate,
                       Admin.Participant.birthYear,
                       Admin.Participant.age,
                       Admin.Participant.PostalCode,
                       Admin.Action.comment
                       )

colnames(participants)

seleccio <- select(seleccio, entity_id, TIPO_CUESTIONARIO)

## Remove instrument metadata

altura <- select(altura, -(InstrumentRun.user:InstrumentRun.Contraindication.type))
cinturacadera <- select(cinturacadera, -(InstrumentRun.user:InstrumentRun.Contraindication.type))
peso <- select(peso, -(InstrumentRun.user:InstrumentRun.Contraindication.type))
tensionarterial <- select(tensionarterial, -(InstrumentRun.user:InstrumentRun.Contraindication.type))

## Merge all

all <- left_join(participants, questionari) %>%
  left_join(altura) %>%
  left_join(cinturacadera) %>%
  left_join(peso) %>%
  left_join(tensionarterial) %>%
  left_join(seleccio)

write.csv2(all, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'data.csv'), row.names = FALSE)
