library(data.table)
library(dplyr)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
date <- '2016-10-01'

## Load datasets

participants <- fread(file.path(directory, 'PARTICIPANTES/data.csv'))
gcat <- fread(file.path(directory, 'CUESTIONARIO/data.csv'))
altura <- fread(file.path(directory, 'MEDIDAS_ALTURA/data.csv'))
cinturacadera <- fread(file.path(directory, 'MEDIDAS_CINTURA_CADERA/data.csv'))
peso <- fread(file.path(directory, 'MEDIDAS_PESO/data.csv'))
tensionarterial <- fread(file.path(directory, 'MEDIDAS_TENSION_ARTERIAL/data.csv'))
seleccio <- fread(file.path(directory, 'SELECCIO/data.csv'))

## Merge all

participants <- filter(participants,
                       participants$PARTICIPANTE_ENTREVISTA_STATUS == 'COMPLETED',
                       participants$PARTICIPANTE_ENTREVISTA_INICIO < date)

seleccio <- select(seleccio, entity_id, TIPO_CUESTIONARIO)

all <- left_join(participants, gcat) %>%
  left_join(altura) %>%
  left_join(cinturacadera) %>%
  left_join(peso) %>%
  left_join(tensionarterial) %>%
  left_join(seleccio)

write.csv2(all, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'data.csv'), row.names = FALSE)

gcat_participants <- select(participants, entity_id)
gcat <- left_join(participants, as.data.frame(gcat))

gcat_1 <- gcat[, 1:1000]
gcat_2 <- gcat[, c(1, 1001:1999)]
gcat_3 <- gcat[, c(1, 2000:2998)]
gcat_4 <- gcat[, c(1, 2999:3487)]

gcat_0 <- left_join(participants, altura) %>%
  left_join(cinturacadera) %>%
  left_join(peso) %>%
  left_join(tensionarterial) %>%
  left_join(seleccio)

write.csv2(gcat_0, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'gcat_0.csv'), row.names = FALSE)
write.csv2(gcat_1, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'gcat_1.csv'), row.names = FALSE)
write.csv2(gcat_2, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'gcat_2.csv'), row.names = FALSE)
write.csv2(gcat_3, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'gcat_3.csv'), row.names = FALSE)
write.csv2(gcat_4, file.path(sprintf('/home/labs/dnalab/share/lims/GCAT database/%s', date), 'gcat_4.csv'), row.names = FALSE)
