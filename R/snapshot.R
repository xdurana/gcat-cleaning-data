options(java.parameters = "-Xmx4g")

library(data.table)
library(dplyr)
library(xlsx)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
directory.genotyped <- '/home/labs/dnalab/share/lims/R/gcat-seleccio/output/describe'
directory.snapshot <- '/home/labs/dnalab/share/lims/GCAT database'

date <- '2016-10-01'
participants <- fread(file.path(directory, 'PARTICIPANTES/data.csv'))

snapshot <- function(date, participants, prefix = 'gcat') {
  
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
  
  gcat_1 <- gcat[, 1:500]
  gcat_2 <- gcat[, c(1,  501: 599)]
  gcat_3 <- gcat[, c(1,  600: 998)]
  gcat_4 <- gcat[, c(1,  999:1497)]
  gcat_5 <- gcat[, c(1, 1498:1996)]
  gcat_6 <- gcat[, c(1, 1997:2495)]
  gcat_7 <- gcat[, c(1, 2496:2994)]
  gcat_8 <- gcat[, c(1, 2995:3487)]
  
  gcat_0 <- left_join(participants, altura) %>%
    left_join(cinturacadera) %>%
    left_join(peso) %>%
    left_join(tensionarterial) %>%
    left_join(seleccio)

  for (n in 0:8) {
    ds <- eval(parse(text=sprintf("gcat_%s", n)))
    write.csv2(ds, file.path(file.path(directory.snapshot, date), sprintf('%s_%s.csv', prefix, n)), row.names = FALSE)
    write.xlsx2(ds, file.path(file.path(directory.snapshot, date), sprintf('%s_%s.xlsx', prefix, n)), row.names = FALSE)
  }
}


core <- function() {
  genotyped <- read.table(file.path(directory.genotyped, 'genotyped.csv'), sep = ',', stringsAsFactors = FALSE, header = TRUE)
  participants.genotyped <- filter(participants, participants$entity_id %in% genotyped$Sample.Id)
  snapshot(date, participants.genotyped, prefix = 'gcat_core')
}