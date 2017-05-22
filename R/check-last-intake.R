library(dplyr)

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
lab_dir <- '/home/labs/dnalab/share/lims/R/gcat-laboratori/inst/extdata'

### Cal actualitzar el fitxer d'extraccions amb la informació del LIMS

pst_lims <- read.csv(file.path(lab_dir, 'extraccions/20161214_PST_LIMS.csv'), sep = ';')
pst_lims <- pst_lims %>%
  transform(
    entity_id=sprintf('=%s', External.Id)
  )

gcat <- read.csv(file.path(export_dir, 'GCAT/data.csv'), sep = ',')

gcat <- gcat %>%
  select(
    entity_id,
    ULTIMA_INGESTA.HORA.HORA_ULTIMA_INGESTA,
    ULTIMA_INGESTA.HORA.MINUTOS_ULTIMA_INGESTA
  ) %>%
  transform(
    Ultima.ingesta=sprintf('%02d:%02d:00', ULTIMA_INGESTA.HORA.HORA_ULTIMA_INGESTA, ULTIMA_INGESTA.HORA.MINUTOS_ULTIMA_INGESTA)
  )

participants <- read.csv(file.path(export_dir, 'Participants/data.csv'), sep = ',', stringsAsFactors = FALSE)
participants <- participants %>%
  filter(
    Admin.Interview.status == 'COMPLETED'
  ) %>%
  select(
    entity_id,
    Admin.Interview.startDate,
    Admin.Participant.firstName,
    Admin.Participant.gender,
    Admin.Participant.age
  )

selection <- participants %>%
  merge(gcat) %>%
  merge(pst_lims)

selection <- selection %>%
  transform(
    Ultima.ingesta.data = as.POSIXct(as.character(Ultima.ingesta), format="%H:%M:%S"),
    Extraction.time.data = as.POSIXct(as.character(Extraction.time), format="%H:%M:%S"),
    inici = as.POSIXct(substring(Admin.Interview.startDate, 12, 19), format="%H:%M:%S")
  ) %>%
  transform(
    dif_extraccio_inici = difftime(as.POSIXlt(Extraction.time.data), as.POSIXlt(inici), units = 'hours')
  ) %>%
  transform(
    inici=as.POSIXct(as.character(Extraction.time), format="%H:%M:%S")
  )

selection <- selection %>%
  transform(
    diferencia = difftime(inici, Ultima.ingesta.data, units = 'hours')
  ) %>%
  transform(
    diferencia = ifelse(diferencia > 0, diferencia, diferencia + 24)
  ) %>%
  filter(
    !is.na(Ultima.ingesta.data) &
      !is.na(Extraction.time.data)
  )

selection %>% write.csv2('output/check/intake/data.csv', row.names = FALSE)
