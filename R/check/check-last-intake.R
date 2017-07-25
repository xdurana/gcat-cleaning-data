library(tidyverse)

directory <- 'intake'
export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
lab_dir <- '/home/labs/dnalab/share/lims/R/gcat-laboratori/inst/extdata'

### Cal actualitzar el fitxer d'extraccions amb la informació del LIMS

pst_lims <- read_csv2(file.path(lab_dir, 'extraccions/20161214_PST_LIMS.csv'))
pst_lims <- pst_lims %>%
  transform(
    entity_id=sprintf('=%s', `External Id`)
  )

ds <- gcat %>%
  select(
    entity_id,
    ULTIMA_INGESTA_DIA_HORA,
    ULTIMA_INGESTA_DIA_MINUTOS
  ) %>%
  transform(
    Ultima.ingesta=sprintf('%02d:%02d:00', ULTIMA_INGESTA_DIA_HORA, ULTIMA_INGESTA_DIA_MINUTOS)
  )

ds <- participants %>%
  filter(
    Admin.Interview.status == 'COMPLETED'
  ) %>%
  select(
    entity_id,
    Admin.Interview.startDate,
    Admin.Participant.firstName,
    Admin.Participant.gender,
    Admin.Participant.age
  ) %>%
  left_join(ds) %>%
  left_join(pst_lims) %>%
  group_by(entity_id) %>%
  filter(row_number()==n()) %>%
  ungroup()

ds <- ds %>%
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

ds <- ds %>%
  transform(
    fasting_time = difftime(inici, Ultima.ingesta.data, units = 'hours')
  ) %>%
  transform(
    fasting_time = ifelse(fasting_time > 0, fasting_time, fasting_time + 24)
  ) %>%
  mutate(
    last_intake = as.numeric(substring(Ultima.ingesta.data, 12, 13))
  )

ds <- ds %>%
  transform(
    fasting_time_range = cut(
      fasting_time,
      breaks = c(0, 3, 6, 12, 24),
      labels = c("00 to 03 hours", "03 to 06 hours", "06 to 12 hours", "12 to 24 hours"),
      right = FALSE
    ),
    last_intake_range = cut(
      last_intake,
      breaks = c(0, 6, 12, 16, 21, 24),
      labels = c("00h to 06h", "06h to 12h", "12h to 16h", "16h to 21h", "21h to 24h"),
      right = FALSE
    )
  )

ds <- ds %>%
  select(
    entity_id,
    fasting_time,
    fasting_time_range,
    last_intake,
    last_intake_range
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))