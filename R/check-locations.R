library(data.table)
library(xlsx)
library(dplyr)
library(plyr)

municipios <- read.xlsx2('inst/extdata/municipios/17codmun.xlsx', sheetIndex = 1, startRow = 2) %>%
  mutate(
    NOMBRE=iconv(toupper(NOMBRE), to='ASCII//TRANSLIT')
  )

correccions <- read.xlsx2('inst/extdata/municipios/correccions.xlsx', sheetIndex = 1) %>%
  mutate(
    correccio=iconv(toupper(correccio), to='ASCII//TRANSLIT')
  ) %>%
  transform(
    x=gsub("\\?", "", x)
  ) %>%
  transform(
    x=gsub("\\(", "", x)
  ) %>%
  transform(
    x=gsub("\\)", "", x)
  )
E00251715849021
export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
gcat <- read.csv2(file.path(export_dir, 'QUESTIONARI/data.csv'), sep = ',', encoding="UTF-8")

gcat.pais <- gcat %>%
  select(
    entity_id,
    MUNICIPIO_NACIMIENTO,
    MUNICIPIO_RESIDENCIA_OTRO,
    MUNICIPIO_NACIMIENTO_OTRO,
    MUNICIPIO_RESIDENCIA,
    PAIS_NACIMIENTO,
    PAIS_NACIMIENTO_OTRO
  ) %>%
  mutate(
    PAIS_NACIMIENTO=replace(PAIS_NACIMIENTO, PAIS_NACIMIENTO=='0', '')
  ) %>%
  mutate(
    PAIS_NACIMIENTO=replace(PAIS_NACIMIENTO, PAIS_NACIMIENTO=='1', 'SPAIN')
  ) %>%
  mutate(
    PAIS_NACIMIENTO=replace(PAIS_NACIMIENTO, PAIS_NACIMIENTO=='2', '')
  ) %>%
  mutate(
    PAIS_NACIMIENTO=replace(PAIS_NACIMIENTO, is.na(PAIS_NACIMIENTO), '')
  ) %>%
  mutate(
    PAIS_NACIMIENTO=paste(PAIS_NACIMIENTO, PAIS_NACIMIENTO_OTRO, sep = '')
  ) %>%
  select(
    entity_id,
    MUNICIPIO_NACIMIENTO,
    MUNICIPIO_RESIDENCIA_OTRO,
    MUNICIPIO_NACIMIENTO_OTRO,
    MUNICIPIO_RESIDENCIA,
    PAIS_NACIMIENTO
  ) %>%
  mutate(
    MUNICIPIO_NACIMIENTO=iconv(toupper(MUNICIPIO_NACIMIENTO), to='ASCII//TRANSLIT'),
    MUNICIPIO_NACIMIENTO_OTRO=iconv(toupper(MUNICIPIO_NACIMIENTO_OTRO), to='ASCII//TRANSLIT'),
    PAIS_NACIMIENTO=iconv(toupper(PAIS_NACIMIENTO), to='ASCII//TRANSLIT')
  ) %>%
  mutate(
    MUNICIPIO_NACIMIENTO = ifelse(
      MUNICIPIO_NACIMIENTO == '',
      MUNICIPIO_NACIMIENTO_OTRO,
      MUNICIPIO_NACIMIENTO
    )
  )

gcat.municipis <- gcat.pais %>%
  transform(
    MUNICIPIO_NACIMIENTO = gsub("\\?", "", MUNICIPIO_NACIMIENTO)
  ) %>%
  transform(
    MUNICIPIO_NACIMIENTO = gsub("\\(", "", MUNICIPIO_NACIMIENTO)
  ) %>%
  transform(
    MUNICIPIO_NACIMIENTO = gsub("\\)", "", MUNICIPIO_NACIMIENTO)
  )

for (i in 1:nrow(correccions)) {
  gcat.municipis$MUNICIPIO_NACIMIENTO <- gsub(sprintf("^%s$", as.character(correccions$x[i])), as.character(correccions$correccio[i]), gcat.municipis$MUNICIPIO_NACIMIENTO)
}

errors <- gcat.municipis %>%
  filter(
    !(MUNICIPIO_NACIMIENTO %in% municipios$NOMBRE) & MUNICIPIO_NACIMIENTO != ''
  )

errors$MUNICIPIO_NACIMIENTO %>% unique %>% write.xlsx2('output/check/locations/errors.xlsx', row.names = FALSE)
gcat.municipis %>% 
  select(
    entity_id,
    MUNICIPIO_NACIMIENTO,
    PAIS_NACIMIENTO
  ) %>%
  write.csv2('output/check/locations/location.csv', row.names = FALSE)

### Create text file with municipios

municipios_nombre <- paste(shQuote(as.character((municipios %>% arrange(NOMBRE))$NOMBRE), type="cmd"), collapse=", ")
fileConn <- file("output/locations/municipios.txt")
writeLines(municipios_nombre, fileConn)
close(fileConn)
