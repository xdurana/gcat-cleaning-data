library(tidyverse)

municipis <- read_csv2('inst/extdata/municipios/t446mun.csv', skip = 5) %>%
  filter(
    !is.na(Codi)
  ) %>%
  mutate(
    `Tipus municipi` = cut(as.numeric(Total), breaks = c(0, 2000, 20000, 500000, 5000000), labels = c('1 Poble petit', '2 Poble', '3 Ciutat', '4 Ciutat gran'))
  )

locations_residencia <- read_csv2('output/check/locations/location.csv') %>%
  select(
    entity_id,
    MUNICIPIO_RESIDENCIA
  ) %>%
  separate(
    MUNICIPIO_RESIDENCIA, c('Codi', 'Municipi'), ';'
  ) %>%
  mutate(
    Codi=gsub('-', '', Codi)
  )

residencia <- locations_residencia %>%
  left_join(municipis)

residencia %>%
  write_csv('output/check/locations/residencia.csv')

locations_municipi <- read_csv2('output/check/locations/location.csv') %>%
  select(
    entity_id,
    MUNICIPIO_NACIMIENTO
  )

municipios <- read_csv2('inst/extdata/municipios/municipios.csv') %>%
  transform(
    MUNICIPIO_NACIMIENTO = toupper(iconv(NOMBRE, to='ASCII//TRANSLIT')),
    CODI_MUN=gsub('-', '', CODI_MUN)
  )

naixement <- locations_municipi %>%
  left_join(municipios) %>%
  dplyr::rename(
    Codi=CODI_MUN,
    Municipi=MUNICIPIO_NACIMIENTO,
    Literal=NOMBRE
  ) %>%
  select(
    entity_id,
    Codi,
    Municipi,
    Literal
  )

naixement %>%
  write_csv('output/check/locations/naixement.csv')

core <- read_csv('output/genotyped/data.csv') %>%
  dplyr::rename(
    entity_id=Sample.Id
  ) %>%
  filter(
    plate <= 60
  )

residencia_core <- core %>%
  left_join(
    residencia
  )
  
as.data.frame(table(residencia$`Tipus municipi`)) %>%
  write_csv('output/check/locations/tipus_municipi_residencia.csv')


as.data.frame(table(residencia_core$`Tipus municipi`)) %>%
  write_csv('output/check/locations/tipus_municipi_residencia_core.csv')
