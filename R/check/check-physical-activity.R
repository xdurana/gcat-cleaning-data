options(java.parameters = "-Xmx4096m")

library(tidyverse)
library(dplyr)
library(stringdist)
library(xlsx)
library(plyr)

get_ds_deporte <- function(deporte) {
  
  var.deporte <- as.symbol(sprintf('%s', deporte))
  
  deporte <- ifelse(deporte == 'LIBRE_BALONMANO', 'LIBRE_HANDBOL', deporte)
  deporte <- ifelse(deporte == 'LIBRE_SENDERIMO', 'LIBRE_SENDERISMO', deporte)
  
  var.duracion_horas <- as.symbol(sprintf('%s_DURACION_HORAS', deporte))
  var.duracion_minutos <- as.symbol(sprintf('%s_DURACION_MINUTOS', deporte))
  var.meses <- as.symbol(sprintf('%s_MESES', deporte))
  var.frecuencia <- as.symbol(sprintf('%s_FRECUENCIA', deporte))
  
  ds <- gcat %>%
    select_(
      'entity_id',
      var.deporte,
      var.duracion_horas,
      var.duracion_minutos,
      var.meses,
      var.frecuencia
    ) %>%
    mutate_(
      original = var.deporte,
      frecuencia = var.frecuencia,
      meses = var.meses,
      horas = var.duracion_horas,
      minutos = var.duracion_minutos
    ) %>%
    mutate(
      tipo = deporte
    ) %>%
    filter(
      original == 'true'
    )
  
  ds <- ds %>%
    mutate(
      tiempo = ifelse(!is.na(horas), as.numeric(horas), 0) * 60 + ifelse(!is.na(minutos), as.numeric(minutos), 0)
    ) %>%
    select(
      entity_id,
      tipo,
      frecuencia,
      meses,
      horas,
      minutos,
      tiempo
    )
  
  ds
}

get_ds_libre_generico_type <- function(type) {
  
  var.deporte <- as.symbol(sprintf('%s_DEPORTE', type))
  var.duracion_horas <- as.symbol(sprintf('%s_DURACION_HORAS', type))
  var.duracion_minutos <- as.symbol(sprintf('%s_DURACION_MINUTOS', type))
  var.meses <- as.symbol(sprintf('%s_MESES', type))
  var.frecuencia <- as.symbol(sprintf('%s_FRECUENCIA', type))
  
  ds <- gcat %>%
    select_(
      'entity_id',
      var.deporte,
      var.duracion_horas,
      var.duracion_minutos,
      var.meses,
      var.frecuencia
    ) %>%
    mutate_(
      original = var.deporte,
      frecuencia = var.frecuencia,
      meses = var.meses,
      horas = var.duracion_horas,
      minutos = var.duracion_minutos
    ) %>%
    mutate(
      tipo = type
    ) %>%
    filter(
      !is.na(original)
    ) %>%
    mutate(
      tiempo = ifelse(!is.na(horas), horas, 0) * 60 + ifelse(!is.na(minutos), minutos, 0)
    ) %>%
    select(
      entity_id,
      tipo,
      original,
      frecuencia,
      meses,
      horas,
      minutos,
      tiempo
    )
  
  ds
}
  
get_ds_libre_generico <- function() {
  do.call(rbind, lapply(
    c('LIBRE_GENERICO_LIGERO', 'LIBRE_GENERICO_MODERADO', 'LIBRE_GENERICO_VIGOROSO'),
    get_ds_libre_generico_type
  ))
}

traduir_esports <- function(ds) {
  
  ds_trad <- ds %>%
    mutate(
      value = toupper(iconv(original, from="UTF-8", to="ASCII//TRANSLIT"))
    ) %>%
    #separate_rows(value, sep = ",") %>%
    #separate_rows(value, sep = "\\+") %>%
    #separate_rows(value, sep = "/") %>%
    #separate_rows(value, sep = " I ") %>%
    #separate_rows(value, sep = " Y ") %>%
    mutate(
      value = gsub('-', ' ', value)
    ) %>%
    mutate(
      value = gsub(' DE ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' EN ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' PER ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' POR ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' LA ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' AL ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' DEL ', ' ', value)
    ) %>%
    mutate(
      value = gsub(' A ', ' ', value)
    ) %>%
    mutate(
      value = gsub('\\(', ' ', value)
    ) %>%
    mutate(
      value = gsub('\\)', '', value)
    ) %>%
    mutate(
      value = gsub('ETC', '', value)
    ) %>%
    mutate(
      value = gsub('EXTREM', '', value)
    ) %>%
    mutate(
      value = gsub('APARELLS', '', value)
    ) %>%
    mutate(
      value = gsub('RAPIDO', '', value)
    ) %>%
    mutate(
      value = gsub('RAPID', '', value)
    ) %>%
    mutate(
      trad_index = amatch(toupper(value), diccionari$AF.REPORTED, matchNA = FALSE, nomatch = 0, method = "lv")
    ) %>%
    mutate(
      trad_index = ifelse(
        trad_index == 0,
        amatch(toupper(value), diccionari$AF.REPORTED, matchNA = FALSE, nomatch = 0, maxDist = 1, method = "lv"),
        trad_index
      )
    ) %>%
    as.data.frame()
  
  ds_trad$trad <- sapply(ds_trad$trad_index, function(x) ifelse(x == 0, NA, diccionari$AF.REPORTED[x]))
  
  ds_trad %>%
    select(
      -trad_index
    )
}

###

diccionari <- read.xlsx2('inst/extdata/physical_activity/Diccionari_Generic_AF_2017-09-14.xlsx', sheetIndex = 1, stringsAsFactors = FALSE) %>%
  unique()

ds_libre <- do.call(rbind, lapply(colnames(gcat)[grepl('^LIBRE_[a-zA-Z]+$', colnames(gcat))], get_ds_deporte))

ds_libre_diccionari <- ds_libre %>%
  mutate(AF.REPORTED = gsub('LIBRE_', '', tipo)) %>%
  left_join(diccionari) %>%
  mutate(
    original = AF.REPORTED,
    value = AF.REPORTED
  )

ds_libre_diccionari %>%
  as.data.frame() %>%
  write.xlsx2('output/physical_activity/activitat_fisica.xlsx', row.names = FALSE)

ds_trad <- get_ds_libre_generico() %>%
  traduir_esports() %>%
  dplyr::rename(
    AF.REPORTED = trad
  ) %>%
  left_join(
    diccionari
  )

ds_trad %>%
  write_csv('output/physical_activity/corregides.csv', na = "")

ds_trad %>% 
  select(-value) %>%
  write.xlsx2('output/physical_activity/corregides.xlsx', row.names = FALSE)

ds_notrad <- ds_trad %>%
  filter(
    is.na(AF.REPORTED)
  )

table(ds_notrad$value) %>% as.data.frame() %>% arrange(Freq)

# merge them

ds_libre_diccionari %>%
  rbind(ds_trad) %>%
  select(
    entity_id,
    frecuencia,
    meses,
    horas,
    minutos,
    tiempo,
    AF.REPORTED,
    COMPEDIUM.PA,
    NUMBER,
    METS
  ) %>%
  filter(
     METS != ''
  ) %>%
  as.data.frame() %>%
  write.xlsx2('output/physical_activity/totes.xlsx', row.names = FALSE)

ds_libre_diccionari_semana <- 
  ds_libre_diccionari %>%
  filter(
    tiempo > 0 & METS > 0
  ) %>%
  mutate(
    dias_semana = as.numeric(ifelse(frecuencia == 0, NA, revalue(frecuencia, c("0" = "0", "1" = "0.12", "2" = "0.23", "3" = "0.58", "4" = "1", "5" = "2.5", "6" = "4.5", "7" = "7"))))
  ) %>%
  mutate(
    METS_semana = dias_semana * as.numeric(METS) * as.numeric(tiempo)/60
  ) %>%
  select(
    entity_id,
    meses,
    dias_semana,
    horas,
    minutos,
    tiempo,
    COMPEDIUM.PA,
    METS,
    METS_semana
  ) %>%
  filter(
    !is.na(METS_semana)
  )

#TODO control errors

ds_libre_diccionari_semana %>%
  filter(
    minutos == 60 & horas != 0
  )

ds_libre_diccionari_semana %>%
  as.data.frame() %>%
  write.xlsx2('output/physical_activity/mets_semana.xlsx', row.names = FALSE)

ds_libre_diccionari %>%
  as.data.frame() %>%
  write.xlsx2('output/physical_activity/totes.xlsx', row.names = FALSE)

ds_notrad %>% arrange(value) %>% View()
