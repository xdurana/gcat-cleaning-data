library(tidyverse)
library(gtools)
library(xlsx)

directory <- '/home/labs/dnalab/share/lims/R/gcat-seleccio'

getCurrentSelectedPlates <- function() {
  dirs <- list.dirs(file.path(directory, 'plates'))
  sort(as.numeric(gsub(file.path(directory, "plates/"), "", dirs[grepl("plates/", dirs)])))
}

getSeguiment <- function(plate) {
  read.xlsx2(file.path(directory, sprintf('plates/%s/seguiment.xlsx', plate)), sheetIndex = 1) %>%
    transform(
      entity_id=sprintf('=%s', Sample.Id),
      plate=plate
    ) %>%
    select(
      -Sample.Id
    )
}

getCoreParticipants <- function() {
  do.call(
    smartbind,
    lapply(getCurrentSelectedPlates(), getSeguiment))
}

core <- getCoreParticipants()

ds <- core %>%
  mutate(
    core_plate=plate,
    core_extraction_plate=FINAL.PLATE,
    core_sample_name=Sample.name
  ) %>%
  select(
    entity_id,
    core_plate,
    core_extraction_plate,
    core_sample_name
  )

ds %>% write_csv('output/check/core/data.csv')
