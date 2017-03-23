library(xlsx)
library(dplyr)
library(gtools)

directory <- '/home/labs/dnalab/share/lims/R/gcat-seleccio'

getCurrentSelectedPlates <- function() {
  dirs <- list.dirs(file.path(directory, 'plates'))
  sort(as.numeric(gsub(file.path(directory, "plates/"), "", dirs[grepl("plates/", dirs)])))
}

getConcentracions <- function(plate) {
  read.xlsx2(file.path(directory, sprintf('plates/%s/concentracions.xlsx', plate)), sheetIndex = 1) %>%
    transform(
      entity_id=sprintf('=%s', Sample.Id),
      plate=plate
    ) %>%
    select(
      -Sample.Id
    )
}

getCurrentParticipants <- function() {
  do.call(
    smartbind,
    lapply(getCurrentSelectedPlates(), getConcentracions))
}

getCurrentParticipants() %>%
  write.csv('output/check/genotyped/data.csv', row.names = FALSE)
