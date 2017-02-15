options(java.parameters = "-Xmx16g")

library(data.table)
library(dplyr)
library(xlsx)

directory <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
directory.genotyped <- '/home/labs/dnalab/share/lims/R/gcat-seleccio/output/describe'
directory.snapshot <- '/home/labs/dnalab/share/lims/GCAT database'

date <- '2015-02-01'
participants <- fread(file.path(directory, 'Participants/data.csv'))

participants <- participants %>%
  filter(Admin.Interview.status == 'COMPLETED',
         Admin.Interview.startDate < date)

genotyped <- fread(file.path('/home/labs/dnalab/share/lims/R/gcat-cleaning-data/output/genotyped', 'data.csv')) %>%
  rename(
    entity_id=Sample.Id
  ) %>%
  merge(participants)
