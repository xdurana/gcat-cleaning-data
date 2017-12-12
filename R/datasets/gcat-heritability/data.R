library(tidyverse)
library(openxlsx)
library(xtable)
library(knitr)

participants <- read_csv('/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/Participants/data.csv')

data <- read_csv('output/datasets/gcat/data.csv')

variables <-
  read_csv('output/datasets/gcat/variables.csv') %>%
  arrange(
    category,
    name
  )

#variables[variables['name'] == 'working_shift', 'name'] <- 'night_shift'

traits <- read_delim('output/datasets/gcat-heritability/pheno_gcat_206_traits.txt', delim = ' ') %>%
  mutate(name = variable) %>%
  select(name)

phenotypes <- traits %>%
  left_join(variables)

participants_heritability <- participants %>%
  filter(
    Admin.Participant.captureEndDate < '2018-07-28'
  ) %>%
  select(entity_id) %>%
  left_join(data) %>%
  filter(
    status == 'COMPLETED'
  ) %>%
  select(one_of(c('entity_id', phenotypes$name)))

phenotypes <- variables %>%
  filter(
    name %in% phenotypes$name
  ) %>%
  select(
    name,
    area,
    subarea,
    description,
    type
  ) %>%
  unique()

participants_heritability %>%
  write_csv('output/datasets/gcat-heritability/data.csv')

phenotypes %>%
  write_csv('output/datasets/gcat-heritability/variables.csv')

dictionary <- phenotypes %>%
  mutate(
    table = 'HERITABILITY',
    valueType = recode(type, 'binary' = 'boolean', 'categorical' = 'text', 'numeric' = 'integer', 'time' = 'integer'),
    entityType = 'Participant',
    referencedEntityType = '',
    mimeType = '',
    unit = '',
    repeatable = 0,
    occurrenceGroup = '',
    index = 0,
    `opal::derivedFrom` = '/datasource/GCAT/table/GCAT',
    questionnaire = 'GCAT',
    questionName = '',
    label = description,
    script = "$('')"
  ) %>%
  mutate(
    subarea = recode(
      subarea,
      "Anthropometry" = "Anthropo_measures",
      "Circulation and respiration" = "Circulation_respiration",
      "Physical characteristics" = "Physical_characteristics",
      "Skin and subcutaneous tissue" = "Skin_subcutaneous",
      "Nutrition" = "Nutrition",
      "Sleep" = "Sleep",
      "Tobacco" = "Tobacco",
      "Psychological distress" = "Psychological_emotional_distress",
      "Neoplasms (C00-D48)" = "Neoplasms",
      "Medication and supplements" = "Medication_supplements",
      "Contraception and family planning" = "Contraception",
      "Gravidity, pregnancy outcomes, parity (female) and fertility (male)" = "Gravidity_fertility",
      "Menstruation, menopause and andropause" = "Menstr_menop_andropause"
    )
  ) %>%
  mutate(
    `Mlstr_area::Sociodemographic_economic_characteristics` = ifelse(area == 'Socio-demographic and economic characteristics', subarea, NA),
    `Mlstr_area::Physical_measures` = ifelse(area == 'Physical measures', subarea, NA),
    `Mlstr_area::Social_environment` = NA,
    `Mlstr_area::Health_community_care_utilization` = NA,
    `Mlstr_area::Lifestyle_behaviours` = ifelse(area == 'Lifestyle and health behaviours', subarea, NA),
    `Mlstr_habits::Tobacco` = ifelse(area == 'Tobacco', subarea, NA),
    `Mlstr_area::Cognitive_psychological_measures` = ifelse(area == 'Cognition, personality and other psychological measures', subarea, NA),
    `Mlstr_area::Diseases` = ifelse(area == 'Diseases', subarea, NA),
    `Mlstr_area::Medication_supplements` = ifelse(area == 'Medication and supplements', subarea, NA),
    `Mlstr_area::Non_pharmacological_interventions` = NA,
    `Mlstr_area::Reproduction` = ifelse(area == 'Reproduction', subarea, NA)
  ) %>%
  select(
    table,
    name,
    valueType,
    entityType,
    referencedEntityType,
    mimeType,
    unit,
    repeatable,
    occurrenceGroup,
    index,
    `opal::derivedFrom`,
    questionnaire,
    questionName,
    label,
    script,
    `Mlstr_area::Sociodemographic_economic_characteristics`,
    `Mlstr_area::Physical_measures`,
    `Mlstr_area::Social_environment`,
    `Mlstr_area::Health_community_care_utilization`,
    `Mlstr_area::Lifestyle_behaviours`,
    `Mlstr_habits::Tobacco`,
    `Mlstr_area::Cognitive_psychological_measures`,
    `Mlstr_area::Diseases`,
    `Mlstr_area::Medication_supplements`,
    `Mlstr_area::Non_pharmacological_interventions`,
    `Mlstr_area::Reproduction`
  ) %>%
  unique()

dictionary %>%
  write.xlsx('output/datasets/gcat-heritability/HERITABILITY-dictionary.xlsx', sheetName = 'Variables')

or <- read_csv('output/datasets/gcat-heritability/odds-ratio.csv')
or <- or %>%
  filter(
    or > 10
  ) %>%
  arrange(desc(or)) %>%
  mutate(medication = gsub("\\s*\\([^\\)]+\\)","",as.character(medication))) %>%
  mutate(diagnosis = gsub("\\s*\\([^\\)]+\\)","",as.character(diagnosis))) %>%
  select(
    diagnosis_code,
    diagnosis,
    medication_code,
    medication,
    or
  )

print(xtable(phenotypes, type = "latex"), file = "output/datasets/gcat-heritability/phenotypes.tex")
print(xtable(or, type = "latex"), file = "output/datasets/gcat-heritability/or.tex")

phens <- phenotypes %>%
  select(
    name,
    description,
    area,
    subarea
  ) %>%
  arrange(
    area,
    subarea,
    name
  )

participants_heritability_bool <- participants_heritability %>%
  mutate_at(
    vars(dictionary[dictionary$valueType == "boolean", ]$name),
    funs(ifelse(. == 1, "true", ifelse(. == 0, "false", NA)))
  )

participants_heritability_bool %>%
  write_csv('output/datasets/gcat-heritability/data_bool.csv', na = '')


## Generate supplementary tables 1 and 2

options(xtable.comment = FALSE)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
rmarkdown::render('R/datasets/gcat-heritability/sup-table-1.Rmd', 'pdf_document', output_dir = 'output/datasets/gcat-heritability', clean = FALSE)
rmarkdown::render('R/datasets/gcat-heritability/sup-table-2.Rmd', 'pdf_document', output_dir = 'output/datasets/gcat-heritability')
