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

participants <- participants %>%
  filter(
    Admin.Participant.captureEndDate < '2018-07-28'
  ) %>%
  select(entity_id) %>%
  left_join(data) %>%
  filter(
    status == 'COMPLETED'
  ) %>%
  select(one_of(c('entity_id', variables$name)))

phenotypes <- variables %>%
  select(
    name,
    area,
    subarea,
    description,
    type
  ) %>%
  unique()

dictionary <- phenotypes %>%
  mutate(
    table = 'GCAT',
    valueType = recode(type, 'binary' = 'boolean', 'categorical' = 'text', 'string' = 'text', 'numeric' = 'decimal', 'time' = 'decimal'),
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

data_categorical <- data %>%
  select_(.dots = (variables %>% filter(type == 'categorical'))$name) %>%
  gather("variable", "name") %>%
  unique() %>%
  arrange(
    variable,
    name
  ) %>%
  mutate(
    table = 'GCAT',
    code = '',
    missing = 0,
    label = ''
  ) %>%
  select(
    table,
    variable,
    name,
    code,
    missing,
    label
  )

list("Variables" = dictionary, "Categories" = data_categorical) %>%
  write.xlsx('output/datasets/gcat/GCAT-dictionary.xlsx')

data_opal <- data %>% mutate_at((dictionary %>% filter(valueType == 'boolean'))$name, funs(as.logical(.)))

data_boolean <- data %>%
  select_(.dots = (variables %>% filter(type == 'binary' | type == 'boolean'))$name) %>%
  gather("variable", "name") %>%
  unique() %>%
  arrange(
    variable,
    name
  ) %>%
  mutate(
    table = 'GCAT',
    code = '',
    missing = 0,
    label = ''
  ) %>%
  select(
    table,
    variable,
    name,
    code,
    missing,
    label
  )

data_boolean %>% write.xlsx('output/datasets/gcat/GCAT-bool.xlsx')

data_opal %>% write_csv('output/datasets/gcat/opal.csv', na = '')
