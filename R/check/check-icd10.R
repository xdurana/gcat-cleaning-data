library(icdcoder)
library(tidyverse)
library(data.table)
library(icd)
library(xlsx)

icd9 <- read_csv('inst/extdata/conditions/summary-cim9-updated.csv') %>%
  filter(
    Freq >= 1
  )

icd9$icd10 <- sapply((icd9)$Codi, function(codi) {
  (convICD(codi, 'icd9')$icd10 %>% head(1)) %>% as.character
})

icd9 %>%
  filter(
    is.na(Codi)
  ) %>%
  View()

icd10 <- icd9 %>%
  dplyr::rename(code = icd10) %>%
  dplyr::rename(participants = Freq) %>%
  dplyr::rename(text = Text) %>%
  select(code, participants, text) %>%
  arrange(desc(participants)) %>%
  filter(!is.na(code))

icd10 <- icd10 %>%
  left_join(icd_explain_table(icd10$code)) %>%
  unique()

long <- read_csv('output/conditions/text/long.csv') %>%
  dplyr::rename(text = condition) %>%
  left_join(icd10)

# cancer

cancer <- read_csv('output/check/cancer/data.csv') %>%
  gather(icd10, count, cancer_C00:cancer_C91) %>%
  filter(count > 0) %>%
  mutate(icd10 = gsub('cancer_', '', icd10)) %>%
  dplyr::rename(
    code = icd10
  ) %>%
  mutate(
    text = "CANCER",
    participants = count
  )

cancer <- cancer %>%
  left_join(icd_explain_table(cancer$code)) %>%
  unique()

# bind diseases and cancer

long <- long %>% rbind(cancer)

long %>%
  as.data.frame %>%
  select(
    -count,
    -participants
  ) %>%
  write_csv('output/check/icd10/long.csv')

# summary

icd10_summary <- long %>%
  group_by(code) %>%
  summarise(
    participants = sum(count),
    text = toString(unique(text))
  ) %>%
  filter(
    !is.na(code)
  ) %>%
  left_join(long %>% select(-entity_id, -participants, -text) %>% unique) %>%
  arrange(desc(participants)) %>%
  select(-count) %>%
  select(
    code,
    participants,
    short_desc,
    long_desc,
    three_digit,
    major,
    sub_chapter,
    chapter,
    text
  )

icd10_summary %>% write_csv('output/check/icd10/summary.csv')
