library(tidyverse)
library(icd)

long <- read_csv('output/check/icd9_code3/long.csv')
heritability <- read_csv('output/datasets/heritability/data.csv') %>%
  select(entity_id, gender) %>%
  left_join(long)

tuber <- heritability %>%
  mutate(codi = gsub('icd9_code3_', '', codi))





icd10 <-  
  heritability %>%
  filter(
    codi %in% c(
      'icd9_code3_356',
      'icd9_code3_357',
      'icd9_code3_280',
      'icd9_code3_281',
      'icd9_code3_466',
      'icd9_code3_300',
      'icd9_code3_301',
      'icd9_code3_302',
      'icd9_code3_312',
      'icd9_code3_V40'
    )
  )

tuberculosi <-
  heritability %>%
  filter(
    codi %in% c(
      'icd9_code3_011',
      'icd9_code3_012',
      'icd9_code3_013',
      'icd9_code3_014',
      'icd9_code3_015',
      'icd9_code3_016',
      'icd9_code3_017',
      'icd9_code3_018'
    )
  )

mood <-
  heritability %>%
  filter(
    codi %in% c(
      'icd9_code3_296',
      'icd9_code3_300',
      'icd9_code3_301'
    )
  )

neoplasms <-
  heritability %>%
  filter(
    codi %in% c(
      'icd9_code3_199',
      'icd9_code3_200',
      'icd9_code3_201',
      'icd9_code3_202',
      'icd9_code3_203',
      'icd9_code3_204',
      'icd9_code3_205',
      'icd9_code3_206',
      'icd9_code3_207',
      'icd9_code3_208',
      'icd9_code3_238',
      'icd9_code3_273',
      'icd9_code3_277'
    )
  )

(neoplasms %>% rbind(tuberculosi) %>% select(-codi) %>% unique)$gender %>% table
(mood %>% rbind(tuberculosi) %>% select(-codi) %>% unique)$gender %>% table

binomi_1 <- tuberculosi[tuberculosi$entity_id %in% mood$entity_id,]
binomi_2 <- tuberculosi[tuberculosi$entity_id %in% neoplasms$entity_id,]

table(heritability$codi)


tuber <- heritability %>%
  filter(entity_id %in% tuberculosi$entity_id) %>%
  mutate(codi = gsub('icd9_code3_', '', codi)) %>%
  mutate(desc = sapply(as.character(codi), icd_explain))

tuber %>%
  write_csv('output/tuberculosis.csv')
