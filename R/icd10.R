library(icdcoder)
library(tidyverse)
require(data.table)
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

convert <- icd9 %>%
  mutate(
    icd9 = Codi
  ) %>%
  select(
    icd9,
    icd10,
    Text
  ) %>%
  mutate(desc = sapply(as.character(icd9), icd_explain, warn = FALSE)) %>%
  mutate(desc = ifelse(desc == "character(0)", "", desc)) %>%
  dplyr::rename(text = Text)

long <- read_csv('output/conditions/text/long.csv') %>%
  dplyr::rename(text = condition) %>%
  left_join(convert)

# cancer

cancer <- read_csv('output/check/cancer/data.csv') %>%
  gather(icd10, count, cancer_C00:cancer_C91) %>%
  filter(count > 0) %>%
  mutate(icd10 = gsub('cancer_', '', icd10)) %>%
  mutate(desc = sapply(as.character(icd10), icd_explain, warn = FALSE)) %>%
  mutate(text = "")

cancer$icd9 <- sapply(cancer$icd10, function(codi) {
  (convICD(codi, 'icd10')$icd9 %>% head(1)) %>% as.character
})

long <- long %>% rbind(cancer)

#long %>% as.data.frame %>% write_csv('output/check/icd10/long.csv')

### get cases

gcat <- read_csv('output/datasets/gcat/data.csv')
gcat_core <- read_csv('output/check/core/data.csv')
gcat_core_spain <- read_csv('output/datasets/heritability/data.csv')

long <- long %>%
  mutate(
    is_core = entity_id %in% gcat_core$entity_id,
    is_core_spain = entity_id %in% gcat_core_spain$entity_id
  )

build_table <- function(ds, category, category_text) {
  data.frame(
    'Category' = category,
    'Description' = category_text,
    'Text' = (ds %>% arrange(text))$text %>% unique %>% paste(collapse = ', '),
    'ICD-10' = (ds %>% arrange(icd10))$icd10 %>% unique %>% paste(collapse = ', '),
    'GCAT' = ds %>% nrow,
    'GCAT Core' = ds %>% filter(is_core) %>% nrow,
    'GCAT Core Spain' = ds %>% filter(is_core_spain) %>% nrow
  )
}

ds_table <- do.call("rbind", list(
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('F%02d', 60:69)) %>% build_table(category = 'F60-F69', category_text = 'Disorders of adult personality and behaviour'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('F%02d', 00:99)) %>% build_table(category = 'F00-F99', category_text = 'Mental and behavioural disorders'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('J%02d', 20:22)) %>% build_table(category = 'J20-J22', category_text = 'Other acute lower respiratory infections'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('J%02d', 00:99)) %>% build_table(category = 'J00-J99', category_text = 'Diseases of the respiratory system'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('D%02d', 50:53)) %>% build_table(category = 'D50-D53', category_text = 'Nutritional anaemias'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('D%02d', 50:89)) %>% build_table(category = 'D50-D89', category_text = 'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('G%02d', 60:65)) %>% build_table(category = 'G60-G64', category_text = 'Polyneuropathies and other disorders of the peripheral nervous system'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('G%02d', 00:99)) %>% build_table(category = 'G60-G99', category_text = 'Diseases of the nervous system'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('A%02d', 15:19)) %>% build_table(category = 'A15-A19', category_text = 'Tuberculosis'),
  long %>% filter(substring(long$icd10, 1, 3) %in% c(sprintf('A%02d', 00:99), sprintf('B%02d', 00:99))) %>% build_table(category = 'A00-B99', category_text = 'Certain infectious and parasitic diseases'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('F%02d', 30:39)) %>% build_table(category = 'F30-F39', category_text = 'Mood [affective] disorders'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('F%02d', 00:99)) %>% build_table(category = 'F00-F99', category_text = 'Mental and behavioural disorders'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('C%02d', 81:96)) %>% build_table(category = 'C81-C96', category_text = 'Malignant neoplasms of lymphoid, haematopoietic and related tissue'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('C%02d', 00:97)) %>% build_table(category = 'C00-C97', category_text = 'Malignant neoplasms'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('K%02d', 70:77)) %>% build_table(category = 'K70-K77', category_text = 'Diseases of liver'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('K%02d', 00:93)) %>% build_table(category = 'K00-K93', category_text = 'Diseases of the digestive system')
)) %>% 
  mutate(Category = as.character(Category)) %>%
  arrange(Category) %>%
  unique()

ds_table <- do.call("rbind", list(
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('K%02d', 90:93)) %>% build_table(category = 'K90-K93', category_text = 'Other diseases of the digestive system'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('T%02d', 66:78)) %>% build_table(category = 'T66-T78', category_text = 'Other and unspecified effects of external causes'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('M%02d', 60:63)) %>% build_table(category = 'M60-M63', category_text = 'Disorders of muscles'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('H%02d', 30:36)) %>% build_table(category = 'H30-H36', category_text = 'Disorders of choroid and retina'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('G%02d', 60:64)) %>% build_table(category = 'G60-G64', category_text = 'Polyneuropathies and other disorders of the peripheral nervous system'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('F%02d', 20:29)) %>% build_table(category = 'F20-F29', category_text = 'Schizophrenia, schizotypal and delusional disorders'),
  long %>% filter(substring(long$icd10, 1, 3) %in% sprintf('D%02d', 70:77)) %>% build_table(category = 'D70-D77', category_text = 'Other diseases of blood and blood-forming organs')
)) %>% 
  mutate(Category = as.character(Category)) %>%
  arrange(Category) %>%
  unique()


ds_table %>% write.xlsx2('output/datasets/gcat-core/cases.xlsx', row.names = FALSE)

long %>% 
  mutate(desc = as.character(desc)) %>%
  mutate(is_K90_F93 = substring(icd10, 1, 3) %in% sprintf('K%02d', 90:93)) %>%
  mutate(is_T66_F78 = substring(icd10, 1, 3) %in% sprintf('T%02d', 66:78)) %>%
  mutate(is_M60_J63 = substring(icd10, 1, 3) %in% sprintf('M%02d', 60:63)) %>%
  mutate(is_H30_J36 = substring(icd10, 1, 3) %in% sprintf('H%02d', 30:36)) %>%
  mutate(is_G60_D64 = substring(icd10, 1, 3) %in% sprintf('G%02d', 60:64)) %>%
  mutate(is_F20_D29 = substring(icd10, 1, 3) %in% sprintf('F%02d', 20:29)) %>%
  mutate(is_D70_G77 = substring(icd10, 1, 3) %in% sprintf('D%02d', 70:77)) %>%
  left_join(gcat_core %>% select(entity_id, core_sample_name)) %>%
  as.data.frame %>%
  write.csv('/home/labs/dnalab/xduran/Downloads/cases.csv', row.names = FALSE)

long %>% 
  mutate(desc = as.character(desc)) %>%
  mutate(is_F60_F99 = substring(icd10, 1, 3) %in% sprintf('F%02d', 60:69)) %>%
  mutate(is_F00_F69 = substring(icd10, 1, 3) %in% sprintf('F%02d', 00:69)) %>%
  mutate(is_J20_J22 = substring(icd10, 1, 3) %in% sprintf('J%02d', 20:22)) %>%
  mutate(is_J00_J99 = substring(icd10, 1, 3) %in% sprintf('J%02d', 00:99)) %>%
  mutate(is_D50_D53 = substring(icd10, 1, 3) %in% sprintf('D%02d', 50:53)) %>%
  mutate(is_D50_D89 = substring(icd10, 1, 3) %in% sprintf('D%02d', 50:89)) %>%
  mutate(is_G50_G89 = substring(icd10, 1, 3) %in% sprintf('G%02d', 60:65)) %>%
  mutate(is_G00_G99 = substring(icd10, 1, 3) %in% sprintf('G%02d', 00:99)) %>%
  mutate(is_A15_A19 = substring(icd10, 1, 3) %in% sprintf('A%02d', 15:19)) %>%
  mutate(is_A00_B99 = substring(icd10, 1, 3) %in% c(sprintf('A%02d', 00:99), sprintf('B%02d', 00:99))) %>%
  mutate(is_F00_F39 = substring(icd10, 1, 3) %in% sprintf('F%02d', 30:39)) %>%
  mutate(is_F00_F99 = substring(icd10, 1, 3) %in% sprintf('F%02d', 00:99)) %>%
  mutate(is_C81_C99 = substring(icd10, 1, 3) %in% sprintf('C%02d', 81:99)) %>%
  mutate(is_C00_C97 = substring(icd10, 1, 3) %in% sprintf('C%02d', 00:97)) %>%
  mutate(is_K00_K77 = substring(icd10, 1, 3) %in% sprintf('K%02d', 70:77)) %>%
  mutate(is_K00_K93 = substring(icd10, 1, 3) %in% sprintf('K%02d', 00:93)) %>%
  left_join(gcat_core %>% select(entity_id, core_sample_name)) %>%
  as.data.frame %>%
  write.csv('/home/labs/dnalab/xduran/Downloads/cases.csv', row.names = FALSE)


taula <- long %>%
  select(
    entity_id,
    icd9,
    icd10,
    count
  ) %>%
  unique %>%
  group_by(
    icd9
  ) %>%
  dplyr::summarise(
    freq = sum(count)
  ) %>%
  arrange(
    desc(freq)
  )

ds$screening_stool %>% table

gc <- read_csv('/home/labs/dnalab/share/lims/R/gcat-cohort/output/export/GCAT/data.csv')
gc$PROSTATA %>% table
