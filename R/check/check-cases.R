library(icdcoder)
library(tidyverse)
library(data.table)
library(icd)
library(xlsx)

long <- read_csv('output/check/icd10/long.csv')

gcat <- read_csv('output/datasets/gcat/data.csv')
gcat_core <- read_csv('output/check/core/data.csv')
gcat_core_spain <- read_csv('output/datasets/heritability/data.csv')

long <- long %>%
  mutate(
    is_core = entity_id %in% gcat_core$entity_id,
    is_core_spain = entity_id %in% gcat_core_spain$entity_id
  )

a <- long %>%
  filter(is_core_spain) %>%
  group_by(code, short_desc) %>%
  summarize(count = n())

build_table <- function(ds, category, category_text) {
  data.frame(
    'Category' = category,
    'Description' = category_text,
    'Text' = (ds %>% arrange(text))$text %>% unique %>% paste(collapse = ', '),
    'ICD-10' = (ds %>% arrange(code))$code %>% unique %>% paste(collapse = ', '),
    'GCAT' = ds %>% nrow,
    'GCAT Core' = ds %>% filter(is_core) %>% nrow,
    'GCAT Core Spain' = ds %>% filter(is_core_spain) %>% nrow
  )
}

ds_table <- do.call("rbind", list(
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('F%02d', 60:69)) %>% build_table(category = 'F60-F69', category_text = 'Disorders of adult personality and behaviour'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('F%02d', 00:99)) %>% build_table(category = 'F00-F99', category_text = 'Mental and behavioural disorders'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('J%02d', 20:22)) %>% build_table(category = 'J20-J22', category_text = 'Other acute lower respiratory infections'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('J%02d', 00:99)) %>% build_table(category = 'J00-J99', category_text = 'Diseases of the respiratory system'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('D%02d', 50:53)) %>% build_table(category = 'D50-D53', category_text = 'Nutritional anaemias'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('D%02d', 50:89)) %>% build_table(category = 'D50-D89', category_text = 'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('G%02d', 60:65)) %>% build_table(category = 'G60-G64', category_text = 'Polyneuropathies and other disorders of the peripheral nervous system'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('G%02d', 00:99)) %>% build_table(category = 'G60-G99', category_text = 'Diseases of the nervous system'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('A%02d', 15:19)) %>% build_table(category = 'A15-A19', category_text = 'Tuberculosis'),
  long %>% filter(substring(long$code, 1, 3) %in% c(sprintf('A%02d', 00:99), sprintf('B%02d', 00:99))) %>% build_table(category = 'A00-B99', category_text = 'Certain infectious and parasitic diseases'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('F%02d', 30:39)) %>% build_table(category = 'F30-F39', category_text = 'Mood [affective] disorders'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('C%02d', 81:96)) %>% build_table(category = 'C81-C96', category_text = 'Malignant neoplasms of lymphoid, haematopoietic and related tissue'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('C%02d', 00:97)) %>% build_table(category = 'C00-C97', category_text = 'Malignant neoplasms'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('K%02d', 70:77)) %>% build_table(category = 'K70-K77', category_text = 'Diseases of liver'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('K%02d', 00:93)) %>% build_table(category = 'K00-K93', category_text = 'Diseases of the digestive system')
)) %>% 
  mutate(Category = as.character(Category)) %>%
  arrange(Category) %>%
  unique()

ds_table <- do.call("rbind", list(
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('K%02d', 90:93)) %>% build_table(category = 'K90-K93', category_text = 'Other diseases of the digestive system'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('T%02d', 66:78)) %>% build_table(category = 'T66-T78', category_text = 'Other and unspecified effects of external causes'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('M%02d', 60:63)) %>% build_table(category = 'M60-M63', category_text = 'Disorders of muscles'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('H%02d', 30:36)) %>% build_table(category = 'H30-H36', category_text = 'Disorders of choroid and retina'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('H%02d', 00:59)) %>% build_table(category = 'H00-H59', category_text = 'Diseases of the eye and adnexa'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('G%02d', 60:64)) %>% build_table(category = 'G60-G64', category_text = 'Polyneuropathies and other disorders of the peripheral nervous system'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('G%02d', 00:99)) %>% build_table(category = 'G00-G99', category_text = 'Diseases of the nervous system'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('F%02d', 20:29)) %>% build_table(category = 'F20-F29', category_text = 'Schizophrenia, schizotypal and delusional disorders'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('D%02d', 70:77)) %>% build_table(category = 'D70-D77', category_text = 'Other diseases of blood and blood-forming organs'),
  long %>% filter(substring(long$code, 1, 3) %in% sprintf('D%02d', 50:89)) %>% build_table(category = 'D50-D89', category_text = 'Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism')
)) %>% 
  mutate(Category = as.character(Category)) %>%
  arrange(Category) %>%
  unique()


ds_table %>% write.xlsx2('output/check/cases/summary.xlsx', row.names = FALSE)

long %>% 
  select(entity_id, code, is_core, is_core_spain) %>%
  mutate(is_K90_K93 = substring(code, 1, 3) %in% sprintf('K%02d', 90:93)) %>%
  mutate(is_T66_T78 = substring(code, 1, 3) %in% sprintf('T%02d', 66:78)) %>%
  mutate(is_M60_M63 = substring(code, 1, 3) %in% sprintf('M%02d', 60:63)) %>%
  mutate(is_H30_H36 = substring(code, 1, 3) %in% sprintf('H%02d', 30:36)) %>%
  mutate(is_H00_H59 = substring(code, 1, 3) %in% sprintf('H%02d', 00:59)) %>%
  mutate(is_G60_G64 = substring(code, 1, 3) %in% sprintf('G%02d', 60:64)) %>%
  mutate(is_G00_G99 = substring(code, 1, 3) %in% sprintf('G%02d', 00:99)) %>%
  mutate(is_F20_F29 = substring(code, 1, 3) %in% sprintf('F%02d', 20:29)) %>%
  mutate(is_D70_D77 = substring(code, 1, 3) %in% sprintf('D%02d', 70:77)) %>%
  mutate(is_D50_D89 = substring(code, 1, 3) %in% sprintf('D%02d', 50:89)) %>%
  left_join(gcat_core %>% select(entity_id, core_sample_name)) %>%
  as.data.frame %>%
  write_csv('output/check/cases/cases.csv')

long %>% 
  select(entity_id, code, is_core, is_core_spain) %>%
  mutate(is_F60_F99 = substring(code, 1, 3) %in% sprintf('F%02d', 60:69)) %>%
  mutate(is_F00_F69 = substring(code, 1, 3) %in% sprintf('F%02d', 00:69)) %>%
  mutate(is_J20_J22 = substring(code, 1, 3) %in% sprintf('J%02d', 20:22)) %>%
  mutate(is_J00_J99 = substring(code, 1, 3) %in% sprintf('J%02d', 00:99)) %>%
  mutate(is_D50_D53 = substring(code, 1, 3) %in% sprintf('D%02d', 50:53)) %>%
  mutate(is_D50_D89 = substring(code, 1, 3) %in% sprintf('D%02d', 50:89)) %>%
  mutate(is_G50_G89 = substring(code, 1, 3) %in% sprintf('G%02d', 60:65)) %>%
  mutate(is_G00_G99 = substring(code, 1, 3) %in% sprintf('G%02d', 00:99)) %>%
  mutate(is_A15_A19 = substring(code, 1, 3) %in% sprintf('A%02d', 15:19)) %>%
  mutate(is_A00_B99 = substring(code, 1, 3) %in% c(sprintf('A%02d', 00:99), sprintf('B%02d', 00:99))) %>%
  mutate(is_F00_F39 = substring(code, 1, 3) %in% sprintf('F%02d', 30:39)) %>%
  mutate(is_F00_F99 = substring(code, 1, 3) %in% sprintf('F%02d', 00:99)) %>%
  mutate(is_C81_C99 = substring(code, 1, 3) %in% sprintf('C%02d', 81:99)) %>%
  mutate(is_C00_C97 = substring(code, 1, 3) %in% sprintf('C%02d', 00:97)) %>%
  mutate(is_K00_K77 = substring(code, 1, 3) %in% sprintf('K%02d', 70:77)) %>%
  mutate(is_K00_K93 = substring(code, 1, 3) %in% sprintf('K%02d', 00:93)) %>%
  left_join(gcat_core %>% select(entity_id, core_sample_name)) %>%
  as.data.frame %>%
  write_csv('output/check/cases/cases.csv')
