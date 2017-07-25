library(tidyverse)
library(xlsx)

all <- participants %>%
  left_join(gcat) %>%
  select(
    entity_id,
    Admin.Participant.gender,
    SEXO
  ) %>%
  mutate(
    gender = ifelse(Admin.Participant.gender == 'MALE' & (SEXO == 1 | is.na(SEXO)), 0, ifelse(Admin.Participant.gender == 'FEMALE' & (SEXO == 2 |  is.na(SEXO)), 1, NA))
  )

all %>% select(entity_id, gender) %>% write_csv('output/check/gender/data.csv')
all %>% filter(is.na(gender)) %>% select(-gender) %>%  write_csv('output/check/gender/errors.csv')
