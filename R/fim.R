library(tidyverse)

heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))
variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))

vs <- c('entity_id', (variables %>% filter(type == 'binary') %>% select(name))$name)

ds <- heritability %>%
  select(one_of(vs))

ds <- ds %>% select(-starts_with("atc_"))

ds[is.na(ds)] <- 0

names <- ds %>% select(-entity_id) %>% colnames

lamp_variable(ds, 'icd9_code3_401')

lapply(names, function(variable) {
  lamp_variable(ds, variable)
})

lamp_variable <- function(ds, variable) {
  
  file.dat <- sprintf('output/fim/data_%s.csv', variable)
  file.val <- sprintf('output/fim/values_%s.csv', variable)
  file.log <- sprintf('output/fim/lamp_%s.log', variable)
  file.out <- sprintf('output/fim/lamp_%s.csv', variable)
  file.eut <- sprintf('output/fim/lamp_%s-purged.csv', variable)
  
  ds_dat <- ds %>%
    dplyr::rename(id=entity_id) %>%
    select(-c(matches(variable)))
  
  ds_val <- ds %>%
    mutate(id=entity_id) %>%
    select(id, matches(variable))
  
  ds_dat %>% write.table(file.dat, row.names = FALSE, quote = FALSE, sep = ',')
  ds_val %>% write.table(file.val, row.names = FALSE, quote = FALSE, sep = ',')
  
  system(sprintf("sed -i '1s/^/#/' %s", file.dat))
  system(sprintf("sed -i '1s/^/#/' %s", file.val))
  
  command.lamp <- sprintf('python /imppc/labs/dnalab/xduran/fim/bin/lamp/lamp.py -p fisher %s %s 0.05 -e %s > %s', file.dat, file.val, file.log, file.out)
  print(command.lamp)
  #system(command.lamp)

  command.prune <- sprintf('python /imppc/labs/dnalab/xduran/fim/bin/lamp/eliminate_comb.py %s > %s', file.out, file.eut)
  print(command.prune)
  #system(command.prune)
}
