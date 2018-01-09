gcat <- read_csv('output/datasets/gcat/data.csv')
gcat_core <- read_csv('output/check/core/data.csv')
gcat_core_spain <- read_csv('output/datasets/heritability/data.csv')

long <- read_csv('output/conditions/text/long.csv')

filterByComorbidity <- function(ds, max) {
  comorbidities <- long %>%
    filter(
      entity_id %in% ds$entity_id
    ) %>%
    unique() %>%
    select(
      -condition
    ) %>%
    group_by(entity_id) %>%
    dplyr::summarise(n = sum(count)) %>%
    filter(
      n >= 2
    )
  comorbidities
}

comorbidities_gcat <- gcat %>% filterByComorbidity(2)
comorbidities_gcat_core <- gcat_core %>% filterByComorbidity(2)
comorbidities_gcat_core_spain <- gcat_core_spain %>% filterByComorbidity(2)
