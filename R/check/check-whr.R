library(tidyverse)

directory <- 'whr'

whr <- function(waist, hip) {
  round(waist/hip, 2)
}

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'

ds <- read_csv(file.path(export_dir, 'CinturaCadera/data.csv')) %>%
  select(
    entity_id,
    `1_Cintura`,
    `2_Cintura`,
    `1_Cadera`,
    `2_Cadera`
  ) %>%
  mutate(
    diff_cintura=abs(`1_Cintura`-`2_Cintura`),
    diff_cadera=abs(`1_Cadera`-`2_Cadera`)
  ) %>%
  mutate(
    waist=ifelse(diff_cintura > 1, NA, round((`1_Cintura` + `2_Cintura`)/2, digits=2)),
    hip=ifelse(diff_cadera > 1, NA, round((`1_Cadera` + `2_Cadera`)/2, digits=2)),
    whr=whr(waist, hip)
  ) %>%
  left_join(
    gcat %>% select(entity_id, SEXO)
  ) %>%
  mutate(
    whr_who = ifelse(
      SEXO == 1,
      #men
      cut(
        whr,
        breaks = c(-Inf, 0.9, 1, Inf),
        labels = c("normal weight", "overweight", "obesity"),
        right = FALSE
      ),
      #women
      cut(
        whr,
        breaks = c(-Inf,0.8, 0.85, Inf),
        labels = c("normal weight", "overweight", "obesity"),
        right = FALSE
      )
    )
  ) %>%
  mutate(
    whr_who_obesity = ifelse(whr_who %in% c(3), 1, 0)
  )

ds <- ds %>%
  select(
    entity_id,
    waist,
    hip,
    whr,
    whr_who,
    whr_who_obesity
  )
  
ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))

missings_plot(ds, directory)
pair_plot(ds %>% select(waist, hip), directory, 'whr')
histogram_plot(ds, directory, 'whr')
