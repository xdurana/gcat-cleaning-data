library(tidyverse)

directory <- 'bmi'

bmi <- function(height, weight) {
  round(weight/(height/100)^2, 2)
}

height <- read_csv('output/check/height/data.csv')
weight <- read_csv('output/check/weight/data.csv')

ds <- height %>% merge(weight)

MIN_HEIGHT = 100
MAX_HEIGHT = 210

MIN_WEIGHT = 40
MAX_WEIGHT = 160

MIN_BMI = 15
MAX_BMI = 55

ds <- ds %>%
  mutate(
    bmi = ifelse(
      FALSE & height==weight | height < MIN_HEIGHT | weight < MIN_WEIGHT | height > MAX_HEIGHT | weight > MAX_WEIGHT,
      NA,
      bmi(height, weight)
    )
  ) %>%
  mutate(
    bmi = ifelse(
      bmi > MAX_BMI | bmi < MIN_BMI,
      NA,
      bmi
    )
  ) %>%
  mutate(
    height = ifelse(
      is.na(bmi),
      NA,
      height
    ),
    weight = ifelse(
      is.na(bmi),
      NA,
      weight
    )
  ) %>%
  mutate(
    bmi_who = cut(
      bmi,
      breaks = c(-Inf, 18.5, 25, 30, Inf),
      labels = c("underweight", "normal weight", "overweight", "obesity"),
      right = FALSE
    )
  ) %>%
  mutate(
    bmi_who_obesity = ifelse(bmi_who %in% c('obesity'), 1, 0)
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))

missings_plot(ds, directory)
pair_plot(ds %>% select(height, weight), directory, 'bmi')
histogram_plot(ds, directory, 'bmi')
