library(tidyverse)
library(VIM)

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
    ),
    bmi_deep = cut(
      bmi,
      breaks = c(0, 16.00, 17.00, 18.50, 25.00, 30.00, 35.00, 40.00, Inf),
      labels = c("Severe thinness", "Moderate thinness", "Mild thinness", "Normal range", "Pre-obese", "Obese class I", "Obese class II", "Obese class III"),
      right = FALSE
    )
  ) %>%
  mutate(
    bmi_who_obesity = ifelse(bmi_who %in% c('obesity'), 1, 0),
    height_c = NA,
    weight_c = NA
  )

ds %>% write_csv(sprintf('output/check/%s/data.csv', directory))

missings_plot(ds, directory)
pair_plot(ds %>% select(height, weight), directory, 'bmi')
histogram_plot(ds, directory, 'bmi')