library(tidyverse)

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
  )

ds %>% write_csv('output/check/bmi/data.csv')
