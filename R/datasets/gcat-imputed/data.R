library(tidyverse)
library(lattice)
library(mice)
library(VIM)

whr <- function(waist, hip) {
  round(waist/hip, 2)
}

bmi <- function(height, weight) {
  round(weight/(height/100)^2, 2)
}

all <- read_csv('output/datasets/gcat/data.csv')
age <- read_csv('output/check/age/errors.csv')
gender <- read_csv('output/check/gender/errors.csv')
variables <- read_csv('output/datasets/gcat/variables.csv')

all_impute <-
  all %>%
  select(
    entity_id,
    age,
    gender,
    height,
    weight,
    waist,
    hip,
    systolic_blood_pressure,
    diastolic_blood_pressure,
    heart_rate
  )

# corregir edat

all_impute_age <-
  all_impute %>%
  filter(
    is.na(age)
  ) %>%
  select(-age) %>%
  left_join(age) %>%
  mutate(
    age = EDAD_ANOS
  ) %>%
  select(
    entity_id,
    age,
    gender,
    height,
    weight,
    waist,
    hip,
    systolic_blood_pressure,
    diastolic_blood_pressure,
    heart_rate
  )

all_impute <-
  all_impute %>%
  filter(!is.na(age)) %>%
  rbind(all_impute_age)

# corregir gènere

all_impute_gender <-
  all_impute %>%
  filter(
    is.na(gender)
  ) %>%
  select(-gender) %>%
  left_join(gender) %>%
  mutate(
    gender = ifelse(Admin.Participant.gender == 'FEMALE', 1, 0)
  ) %>%
  select(
    entity_id,
    age,
    gender,
    height,
    weight,
    waist,
    hip,
    systolic_blood_pressure,
    diastolic_blood_pressure,
    heart_rate
  )

all_impute <-
  all_impute %>%
  filter(
    !is.na(gender)
  ) %>%
  rbind(all_impute_gender)

# imputar

ds_impute <-
  all_impute %>%
  select(
    entity_id,
    gender,
    height,
    weight,
    waist,
    hip,
    systolic_blood_pressure,
    diastolic_blood_pressure,
    heart_rate
  )

aggr_plot <- aggr(
  ds_impute,
  col=c('navyblue','red'), 
  numbers=TRUE, 
  sortVars=TRUE, 
  labels=names(ds_impute),
  cex.axis=.7,
  gap=0,
  ylab=c("Histogram of missing data","Pattern")
)

# homes

ds_impute_men <- ds_impute %>% filter(gender == 0)

data.imp <- mice(ds_impute_men, seed = 1233, m = 5)
summary(data.imp)
ds_impute_men <- complete(data.imp)

# Inspecting the distribution of original and imputed data
png(file.path('output/datasets/gcat-imputed', 'xyplot_men.png'), width = 800, height = 600)
xyplot(data.imp, height ~ weight + hip + waist, pch=18, cex=1)
dev.off()

# The density of the imputed data for each imputed dataset
png(file.path('output/datasets/gcat-imputed', 'density_men.png'), width = 800, height = 600)
densityplot(data.imp)
dev.off()

# dones

ds_impute_women <- ds_impute %>% filter(gender == 1)

data.imp <- mice(ds_impute_women, seed = 1233, m = 5)
summary(data.imp)
ds_impute_women <- complete(data.imp)

# Inspecting the distribution of original and imputed data
png(file.path('output/datasets/gcat-imputed', 'xyplot_women.png'), width = 800, height = 600)
xyplot(data.imp, height ~ weight + hip + waist, pch=18, cex=1)
dev.off()

# The density of the imputed data for each imputed dataset
png(file.path('output/datasets/gcat-imputed', 'density_women.png'), width = 800, height = 600)
densityplot(data.imp)
dev.off()

ds_impute <-
  rbind(ds_impute_men, ds_impute_women) %>%
  mutate(
    height_c = height,
    weight_c = weight,
    waist_c = waist,
    hip_c = hip,
    systolic_blood_pressure_c = systolic_blood_pressure,
    diastolic_blood_pressure_c = diastolic_blood_pressure,
    heart_rate_c = heart_rate
  ) %>%
  select(
    entity_id,
    height_c,
    weight_c,
    waist_c,
    hip_c,
    systolic_blood_pressure_c,
    diastolic_blood_pressure_c,
    heart_rate_c
  )
  
all <-
  all %>%
  select(
    -height,
    -weight,
    -waist,
    -hip,
    -systolic_blood_pressure,
    -diastolic_blood_pressure,
    -heart_rate,
    -height_c,
    -weight_c,
    -waist_c,
    -hip_c,
    -systolic_blood_pressure_c,
    -diastolic_blood_pressure_c,
    -heart_rate_c,
    -age,
    -gender
  ) %>%
  left_join(
    all_impute %>% left_join(ds_impute)
  ) %>%
  mutate(
    whr=whr(waist_c, hip_c)
  ) %>%
  mutate(
    whr_who = ifelse(
      gender == 0,
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
  ) %>%
  mutate(
    bmi = bmi(height_c, weight_c)
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
    bmi_who_obesity = ifelse(bmi_who %in% c('obesity'), 1, 0)
  ) %>%
  unique()

all %>% write_csv('output/datasets/gcat-imputed/data.csv')
variables %>% write_csv('output/datasets/gcat-imputed/variables.csv')
