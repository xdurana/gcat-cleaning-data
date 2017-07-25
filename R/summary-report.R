library(tidyverse)
library(ggplot2)
library(xtable)
library(knitr)
library(psych)
library(plyr)
library(reporttools)

source('R/summary-report-recruitment.R')

export_dir <- '/home/labs/dnalab/share/lims/R/gcat-cohort/output/export'
gcat <- read_csv(file.path(export_dir, 'QUESTIONARI/data.csv'))
participants <- read_csv(file.path(export_dir, 'Participants/data.csv'))

variables <- read_csv(file.path('output/check/heritability', 'variables.csv'))
heritability <- read_csv(file.path('output/check/heritability', 'data.csv'))

## Continuous variables

continuous_variables <- variables %>% filter(type == 'numeric') %>% select(name) %>% collect %>% .[['name']]
continuous <- heritability %>% select(one_of(continuous_variables))

desc <- describe(continuous, skew = FALSE)
desc <- desc %>%
  transform(
    missings = nrow(heritability) - n
  ) %>%
  select(
    -vars,
    -n,
    -range
  )

## Binary variables

variables <- variables %>%
  mutate(
    missings = sapply(variables$name, function(variable) {
      sum(is.na(heritability[variable]))
    }),
    v0 = sapply(variables$name, function(variable) {
      table(heritability[variable])[1]
    }),
    v1 = sapply(variables$name, function(variable) {
      table(heritability[variable])[2]
    })
  ) %>%
  mutate(
    p0 = round(v0*100/nrow(participants), digits = 2),
    p1 = round(v1*100/nrow(participants), digits = 2)
  )

binary <- variables %>%
  filter(type == 'binary') %>%
  filter(!(category %in% c('Medication', 'Conditions'))) %>%
  select(
    name,
    description,
    missings,
    v0,
    v1,
    p0,
    p1
  )

## Categorical variables

categorical_variables <- variables %>% filter(type == 'categorical') %>% select(name) %>% collect %>% .[['name']]
categorical <- heritability %>% select(one_of(categorical_variables))

## Conditions

conditions <- read_csv('output/conditions/summary.csv') %>%
  mutate(Descr_codi = substring(Descr_codi, 1, 80))

## Describe variables not in heritability

ds_gcat <- gcat %>%
  select(
    entity_id,
    
    #Secció A
    ETNIA_PARTICIPANTE,
    ETNIA_PADRE,
    ETNIA_MADRE,
    INGRESOS,
    SANIDAD,
    
    #Secció C
    ACTIVIDAD_FISICA_LIBRE_CAMINAR_DIAS,
    ACTIVIDAD_FISICA_LIBRE_CAMINAR_MINUTOS,
    ACTIVIDAD_FISICA_LIBRE_DEPORTE_REGULAR,
    ACTIVIDAD_FISICA_LIBRE_DEPORTE_ANUALIDAD,
    ACTIVIDAD_HOGAR_LABORABLE_ACTIVIDAD_SEDENTARIA_HORAS,
    ACTIVIDAD_HOGAR_NO_LABORABLE_ACTIVIDAD_SEDENTARIA_HORAS,
    ACTIVIDAD_DIARIO_ESCALERAS,
    
    #Secció E
    FUMAR_100,
    FUMAR_ACTUALIDAD,
    
    #Secció F
    ALCOHOL_ACTUAL,
    ALCOHOL_ACTUAL_COMIDA,
    
    #Secció G

    PREDIMED_ACEITE_GRASA,
    ALIMENTOS_FRUTAS,
    ALIMENTOS_CARNE_BLANCA,
    ALIMENTOS_CARNE_ROJA,
    ALIMENTOS_PESCADOS,
    ALIMENTOS_PASTA,
    ALIMENTOS_PATATA,
    ALIMENTOS_PAN,
    ALIMENTOS_VERDURA,
    ALIMENTOS_LEGUMBRE,
    ALIMENTOS_EMBUTIDOS,
    ALIMENTOS_LACTEOS,
    ALIMENTOS_HUEVOS,
    ALIMENTOS_DULCES,
    
    #Secció H
    
    ESTADO_DE_SALUD,
    ESTADO_PESO_MAXIMO,
    ESTADO_PESO_MINIMO,
    SALUD_MENTAL_FELIZ,
    SALUD_MENTAL_NERVIOSO,
    SALUD_MENTAL_BAJAMORAL,
    SALUD_MENTAL_CALMA,
    SALUD_MENTAL_TRISTE,
    SALUD_MENTAL_INCAPACIDAD,    
    
    #Secció I
    
    SALUD_MUJER_MENSTRUACION_ACTUAL,
    SALUD_MUJER_MENSTRUACION_ULTIMA_CAUSA,
    SALUD_MUJER_MENSTRUACION_ACTUAL_CICLO,
    SALUD_MUJER_EMBARAZOS,
    SALUD_MUJER_HIJOS,
    SALUD_MUJER_ESTADO_DE_SALUD_PERFIL,
    
    #Secció J
    
    SALUD_HOMBRES_PROSTATA,
    SALUD_HOMBRES_PROSTATA_MEDICACION,
    SALUD_HOMBRE_BARBA,
    SALUD_HOMBRE_VOZ,
    SALUD_HOMBRE_CALVICIE_20,
    SALUD_HOMBRE_CALVICIE_30,
    SALUD_HOMBRE_CALVICIE_40,
    SALUD_HOMBRES_ESTADO_DE_SALUD_PERFIL,
    SALUD_HOMBRES_HIJOS,

    #Secció K
    
    ADOPTADO,
    PARTO_MULTIPLE,
    
    #Secció L
    
    FAMILIA_HERMANOS
  )



## Describe

ds <- heritability %>%
  left_join(ds_gcat) %>%
  mutate(
    gender = revalue(gender %>% as.factor, c("0" = 'home', "1" = 'dona')),
    ethnicity_mother = revalue(ETNIA_MADRE %>% as.factor, c("1" = "blanca o caucàssica", "2" = "negra", "3" = "asiàtica", "4" = "gitana", "5" = "magribina", "6" = "hispana o llatina", "7" = "altres")),
    ethnicity_father = revalue(ETNIA_PADRE %>% as.factor, c("1" = "blanca o caucàssica", "2" = "negra", "3" = "asiàtica", "4" = "gitana", "5" = "magribina", "6" = "hispana o llatina", "7" = "altres")),
    ethnicity = revalue(ETNIA_PARTICIPANTE %>% as.factor, c("1" = "blanca o caucàssica", "2" = "negra", "3" = "asiàtica", "4" = "gitana", "5" = "magribina", "6" = "hispana o llatina", "7" = "altres")),
    civil_status = revalue(civil_status %>% as.factor, c("1" = "solter", "2" = "casat", "3" = "vidu", "4" = "divorciat/separat", "5" = "parella de fet")),
    income = revalue(INGRESOS %>% as.factor, c("1" = "< 18.000", "2" = "18.000 a 30.999", "3" = "31.000 a 51.999", "4" = "52.000 a 100.000", "5" = "+100.000")),
    health_usage = revalue(SANIDAD %>% as.factor, c("1" = "seguretat social", "2" = "mútua privada", "3" = "altre privat", "4" = "públic i privat")),
    education = revalue(education %>% as.factor, c("1" = "sense estudis", "2" = "estudis primaris o equivalents", "3" = "educació secundària", "4" = "ensenyament professional de segon grau", "5" = "educació secundària", "6" = "ensenyaments professionals superiors", "7" = "estudis universitaris o equivalents")),
    working_status = revalue(working_status %>% as.factor, c("1" = "treballant remuneradament o autònom", "2" = "jubilat", "3" = "em faig càrrec de la casa/familia", "4" = "incapacitat laboral", "5" = "en situació d'atur", "6" = "voluntari o treballant sense sou", "7" = "estudiant", "8" = "cap de les anteriors")),
    walking_days_a_week = ACTIVIDAD_FISICA_LIBRE_CAMINAR_DIAS,
    walking_minutes_a_day = ACTIVIDAD_FISICA_LIBRE_CAMINAR_MINUTOS*5,
    sport = revalue(ACTIVIDAD_FISICA_LIBRE_DEPORTE_REGULAR %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    sport_last_year = revalue(ACTIVIDAD_FISICA_LIBRE_DEPORTE_ANUALIDAD %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    weekly_domestic_labour = ACTIVIDAD_HOGAR_LABORABLE_ACTIVIDAD_SEDENTARIA_HORAS*5 + ACTIVIDAD_HOGAR_NO_LABORABLE_ACTIVIDAD_SEDENTARIA_HORAS*2,
    stairs = revalue(ACTIVIDAD_DIARIO_ESCALERAS %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    hair_color = revalue(hair_color %>% as.factor, c("1" = "negre", "2" = "castany fosc", "3" = "castany clar", "4" = "ros", "5" = "pèl-roig", "0" = NA)),
    skin_color = revalue(skin_color %>% as.factor, c("1" = "negra", "2" = "morena", "3" = "mitjana", "4" = "blanca", "5" = "molt blanca", "0" = NA)),
    eye_color = revalue(eye_color %>% as.factor, c("1" = "negres o marrons foscos", "2" = "marrons", "3" = "marrons clars o color mel", "4" = "blaus, grisos o verds", "5" = "blaus, grisos o verds clars", "0" = NA)),
    freckling = revalue(freckling %>% as.factor, c("1" = "abundants", "2" = "algunes", "3" = "poques", "4" = "ocasionals", "5" = "no", "0" = NA)),
    smoking_100 = revalue(FUMAR_100 %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    smoking_now = revalue(FUMAR_ACTUALIDAD %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    smoking_intensity = revalue(smoking_intensity %>% as.factor, c("1" = "never", "2" = "current, occasionally", "3" = "current, unknown", "4" = "current, <= 15 cig/day", "5" = "current,16-25 cig/day", "6" = "current, 26+ cig/day", "7" = "former, unknown", "8" = "former, quit <= 10 yrs", "9" = "Former,quit 11-20 yrs", "10" = "Former,quit 20+ yrs", "0" = NA)),
    smoking_habit = revalue(smoking_habit %>% as.factor, c("1" = "never", "2" = "ex-smoker", "3" = "smoker", "0" = NA)),
    alcohol_use = revalue(ALCOHOL_ACTUAL %>% as.factor, c("1"="Mai o menys d'un cop al mes", "2"="1 cop al mes","3"="2 o 3 cops al mes","4=1 cop a la setmana","5"="2 o 3 cops a la setmana", "6"="De 4 a 6 cops a la setmana", "7"="1 cop al dia", "8"="2 cops o més al dia", "0" = NA)),
    alcohol_meals = revalue(ALCOHOL_ACTUAL_COMIDA %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    cook_with_oil = revalue(PREDIMED_ACEITE_GRASA %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    health_status = revalue(ESTADO_DE_SALUD %>% as.factor, c("1" = "molt bo", "2" = "bo", "3" = "regular", "4" = "dolent", "5" = "molt dolent", "0" = NA)),
    weight_max = ESTADO_PESO_MAXIMO,
    weight_min = ESTADO_PESO_MINIMO,
    menopause_cause = revalue(SALUD_MUJER_MENSTRUACION_ULTIMA_CAUSA %>% as.factor, c("1" = "menopausa natural", "2" = "extirpació d'un ovari", "3" = "extirpació dels dos ovaris", "4" = "extirpació de l'úter o la matriu", "5" = "extirpació de l'úter o la matriu i els ovaris", "6" = "tractament hormonal", "7" = "quimioterapia", "0" = NA)),
    menstruation = revalue(SALUD_MUJER_MENSTRUACION_ACTUAL %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    menstruation_duration = revalue(SALUD_MUJER_MENSTRUACION_ACTUAL_CICLO %>% as.factor, c("1" = "menys de 20", "2" = "de 20 a 24", "3" = "de 25 a 28","4" = "de 29 a 32","5" = "de 33 a 36","6" = "de 37 a 40","7" = "més de 40")),
    pregnancy = ifelse(SALUD_MUJER_EMBARAZOS == 0, NA, ifelse(SALUD_MUJER_EMBARAZOS == 16, 0, SALUD_MUJER_EMBARAZOS)),
    oc_use = revalue(oc_use %>% as.factor, c("1" = "sí", "0" = "no")),
    hrt_use = revalue(hrt_use %>% as.factor, c("1" = "sí", "0" = "no")),
    women_children = SALUD_MUJER_HIJOS,
    women_profile = SALUD_MUJER_ESTADO_DE_SALUD_PERFIL,
    prostate = revalue(SALUD_HOMBRES_PROSTATA %>% as.factor, c("1" = "prostatitis o infecció de pròstata", "2" = "hiperplàsia benigna de pròstata", "3" = "ambdues: prostatitis i hiperplàsia", "4" = "No m'han diagnosticat de cap d'aquestes malalties", "0" = NA)),
    prostate_medication = revalue(SALUD_HOMBRES_PROSTATA_MEDICACION %>% as.factor, c("1" = "sí", "2" = "no", "0" = NA)),
    beard_change = SALUD_HOMBRE_BARBA,
    voice_change = SALUD_HOMBRE_VOZ,
    bald_20 = ifelse(SALUD_HOMBRE_CALVICIE_20 == 0, NA, SALUD_HOMBRE_CALVICIE_20),
    bald_30 = ifelse(SALUD_HOMBRE_CALVICIE_30 == 0, NA, SALUD_HOMBRE_CALVICIE_30),
    bald_40 = ifelse(SALUD_HOMBRE_CALVICIE_40 == 0, NA, SALUD_HOMBRE_CALVICIE_40),
    men_profile = SALUD_HOMBRES_ESTADO_DE_SALUD_PERFIL,
    men_children = SALUD_HOMBRES_HIJOS,
    adopted = revalue(ADOPTADO %>% as.factor, c("SI" = "sí", "NO" = "no", "NS_NC" = NA)),
    multiple_delivery = revalue(PARTO_MULTIPLE %>% as.factor, c("SI" = "sí", "NO" = "no", "NS_NC" = NA)),
    brothers = ifelse(FAMILIA_HERMANOS == "NS_NC", NA, as.integer(FAMILIA_HERMANOS)),
    alimentos_frutas = revalue(ALIMENTOS_FRUTAS %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_carne_blanca = revalue(ALIMENTOS_CARNE_BLANCA %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_carne_roja = revalue(ALIMENTOS_CARNE_ROJA %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_pescados = revalue(ALIMENTOS_PESCADOS %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_pasta = revalue(ALIMENTOS_PASTA %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_patata = revalue(ALIMENTOS_PATATA %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_pan = revalue(ALIMENTOS_PAN %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_verdura = revalue(ALIMENTOS_VERDURA %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_legumbre = revalue(ALIMENTOS_LEGUMBRE %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_embutidos = revalue(ALIMENTOS_EMBUTIDOS %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_lacteos = revalue(ALIMENTOS_LACTEOS %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_huevos = revalue(ALIMENTOS_HUEVOS %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    alimentos_dulces = revalue(ALIMENTOS_DULCES %>% as.factor, c("1" = "mai o gairebé mai", "2" = "menys d'1 cop a la setmana", "3" = "1 o 2 cops/setmana", "4" = "3 o 4 cops/setmana", "5" = "5 o 6 cops/setmana", "6" = "1 cop/dia", "7" = "2o 3 cops/dia", "8" = "més de 3 cops/dia", "0" = NA)),
    salud_mental_feliz = revalue(SALUD_MENTAL_FELIZ %>% as.factor, c("1" = "sempre", "2" = "quasi sempre", "3" = "moltes vegades", "4" = "algunes vegades", "5" = "només alguna vegada", "6" = "mai", "0" = NA)),
    salud_mental_nervioso = revalue(SALUD_MENTAL_NERVIOSO %>% as.factor, c("1" = "sempre", "2" = "quasi sempre", "3" = "moltes vegades", "4" = "algunes vegades", "5" = "només alguna vegada", "6" = "mai", "0" = NA)),
    salud_mental_baja_moral = revalue(SALUD_MENTAL_BAJAMORAL %>% as.factor, c("1" = "sempre", "2" = "quasi sempre", "3" = "moltes vegades", "4" = "algunes vegades", "5" = "només alguna vegada", "6" = "mai", "0" = NA)),
    salud_mental_calma = revalue(SALUD_MENTAL_CALMA %>% as.factor, c("1" = "sempre", "2" = "quasi sempre", "3" = "moltes vegades", "4" = "algunes vegades", "5" = "només alguna vegada", "6" = "mai", "0" = NA)),
    salud_mental_trieste = revalue(SALUD_MENTAL_TRISTE %>% as.factor, c("1" = "sempre", "2" = "quasi sempre", "3" = "moltes vegades", "4" = "algunes vegades", "5" = "només alguna vegada", "6" = "mai", "0" = NA)),
    salud_mental_incapacidad = revalue(SALUD_MENTAL_INCAPACIDAD %>% as.factor, c("1" = "sempre", "2" = "quasi sempre", "3" = "moltes vegades", "4" = "algunes vegades", "5" = "només alguna vegada", "6" = "mai", "0" = NA)),
    whr_who = revalue(whr_who %>% as.factor, c("1" = "normal weight", "2" = "overweight", "3" = "obesity")),
    bmi_who = revalue(whr_who %>% as.factor, c("underweight" = "underweight", "normal weight" = "normal weight", "overweight" = "overweight", "obesity" = "obesity")),
    bmi_deep = revalue(whr_who %>% as.factor, c("Mild thinness" = "mild thinness", "Moderate thinness" = "moderate thinness", "Normal range" = "normal range", "Pre-obese" = "pre-obese", "Obese class I" = "obese class I", "Obese class II" = "obese class II"))
  )

ds <- ds %>%
  select(
    gender,
    ethnicity,
    ethnicity_mother,
    ethnicity_father,
    civil_status,
    education,
    working_status,
    walking_days_a_week,
    walking_minutes_a_day,
    sport,
    sport_last_year,
    weekly_domestic_labour,
    stairs,
    hair_color,
    skin_color,
    eye_color,
    freckling,
    smoking_status,
    smoking_intensity,
    smoking_100,
    smoking_now,
    smoking_habit,
    alcohol_use,
    alcohol_meals,
    cook_with_oil,
    health_status,
    weight_max,
    weight_min,
    last_intake,
    predimed_score,
    age_at_menarche,
    age_at_menopause,
    menstruation,
    menstruation_duration,
    menopause_cause,
    reproductive_life_span,
    women_children,
    women_profile,
    pregnancy,
    oc_use,
    hrt_use,
    prostate,
    prostate_medication,
    beard_change,
    voice_change,
    bald_20,
    bald_30,
    bald_40,
    men_profile,
    men_children,
    adopted,
    multiple_delivery,
    brothers,
    alimentos_frutas,
    alimentos_carne_blanca,
    alimentos_carne_roja,
    alimentos_pescados,
    alimentos_pasta,
    alimentos_patata,
    alimentos_pan,
    alimentos_verdura,
    alimentos_legumbre,
    alimentos_embutidos,
    alimentos_lacteos,
    alimentos_huevos,
    alimentos_dulces,
    salud_mental_feliz,
    salud_mental_nervioso,
    salud_mental_baja_moral,
    salud_mental_calma,
    salud_mental_trieste,
    salud_mental_incapacidad,
    bmi,
    bmi_who,
    bmi_deep,
    whr,
    whr_who,
    waist,
    hip,
    weight,
    height,
    heart_rate,
    diastolic_blood_pressure,
    systolic_blood_pressure
  )

## Generate report

options(xtable.comment = FALSE)
Sys.setlocale("LC_CTYPE", "en_US.UTF-8")
rmarkdown::render('inst/reports/summary-report.Rmd', 'pdf_document')
