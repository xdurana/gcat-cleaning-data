library(tidyverse)

directory <- 'sleep'

MIN_SLEEP = 4
MAX_SLEEP = 12

to_hours <- function(hour, minutes) {
  round(hour %% 24 + minutes/60, 2)
}

ds <- gcat %>%
  select(
    entity_id,
    ACTIVIDAD_DORMIR_LABORABLE_ACOSTARSE_HORA,
    ACTIVIDAD_DORMIR_LABORABLE_ACOSTARSE_MINUTOS,
    ACTIVIDAD_DORMIR_LABORABLE_LEVANTARSE_HORA,
    ACTIVIDAD_DORMIR_LABORABLE_LEVANTARSE_MINUTOS,
    ACTIVIDAD_DORMIR_LABORABLE_SIESTA,
    ACTIVIDAD_DORMIR_LABORABLE_SIESTA_DURACION_HORAS,
    ACTIVIDAD_DORMIR_LABORABLE_SIESTA_DURACION_MINUTOS,
    LABORAL_1_HORARIO,
    LABORAL_2_HORARIO,
    LABORAL_3_HORARIO
  ) %>%
  mutate(
    nap_time_decimal = to_hours(
      ACTIVIDAD_DORMIR_LABORABLE_SIESTA_DURACION_HORAS,
      ACTIVIDAD_DORMIR_LABORABLE_SIESTA_DURACION_MINUTOS
    ),
    sleep_time_decimal = to_hours(
      ACTIVIDAD_DORMIR_LABORABLE_ACOSTARSE_HORA,
      ACTIVIDAD_DORMIR_LABORABLE_ACOSTARSE_MINUTOS
    ),
    get_up_time_decimal = to_hours(
      ACTIVIDAD_DORMIR_LABORABLE_LEVANTARSE_HORA,
      ACTIVIDAD_DORMIR_LABORABLE_LEVANTARSE_MINUTOS
    ),
    night_shift = ifelse(
      (!is.na(LABORAL_1_HORARIO) & LABORAL_1_HORARIO == 4) |
      (!is.na(LABORAL_2_HORARIO) & LABORAL_2_HORARIO == 4) | 
      (!is.na(LABORAL_3_HORARIO) & LABORAL_3_HORARIO == 4), 1, 0)
  ) %>%
  mutate(
    sleep_duration = get_up_time_decimal - sleep_time_decimal,
    sleep_time_decimal = ifelse(sleep_time_decimal == get_up_time_decimal, NA, sleep_time_decimal),
    get_up_time_decimal = ifelse(sleep_time_decimal == get_up_time_decimal, NA, get_up_time_decimal)
  )

ds_corrected <- ds %>%
  mutate(
    sleep_duration = ifelse(sleep_duration < -10, sleep_duration + 24, sleep_duration)
  ) %>%
  mutate(
    sleep_duration_corrected = ifelse(between(sleep_duration, -10, 0) & sleep_time_decimal < 17, sleep_duration + 12, sleep_duration),
    sleep_time_decimal = ifelse(between(sleep_duration, -10, 0) & sleep_time_decimal < 17, (sleep_time_decimal + 12) %% 24, sleep_time_decimal)
  ) %>%
  mutate(
    sleep_duration_corrected = ifelse(between(sleep_duration_corrected, MIN_SLEEP, MAX_SLEEP), sleep_duration_corrected, NA)
  )

pair_plot(ds_corrected %>% select(sleep_time_decimal, get_up_time_decimal), 'sleep', 'sleep_duration')
  
ds_corrected <- ds_corrected %>%
  mutate(
    nap = ifelse(ACTIVIDAD_DORMIR_LABORABLE_SIESTA %in% c(1), 1, ifelse(ACTIVIDAD_DORMIR_LABORABLE_SIESTA %in% c(2), 0, NA)),
    nap_duration = ifelse(nap_time_decimal > 5, to_hours(0, nap_time_decimal), nap_time_decimal)
  )

ds_corrected <- ds_corrected %>%
  mutate(
    sleep_duration = sleep_duration_corrected,
    bedtime = sleep_time_decimal,
    morningness = get_up_time_decimal #TODO ifelse(get_up_time_decimal < sleep_time_decimal, get_up_time_decimal + 24, get_up_time_decimal)
  ) %>%
  select(
    entity_id,
    night_shift,
    bedtime,
    morningness,
    sleep_duration,
    nap,
    nap_duration
  )

ds <- ds_corrected %>%
  mutate(bedtime = ifelse(bedtime < 17, bedtime + 24, bedtime)) %>%
  mutate(bedtime = ifelse(bedtime - morningness >= 24, bedtime - 12, bedtime)) %>%
  mutate(morningness = morningness + 24)
  
#ds <- ds %>% mutate(bedtime = bedtime %% 24)

ds %>% write_csv(sprintf('output/check/sleep/data.csv', directory))

missings_plot(ds, directory)
pair_plot(ds %>% select(bedtime, morningness), directory, 'sleep_duration')
