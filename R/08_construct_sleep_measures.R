#' Overwrite the Sleep Data with the Corrected Version
#'
#' The sleep data had to be manually cleaned since we had a lot of people saying things
#' like they went to sleep at 10am and woke up at 7am where it's quite obvious that
#' they meant they go to sleep at 10pm.
#'
#' Not all of the sleep data were corrected, as some looked like perhaps the participants
#' work night-shifts and sleep when they get home in the early morning until mid-afternoon
#' or have otherwise unconventional sleep patterns.

overwrite_sleep_data_with_cleaned_version <- function(df) {

  # if they only filled out the hours column but not the minutes column, put in 00 for
  # the minutes column;  I also looked at the missing AM/PM values and they all look like
  # they should be PM for asleep times and AM for awake times.
  df$ASLEEP_WK_2_1[is.na(df$ASLEEP_WK_2_1)] <- 1 # 1 is the level for "00"
  df$ASLEEP_WK_3_1[is.na(df$ASLEEP_WK_3_1)] <- 2 # 2 is the level for "PM"
  df$AWAKE_WK_2_1[is.na(df$AWAKE_WK_2_1)] <- 1
  df$AWAKE_WK_3_1[is.na(df$AWAKE_WK_3_1)] <- 1 # 1 is the level for "AM"

  df %<>% mutate(
    fall_asleep_weekdays = stringr::str_c(haven::as_factor(ASLEEP_WK_1_1), ":", haven::as_factor(ASLEEP_WK_2_1), " ", haven::as_factor(ASLEEP_WK_3_1)),
    fall_asleep_weekdays = lubridate::ymd_hm(stringr::str_c("2020-01-01 ", fall_asleep_weekdays)),
    wake_up_weekdays = stringr::str_c(haven::as_factor(AWAKE_WK_1_1), ":", haven::as_factor(AWAKE_WK_2_1), " ", haven::as_factor(AWAKE_WK_3_1)),
    wake_up_weekday_opt1 = lubridate::ymd_hm(stringr::str_c("2020-01-01 ", wake_up_weekdays)),
    wake_up_weekday_opt2 = lubridate::ymd_hm(stringr::str_c("2020-01-02 ", wake_up_weekdays)),
    wake_up_weekdays = if_else(wake_up_weekday_opt1 > fall_asleep_weekdays, wake_up_weekday_opt1, wake_up_weekday_opt2)
  ) %>% select(-wake_up_weekday_opt1, -wake_up_weekday_opt2)


  # read in the manually cleaned sleep times
  corrected_sleep_times <- readxl::read_excel(
    system.file("REDACTED_FILEPATH/REDACTED_FILENAME.xlsx",
                package = "LifeAndHealth")
  )

  corrected_sleep_times$id %<>% as.character() # cast IDs to character type

  corrected_sleep_times %<>% select(id, ends_with("_corrected"))

  # make sure that the wake-up datetime happens after the fall-asleep datetime
  corrected_sleep_times %<>% mutate(
    fall_asleep_weekdays_corrected = if_else(fall_asleep_weekdays_corrected < wake_up_weekdays_corrected, fall_asleep_weekdays_corrected,
                                             fall_asleep_weekdays_corrected - lubridate::days(1)))

  corrected_sleep_times %<>% mutate(
    wake_up_weekdays_corrected = if_else(wake_up_weekdays_corrected > fall_asleep_weekdays_corrected + lubridate::days(1), wake_up_weekdays_corrected - lubridate::days(1),
                                         wake_up_weekdays_corrected))

  # calculate the sleep duration for weekdays
  corrected_sleep_times %<>% mutate(sleep_duration_weekdays_corrected = as.numeric(wake_up_weekdays_corrected - fall_asleep_weekdays_corrected))

  # merge corrected data (including weekday sleep duration) into the original data
  df %<>% left_join(corrected_sleep_times, by = 'id')

  # replace those corrected data
  df %<>% mutate(
    sleep_duration_weekdays = lubridate::interval(fall_asleep_weekdays, wake_up_weekdays) %>% as.numeric('hours'),
    replace_sleep = sleep_duration_weekdays > 10,
    sleep_duration_weekdays = if_else(replace_sleep, sleep_duration_weekdays_corrected, sleep_duration_weekdays),
    fall_asleep_weekdays = if_else(replace_sleep, fall_asleep_weekdays_corrected, fall_asleep_weekdays),
    wake_up_weekdays = if_else(replace_sleep, wake_up_weekdays_corrected, wake_up_weekdays)
  ) %>% select(-replace_sleep)

  # make all the sleep datetimes start on the same day so they can be visualized easily
  df %<>% mutate(
    shift_sleep = fall_asleep_weekdays < lubridate::ymd_hms('2020-01-01 12:00:00 PM'),
    fall_asleep_weekdays = if_else(shift_sleep, fall_asleep_weekdays + lubridate::days(1), fall_asleep_weekdays),
    wake_up_weekdays = if_else(shift_sleep, wake_up_weekdays + lubridate::days(1), wake_up_weekdays)
  ) %>% select(-shift_sleep) # drop the indicator for if the sleep datetimes needed to have the day shifted

  # now that we've replaced the data in the original columns with the _corrected data, we can drop
  # those unnecessary _corrected columns
  df %<>% select(-c(fall_asleep_weekdays_corrected, wake_up_weekdays_corrected, sleep_duration_weekdays_corrected))

  return(df)
}

# Construct PROMIS Sleep Disturbance Measures (Adult v1.0)
construct_slp_disturb_measure <- function(df){

  df %<>%
    select(starts_with("SLP_WK_2"), starts_with("SLP_WK_1")) %>%
    rowSums() %>%
    data.frame(raw_score = .) %$%
    bind_cols(df, .) %>%
    mutate(sleep_disturbance = case_when(raw_score == 8 ~ 30.5,
                                         raw_score == 9 ~ 35.3,
                                         raw_score == 10 ~ 38.1,
                                         raw_score == 11 ~ 40.4,
                                         raw_score == 12 ~ 42.2,
                                         raw_score == 13 ~ 43.9,
                                         raw_score == 14 ~ 45.3,
                                         raw_score == 15 ~ 46.7,
                                         raw_score == 16 ~ 47.9,
                                         raw_score == 17 ~ 49.1,
                                         raw_score == 18 ~ 50.2,
                                         raw_score == 19 ~ 51.3,
                                         raw_score == 20 ~ 52.4,
                                         raw_score == 21 ~ 53.4,
                                         raw_score == 22 ~ 54.3,
                                         raw_score == 23 ~ 55.3,
                                         raw_score == 24 ~ 56.2,
                                         raw_score == 25 ~ 57.2,
                                         raw_score == 26 ~ 58.1,
                                         raw_score == 27 ~ 59.1,
                                         raw_score == 28 ~ 60,
                                         raw_score == 29 ~ 61,
                                         raw_score == 30 ~ 62,
                                         raw_score == 31 ~ 63,
                                         raw_score == 32 ~ 64,
                                         raw_score == 33 ~ 65.1,
                                         raw_score == 34 ~ 66.2,
                                         raw_score == 35 ~ 67.4,
                                         raw_score == 36 ~ 68.7,
                                         raw_score == 37 ~ 70.2,
                                         raw_score == 38 ~ 72,
                                         raw_score == 39 ~ 74.1,
                                         raw_score == 40 ~ 77.5)) %>%
    select(-raw_score)

  return(df)
}

# Construct PROMIS Sleep Related Impairment measure (Short Form 8a)
construct_slp_impair_measure <- function(df){

  df %<>%
    select(starts_with("SLP_WK_3")) %>%
    rowSums() %>%
    data.frame(sleep_imp_raw = .) %$%
    bind_cols(df, .) %>%
    mutate(sleep_impairment = case_when(sleep_imp_raw == 8 ~ 30.0,
                                        sleep_imp_raw == 9 ~ 35.2,
                                        sleep_imp_raw == 10 ~ 38.7,
                                        sleep_imp_raw == 11 ~ 41.4,
                                        sleep_imp_raw == 12 ~ 43.6,
                                        sleep_imp_raw == 13 ~ 45.5,
                                        sleep_imp_raw == 14 ~ 47.3,
                                        sleep_imp_raw == 15 ~ 48.9,
                                        sleep_imp_raw == 16 ~ 50.3,
                                        sleep_imp_raw == 17 ~ 51.6,
                                        sleep_imp_raw == 18 ~ 52.9,
                                        sleep_imp_raw == 19 ~ 54.0,
                                        sleep_imp_raw == 20 ~ 55.1,
                                        sleep_imp_raw == 21 ~ 56.1,
                                        sleep_imp_raw == 22 ~ 57.2,
                                        sleep_imp_raw == 23 ~ 58.2,
                                        sleep_imp_raw == 24 ~ 59.3,
                                        sleep_imp_raw == 25 ~ 60.3,
                                        sleep_imp_raw == 26 ~ 61.3,
                                        sleep_imp_raw == 27 ~ 62.3,
                                        sleep_imp_raw == 28 ~ 63.3,
                                        sleep_imp_raw == 29 ~ 64.3,
                                        sleep_imp_raw == 30 ~ 65.3,
                                        sleep_imp_raw == 31 ~ 66.3,
                                        sleep_imp_raw == 32 ~ 67.3,
                                        sleep_imp_raw == 33 ~ 68.4,
                                        sleep_imp_raw == 34 ~ 69.5,
                                        sleep_imp_raw == 35 ~ 70.7,
                                        sleep_imp_raw == 36 ~ 71.9,
                                        sleep_imp_raw == 37 ~ 73.4,
                                        sleep_imp_raw == 38 ~ 75.0,
                                        sleep_imp_raw == 39 ~ 76.9,
                                        sleep_imp_raw == 40 ~ 80.1)) %>%
    select(-sleep_imp_raw)

  return(df)
}
