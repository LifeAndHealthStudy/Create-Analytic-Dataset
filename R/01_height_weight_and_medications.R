#' Clean Height, Weight, and Medications Data
#'
clean_height_weight_medications <- function(dfs) {

  # use janitor to clean column names
  dfs %<>% purrr::map(janitor::clean_names)

  # rename the first column to 'id' for all three
  dfs %<>% purrr::map(~ rename(., 'id' = 1))

  # fenway's data has a lot of participants where they are repeated for
  # every prescription record, so let's de-duplicate that and combine the
  # prescriptions into a list column
  #
  # before doing so, I checked and saw that everyone who had NA for their
  # weight never had any non-NA weight values reported; same for height.
  #
  # perhaps they refused to have their height and weight taken, or it could
  # have been some other exceptional circumstances.
  #
  fenway_last_height_and_weight <- dfs[['fenway']] %>% group_by(id) %>%
    slice_max(
      last_measurement_date, with_ties = FALSE, n = 1
    ) %>%
    select(id, weight_kg, height_cm, height_weight_measurement_date = last_measurement_date)

  # collect medications into a list column, also first and last prescription dates
  fenway_medications <- dfs$fenway %>% group_by(id) %>%
    dplyr::summarize(
      medications = paste0(medication_description, collapse=', '),
      min_prescription_date = min(last_prescribed_date, na.rm=T),
      max_prescription_date = max(last_prescribed_date, na.rm=T)
    )

  # recode "Inf" or "-Inf" as NA for prescription dates
  fenway_medications$min_prescription_date[is.infinite(fenway_medications$min_prescription_date)] <- NA
  fenway_medications$max_prescription_date[is.infinite(fenway_medications$max_prescription_date)] <- NA

  # create merged data
  fenway_df <- left_join(fenway_last_height_and_weight, fenway_medications)

  # convert height to inches and feet
  fenway_df %<>% mutate(
    height_ft = 0.393700787 * height_cm / 12
  ) %>% select(-height_cm)

  # convert weight into lbs
  fenway_df %<>% mutate(
    weight_lbs = 2.20462262185 * weight_kg
  ) %>% select(-weight_kg)

  # there are a couple values that are unreasonable that we have to code as NA:
  # in particular there are two entries, one with height less than 1 foot and
  # one with weight less than 20 lbs
  fenway_df$height_ft[fenway_df$height_ft < 1] <- NA
  fenway_df$weight_lbs[fenway_df$weight_lbs < 20] <- NA

  # we can check to make sure there's no duplicates
  stopifnot(! any(duplicated(fenway_df$id)))

  # overwrite in the dfs list
  dfs$fenway <- fenway_df


  # the harvard st data look pretty much ready to go except we need to
  # clean the height and weight data which are text formatted.
  dfs$harvard %<>% mutate(
    height_ft_only = as.numeric(stringr::str_extract(height_before_taking_the_survey, pattern = "[0-9]+(?= Ft)")),
      height_in = as.numeric(stringr::str_extract(height_before_taking_the_survey, pattern = "[0-9\\.]+(?= inch)"))
  )
  # create one numeric column for their height in units of feet; drop columns used in construction
  dfs$harvard %<>% mutate(
    height_ft = height_ft_only + (height_in/12)
  ) %>% select(-height_before_taking_the_survey, -height_ft_only, -height_in)

  # clean the weight records from text to numeric
  dfs$harvard %<>% mutate(
    weight_lbs_only = as.numeric(stringr::str_extract(weight_before_taking_the_survey, pattern = "[0-9]+(?= lbs)")),
    weight_oz_only = as.numeric(stringr::str_extract(weight_before_taking_the_survey, pattern = "[0-9]+(?= oz)")),
    weight_oz_only = ifelse(is.na(weight_oz_only), 0, weight_oz_only) # code NA ounces as 0 ounces
  )

  # create numeric weight column in lbs and drop columns used for construction
  dfs$harvard %<>% mutate(
    weight_lbs = weight_lbs_only + (weight_oz_only/16)
  ) %>% select(-weight_lbs_only, -weight_oz_only, -weight_before_taking_the_survey)

  # rename to 'medications' to standardize column names
  dfs$harvard %<>% rename(medications = rx_within_the_year_prior_to_date_of_survey)

  # remove their completion date, since this adds no additional info
  dfs$harvard %<>% select(-date)

  # looks like the only cleaning that needs to be done for mattapan
  # is to convert height and weight into feet and lbs and to calculate a last
  # measurement date for their height and weight together
  dfs$mattapan %<>% mutate(
    height_ft = 0.393700787 * height_cm / 12,
    weight_lbs = 2.20462262185 * weight_kg
  ) %>% select(-height_cm, -weight_kg)

  # create a combined last measurement date for height & weight
  dfs$mattapan %<>% mutate(
    height_weight_measurement_date = pmax(height_meas_date, weight_meas_date, na.rm=T)
  ) %>% select(-height_meas_date, -weight_meas_date)

  # remove survey completion date, since it only adds redundant information with
  # what we already have
  dfs$mattapan %<>% select(-survey_date)

  # create one dataframe of height, weight, and medication records
  df <- dfs %>% bind_rows()

  return(df)
}
