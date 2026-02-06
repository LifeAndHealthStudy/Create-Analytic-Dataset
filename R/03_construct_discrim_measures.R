# create EOD frequency measure
construct_eod_measures <- function(df) {
  # convert EOD yes/no measures to factors
  df %<>% mutate(across(c(starts_with("REC_EXP_1_1_"),
                          starts_with("SG_EXP_1_1_"),
                          starts_with("GID_EXP_1_1_"),
                          starts_with("SXOEXP1_1_"),
                          starts_with("LBSEXP1_1_"),
                          starts_with("AGE_EXP_1_1_")
  ),
  ~haven::as_factor(.)
  ))

  # custom scaling function for EOD
  eod_scaling <- function(numeric_part, yesno_part) {
    case_when(
      is.na(numeric_part) ~ ifelse(yesno_part == 'Yes', NA_real_, 0),
      numeric_part == 1 ~ 1,
      numeric_part == 2 ~ 2.5,
      numeric_part == 3 ~ 5
    )
  }

  # function to calculate a single EOD measure
  create_eod_scale_given_prefix <- function(prefix, env) {
    eod_scaling(get(paste0(prefix, "2_1_1"), envir = env), get(paste0(prefix, "1_1_1"), envir = env)) + # at school
      eod_scaling(get(paste0(prefix, "2_1_2"), envir = env), get(paste0(prefix, "1_1_2"), envir = env)) + # getting a job
      eod_scaling(get(paste0(prefix, "2_1_3"), envir = env), get(paste0(prefix, "1_1_3"), envir = env)) + # at work
      eod_scaling(get(paste0(prefix, "2_1_4"), envir = env), get(paste0(prefix, "1_1_4"), envir = env)) + # getting housing
      eod_scaling(get(paste0(prefix, "2_1_5"), envir = env), get(paste0(prefix, "1_1_5"), envir = env)) + # getting medical care
      eod_scaling(get(paste0(prefix, "2_1_6"), envir = env), get(paste0(prefix, "1_1_6"), envir = env)) + # getting restaurant/store service
      eod_scaling(get(paste0(prefix, "2_1_7"), envir = env), get(paste0(prefix, "1_1_7"), envir = env)) + # getting bank/loan/credit service
      eod_scaling(get(paste0(prefix, "2_1_8"), envir = env), get(paste0(prefix, "1_1_8"), envir = env)) + # in public
      eod_scaling(get(paste0(prefix, "2_1_9"), envir = env), get(paste0(prefix, "1_1_9"), envir = env)) + # with police, in court, in legal settings
      eod_scaling(get(paste0(prefix, "2_1_10"), envir = env), get(paste0(prefix, "1_1_10"), envir=env)) # at home
  }

  # apply above functions for each of the 6 areas of discrimination
  df %<>% mutate(
    # how often have you been made to feel discrimination on account of your:
    EOD_racial = create_eod_scale_given_prefix("REC_EXP_", env = environment()),  # race or ancestry
    EOD_man_or_woman = create_eod_scale_given_prefix("SG_EXP_", env = environment()), # being a man or woman
    EOD_gender = create_eod_scale_given_prefix("GID_EXP_", env = environment()), # gender identity
    EOD_orientation = create_eod_scale_given_prefix("SXOEXP", env = environment()), # sexual orientation
    EOD_weight = create_eod_scale_given_prefix("LBSEXP", env = environment()), # weight
    EOD_age = create_eod_scale_given_prefix("AGE_EXP_", env = environment()) # age
  )

  return(df)
}


# create EOD variable: total number of domains
construct_eod_measures2 <- function(df) {

  # convert EOD yes/no measures to factors
  df %<>% mutate(across(c(starts_with("REC_EXP_1_1_"),
                          starts_with("SG_EXP_1_1_"),
                          starts_with("GID_EXP_1_1_"),
                          starts_with("SXOEXP1_1_"),
                          starts_with("LBSEXP1_1_"),
                          starts_with("AGE_EXP_1_1_")
  ),
  ~haven::as_factor(.)
  ))

  # custom scaling function for EOD
  eod_scaling2 <- function(yesno_part) {
    case_when(yesno_part == 'Yes' ~ 1,
              yesno_part == "No" ~ 0,
              TRUE ~ NA)
  }

  # function to calculate a single EOD measure
  calculate_N_EOD_domains <- function(prefix, env) {
    eod_scaling2(get(paste0(prefix, "1_1_1"), envir = env)) + # at school
      eod_scaling2(get(paste0(prefix, "1_1_2"), envir = env)) + # getting a job
      eod_scaling2(get(paste0(prefix, "1_1_3"), envir = env)) + # at work
      eod_scaling2(get(paste0(prefix, "1_1_4"), envir = env)) + # getting housing
      eod_scaling2(get(paste0(prefix, "1_1_5"), envir = env)) + # getting medical care
      eod_scaling2(get(paste0(prefix, "1_1_6"), envir = env)) + # getting restaurant/store service
      eod_scaling2(get(paste0(prefix, "1_1_7"), envir = env)) + # getting bank/loan/credit service
      eod_scaling2(get(paste0(prefix, "1_1_8"), envir = env)) + # in public
      eod_scaling2(get(paste0(prefix, "1_1_9"), envir = env)) + # with police, in court, in legal settings
      eod_scaling2(get(paste0(prefix, "1_1_10"), envir=env)) # at home
  }

  # apply above functions for each of the 6 areas of discrimination
  df %<>%

    mutate(
      # how often have you been made to feel discrimination on account of your:
      EOD_racial_Ndomain = calculate_N_EOD_domains("REC_EXP_", env = environment()),  # race or ancestry
      EOD_man_or_woman_Ndomain = calculate_N_EOD_domains("SG_EXP_", env = environment()), # being a man or woman
      EOD_gender_Ndomain = calculate_N_EOD_domains("GID_EXP_", env = environment()), # gender identity
      EOD_orientation_Ndomain = calculate_N_EOD_domains("SXOEXP", env = environment()), # sexual orientation
      EOD_weight_Ndomain = calculate_N_EOD_domains("LBSEXP", env = environment()), # weight
      EOD_age_Ndomain = calculate_N_EOD_domains("AGE_EXP_", env = environment()) # age
    )

  return(df)

}


# function to convert explicit attitudes to the 7pt scale ranging from -3 to +3
scale_to_range_7pt <- function(var) {
  new_min <- -3
  new_max <- 3
  scaled_var <- ((var - min(var)) / (max(var) - min(var))) * (new_max - new_min) + new_min
  return(scaled_var)
}

# function to convert explicit group discrimination to the 4pt scale ranging from 0 to 3
scale_to_range_4pt <- function(var) {
  new_min <- 0
  new_max <- 3
  scaled_var <- ((var - min(var)) / (max(var) - min(var))) * (new_max - new_min) + new_min
  return(scaled_var)
}

# function to convert food insecurity vars to binary
scale_food_insecurity <- function(values) {
  ifelse(values %in% c(1,2),1, 0)
}

# function to construct single race iat
construct_single_race_iat <- function(df) {

  iat_race_pt <- df %>%
    select(id,race,POC_or_WhiteNH,iat_PT_white_color,iat_PT_white_black,iat_PT_hispanic) %>%
    pivot_longer(cols = starts_with('iat_'), names_to = 'iat_race_task_PT' ,values_to = 'iat_race_PT') %>%
    group_by(id) %>%
    arrange(desc(iat_race_PT)) %>%
    distinct(id,.keep_all = T) %>%
    mutate(iat_race_task_PT = ifelse(is.na(iat_race_PT),'No IAT',iat_race_task_PT)) %>%
    select(id,iat_race_PT,iat_race_task_PT)


  iat_race_gb <- df %>%
    select(id,race,POC_or_WhiteNH,iat_GB_white_color,iat_GB_white_black,iat_GB_hispanic) %>%
    pivot_longer(cols = starts_with('iat_'), names_to = 'iat_race_task_GB' ,values_to = 'iat_race_GB') %>%
    group_by(id) %>%
    arrange(desc(iat_race_GB)) %>%
    distinct(id,.keep_all = T) %>%
    mutate(iat_race_task_GB = ifelse(is.na(iat_race_GB),'No IAT',iat_race_task_GB)) %>%
    select(id,iat_race_GB,iat_race_task_GB)

  df <- df %>%
    left_join(iat_race_pt, 'id') %>%
    left_join(iat_race_gb, 'id') %>%
    mutate(iat_race_cat_pt = case_when(iat_race_task_PT == 'iat_PT_hispanic' ~ 'Hispanic',
                                       iat_race_task_PT == 'iat_PT_white_black' ~ 'Black',
                                       iat_race_task_PT == 'iat_PT_white_color' & POC_or_WhiteNH == 'White non-Hispanic' ~ 'White',
                                       iat_race_task_PT == 'iat_PT_white_color' & POC_or_WhiteNH == 'Person of Color' ~ 'Other POC'),
           iat_race_cat_gb = case_when(iat_race_task_GB == 'iat_GB_hispanic' ~ 'Hispanic',
                                       iat_race_task_GB == 'iat_GB_white_black' ~ 'Black',
                                       iat_race_task_GB == 'iat_GB_white_color' & POC_or_WhiteNH == 'White non-Hispanic' ~ 'White',
                                       iat_race_task_GB == 'iat_GB_white_color' & POC_or_WhiteNH == 'Person of Color' ~ 'Other POC'))


  return(df)

}


# construct variable to count number of types of discrimination experienced
# across 6 types
construct_n_types_discrim <- function(df) {
  eod_vars <- c("EOD_racial_2levels", "EOD_man_or_woman_2levels",
                "EOD_gender_2levels", "EOD_orientation_2levels",
                "EOD_age_2levels", "EOD_weight_2levels")
  df <- df %>%
    rowwise() %>%
    mutate(
      n_types_discrim_sr = {
        values <- c_across(all_of(eod_vars))
        non_missing <- sum(!is.na(values))
        if (non_missing >= 1) {
          sum(values == "1+", na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  return(df)
}


# construct variable to count total number of domains of discrimination experienced
# across 6 types
construct_eod_domain_total <- function(df) {
  eod_vars <- c("EOD_racial_Ndomain", "EOD_man_or_woman_Ndomain",
                "EOD_gender_Ndomain", "EOD_orientation_Ndomain",
                "EOD_age_Ndomain", "EOD_weight_Ndomain")
  df <- df %>%
    rowwise() %>%
    mutate(
      eod_domain_total = {
        values <- c_across(all_of(eod_vars))
        non_missing <- sum(!is.na(values))
        if (non_missing >= 1) {
          sum(values, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  return(df)
}


# construct variable to count number of types of discrimination implicitly
# recognized across 6 types
construct_PT_iat_total <- function(df) {
  iat_pt_vars <- c("iat_race_PT", "iat_PT_gender", "iat_PT_sexuality",
                   "iat_PT_genderid", "iat_PT_age", "iat_PT_weight")
  df <- df %>%
    rowwise() %>%
    mutate(
      implicit_recog_total = {
        values <- c_across(all_of(iat_pt_vars))
        non_missing <- sum(!is.na(values))
        if (non_missing >= 1) {
          sum(values > 0, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  return(df)
}

# construct variable to count number of types of discrimination implicitly
# preferred across 6 types
construct_GB_iat_total <- function(df) {
  iat_gb_vars <- c("iat_race_GB", "iat_GB_gender", "iat_GB_sexuality",
                   "iat_GB_genderid", "iat_GB_age", "iat_GB_weight")
  df <- df %>%
    rowwise() %>%
    mutate(
      implicit_prefer_total = {
        values <- c_across(all_of(iat_gb_vars))
        non_missing <- sum(!is.na(values))
        if (non_missing >= 1) {
          sum(values > 0, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  return(df)
}

# construct variable to count number of types of discrimination explicitly
# recognized across 6 types
construct_exp_recog_total <- function(df) {
  exp_recog_vars <- c("REC_GRFREQ","SG_GRFREQ","GID_GRFREQ","SXO_GRFREQ","LBS_GRFREQ","AGE_GRFREQ")

  df <- df %>%
    rowwise() %>%
    mutate(
      explicit_recog_total = {
        values <- c_across(all_of(exp_recog_vars))
        non_missing <- sum(!is.na(values))
        if (non_missing >= 1) {
          sum(values > 0, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  return(df)
}

# construct variable to count number of types of discrimination explicitly
# preferred across 6 types
construct_exp_pref_total <- function(df) {

  exp_pref_vars <- c("REC_PREF","SG_PREF","GID_PREF","SXO_PREF","LBS_PREF","AGE_PREF")

  df <- df %>%
    rowwise() %>%
    mutate(
      explicit_pref_total = {
        values <- c_across(all_of(exp_pref_vars))
        non_missing <- sum(!is.na(values))
        if (non_missing >= 1) {
          sum(values > 0, na.rm = TRUE)
        } else {
          NA_real_
        }
      }
    ) %>%
    ungroup()
  return(df)
}
