# Converting Categorical Variables to Dummy Variables and Vice-Versa
#
# This document introduces a pair of functions which should be intuitive to
# tidyverse users and standardizes the process for converting categorical
# columns of data back and forth from a set of dummy variables (columns of
# TRUE/FALSE indicators for each of the categorical levels).


#' Convert Categorical Variables to Dummy Variables
#'
#' This function returns a dataframe modified to contain dummy variables
#' (TRUE/FALSE indicator columns) for each level of the variable specified.
#'
#' The new column names are automatically constructed to be {variable}_{level}
#' where variable is given as a function argument and level is specified as
#' each unique level of the variable given.
#'
#' @param df the data.frame to modify
#'
#' @param variable a tidy-evaluation style expression that names a column to
#' convert into dummy variables
#'
#' @param drop_categorical (default: true) an indicator to remove the original
#' variable
#'
#' @return a modified data.frame with new columns that are dummy indicators for
#' each level of the variable specified
#'
#' @seealso dummies_to_categorical
#' @export
#'
categorical_to_dummies <- function(df, variable, drop_categorical = TRUE) {

  # capture the tidy evaluation style variable name
  variable_orig <- rlang::enquo(variable)

  # convert to a character
  variable <- rlang::quo_name(variable_orig)

  # get the unique levels
  unique_levels <- na.omit(unique(df[[variable]]))

  # for each unique level, construct a new dummy variable
  for (level in unique_levels) {
    df[[paste0(variable, '_', level)]] <- (df[[variable]] == level)
  }

  # if drop_categorical is specified, drop the original variable
  if (drop_categorical) { df <- df %>% select(- c(!! variable_orig)) }

  return(df)
}


#' Convert Dummy Variables to Categorical Variables
#'
#'
#' @param df a data.frame or tibble to modify
#'
#' @param varname a tidy-evaluation style expression which is both the
#' new variable name to create and the prefix for the existing dummy variables
#' to convert into the new categorical variable.
#'
#' @param drop_dummies (default: true) an indicator to drop the dummy variables
#' after creating the new categorical variable
#'
#' @return a modified data.frame with a new categorical variable based on the
#' dummy variables starting with variable_prefix
#'
#' @seealso categories_to_dummies
#' @export
#'
dummies_to_categorical <- function(df, varname, cols, drop_dummies = TRUE) {

  # capture tidy-evaluation style expression
  var_prefix_orig <- enquo(varname)

  # get the variable_prefix in character format
  variable_prefix <- rlang::quo_name(var_prefix_orig)

  if (missing(cols)) {

    # construct our regular expression to match variable_prefix columns
    variable_prefix_regex <- stringr::str_glue("^{variable_prefix}_")

    # match on the column names
    matching_varnames <- stringr::str_detect(colnames(df), variable_prefix_regex)

    # extract matching column names
    matching_varnames <- colnames(df)[matching_varnames]

    # get the unique levels based on the column names (after the variable_prefix part)
    unique_levels <- stringr::str_remove_all(matching_varnames, variable_prefix_regex)

  } else {
    # we assume the user specified the columns they want to convert to a categorical
    matching_varnames <- cols

    # get the unique levels based on the column names (after the variable_prefix part)
    unique_levels <- matching_varnames
  }

  # create a new vector to store categorical levels in
  new_vector <- rep(NA, nrow(df))

  # create an indicator for if there were logical inconsistencies -- namely if
  # multiple dummy variables are true at the same time, this means the dummy
  # variables cannot be represented by a categorical vector that takes on
  # discrete values one-at-a-time
  level_conflicts <- FALSE

  for (i in 1:length(unique_levels)) {

    # update the new vector to reflect if the i-th dummy variable is TRUE;
    #
    # if so, set the ith value in the new_vector to the level corresponding to
    # unique_levels[[i]].

    for (j in 1:length(new_vector)) {
      # if the dummy variable is true
      new_vector[[j]] <- if(! is.na(df[[matching_varnames[[i]]]][[j]]) && df[[matching_varnames[[i]]]][[j]]) {
        # if the new_vector hasn't already been written into in the jth term
        if (is.na(new_vector[[j]])) {
          # write in the appropriate categorical level
          unique_levels[[i]]
        } else {
          # otherwise not a logical conflict with the dummy variables.
          level_conflicts <- TRUE
        }
        # if the dummy variable is false
      } else {
        # leave the new_vector alone
        new_vector[[j]]
      }
    }
  }

  # raise errors to reflect issues de-dummying the variables
  if (level_conflicts) {
    stop("conflicts in de-dummying variables")
  }

  # create the new variable
  df[[variable_prefix]] <- new_vector

  # if the user specifies, drop the dummy variables
  if (drop_dummies) {
    df <- df %>% select(-matching_varnames)
  }

  return(df)
}

#' Categorize Age by Cutoff
categorize_age_cutoffs <- function(df) {
  df %<>% mutate(age_cat = case_when(AGE < 45 ~ "25-44",
                                     AGE >= 45 ~ "45-64"))
}

#' Add Recruitment Site to Data Frame based on Participant IDs
#' @export
#'
add_rec_site_from_participant_ids <- function(df, var = 'id') {
  df$site <- case_when(
    substr(df[[var]], 1, 1) == 2 ~ "Fenway",
    substr(df[[var]], 1, 1) == 5 ~ "Mattapan",
    substr(df[[var]], 1, 1) == 8 ~ "Harvard Street",
    substr(df[[var]], 1, 1) == 6 ~ "Social Media",
    substr(df[[var]], 1, 1) == 7 ~ "Recruited In-Person"
  )

  return(df)
}



#' Categorize BMI by Cutoffs
categorize_bmi_cutoffs <- function(df) {
  df %<>% mutate(bmi_cat_6_levels = case_when(
    bmi < 18.5 ~ "<18 (\"Underweight\")",
    bmi >= 18.5 & bmi < 25 ~ "[18.5, 25) (\"Healthy Weight\")",
    bmi >= 25 & bmi < 30 ~ "[25, 30) (\"Overweight\")",
    bmi >= 30 & bmi < 35 ~ "[30, 35) (\"Obese Class 1\")",
    bmi >= 35 & bmi < 40 ~ "[35, 40) (\"Obese Class 2\")",
    bmi >= 40 ~ "≥40 (\"Obese Class 3\")") %>%
      factor(
        levels = c(
          "<18 (\"Underweight\")",
          "[18.5, 25) (\"Healthy Weight\")",
          "[25, 30) (\"Overweight\")",
          "[30, 35) (\"Obese Class 1\")",
          "[35, 40) (\"Obese Class 2\")"
          )
      )
    )

  df %<>% mutate(
    bmi_cat_2_levels = case_when(
    bmi < 30 ~ "<30 (\"Not Obese\")",
    bmi >= 30 ~ "≥30 (\"Obese\")") %>%
      factor(
        levels = c(
          "<30 (\"Not Obese\")",
          "≥30 (\"Obese\")"
        )
      )
  )
  return(df)
}


#' Categorize Racialized Groups
#'
categorize_racialized_groups <- function(df) {
  df %<>% mutate(
    race = case_when(

      # white
      race_white == 'White' &
        is.na(race_black) & is.na(race_hisp) & is.na(race_asian) &
        is.na(race_aian) & is.na(race_nhpi) & is.na(race_mena) &
        (
          RACE_8_TEXT %in% c(
            "Ashkenaz Jewish",
            "Boston",
            "some Unknown heritage",
            "European Jewish",
            "Ashkenazi Eastern European Jewish",
            "Ashkenazi Jewish",
            "Semitic",
            "Ashkenazi",
            "Ashkenazi Jew",
            "Portugese",
            "Ashkenazi",
            "Semitic"
          ) | is.na(race_other)
        ) ~ 'White non-Hispanic Only',

      # black
      is.na(race_white) &
        race_black == "Black or African American" &
        is.na(race_hisp) & is.na(race_asian) &
        is.na(race_aian) & is.na(race_nhpi) & is.na(race_mena) &
        (RACE_8_TEXT %in% c("Cape Verdean", "Barbadian", "Black", "west indian") |
           is.na(race_other)) ~ 'Black non-Hispanic Only',

      # hispanic
      is.na(race_white) &
        is.na(race_black) & race_hisp == "Hispanic, Latino/Latina/Latinx, or Spanish" &
        is.na(race_asian) &
        is.na(race_aian) & is.na(race_nhpi) & is.na(race_mena) &
        (
          RACE_8_TEXT %in% c("Portuguese") |
                               is.na(race_other)) ~ 'Hispanic Only',

      # asian
      is.na(race_white) &
        is.na(race_black) & is.na(race_hisp) &
        ! is.na(race_asian) &
        is.na(race_aian) & is.na(race_nhpi) & is.na(race_mena) &
        is.na(race_other) ~ 'Asian Only',

      # Am. Indian / Alaska Native
      is.na(race_white) &
        is.na(race_black) & is.na(race_hisp) &
        is.na(race_asian) &
        ! is.na(race_aian) & is.na(race_nhpi) & is.na(race_mena) &
        is.na(race_other) ~ 'American Indian/Alaska Native Only',

      # native hawaiian or other pac. islander
      is.na(race_white) &
        is.na(race_black) & is.na(race_hisp) &
        is.na(race_asian) &
        is.na(race_aian) & ! is.na(race_nhpi) &
        is.na(race_mena) &
        is.na(race_other) ~ 'Native Hawaiian or Other Pacific Islander Only',

      # mena
      is.na(race_white) &
        is.na(race_black) & is.na(race_hisp) &
        is.na(race_asian) &
        is.na(race_aian) & is.na(race_nhpi) &
        ! is.na(race_mena) &
        is.na(race_other) ~ 'Middle Eastern or North African Only',

      # otherwise
      TRUE ~ 'Two or More Races or Other Racialized Group'
    )
    )

  df$race %<>% factor(
    levels = c(
      "White non-Hispanic Only",
      "Black non-Hispanic Only",
      "Hispanic Only",
      "Asian Only",
      "Middle Eastern or North African Only",
      "Two or More Races or Other Racialized Group"
    ))

  return(df)
}


# zscore function for standardizing continuous measures
zscore <- function(x, na.rm=T) {
  ((x - mean(x, na.rm=na.rm)) / sd(x, na.rm=na.rm))
}

