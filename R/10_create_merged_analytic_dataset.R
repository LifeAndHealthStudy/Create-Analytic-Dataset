#' Create Merged Analytic Dataset
#'
#' This function creates the main analytic dataset for Life + Health study
create_merged_analytic_dataset <- function(complete_only = TRUE) {

  # merge consent data and main survey data
  consent <- prep_consent_data()
  main_survey <- prep_main_survey()
  main_survey %<>% left_join(consent, by = c('id' = 'id'))

  # merge in height and weight data
  height_weight_medications <- read_height_weight_medications()
  height_weight_medications %<>% clean_height_weight_medications()
  main_survey %<>% left_join(height_weight_medications, by = c('id' = 'id'))

  # load indicators for if they completed the IAT as this will be necessary to
  # create a completion column
  iat_completions <- as.numeric(c(iat_version_1_completions(), iat_version_2_completions()))

  # add IAT completions data
  main_survey %<>% mutate(completed_iat = id %in% iat_completions)

  # add site
  main_survey %<>% add_rec_site_from_participant_ids()

  # calculate completions
  main_survey %<>% mutate(
    complete = completed_consent & completed_main_survey & completed_iat
  )

  # filter for complete records
  if (complete_only) {
    main_survey %<>% filter(complete)
  }

  # add in place of birth ICErace and ICEown --------------------------------
  main_survey %<>% ungroup()
  main_survey %<>% add_place_of_birth_measures()


  # add residential absms ---------------------------------------------------
  main_survey %<>% left_join(
    readr::read_csv(system.file("REDACTED_PATH/REDACTED_FILENAME.csv", package = "LifeAndHealth"),
                    col_types = c('idnum' = 'c')),
    by = c("id" = "idnum")
  )


  # add in IAT data ---------------------------------------------------------
  #' These data were processed using the 2014 Nosek et al. paper Table 8 algorithm
  #' https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4259300/
  #'
  #' Good/Bad BIAT scores multiplied by -1 to ensure directionality matches
  #' Target/Perpetrator scores. Positive scores indicated an implicit preference
  #' for the target group and an implicit recognition of members of the non-dominant
  #' social group as the targets of discrimination. Negative scores indicated an
  #' implicit preference for the dominant group and an implicit recognition of
  #' the dominant social group as the target of discrimination.
  iat <- load_iat_data_processed()

  iat_names <- c('D_GB_age','D_PT_age','D_GB_gender','D_PT_gender','D_GB_genderid',
                 'D_PT_genderid','D_GB_hispanic','D_PT_hispanic','D_GB_sexuality',
                 'D_PT_sexuality','D_GB_weight','D_PT_weight','D_GB_white_black',
                 'D_PT_white_black','D_GB_white_color','D_PT_white_color')

  iat %<>%
    group_by(session_id) %>%
    rowwise() %>%
    mutate(n_completed_iat = sum(!is.na(c_across(all_of(iat_names)))))  %>%
    ungroup() %>%
    arrange(desc(n_completed_iat)) %>%
    distinct(idnumber, .keep_all = T)

  main_survey$id %<>% as.character()
  main_survey %<>% left_join(
    iat %>% select(idnumber, contains("D_")) %>% rename_with(stringr::str_replace, pattern = 'D_', replacement = 'iat_'),
    by = c('id' = 'idnumber'))

  # construct EOD measures --------------------------------------------------
  main_survey %<>% construct_eod_measures()
  main_survey %<>% construct_eod_measures2()


  # construct the K6 psychological distress measure -------------------------
  main_survey %<>% construct_K6_measure()
  main_survey %<>% mutate(
      K6_3cat = factor(
        case_when(
          K6_psychological_distress >= 0 & K6_psychological_distress < 5 ~ 'no/low',
          K6_psychological_distress >= 5 & K6_psychological_distress < 13 ~ 'moderate',
          K6_psychological_distress >= 13 ~ 'severe'),
        levels = c('no/low','moderate','severe')),
      K6_moderate_or_severe = factor(case_when(
        K6_psychological_distress >= 0 & K6_psychological_distress < 5 ~ 'no/low',
        K6_psychological_distress >= 5 ~ 'moderate/severe'),
        levels = c('no/low','moderate/severe'))
  )

  # construct political concern scale ---------------------------------------
  main_survey %<>% construct_polcncrn_scale()

  # construct social desirability metric ------------------------------------
  main_survey %<>% construct_social_desirability()

  # create racialized group membership measures -----------------------------

  # add racialized groups
  main_survey %<>% mutate(
    race_white = RACE_1,
    race_hisp = RACE_2,
    race_black = RACE_3,
    race_asian = RACE_4,
    race_aian = RACE_5,
    race_mena = RACE_6,
    race_nhpi = RACE_7,
    race_other = RACE_8
  )

  main_survey %<>% categorize_racialized_groups()

  main_survey %<>% mutate(
    POC_or_WhiteNH = factor(
      ifelse(race == 'White non-Hispanic Only', 'White non-Hispanic', 'Person of Color'),
      levels = c('White non-Hispanic','Person of Color')))


  # create gender measures --------------------------------------------------
  # convert to factors instead of haven labelled type
  main_survey %<>% mutate(across(c(C_WM, GID_TCNG, T_WMNG), haven::as_factor))

  main_survey %<>% mutate(
    cis_woman = C_WM == 'Cisgender woman',
    cis_man = C_WM == 'Cisgender man',
    gmtgnc = GID_TCNG %in% c("Transgender", "Non-binary/genderqueer"),
    gender = case_when(cis_woman ~ 'Cisgender woman',
                       cis_man ~ 'Cisgender man',
                       gmtgnc ~ 'Transgender/Non-binary/Genderqueer'),
    gender_5levels = factor(case_when(
      cis_woman ~ 'Cisgender woman',
      cis_man ~ 'Cisgender man',
      GID_TCNG == 'Non-binary/genderqueer' |
        T_WMNG == "Transgender non-binary/genderqueer" ~ "Transgender non-binary/non-binary/genderqueer",
      T_WMNG == "Transgender man" ~ "Transgender man",
      T_WMNG == "Transgender woman" ~ "Transgender woman"
    ),
    levels = c('Cisgender man',
               'Cisgender woman',
               "Transgender man",
               "Transgender woman",
               "Transgender non-binary/non-binary/genderqueer")))

  # create social group categories for gender modality & identity
  main_survey %<>% mutate(
    Cis_or_NotCis = case_when(GID_TCNG == "Transgender" ~ "Not Cisgender",
                              GID_TCNG == "Non-binary/genderqueer" ~ "Not Cisgender",
                              GID_TCNG == "Cisgender" ~ "Cisgender",
                              TRUE ~ NA_character_),

    Gender_Identity = factor(case_when(gender_5levels == "Cisgender man" ~ "Man",
                                       gender_5levels == "Transgender man" ~ "Man",
                                       gender_5levels == "Cisgender woman" ~ "Woman",
                                       gender_5levels == "Transgender woman" ~ "Woman",
                                       TRUE ~ "Nonbinary/Genderqueer"),
                             levels = c("Man","Woman","Nonbinary/Genderqueer")),
  )

  # create sexuality measures -----------------------------------------------
  main_survey %<>% mutate(
    sexuality_7_levels = case_when(
      ! is.na(SEX_OR_1) & is.na(SEX_OR_2) & is.na(SEX_OR_3) & is.na(SEX_OR_4) & is.na(SEX_OR_5) & is.na(SEX_OR_6) ~ "Straight or heterosexual only",
      is.na(SEX_OR_1) & ! is.na(SEX_OR_2) & is.na(SEX_OR_3) & is.na(SEX_OR_4) & is.na(SEX_OR_5) & is.na(SEX_OR_6) ~ "Gay or lesbian only",
      is.na(SEX_OR_1) & is.na(SEX_OR_2) & ! is.na(SEX_OR_3) & is.na(SEX_OR_4) & is.na(SEX_OR_5) & is.na(SEX_OR_6) ~ "Bisexual only",
      is.na(SEX_OR_1) & is.na(SEX_OR_2) & is.na(SEX_OR_3) & ! is.na(SEX_OR_4) & is.na(SEX_OR_5) & is.na(SEX_OR_6) ~ "Queer only",
      is.na(SEX_OR_1) & is.na(SEX_OR_2) & is.na(SEX_OR_3) & is.na(SEX_OR_4) & ! is.na(SEX_OR_5) & is.na(SEX_OR_6) ~ "Same-gender loving only",
      is.na(SEX_OR_1) & is.na(SEX_OR_2) & is.na(SEX_OR_3) & is.na(SEX_OR_4) & is.na(SEX_OR_5) & ! is.na(SEX_OR_6) ~ "[Write-in] only",
      TRUE ~ "Two or more sexualities"
  ) %>% factor(levels = c(
    "Straight or heterosexual only",
    "Gay or lesbian only",
    "Bisexual only",
    "Queer only",
    "Same-gender loving only",
    "[Write-in] only",
    "Two or more sexualities"
    )))

  main_survey %<>% mutate(
    sexuality_straight = SEX_OR_1,
    sexuality_gay_lesbian = SEX_OR_2,
    sexuality_bisexual = SEX_OR_3,
    sexuality_queer = SEX_OR_4,
    sexuality_same_gender_loving = SEX_OR_5,
    sexuality_write_in = SEX_OR_6,
    sexuality_write_in_text = SEX_OR_6_TEXT
    )

  main_survey %<>% mutate(
    smlgbq =
        ! is.na(sexuality_gay_lesbian) |
          ! is.na(sexuality_bisexual) |
          ! is.na(sexuality_queer) |
          ! is.na(sexuality_same_gender_loving) |
          ! is.na(sexuality_write_in),
    straight = ! is.na(sexuality_straight),
    sexuality = case_when(smlgbq ~ 'LGBQ',
                          straight ~ 'Straight/Heterosexual'
                          ))

  main_survey %<>% mutate(
    sexuality_10levels =
      factor(case_when(
        sexuality_7_levels == "Straight or heterosexual only" ~ "Straight or heterosexual",
        sexuality_7_levels == "Gay or lesbian only" ~ "Gay or lesbian",
        sexuality_7_levels == "Bisexual only" ~ "Bisexual",
        sexuality_7_levels == "Queer only" ~ "Queer",
        sexuality_7_levels == "Same-gender loving only" ~ "Same-gender loving",
        sexuality_7_levels == "Two or more sexualities" ~ "Two or more sexualities",
        sexuality_7_levels == "[Write-in] only" & str_detect(sexuality_write_in_text,"A|asexual") ~ "Asexual",
        sexuality_7_levels == "[Write-in] only" & str_detect(sexuality_write_in_text,"P|pansexual") ~ "Pansexual",
        sexuality_7_levels == "[Write-in] only" & str_detect(sexuality_write_in_text,"striaght") ~ "Straight or heterosexual",
        sexuality_7_levels == "[Write-in] only" & str_detect(sexuality_write_in_text,"I have no idea") ~ "Questioning",
        TRUE ~ "Unspecified"),
        levels = c("Straight or heterosexual","Gay or lesbian","Queer",
                   "Two or more sexualities","Bisexual","Asexual","Pansexual",
                   "Same-gender loving","Questioning","Unspecified")),
    LGBQ_or_Straight = case_when(
      sexuality_10levels  %in% c("Gay or lesbian","Bisexual","Queer",
                                 "Same-gender loving","Two or more sexualities","Asexual",
                                "Pansexual","Questioning") ~ "LGBQ",
      sexuality_10levels == "Straight or heterosexual" ~ "Straight/Heterosexual",
                                 TRUE ~ NA_character_),

    )


  # add BMI -----------------------------------------------------------------
  main_survey %<>% mutate(bmi = 703 * weight_lbs / (height_ft * 12)^2)

  # add classifications for sleep affecting drugs
  main_survey %<>% left_join(classify_medications_for_participants(), by = c('id' = 'id'))

  # overwrite with cleaned sleep data ---------------------------------------
  main_survey %<>% overwrite_sleep_data_with_cleaned_version()

  main_survey %<>% mutate(insufficient_sleep_lt_7hrs = sleep_duration_weekdays < 7)
  # 3-category sleep duration (insufficient, sufficient & long sleep)
  main_survey %<>% mutate(
    sleep_duration_cat = factor(case_when(
    sleep_duration_weekdays < 8 ~ 'insufficient_sleep',
    sleep_duration_weekdays >= 8 & sleep_duration_weekdays <= 9 ~ 'sufficient_sleep',
    sleep_duration_weekdays > 9 ~ 'long_sleep'),
    levels = c('insufficient_sleep','sufficient_sleep','long_sleep')))

  # add sleep disturbance data -------------------------------------------------
  main_survey %<>% construct_slp_disturb_measure()

  # add sleep impairment data --------------------------------------------------
  main_survey %<>% construct_slp_impair_measure()

  # add age categories
  main_survey %<>% categorize_age_cutoffs()

  # add bmi categories
  main_survey %<>% categorize_bmi_cutoffs()

  # add educational attainment categories
  main_survey %<>% mutate(
    edu_4cat = factor(case_when(EDUC %in% c(1,2,3) ~ "No college",
                                EDUC %in% c(4,5) ~ "Some college/Vocational school",
                                EDUC == 6 ~ "4 years of college",
                                EDUC == 7 ~ "Graduate degree"),
                      levels = c("Graduate degree","4 years of college",
                                 "Some college/Vocational school","No college")))

  # add marital/relationship status vars
  main_survey %<>% mutate(
    ever_married = factor(case_when(MAR_EVER == 1 ~ "Yes",
                                    MAR_EVER == 2 ~ "No",
                                    TRUE ~ NA_character_),
                          levels = c("Yes","No")),

    curr_relationship_status = factor(case_when(
              MAR_EVER == 1 & MAR_STAT == 1 ~ "Married",
              MAR_EVER == 1 & MAR_STAT %in% c(5,6) ~ "In a relationship",
              MAR_EVER == 1 & MAR_STAT %in% c(2,3) ~ "Divorced/Separated",
              MAR_EVER == 1 & MAR_STAT == 4 ~ "Widowed",
              MAR_EVER == 1 & MAR_STAT == 7 ~ "Other",
              MAR_EVER == 2 & MAR_NSTAT %in% c(1,2) ~ "In a relationship",
              MAR_EVER == 2 & MAR_NSTAT == 3 ~ "Single",
              MAR_EVER == 2 & MAR_NSTAT == 4 ~ "Other"),
           levels = c("Married","In a relationship","Single",
                      "Divorced/Separated","Widowed","Other")))

  # add recruitment site
  main_survey %<>% mutate(site = factor(
    ifelse(site %in% c('Recruited In-Person','Social Media'),'CHC affiliate',site),
           levels = c('Fenway','Mattapan','Harvard Street','CHC affiliate')))

  # add health behavior variables ----------------------------------------
  main_survey %<>% mutate(
    smoker_status = factor(case_when(SMOK_100 == 1 & SMOK_FREQ %in% c(1,2) ~ "Current Smoker", #lifetime
                                     SMOK_100 == 2 & SMOK_FREQ %in% c(1,2) ~ "Current Smoker", #new smokers
                                     SMOK_100 == 1 & SMOK_FREQ  == 3 ~ "Former Smoker",
                                     SMOK_100 == 2 & SMOK_FREQ  == 3~ "Never Smoker",
                                     TRUE ~ NA_character_),
                         levels = c("Current Smoker","Former Smoker","Never Smoker")),

    smokeless_tob = factor(case_when(TOBAC_FREQ %in% c(1,2) ~ "Yes",
                                     TOBAC_FREQ  == 3 ~ "No",
                                     TRUE ~ NA_character_),
                           levels = c("Yes","No")),

    ever_vape = factor(case_when(VAPE_EVER == 1 ~ "Yes",
                                 VAPE_EVER == 2 ~ "No",
                                 TRUE ~ NA_character_),
                       levels = c("Yes","No")),

    vape_status = factor(case_when(VAPE_EVER == 1 & VAPE_FREQ %in% c(1,2) ~ "Regular Vaper",
                                   VAPE_EVER == 1 & VAPE_FREQ  == 3 ~ "Irregular Vaper",
                                   VAPE_EVER == 2 ~ "Never Vaper",
                                   TRUE ~ NA_character_),
                         levels = c("Regular Vaper","Irregular Vaper", "Never Vaper")),

    smoke_or_vape =  factor(case_when(smoker_status == "Current Smoker" | vape_status == "Regular Vaper" ~ "Yes",
                                      TRUE ~ "No"),
                            levels = c("Yes","No")),

    binge_drink = factor(case_when(DRUG_SLP_1_1_1 == 1 ~ "Yes",
                                   TRUE ~ "No"),
                         levels = c("Yes","No")),

    exercise = case_when(FITNESS == "0" ~ "0 days",
                         FITNESS %in% c("1","2","3","03") ~ "1-3 days",
                         FITNESS %in% c("4","5","6","7") ~ "4-7 days"),

    # add self-report substance use variables ---------------------------
    cannabis_use = factor(case_when(DRUG_SLP_1_1_2 == 1 ~ "Yes",
                                    TRUE ~ "No"),
                          levels = c("Yes","No")),

    painkiller_use = factor(case_when(DRUG_SLP_1_1_10 == 1 ~ "Yes", # Heroin
                                      DRUG_SLP_1_1_11 == 1 ~ "Yes", # Fentanyl
                                      DRUG_SLP_1_1_12 == 1 ~ "Yes", # vicodin, OcyVontin, Percocet
                                      DRUG_SLP_1_1_13 == 1 ~ "Yes", # Opana
                                      DRUG_SLP_1_1_14 == 1 ~ "Yes", # Morphine, Kadioan, or Avinza
                                      DRUG_SLP_1_1_15 == 1 ~ "Yes", # Codeine
                                      TRUE ~ "No"),
                            levels = c("Yes","No")),


    stimulants_use = factor(case_when(DRUG_SLP_1_1_4 == 1 ~ "Yes", # Cocaine
                                      DRUG_SLP_1_1_8 == 1 ~ "Yes", # Methamphetamine
                                      TRUE ~ "No"),
                            levels = c("Yes","No")),

    poppers = factor(case_when(DRUG_SLP_1_1_9 == 1 ~ "Yes", #poppers
                               TRUE ~ "No"),
                     levels = c("Yes","No")),

    hallucinogen_use = factor(case_when(DRUG_SLP_1_1_3 == 1 ~ "Yes", # LSD
                                        DRUG_SLP_1_1_5 == 1 ~ "Yes", # Ecstasy
                                        DRUG_SLP_1_1_6 == 1 ~ "Yes", # Ketamine
                                        TRUE ~ "No"),
                              levels = c("Yes","No")),

    other_drug_use = factor(case_when(
      painkiller_use == 'Yes' | stimulants_use == 'Yes'|
        poppers == 'Yes'| hallucinogen_use == 'Yes' ~ 'Yes',
      TRUE ~ 'No'), levels = c('Yes','No')),

    slp_drink = factor(case_when(DRUG_SLP_2_1_1 == 1 ~ "Yes",
                                 TRUE ~ "No"),
                       levels = c("Yes","No")),

    slp_cannabis = factor(case_when(DRUG_SLP_2_1_2 == 1 ~ "Yes",
                                    TRUE ~ "No"),
                          levels = c("Yes","No")),

    slp_hallucinogens = factor(case_when(DRUG_SLP_2_1_3 == 1 ~ "Yes", # LSD
                                         DRUG_SLP_2_1_5 == 1 ~ "Yes", # Ecstasy
                                         DRUG_SLP_2_1_6 == 1 ~ "Yes", # Ketamine
                                         TRUE ~ "No"),
                               levels = c("Yes","No")),

    slp_painkillers = factor(case_when(DRUG_SLP_2_1_10 == 1 ~ "Yes", # Heroin
                                       DRUG_SLP_2_1_11 == 1 ~ "Yes", # Fentanyl
                                       DRUG_SLP_2_1_12 == 1 ~ "Yes", # vicodin, OcyVontin, Percocet
                                       DRUG_SLP_2_1_13 == 1 ~ "Yes", # Opana
                                       DRUG_SLP_2_1_14 == 1 ~ "Yes", # Morphine, Kadioan, or Avinza
                                       DRUG_SLP_2_1_15 == 1 ~ "Yes", # Codeine
                                       TRUE ~ "No"),
                             levels = c("Yes","No")),

    slp_stimulants = factor(case_when(DRUG_SLP_2_1_4 == 1 ~ "Yes", # Cocaine
                                      DRUG_SLP_2_1_8 == 1 ~ "Yes", # Methamphetamine
                                      TRUE ~ "No"),
                            levels = c("Yes","No")),

    slp_poppers = factor(case_when(DRUG_SLP_2_1_9 == 1 ~ "Yes", #nitrite inhalers/poppers
                                   TRUE ~ "No"),
                         levels = c("Yes","No")),

    slp_other_drug = factor(case_when(
      slp_painkillers == 'Yes' | slp_stimulants == 'Yes'|
      slp_poppers == 'Yes'| slp_hallucinogens == 'Yes' ~ 'Yes',
      TRUE ~ 'No'), levels = c('Yes','No')),

    # add medical record rx & substance use variables ------------------------
    # be advised only n = 11 participants are actually missing medical record
    # data. many participants have a medical record, but there may have been no
    # medications listed
    missing_med_record = case_when(id %in% c("id_1","id_2","id_3","...","id_11") ~ 1, # Placeholder study identifiers; actual IDs are restricted
                                   TRUE ~ 0),
    # taking psych meds
    rx_addressing_psychological_distress = factor(
      case_when(
        # has medical record & taking psych meds
        missing_med_record == 0 & rx_addressing_psychological_distress == T ~ "Yes",
        # has medical record but not psych meds
        missing_med_record == 0 & rx_addressing_psychological_distress == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_addressing_psychological_distress) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_psychological_distress_side_effect = factor(
      case_when(
        # has medical record & taking meds w/ psych side effects
        missing_med_record == 0 & rx_psychological_distress_side_effect == T ~ "Yes",
        # has medical record but not meds w/ psych side effects
        missing_med_record == 0 & rx_psychological_distress_side_effect == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_psychological_distress_side_effect) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_address_sleep_disorders = factor(
      case_when(
        # has medical record & taking meds for sleep
        missing_med_record == 0 & rx_address_sleep_disorders == T ~ "Yes",
        # has medical record but not taking meds for sleep
        missing_med_record == 0 & rx_address_sleep_disorders == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_address_sleep_disorders) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_otc_address_sleep_disorders = factor(
      case_when(
        # has medical record & taking otc meds to help sleep
        missing_med_record == 0 & rx_otc_address_sleep_disorders == T ~ "Yes",
        # has medical record but not otc meds to help sleep
        missing_med_record == 0 & rx_otc_address_sleep_disorders == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_otc_address_sleep_disorders) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_sleep_side_effect = factor(
      case_when(
        # has medical record & taking meds that affect sleep as a side effect
        missing_med_record == 0 & rx_sleep_side_effect == T ~ "Yes",
        # has medical record but not meds that affect sleep as a side effect
        missing_med_record == 0 & rx_sleep_side_effect == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_sleep_side_effect) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_other_substances = factor(
      case_when(
        # has medical record & taking other meds
        missing_med_record == 0 & rx_other_substances == T ~ "Yes",
        # has medical record but not taking other meds
        missing_med_record == 0 & rx_other_substances == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_other_substances) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_address_sleep_disorders_drowsy = factor(
      case_when(
        # has medical record & taking meds for sleep that result in drowsiness
        missing_med_record == 0 & rx_address_sleep_disorders_drowsy == T ~ "Yes",
        # has medical record but taking meds for sleep that result in drowsiness
        missing_med_record == 0 & rx_address_sleep_disorders_drowsy == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_address_sleep_disorders_drowsy) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_address_sleep_disorders_wakeful = factor(
      case_when(
        # has medical record & taking meds for sleep that result in wakefulness
        missing_med_record == 0 & rx_address_sleep_disorders_wakeful == T ~ "Yes",
        # has medical record but taking meds for sleep that result in wakefulness
        missing_med_record == 0 & rx_address_sleep_disorders_wakeful == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_address_sleep_disorders_wakeful) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_otc_address_sleep_disorders_drowsy = factor(
      case_when(
        # has medical record & taking otc meds for sleep that result in drowsiness
        missing_med_record == 0 & rx_otc_address_sleep_disorders_drowsy == T ~ "Yes",
        # has medical record but taking otc meds for sleep that result in drowsiness
        missing_med_record == 0 & rx_otc_address_sleep_disorders_drowsy == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_otc_address_sleep_disorders_drowsy) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_otc_address_sleep_disorders_wakeful = factor(
      case_when(
        # has medical record & taking otc meds for sleep that result in wakefulness
        missing_med_record == 0 & rx_otc_address_sleep_disorders_wakeful == T ~ "Yes",
        # has medical record but taking otc meds for sleep that result in wakefulness
        missing_med_record == 0 & rx_otc_address_sleep_disorders_wakeful == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_otc_address_sleep_disorders_wakeful) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_sleep_side_effect_drowsy = factor(
      case_when(
        # has medical record & taking meds that affect sleep as side effect that
        # result in drowsiness
        missing_med_record == 0 & rx_sleep_side_effect_drowsy == T ~ "Yes",
        # has medical record but not taking meds that affect sleep as side effect
        # that result in drowsiness
        missing_med_record == 0 & rx_sleep_side_effect_drowsy == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_sleep_side_effect_drowsy) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),

    rx_sleep_side_effect_wakeful = factor(
      case_when(
        # has medical record & taking meds that affect sleep as side effect that
        # result in wakefulness
        missing_med_record == 0 & rx_sleep_side_effect_wakeful == T ~ "Yes",
        # has medical record but not taking meds that affect sleep as side effect
        # that result in wakefulness
        missing_med_record == 0 & rx_sleep_side_effect_wakeful == F ~ "No",
        # no medications listed in the chart
        missing_med_record == 0 & is.na(rx_sleep_side_effect_wakeful) ~ "No",
        # truly missing medical record
        TRUE ~ NA_character_),
      levels = c("Yes","No")),


    # add medical conditions -----------------------------------------------
    # cardiovascular conditions
    cv_cond = factor(case_when(DIAGNOSE_1_1 == 1 ~ "Yes", # heart attack
                               DIAGNOSE_1_2 == 1 ~ "Yes", # angina or coronary heart disease
                               DIAGNOSE_1_3 == 1 ~ "Yes", # stroke
                               TRUE ~ "No"),
                     levels = c("Yes","No")),
    # ever asthma
    ever_asthma = factor(case_when(DIAGNOSE_1_4 == 1 ~ "Yes",
                                   TRUE ~ "No"),
                         levels = c("Yes","No")),
    # current asthma
    curr_asthma = factor(case_when(DIAGNOSE_1_4 == 1 & DIAG_EX_1 == 1 ~ "Yes",
                                   DIAGNOSE_1_4 == 1 & DIAG_EX_1 == 16 ~ "No",
                                   TRUE ~ "No"),
                         levels = c("Yes","No")),
    # cancer
    cancer = factor(case_when(DIAGNOSE_1_5 == 1 ~ "Yes", # skin cancer
                              DIAGNOSE_1_6 == 1 ~ "Yes", # other cancer
                              TRUE ~ "No"),
                    levels = c("Yes","No")),
    #  COPD
    clrd_cond = factor(case_when(DIAGNOSE_1_7 == 1 ~ "Yes",
                                 TRUE ~ "No"),
                       levels = c("Yes","No")),
    # arthritis
    arthritis = factor(case_when(DIAGNOSE_1_8 == 1 ~ "Yes",
                                 TRUE ~ "No"),
                       levels = c("Yes","No")),
    # depression
    depression = factor(case_when(DIAGNOSE_1_9 == 1 ~ "Yes",
                                  TRUE ~ "No"),
                        levels = c("Yes","No")),
    # kidney disease
    kidney_disease = factor(case_when(DIAGNOSE_1_10 == 1 ~ "Yes",
                                      TRUE ~ "No"),
                            levels = c("Yes","No")),
    # diabetes
    diabetes = factor(case_when(DIAGNOSE_1_11 == 1 ~ "Yes",
                                TRUE ~ "No"),
                      levels = c("Yes","No")),
    # HIV
    HIV = factor(case_when(DIAGNOSE_1_12 == 1 ~ "Yes",
                           TRUE ~ "No"),
                 levels = c("Yes","No")),
    # mental illness
    mental_illness = factor(case_when(DIAGNOSE_1_13 == 1 ~ "Yes",
                                      TRUE ~ "No"),
                            levels = c("Yes","No")),
    # organ failure
    organ_failure = factor(case_when(DIAGNOSE_1_14 == 1 ~ "Yes",
                                     TRUE ~ "No"),
                           levels = c("Yes","No")),
    # substance abuse
    sub_abuse = factor(case_when(DIAGNOSE_1_15 == 1 ~ "Yes",
                                 TRUE ~ "No"),
                       levels = c("Yes","No")),

    # covid-19 diagnosis
    covid = factor(case_when(DIAGNOSE_1_16 == 1 ~ "COVID-19 Diagnosis",
                             DIAGNOSE_1_16 == 2 & DIAG_EX_3 == 1 ~ "No Diagnosis but suspect they may have",
                             DIAGNOSE_1_16 == 2 & DIAG_EX_3 == 3 ~ "No diagnosis but not sure",
                             DIAGNOSE_1_16 == 2 & DIAG_EX_3 == 2 ~ "Did not have COVID-19",
                             TRUE ~ NA_character_),
                   levels = c("COVID-19 Diagnosis","No Diagnosis but suspect they may have",
                              "No diagnosis but not sure","Did not have COVID-19")),
    # household diagnosis
    hh_covid = factor(case_when(DIAG_EX_4 == 1 ~ "Yes",
                                DIAG_EX_4 == 2 ~ "No",
                                DIAG_EX_4 == 3 ~ "Unsure",
                                DIAG_EX_4 == 4 ~ "Live alone"),
                      levels = c("Yes","No","Unsure","Live alone")),


    # clean self-report/explicit discrimination measures -----------------

    # create 3 category EOD domains
    EOD_racial_3levels = case_when(EOD_racial_Ndomain < 1 ~ "0",
                                   EOD_racial_Ndomain >= 1  & EOD_racial_Ndomain < 3 ~ "1-2",
                                   EOD_racial_Ndomain >= 3 ~ "3+"),

    EOD_man_or_woman_3levels = case_when(EOD_man_or_woman_Ndomain < 1 ~ "0",
                                         EOD_man_or_woman_Ndomain >= 1  & EOD_man_or_woman_Ndomain < 3 ~ "1-2",
                                         EOD_man_or_woman_Ndomain >= 3 ~ "3+"),

    EOD_gender_3levels = case_when(EOD_gender_Ndomain < 1 ~ "0",
                                   EOD_gender_Ndomain >= 1  & EOD_gender_Ndomain < 3 ~ "1-2",
                                   EOD_gender_Ndomain >= 3 ~ "3+"),

    EOD_orientation_3levels = case_when(EOD_orientation_Ndomain < 1 ~ "0",
                                        EOD_orientation_Ndomain >= 1  & EOD_orientation_Ndomain < 3 ~ "1-2",
                                        EOD_orientation_Ndomain >= 3 ~ "3+"),

    EOD_age_3levels = case_when(EOD_age_Ndomain < 1 ~ "0",
                                EOD_age_Ndomain >= 1  & EOD_age_Ndomain < 3 ~ "1-2",
                                EOD_age_Ndomain >= 3 ~ "3+"),

    EOD_weight_3levels = case_when(EOD_weight_Ndomain < 1 ~ "0",
                                   EOD_weight_Ndomain >= 1  & EOD_weight_Ndomain < 3 ~ "1-2",
                                   EOD_weight_Ndomain >= 3 ~ "3+"),

    # create binary EOD domains
    EOD_racial_2levels = case_when(EOD_racial_Ndomain < 1 ~ "0",
                                   EOD_racial_Ndomain >= 1 ~ "1+"),

    EOD_man_or_woman_2levels = case_when(EOD_man_or_woman_Ndomain < 1 ~ "0",
                                         EOD_man_or_woman_Ndomain >= 1 ~ "1+"),

    EOD_gender_2levels = case_when(EOD_gender_Ndomain  < 1 ~ "0",
                                   EOD_gender_Ndomain  >= 1 ~ "1+"),

    EOD_orientation_2levels = case_when(EOD_orientation < 1 ~ "0",
                                        EOD_orientation >= 1~ "1+"),

    EOD_age_2levels = case_when(EOD_age_Ndomain  < 1 ~ "0",
                                EOD_age_Ndomain  >= 1~ "1+"),

    EOD_weight_2levels = case_when(EOD_weight_Ndomain < 1 ~ "0",
                                   EOD_weight_Ndomain >= 1~ "1+"),

    ###------ TARGET/PERPETRATOR STATUS INDICATORS
    racial_discrim_target = case_when(POC_or_WhiteNH == 'Person of Color' ~ 1,
                                      TRUE ~ 0),
    gender_discrim_target = case_when(Gender_Identity != 'Man' ~ 1,
                                      TRUE ~ 0),
    SO_discrim_target = case_when(LGBQ_or_Straight == 'LGBQ' ~ 1,
                                  TRUE ~ 0),
    GI_discrim_target = case_when(Cis_or_NotCis == 'Not Cisgender' ~ 1,
                                  TRUE ~ 0),
    age_discrim_target = case_when(age_cat == '45-64' ~ 1,
                                   TRUE ~ 0),
    wbd_discrim_target = case_when(bmi_cat_2_levels == '≥30 ("Obese")' ~ 1,
                                   TRUE ~ 0),
    n_domains_target = rowSums(across(c(racial_discrim_target,gender_discrim_target,SO_discrim_target,
                                        GI_discrim_target,age_discrim_target,wbd_discrim_target))),


    # add measures of childhood racialized & economic adversity

    born_jim_crow = case_when(BORN_STATE %in% c('Texas', 'Oklahoma', 'Missouri',
                                                'Arkansas', 'Louisiana', 'Mississippi',
                                                'Tennessee', 'Kentucky', 'Alabama',
                                                'Georgia', 'Florida','South Carolina',
                                                'North Carolina', 'Virginia', 'West Virginia',
                                                'District of Columbia', 'Maryland', 'Delaware') ~ "Yes",
                              TRUE ~ "No"),

    occupational_class = factor(case_when(M_MAIN == 4 ~ 'Non-supervisory employee',
                                          M_MAIN %in% c(1,2,3) ~ 'Owner, self-employed, or supervisory employee',
                                          M_MAIN %in% c(5,6) ~ 'Unemployed or not in  the paid labor',
                                          M_MAIN == 7 ~ 'Other',
                                          M_MAIN == 8 ~ 'Not specified'),
                                levels = c ('Non-supervisory employee',
                                            'Owner, self-employed, or supervisory employee',
                                            'Unemployed or not in  the paid labor',
                                            'Other',
                                            'Not specified')),
    housing_tenure = factor(case_when(M_HOMEOWN == 1 ~ 'Home owned with a mortgage/loan',
                                      M_HOMEOWN == 2 ~ 'Home owned free and clear',
                                      M_HOMEOWN == 3 ~ 'Rent home',
                                      M_HOMEOWN == 4 ~ 'Occupied without cash payment of rent',
                                      M_HOMEOWN == 5 ~ 'Not specified',
                                      TRUE ~ 'Homeless'),
                            levels = c('Home owned with a mortgage/loan',
                                       'Home owned free and clear',
                                       'Rent home',
                                       'Occupied without cash payment of rent',
                                       'Not specified',
                                       'Homeless')),
    chld_e_insecurity = factor(case_when(M_YOUTH_1_1 == 1|
                                           M_YOUTH_1_2 == 1|
                                           M_YOUTH_1_3 == 1 |
                                           M_YOUTH_1_4 == 1 ~ 'Yes',
                                         TRUE ~ 'No'),
                               levels = c("Yes","No")),
    adult_e_insecurity = factor(case_when(M_12_MTH_1_1 == 1|
                                            M_12_MTH_1_2 == 1|
                                            M_12_MTH_1_3 == 1 |
                                            M_12_MTH_1_4 == 1 ~ 'Yes',
                                          TRUE ~ 'No'),
                                levels = c("Yes","No")),
    hh_n_adult = case_when(
      M_ADULT == 1 ~ '1',
      M_ADULT == 2 ~ '2',
      M_ADULT == 3 ~ '3',
      M_ADULT == 4 ~ '4',
      M_ADULT == 5 ~ '5',
      M_ADULT == 6 ~ '6+'),

    hh_n_child = case_when(
      M_CHLDN == 1 ~ '0',
      M_CHLDN == 2 ~ '1',
      M_CHLDN == 3 ~ '2',
      M_CHLDN == 4 ~ '3',
      M_CHLDN == 5 ~ '4',
      M_CHLDN == 6 ~ '5',
      M_CHLDN == 7 ~ '6+')

      )

    # convert explicit attitudes to 7pt scale (-3 to +3)
    main_survey %<>% mutate_at(vars(REC_PREF, SG_PREF,GID_PREF,SXO_PREF,LBS_PREF,AGE_PREF),
                               ~ scale_to_range_7pt(.))

    main_survey %<>% mutate_at(vars(REC_PREF,SG_PREF,GID_PREF,SXO_PREF,LBS_PREF,AGE_PREF),
                               ~ .*(-1)) # flip the sign direction

    # convert explicit group discrimination to 4pt scale (0 to +3)
    main_survey %<>% mutate_at(vars(REC_GRFREQ,SG_GRFREQ,GID_GRFREQ,SXO_GRFREQ,LBS_GRFREQ,AGE_GRFREQ),
                               ~scale_to_range_4pt(.))

    # convert food insecurity vars to binary
    main_survey %<>%  mutate_at(vars(starts_with('M_FOOD_1')), ~ scale_food_insecurity(.))

    # create overall food insecurity variable
    main_survey %<>% mutate(food_insecurity = factor(
      case_when(M_FOOD_1_1 %in% c(1,2) ~ "Yes",
                M_FOOD_1_2 %in% c(1,2) ~ "Yes",
                M_FOOD_1_3 %in% c(1,2) ~ "Yes",
                TRUE ~ "No"),
      levels = c("Yes","No")))

    main_survey %<>% mutate(
      pol_activism = rowSums(select(., POLACT_1, POLACT_2, POLACT_3), na.rm = TRUE),
      pol_activism_cat = ifelse(pol_activism >= 1,'Yes','No'))

  # make sure the data are not grouped before returning
  main_survey %<>% ungroup()

  # construct single race IAT measure
  main_survey %<>% construct_single_race_iat()

  # create cross-discrimination type implicit/explicit recognition and preference totals
  main_survey %<>% construct_n_types_discrim()
  main_survey %<>% construct_eod_domain_total()
  main_survey %<>% construct_PT_iat_total()
  main_survey %<>% construct_GB_iat_total()
  main_survey %<>% construct_exp_recog_total()
  main_survey %<>% construct_exp_pref_total()


  return(main_survey)
}


#' Prepare Consent Data
#'
prep_consent_data <- function() {

  # load the original data
  consent <- load_consent_survey()

  # clean up the ID numbers
  consent$CONSENT %<>% readr::parse_number()
  consent %<>% rename(id = CONSENT)

  # drop extra data before merging everything together
  consent %<>% select(id:BORN_CITY, -C_VERIFY, EndDate, Finished)

  # Convert Finished to completed_consent
  consent %<>% rename(completed_consent = Finished)
  consent %<>% mutate(completed_consent = as.logical(haven::as_factor(completed_consent)))

  # process date of birth / age info
  consent %<>% mutate(
    DOB = lubridate::mdy(DOB),  # convert to Date type
    AGE_FROM_DOB = # calculate age based on DOB
      lubridate::interval(
        DOB,
        lubridate::as_date(lubridate::ymd_hms(EndDate))) %>%
      lubridate::as.period() %>%
      lubridate::year()
  )

  # convert AGE from a labelled factor to numeric
  consent %<>% mutate(AGE = as.integer(as.character(haven::as_factor(AGE))))

  # if AGE_FROM_DOB is off while AGE is in the right range, assume that the AGE
  # is correct and calculate an estimated DOB
  consent %<>% mutate(
    DOB = if_else((abs(AGE_FROM_DOB) >= 65 |
                     AGE_FROM_DOB < 25) & AGE %in% 25:64,
                  lubridate::as_date(EndDate) - lubridate::years(AGE),
                  DOB
    ))

  # convert labelled haven BORN_STATE and RACE_* categories to factors
  consent %<>% mutate(across(where(haven::is.labelled), haven::as_factor))

  # save only the completed consent survey data
  consent %<>% filter(completed_consent)

  # remove duplicated entries
  consent %<>% group_by(id) %>% filter(EndDate == min(EndDate))

  return(consent)
}


