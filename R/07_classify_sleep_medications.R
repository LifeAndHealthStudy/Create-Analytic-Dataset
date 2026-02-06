
#' Load Medications that Can Affect Sleep
#'
load_medications_that_can_affect_sleep <- function() {

  # read in data tables detailing different classes of drugs

  # addressing psychological distress
  addressing_psychological_distress <- readxl::read_excel(
    system.file(
      'sleep_medications/sleep_affecting_medications_AO update.xlsx',
      package = 'LifeAndHealth'
    ),
    sheet = 1,
    skip = 2
  )

  # drugs with psychological distress as a side effect
  psychological_distress_side_effect <- readxl::read_excel(
    system.file(
      'sleep_medications/sleep_affecting_medications_AO update.xlsx',
      package = 'LifeAndHealth'
    ),
    sheet = 2,
    skip = 2
  )

  # drugs to treat sleep disorders
  address_sleep_disorders <-
    bind_rows(
      # sleep medications based on the mechanism of action
      readxl::read_excel(
        system.file(
          'sleep_medications/sleep_affecting_medications_AO update.xlsx',
          package = 'LifeAndHealth'
        ),
        sheet = 3,
        range = "A5:D19"
      ),
      # used to address sleep-related breathing disorders
      readxl::read_excel(
        system.file(
          'sleep_medications/sleep_affecting_medications_AO update.xlsx',
          package = 'LifeAndHealth'
        ),
        sheet = 3,
        range = "A23:D26"
      )
    )

  # over-the-counter drugs prescribed or used for sleep
  otc_address_sleep_disorders <- readxl::read_excel(
    system.file(
      'sleep_medications/sleep_affecting_medications_AO update.xlsx',
      package = 'LifeAndHealth'
    ),
    sheet = 4,
    range = "A5:C8"
  )

  # Medications that can affect sleep as a side effect
  sleep_side_effect <-
    bind_rows(
      # Drugs that can interfere with sleep
      readxl::read_excel(
        system.file(
          'sleep_medications/sleep_affecting_medications_AO update.xlsx',
          package = 'LifeAndHealth'
        ),
        sheet = 5,
        range = "A4:D60"
      ),
      # Drugs that can worsen sleep-related breathing disorders:
      readxl::read_excel(
        system.file(
          'sleep_medications/sleep_affecting_medications_AO update.xlsx',
          package = 'LifeAndHealth'
        ),
        sheet = 5,
        range = "A63:B75"
      ),
      # Drugs that can potentially improve sleep-related breathing disorders11:
      readxl::read_excel(
        system.file(
          'sleep_medications/sleep_affecting_medications_AO update.xlsx',
          package = 'LifeAndHealth'
        ),
        sheet = 5,
        range = "A78:B85"
      )
    )

  # other substances
  other_substances <- readxl::read_excel(
    system.file('sleep_medications/sleep_affecting_medications_AO update.xlsx',
                package = 'LifeAndHealth'),
    sheet = 6,
    range = "A3:C14"
  )


  # list together for use
  drugs <- list(
    addressing_psychological_distress = addressing_psychological_distress,
    psychological_distress_side_effect = psychological_distress_side_effect,
    address_sleep_disorders = address_sleep_disorders,
    otc_address_sleep_disorders = otc_address_sleep_disorders,
    sleep_side_effect = sleep_side_effect,
    other_substances = other_substances
  )

  return(drugs)
}

#' Classify Medications
#'
#' @examples
#' medication_chr <- c(
#' "bupropion hcl er (xl) 150 mg xr24h-tab",
#' "glucophage xr 750 mg oral tablet extended release 24 hour",
#' "lisinopril 10 mg tabs",
#' "naltrexone hcl 50 mg oral tablet",
#' "wellbutrin xl 150 mg oral tablet extended release 24 hour",
#' "levothyroxine sodium 137 mcg oral tablet",
#' "propranolol hcl 20 mg oral tablet",
#' "propranolol hcl er 60 mg oral capsule extended release 24 hour",
#' "testosterone cypionate 200 mg/ml intramuscular solution",
#' "amphetamine-dextroamphetamine 10 mg oral tablet",
#' "proair hfa 108 (90 base) mcg/act inhalation aerosol solution",
#' "tri-lo-marzia 0.18/0.215/0.25 mg-25 mcg oral tablet",
#' "adderall 5 mg oral tablet",
#' "adderall xr 10 mg oral capsule extended release 24 hour",
#' "adderall xr 15 mg oral capsule extended release 24 hour",
#' "buspirone hcl 10 mg oral tablet",
#' "buspirone hcl 15 mg oral tablet",
#' "buspirone hcl 30 mg oral tablet",
#' "clonazepam 0.5mg tab 340b",
#' "genvoya 150-150-200-10 340b",
#' "nifedipine 2% with lidocone 2% in petrolatum base",
#' "prezista 800 mg tabs",
#' "prezista 800mg tab 340b",
#' "rectal supp: hydrocort 2%",
#' "lidocaine 3%",
#' "nifiedpine 0.3%",
#' "doxycycline hyclate 100 mg oral capsule",
#' "lidocaine 5 % external ointment",
#' "nifedipine 20 mg oral capsule",
#' "truvada 200-300 mg oral tablet",
#' "valtrex 1 gm oral tablet",
#' "truvada 200-300 mg tablet",
#' "truvada tab 200-300mg",
#' "bupropion hcl er (xl) 150 mg oral tablet extended release 24 hour",
#' "escitalopram oxalate 10 mg oral tablet",
#' "descovy 200-25 mg oral tablet",
#' "finasteride 5 mg tablet",
#' "ondansetron 4 mg oral tablet disintegrating",
#' "truvada tabs 30's 200/300",
#' "citalopram hbr 40 mg tablet",
#' "estradiol 2 mg tablet",
#' "prometrium 100 mg oral capsule",
#' "spironolactone 25 mg oral tablet",
#' "spironolactone 50 mg tablet",
#' "flovent hfa 110 mcg/act inhalation aerosol",
#' "fluticasone prop 50 mcg spray",
#' "fluticasone propionate 50 mcg/act nasal suspension",
#' "lorazepam 1 mg oral tablet",
#' "lorazepam 1 mg tablet",
#' "lorazepam 1 mg tabs",
#' "omeprazole 20 mg cpdr",
#' "omeprazole 20 mg oral capsule delayed release",
#' "omeprazole dr 20 mg capsule",
#' "proair hfa 108 (90 base) mcg/act inhalation aerosol solution",
#' "triumeq 600-50-300 mg tablet",
#' "valtrex 1 gm oral tablet",
#' "levaquin 500 mg oral tablet",
#' "testosterone cypionate 200 mg/ml intramuscular solution",
#' "bupropion hcl sr 150 mg tablet",
#' "testosterone cyp 200 mg/ml",
#' "testosterone cypionate 200 mg/ml soln",
#' "cialis 20 mg oral tablet",
#' "polymyxin b-trimethoprim 10000-0.1 unit/ml-% ophthalmic solution",
#' "triamcinolone acetonide 0.1 % external cream",
#' "truvada 200-300 mg oral tablet",
#' "truvada 200/300mg",
#' "lisinopril 10 mg oral tablet",
#' "lisinopril 10 mg tablet",
#' "diclofenac sodium 50 mg oral tablet delayed release",
#' "fluticasone propionate 0.05 % external cream",
#' "levothyroxine 0.100mg (100mcg) tab",
#' "levothyroxine sodium 100 mcg tablet",
#' "truvada 200 mg-300 mg tablet",
#' "truvada 200-300 mg oral tablet",
#' "estring 2 mg vaginal ring",
#' "protonix 40 mg oral tablet delayed release",
#' "chlorthalidone 25 mg oral tablet",
#' "hydrochlorothiazide 25 mg tab",
#' "azithromycin 500 mg oral tablet",
#' "ciclopirox 8 % external solution",
#' "escitalopram 10 mg tablet",
#' "lexapro 10 mg oral tablet",
#' "lexapro 5 mg oral tablet",
#' "metronidazole 500 mg oral tablet",
#' "bupropion hcl er (xl) 300 mg oral tablet extended release 24 hour",
#' "clonazepam 0.5 mg oral tablet",
#' "omeprazole 20 mg oral capsule delayed release",
#' "omeprazole dr 20 mg capsule",
#' "sertraline hcl 100 mg tablet",
#' "testosterone cypionate 200 mg/ml intramuscular solution",
#' "escitalopram 20 mg tablet",
#' "fiorinal 50-325-40 mg oral capsule",
#' "ibuprofen 800 mg oral tablet",
#' "estradiol 0.1 mg patch",
#' "spironolactone 50 mg oral tablet",
#' "spironolactone 50 mg tablet",
#' "vivelle-dot 0.1 mg/24hr transdermal patch twice weekly",
#' "truvada 200 mg-300 mg tablet",
#' "escitalopram oxalate 10 mg oral tablet",
#' "truvada 200-300 mg oral tablet",
#' "viagra 100 mg oral tablet",
#' "amoxicillin-pot clavulanate 875-125 mg oral tablet",
#' "biktarvy 50-200-25 mg oral tablet",
#' "valtrex 1 gm oral tablet",
#' "amoxicillin-pot clavulanate 875-125 mg oral tablet",
#' "atenolol 50 mg oral tablet",
#' "atenolol 50 mg tabs",
#' "carbamazepine 200mg tablets",
#' "clomipramine 75mg capsules",
#' "clomipramine hcl 25 mg oral capsule",
#' "diazepam 2 mg oral tablet",
#' "flovent hfa 110 mcg/act inha",
#' "flovent hfa 110 mcg/act inhalation aerosol",
#' "fluconazole 150 mg oral tablet",
#' "lorazepam 1 mg oral tablet",
#' "perphenazine 2 mg oral tablet",
#' "perphenazine 2mg tablets",
#' "proair hfa aer w/counter 340b",
#' "proair hfa 108 (90 base) mcg/act inhalation aerosol solution",
#' "pulmicort inh 90mcg 340b",
#' "pulmicort flexhaler 90 mcg/act inhalation aerosol powder breath activated",
#' "valium 2 mg oral tablet",
#' "ventolin hfa 108 (90 base) mcg/act inhalation aerosol solution",
#' "lisinopril 10 mg tablet",
#' "lisinopril 5 mg oral tablet",
#' "amoxicillin-pot clavulanate 875-125 mg oral tablet",
#' "atorvastatin calcium 20 mg oral tablet",
#' "azithromycin 250 mg oral tablet",
#' "ketoconazole 2 % external shampoo",
#' "truvada 200 mg-300 mg tablet",
#' "viagra 100 mg oral tablet",
#' "bactrim ds 800-160 mg oral tablet",
#' "biktarvy 50-200-25 mg oral tablet",
#' "doxycycline hyclate 100 mg oral tablet",
#' "escitalopram oxalate 10 mg oral tablet",
#' "lorazepam 0.5 mg oral tablet",
#' "escitalopram 10 mg tablet",
#' "escitalopram 5 mg tablet",
#' "lexapro 10 mg oral tablet",
#' "propranolol hcl 20 mg oral tablet",
#' "testosterone cypionate 200 mg/ml intramuscular solution",
#' "bupropion hcl xl 150 mg tablet",
#' "flovent hfa 110mcg",
#' "montelukast sod 10 mg tablet",
#' "proair hfa aer",
#' "singulair 10 mg oral tablet",
#' "apri 28 day tablet",
#' "ventolin hfa 108 (90 base) mcg/act inhalation aerosol solution",
#' "junel fe 1/20 1-20 mg-mcg oral tablet",
#' "bd eclipse needle 21g x 1\"",
#' "bd syringe luer-lok 3 ml",
#' "escitalopram 10 mg tablet",
#' "escitalopram oxalate 10 mg oral tablet",
#' "testopel 75 mg implant pellet",
#' "testopel pellets 340b",
#' "testosterone cypionate 100 mg/ml intramuscular solution")
#'
#' df <- classify_medications(medication_chr)
#'
#' df %>%
#'   select(-medications) %>%
#'   summarize(across(everything(), sum)) %>%
#'   t() %>%
#'   as.data.frame() %>%
#'   tibble::rownames_to_column(var = 'type') %>%
#'   rename(count = V1) ->
#'     classification_table
#'
#' classification_table %>%
#' mutate(
#'  type = recode(type,
#'    addressing_psychological_distress = 'Medications Addressing Psychological Distress',
#'    psychological_distress_side_effect = 'Medications with Psychological Distress Side Effects',
#'    address_sleep_disorders = 'Medications Addressing Sleep Disorders',
#'    otc_address_sleep_disorders = 'Over-the-Counter Products Addressing Sleep Disorders',
#'    sleep_side_effect = 'Medications with Sleep Side Effects',
#'    other_substances = 'Other Substances [CNS Stimulants, Depressants]')) %>%
#' mutate(type = reorder(factor(type), count)) %>%
#' ggplot(aes(x = count, y = type)) +
#' geom_col() +
#' geom_text(aes(x = count + 2.5, label = scales::number_format()(count)), size = 2) +
#' ylab("") +
#' ggtitle("Classification of 156 Prescriptions from the first 50 Fenway Participants") +
#' theme_bw() +
#' theme(text = element_text(size = 7))                    # All font sizes
#'
#' library(here)
#'
#' output_png <- here("analysis/2022_04_19_sleep_affecting_medications/classification_of_medications.png")
#' ggsave(output_png, height = 2, width = 6, dpi = 600)
#' system(str_c('open ', output_png))
#'


classify_medications <- function(medication_chr) {

  medication_chr %<>% tolower()

  sleep_affecting_drugs <- load_medications_that_can_affect_sleep()

  classification <- tibble::tibble(
    medications = medication_chr,
    addressing_psychological_distress = NA,
    psychological_distress_side_effect = NA,
    address_sleep_disorders = NA,
    otc_address_sleep_disorders = NA,
    sleep_side_effect = NA,
    other_substances = NA
  )

  # regex classification functions ----------------------------------

  # 1: create | separated string of all drug names in the given df
  create_classifier_regex <- function(drugs_df) {
    drugs_df$`Drug Names` %>%
      str_extract_all("[A-Za-z]{4,}") %>%
      unlist() %>%
      base::tolower() %>%
      setdiff(c('active', 'with', 'acid', 'headache', 'relief', 'time', 'tabs', 'allergy', 'children',
                'simply', 'hour', 'cold', 'smart', 'walgreens', 'skin',
                'severe', 'solu', 'hydro', 'lido', 'derm')) %>%
      sort() %>%
      paste0(collapse = '|')
  }

  # 2: create | separated string of all drug names in the given df for drowsy effect
  create_med_effect_drowsy_classifier_regex <- function(drugs_df) {

    med_effect_drowsy <- drugs_df |>
      select(`Drug Names`,`Effect(s) on sleep`) |>
      mutate(`Effect(s) on sleep` = stringr::str_to_lower(`Effect(s) on sleep`),
             effect_drowsy = case_when(stringr::str_detect(`Effect(s) on sleep`,'\\b[Ss]leepiness\\b[\\s,]*') |
                                         stringr::str_detect(`Effect(s) on sleep`,'\\bMaintain/promote sleep\\b[\\s,]*') ~ TRUE,
                                       TRUE ~ FALSE)) |>
      filter(effect_drowsy == T)

    # list all medications within drug class that result in sleepiness or promote sleep
    med_effect_drowsy$`Drug Names` |>
      str_extract_all("[A-Za-z]{4,}") |>
      unlist() |>
      base::tolower() |>
      setdiff(c('active', 'with', 'acid', 'headache', 'relief', 'time', 'tabs', 'allergy', 'children',
                'simply', 'hour', 'cold', 'smart', 'walgreens', 'skin',
                'severe', 'solu', 'hydro', 'lido', 'derm')) |>
      sort() |>
      paste0(collapse = '|')

  }

  # 3: create | separated string of all drug names in the given df for wakeful effect
  create_med_effect_wakeful_classifier_regex <- function(drugs_df) {

    # make a wakeful effect dataset
    med_effect_wakeful <- drugs_df |>
      select(`Drug Names`,`Effect(s) on sleep`) |>
      mutate(`Effect(s) on sleep` = stringr::str_to_lower(`Effect(s) on sleep`),
             effect_wakeful = case_when(stringr::str_detect(`Effect(s) on sleep`,'\\b[Ii]nsomnia\\b[\\s,]*') |
                                          stringr::str_detect(`Effect(s) on sleep`,'\\b[Nn]ightmares\\b[\\s,]*') |
                                          stringr::str_detect(`Effect(s) on sleep`,'\\b[Ii]ncrease wakefulness\\b[\\s,]*') ~ TRUE,
                                        TRUE ~ FALSE)) |>
      filter(effect_wakeful == T)

    # list all medications within drug class that result in increased wakefulness
    med_effect_wakeful$`Drug Names` |>
      str_extract_all("[A-Za-z]{4,}") |>
      unlist() |>
      base::tolower() |>
      setdiff(c('active', 'with', 'acid', 'headache', 'relief', 'time', 'tabs', 'allergy', 'children',
                'simply', 'hour', 'cold', 'smart', 'walgreens', 'skin',
                'severe', 'solu', 'hydro', 'lido', 'derm')) |>
      sort() |>
      paste0(collapse = '|')

  }

  # code drugs addressing psychological distress ---------------------------
  for (category in c(
    'addressing_psychological_distress', 'psychological_distress_side_effect',
    'address_sleep_disorders', 'otc_address_sleep_disorders',
    'sleep_side_effect', 'other_substances'
  )) {
    classification[[category]] <-
      stringr::str_detect(
        medication_chr,
        create_classifier_regex(sleep_affecting_drugs[[category]])
      )
  }


  sleep_categories <- c("address_sleep_disorders","otc_address_sleep_disorders", "sleep_side_effect")

  for (category in sleep_categories) {

    drowsy_regex <- create_med_effect_drowsy_classifier_regex(sleep_affecting_drugs[[category]])
    wakeful_regex <- create_med_effect_wakeful_classifier_regex(sleep_affecting_drugs[[category]])

    # Update classification for drowsy effect if drowsy regex is not empty
    if (drowsy_regex != "") {
      classification <- classification %>%
        mutate(
          "{category}_drowsy" := if_else(
            is.na(!!sym(category)), NA,
            if_else(!!sym(category) == TRUE & str_detect(medications, drowsy_regex), TRUE, FALSE)
          )
        )
    } else {
      classification <- classification %>%
        mutate("{category}_drowsy" := if_else(is.na(!!sym(category)), NA, FALSE))
    }

    # Update classification for wakeful effect if wakeful regex is not empty
    if (wakeful_regex != "") {
      classification <- classification %>%
        mutate(
          "{category}_wakeful" := if_else(
            is.na(!!sym(category)), NA,
            if_else(!!sym(category) == TRUE & str_detect(medications, wakeful_regex), TRUE, FALSE)
          )
        )
    } else {
      classification <- classification %>%
        mutate("{category}_wakeful" := if_else(is.na(!!sym(category)), NA, FALSE))
    }
  }

  return(classification)

}

#' Classify Medications for LifeAndHealth Participants
#'
classify_medications_for_participants <- function() {

  medications <- read_height_weight_medications()

  medications$fenway$`Medication Description` %<>% tolower()
  medications$mattapan$medications %<>% tolower()
  medications$harvard$`Rx within  the year prior to date of survey` %<>% tolower()

  fenway_med_classification <- classify_medications(medications$fenway$`Medication Description`) %>% filter(! is.na(medications))
  mattapan_med_classification <- classify_medications(medications$mattapan$medications) %>% filter(! is.na(medications))
  harvard_med_classification <- classify_medications(medications$harvard$`Rx within  the year prior to date of survey`) %>% filter(! is.na(medications))

  medications$fenway %<>% left_join(
    fenway_med_classification,
    by = c(`Medication Description` = 'medications'),
    relationship = 'many-to-many')

  medications$mattapan %<>% left_join(
    mattapan_med_classification,
    by = c(medications = 'medications'))

  medications$harvard %<>% left_join(
    harvard_med_classification,
    by = c(`Rx within  the year prior to date of survey` = 'medications'))

  fenway_meds_summarized <- medications$fenway %>%
    dplyr::summarize(
      rx_addressing_psychological_distress = any(addressing_psychological_distress),
      rx_psychological_distress_side_effect = any(psychological_distress_side_effect),
      rx_address_sleep_disorders = any(address_sleep_disorders),
      rx_otc_address_sleep_disorders = any(otc_address_sleep_disorders),
      rx_sleep_side_effect = any(sleep_side_effect),
      rx_other_substances = any(other_substances),

      rx_address_sleep_disorders_drowsy = any(address_sleep_disorders_drowsy),
      rx_address_sleep_disorders_wakeful = any(address_sleep_disorders_wakeful),
      rx_otc_address_sleep_disorders_drowsy = any(otc_address_sleep_disorders_drowsy),
      rx_otc_address_sleep_disorders_wakeful = any(otc_address_sleep_disorders_wakeful),
      rx_sleep_side_effect_drowsy = any(sleep_side_effect_drowsy),
      rx_sleep_side_effect_wakeful = any(sleep_side_effect_wakeful),
      .by = `L+H ID`
    )

  mattapan_meds_summarized <- medications$mattapan %>%
    dplyr::summarize(
      rx_addressing_psychological_distress = any(addressing_psychological_distress),
      rx_psychological_distress_side_effect = any(psychological_distress_side_effect),
      rx_address_sleep_disorders = any(address_sleep_disorders),
      rx_otc_address_sleep_disorders = any(otc_address_sleep_disorders),
      rx_sleep_side_effect = any(sleep_side_effect),
      rx_other_substances = any(other_substances),

      rx_address_sleep_disorders_drowsy = any(address_sleep_disorders_drowsy),
      rx_address_sleep_disorders_wakeful = any(address_sleep_disorders_wakeful),
      rx_otc_address_sleep_disorders_drowsy = any(otc_address_sleep_disorders_drowsy),
      rx_otc_address_sleep_disorders_wakeful = any(otc_address_sleep_disorders_wakeful),
      rx_sleep_side_effect_drowsy = any(sleep_side_effect_drowsy),
      rx_sleep_side_effect_wakeful = any(sleep_side_effect_wakeful),
      .by = MCHC_dummyID)

  harvard_meds_summarized <- medications$harvard %>%
    dplyr::summarize(
      rx_addressing_psychological_distress = any(addressing_psychological_distress),
      rx_psychological_distress_side_effect = any(psychological_distress_side_effect),
      rx_address_sleep_disorders = any(address_sleep_disorders),
      rx_otc_address_sleep_disorders = any(otc_address_sleep_disorders),
      rx_sleep_side_effect = any(sleep_side_effect),
      rx_other_substances = any(other_substances),

      rx_address_sleep_disorders_drowsy = any(address_sleep_disorders_drowsy),
      rx_address_sleep_disorders_wakeful = any(address_sleep_disorders_wakeful),
      rx_otc_address_sleep_disorders_drowsy = any(otc_address_sleep_disorders_drowsy),
      rx_otc_address_sleep_disorders_wakeful = any(otc_address_sleep_disorders_wakeful),
      rx_sleep_side_effect_drowsy = any(sleep_side_effect_drowsy),
      rx_sleep_side_effect_wakeful = any(sleep_side_effect_wakeful),
      .by = DummyID)

  meds_summarized <- bind_rows(
    fenway_meds_summarized %>% rename(id = `L+H ID`),
    mattapan_meds_summarized %>% rename(id = MCHC_dummyID),
    harvard_meds_summarized %>% rename(id = DummyID))

  meds_summarized$id %<>% as.character()

  return(meds_summarized)
}

