#' Create Residential Area Based Social Metrics (ABSMs)
#'
create_residential_absms <- function() {

  # Life+Health geocoded residential census tracts would be loaded here in the secure environment
  df <- readxl::read_excel("restricted_file.xlsx")

  # Create a data dictionary for ABSMS. The first column indicates the total
  # variable code, the second the variable name, and the third the description.
  absms_dictionary <- tibble::tribble(
    ~var, ~varname, ~description,
    # total population
    "B01001_001",  "total_popsize", "total population estimate",

    # racial composition
    'B01003_001',  "race_ethnicity_total", "race_ethnicity_total",

    # ICEraceinc
    "B19001_001",  'hhinc_total',   "total population for household income estimates",
    "B19001A_002", 'hhinc_w_1',     "white n.h. pop with household income <$10k",
    "B19001A_003", 'hhinc_w_2',     "white n.h. pop with household income $10k-14 999k",
    "B19001A_004", 'hhinc_w_3',     "white n.h. pop with household income $15k-19 999k",
    "B19001A_005", 'hhinc_w_4',     "white n.h. pop with household income $20k-24 999k",
    "B19001A_014", 'hhinc_w_5',     "white n.h. pop with household income $100 000 to $124 999",
    "B19001A_015", 'hhinc_w_6',     "white n.h. pop with household income $125k-149 999k",
    "B19001A_016", 'hhinc_w_7',     "white n.h. pop with household income $150k-199 999k",
    "B19001A_017", 'hhinc_w_8',     "white n.h. pop with household income $196k+",
    "B19001_002",  'hhinc_total_1', "total pop with household income <$10k",
    "B19001_003",  'hhinc_total_2', "total pop with household income $10k-14 999k",
    "B19001_004",  'hhinc_total_3', "total pop with household income $15k-19 999k",
    "B19001_005",  'hhinc_total_4', "total pop with household income $20k-24 999k",

    # poverty
    "B05010_002",  'in_poverty',    "population with household income < poverty line",
    "B05010_001",  'total_pop_for_poverty_estimates',  "total population for poverty estimates",

    # median income
    "B06011_001",  'median_income',  "median income estimate for total population",

    # crowded housing
    "B25014_005",  'owner_occupied_crowding1', 'owner occupied, 1 to 1.5 per room',
    "B25014_006",  'owner_occupied_crowding2', 'owner occupied, 1.51 to 2 per room',
    "B25014_007",  'owner_occupied_crowding3', 'owner occupied, 2.01 or more per room',
    "B25014_011",  'renter_occupied_crowding1', 'owner occupied, 1 to 1.5 per room',
    "B25014_012",  'renter_occupied_crowding2', 'owner occupied, 1.51 to 2 per room',
    "B25014_013",  'renter_occupied_crowding3', 'owner occupied, 2.01 or more per room',
    "B25014_001",  'crowding_total',            'total for crowding (occupants per room)',

    "B01001I_001",  'total_hispanic',           'total hispanic population estimate',
    "B01001B_001",  'total_black',              'total black, hispanic or non-hispanic estimate',
    "B01001H_001",  'total_white_nh',           'total white, non-hispanic population estimate',
    "B01001D_001",  'total_asian',               'total_asian population estimate',

    # ICE for housing tenure
    "B07013_001",   'total_housing_tenure',     'total for housing tenure estimate',
    "B07013_002",   'total_owner_occupied',     'total for owner occupied housing',
    "B07013_003",   'total_renter_occupied',    'total for renter occupied housing'
  )

  # Helper function to get ABSMs at tract level for desired states
  get_absms <- function(state) {
    absms <- tidycensus::get_acs(
      year = 2019,
      geography = 'tract',
      state = state,
      variables = absms_dictionary$var, #Get the variables indicated in the data dictionary.
      geometry = FALSE # We already have the geometry so we don't need that.
    )

    # pivot wider so that each row corresponds to a tract
    absms %<>% dplyr::select(-moe) %>%
      tidyr::pivot_wider(names_from = variable, values_from = estimate)
    # Change the new column names to reflect variables names from the dictionary
    rename_vars <- setNames(absms_dictionary$var, absms_dictionary$varname)
    absms <- absms %>% rename(!!rename_vars)

    absms %<>%
      mutate(
        # we calculate the people of color low income counts as the overall
        # low income counts minus the white non-hispanic low income counts
        people_of_color_low_income =
          (hhinc_total_1 + hhinc_total_2 + hhinc_total_3 + hhinc_total_4) -
          (hhinc_w_1 + hhinc_w_2 + hhinc_w_3 + hhinc_w_4),
        # sum up the white non-hispanic high income counts
        white_non_hispanic_high_income =
          (hhinc_w_5 + hhinc_w_6 + hhinc_w_7 + hhinc_w_8),
        # calculate the index of concentration at the extremes for racialized
        # economic segregation (high income white non-hispanic vs. low income
        # people of color)
        ICEraceinc =
          (white_non_hispanic_high_income - people_of_color_low_income) /
          hhinc_total,

        ICEown = (total_owner_occupied - total_renter_occupied) / total_housing_tenure,

        prop_in_poverty = in_poverty / total_pop_for_poverty_estimates,

        crowding = (owner_occupied_crowding1 + owner_occupied_crowding2 + owner_occupied_crowding3 +
                      renter_occupied_crowding1 + renter_occupied_crowding2 + renter_occupied_crowding3) / crowding_total,

        prop_black = total_black / total_popsize,
        prop_hispanic = total_hispanic / total_popsize,
        prop_white_nh = total_white_nh / total_popsize,
        prop_asian = total_asian/ total_popsize
      ) %>%
      dplyr::select(GEOID,ICEraceinc,ICEown,prop_in_poverty,median_income,crowding,prop_black,prop_hispanic,prop_white_nh, prop_asian)

    return(absms)
  }

  # get the state fips codes for whom we have participants with residence there
  residence_states_fips <- na.omit(unique(substr(df$GEOID_10, 1, 2)))

  # get the fips <--> state relationship from tigris
  state_fips_codes <- tigris::fips_codes %>% select(state, state_code) %>%
    unique()

  # create a lookup vector
  state_fips_code_lookup <- setNames(state_fips_codes$state, state_fips_codes$state_code)

  # convert our residence state fips to residence state abbreviations
  residence_states <- state_fips_code_lookup[residence_states_fips]

  # get the census tract absms for relevant states
  absms <- get_absms(residence_states)

  # join CT absms into our dataset
  df %<>% left_join(absms, by = c('GEOID_10' = 'GEOID'))

  # make sure the id is a character column
  df$idnum %<>% as.character()

  # save output
  write.csv(df,file.path("restricted_file_output.csv"),row.names = FALSE)

  return(invisible(NULL))
}
