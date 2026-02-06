scale_polcncrn_vars <- function(values) {
  ifelse(values == 1, 3,
         ifelse(values == 2, 2,
                ifelse(values == 3, 1,
                       ifelse(values == 4, 0, NA))))  # Adding NA as the final else condition
}

construct_polcncrn_scale <- function(df){

  df %<>%
    mutate_at(vars(starts_with('POLCNCRN')), ~as.numeric(.)) %>%
    mutate_at(vars(starts_with('POLCNCRN')), ~coalesce(., 0)) %>%
    mutate_at(vars(starts_with('POLCNCRN')),
              ~ scale_polcncrn_vars(.)) %>%
    select(starts_with('POLCNCRN')) %>%
    rowSums() %>%
    data.frame(political_concern = .) %$%
    bind_cols(df, .)

  return(df)
}
