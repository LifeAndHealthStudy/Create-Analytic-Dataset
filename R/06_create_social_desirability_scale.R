#' Construct Social Desirability Measure
# Reference: Hays RD, Hayashi T, Stewart AL. A five-item measure of socially
# desirable response set. Educ Psychol Measurement 1989; 49:629-636.

construct_social_desirability <- function(df){

  df %<>%
    mutate(# if “always courteous” = 1 (definitely true), score as 1, otherwise 0
           SOC_DESIR_1_1 = ifelse(SOC_DESIR_1_1 == 1,1,0),
           # if “take advantage” = 5 (definitely false), score as 1, otherwise 0
           SOC_DESIR_1_2 = ifelse(SOC_DESIR_1_2 == 5,1,0),
           # if” “get even” = 5 (definitely false), score as 1, otherwise 0
           SOC_DESIR_1_3 = ifelse(SOC_DESIR_1_3 == 5,1,0),
           # if “resentful” = 5 (definitely false), score as 1, otherwise 0
           SOC_DESIR_1_4 = ifelse(SOC_DESIR_1_4 == 5,1,0),
           # if “good listener” = 1 (definitely true), score as 1, otherwise 0
           SOC_DESIR_1_5 = ifelse(SOC_DESIR_1_5 == 1,1,0)) %>%
    select(starts_with('SOC_DESIR')) %>%
    # sum scores (range: 0 to 5) then linearly transform to 0-100
    rowSums() %>%
    data.frame(social_desirability = .) %$%
    bind_cols(df, .) %>%
    # linearly transform scale from 1-5 to 0-100 (multiply by 20)
    mutate(social_desirability = social_desirability*20)

  return(df)

}
