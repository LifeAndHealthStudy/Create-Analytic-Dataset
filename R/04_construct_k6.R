# create the Kessler Psychological Distress Scale
construct_K6_measure <- function(df) {

  df %<>% mutate(across(
    starts_with("PSY_DSTR_1_"),
    ~ labelled::labelled(
      (. - 5) * -1,
      labels = c(
        `All of the time` = 4,
        `Most of the time` = 3,
        `Some of the time` = 2,
        `A little of the time` = 1,
        `None of the time` = 0
      ),
      label = attr(., "label")
    )
  ))

  df %<>% mutate(K6_psychological_distress = rowSums(across(starts_with("PSY_DSTR_1_"))))
  df %<>% mutate(K6_serious_psychological_distress = K6_psychological_distress >= 13)

  return(df)
}
