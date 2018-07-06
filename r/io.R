library(tidyverse)

# load and translate data
load_data <- function(path) {
  read_delim(path, ";", col_types = cols(.default = col_character()))  %>%
    lookup_col_names()
}


# prepare a look-up table from LimeSurvey column names to readable names
make_blank_lut <- function(df) {
  col_lut <-
    tibble(col_name_verbose = names(df)) %>%
    separate(col_name_verbose, c("col_name_lime", "question"), sep = ". ", extra = "merge") %>%

    # remove columns irrelevant to the analysis
    filter(

      # timing
      !str_detect(question, "^(Question)|(Group)|(Total) time"),

      # text display
      !str_detect(col_name_lime, "^TXT"),

      # helper questions
      !str_detect(col_name_lime, "^SCREEN"),

      # questions covered by other columns
      !str_detect(col_name_lime, "Please specify below."),

      # limesurvey misc
      !(col_name_lime %in% c("submitdate", "lastpage", "startlanguage", "seed", "startdate", "datestamp"))) %>%

    # add manual column and reorder
    mutate(col_name = "") %>%
    select(col_name, col_name_lime, question)

  # save the translation table for manual mapping
  write_csv(col_lut, "input/col_lut_blank.csv")
}

# translate LimeSurvey column name to readable names
lookup_col_names <- function(df) {
  lut_df <- read_csv("input/col_lut.csv", col_types = "cc")
  df %>%
    rename_all(funs(
      str_sub(
        str_extract(., "(^.+?)\\. "), end = -3)
      )
      ) %>%
    select_at(vars(lut_df$col_name_lime), ~ lut_df$col_name)
}
