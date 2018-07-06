library(tidyverse)
source("r/constants.R")

# NOTE: Expected export parameters from LimeSurvey
#  * Completion state: Completed responses only
#  * Export responses as: Answer code
#  * Export questions as: Question code & question text
# load and translate data
load_data <- function(path, exclude_mismatch = should_exclude_mismatch) {
  df <-
    read_delim(path, ";", col_types = cols(.default = col_character()))  %>%
    lookup_col_names()

  df_materials <-
    df %>%
    make_long_format()


  # detect expertise mismatch and exclude potentially low-quality responses
  if (exclude_mismatch) {

    df_respondent <-
      df %>%
      select(id, starts_with("self_responsible")) %>%
      rename(
        study = self_responsible_for_study,
        prototype = self_responsible_for_prototype) %>%
      gather(key = responsible_type, value = yn_or_na, study, prototype) %>%
      filter(yn_or_na == "Y") %>%
      select(id, responsible_type)


    df_materials <-
      df_materials %>%
      mutate(meta_type = if_else(type %in% c("hardware", "software"), "prototype", "study")) %>%
      inner_join(df_respondent, by = c("id", "meta_type" = "responsible_type"))
  }

  # return
  df_materials
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


make_long_format <- function(df) {
  existence <-
    df %>%
    select(id, ends_with("_exist")) %>%
    gather(key = type, value = is_exist, ends_with("_exist")) %>%

    # re-code materials types that exist to TRUE
    filter(!is.na(is_exist)) %>%
    mutate(is_exist = TRUE) %>%
    mutate(type = str_replace(type, "_exist", "")) %>%

    # remove "others" type (need to check them manually)
    filter(type != "others")


  claim_related <-
    df %>%
    select(id, ends_with("_claim")) %>%
    gather(key = type, value = is_claim_related, ends_with("_claim")) %>%
    filter(!is.na(is_claim_related)) %>%
    mutate(type = str_replace(type, "_claim", "")) %>%
    mutate(is_claim_related = recode(is_claim_related, `Y` = TRUE, `N` = FALSE))

  public <-
    df %>%
    select(id, ends_with("_public")) %>%
    gather(key = type, value = is_public, ends_with("_public")) %>%
    filter(!is.na(is_public)) %>%
    mutate(type = str_replace(type, "_public", "")) %>%
    mutate(is_public = recode(is_public, `Y` = TRUE, `N` = FALSE))


  location <-
    df %>%
    select(id, contains("_in_")) %>%
    gather(key = type_location, value = y_or_na, contains("_in_")) %>%
    filter(!is.na(y_or_na)) %>%
    separate(type_location, into = c("type", "location"), sep = "_", extra = "merge") %>%
    mutate(location = str_replace(location, "in_", "")) %>%

    group_by(id, type) %>%
    summarize(location = str_c(location, collapse = ","))

  reason <-
    df %>%
    select(id, contains("_why_")) %>%
    gather(key = type_reason, value = y_or_na, contains("_why_")) %>%
    filter(!is.na(y_or_na)) %>%
    separate(type_reason, into = c("type", "reason"), sep = "_", extra = "merge") %>%
    mutate(reason = str_replace(reason, "why_", "")) %>%

    group_by(id, type) %>%
    summarize(reason = str_c(reason, collapse = ","))

  df_long <-
    existence %>%
    left_join(claim_related, by = c("id", "type")) %>%
    left_join(public, by = c("id", "type")) %>%
    left_join(location, by = c("id", "type")) %>%
    left_join(reason, by = c("id", "type")) %>%
    select(-is_exist)  # because of left-join, the table contains only the materials that exist


  #-------------------------------------------------------------------------------
  # determine methodology

  study_method <-
    df_long %>%
    mutate(
      qual = (type %in% types_data_qual),
      quan = (type %in% types_data_quan)
    ) %>%
    gather(key = method, value = "tf", qual, quan) %>%
    select(id, type, method, tf) %>%
    filter(tf == TRUE) %>%
    distinct(id, method)

  df_long <-
    df_long %>%
    left_join(study_method, by = "id")

  df_long
}
