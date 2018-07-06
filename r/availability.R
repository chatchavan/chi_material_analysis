library(tidyverse)
import::from(pairwiseCI, pairwiseCI)
source("r/io.R")

#===============================================================================
# load and convert data to a long format
# NOTE: export parameters from LimeSurvey
#  * Export responses as: Answer code
#  * Export questions as: Question code & question text
df <- load_data("input/results-survey332449.csv")


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

rm(list = setdiff(ls(), "df_long"))


#-------------------------------------------------------------------------------
# determine methodology

qual_signals <- c("qualraw", "qualcoded", "qualcodebook", "qualcomplete")
quan_signals <- c("quanraw", "quanprocessed", "quancode")

study_method <-
  df_long %>%
  mutate(
    qual = (type %in% qual_signals),
    quan = (type %in% quan_signals)
  ) %>%
  gather(key = method, value = "tf", qual, quan) %>%
  select(id, type, method, tf) %>%
  filter(tf == TRUE) %>%
  distinct(id, method)

df_long <-
  df_long %>%
  left_join(study_method, by = "id")


rm(study_method, qual_signals, quan_signals)
