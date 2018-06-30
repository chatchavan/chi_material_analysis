library(tidyverse)
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
  left_join(reason, by = c("id", "type"))


#-------------------------------------------------------------------------------
# determine methodology

qual_codes <- c("qualraw", "qualcoded", "qualcodebook", "qualcomplete")
quan_codes <- c("quanraw", "quanprocessed", "quancode")

study_method <-
  df_long %>%
  mutate(
    qual = (type %in% qual_codes),
    quan = (type %in% quan_codes)
  ) %>%
  gather(key = method, value = "tf", qual, quan) %>%
  select(id, type, method, tf) %>%
  filter(tf == TRUE) %>%
  distinct(id, method)

df_long <-
  df_long %>%
  left_join(study_method, by = "id")


rm(list = setdiff(ls(), "df_long"))

#===============================================================================
# H1.1: Study materials or results are available to public more frequently in quantitative than in qualitative studies.

public_by_method_count <-
  df_long %>%
  select(id, method, type, is_public) %>%
  filter(!is.na(method)) %>%
  group_by(id, method) %>%
  summarize(is_public = any(is_public)) %>%
  group_by(method, is_public) %>%
  summarize(n = n()) %>%
  ungroup()

public_by_method_total <-
  public_by_method_count %>%
  group_by(method) %>%
  summarize(total = sum(n)) %>%
  ungroup()

public_by_method_t <-
  public_by_method_count %>%
  spread(is_public, n, fill = 0L) %>%
  column_to_rownames("method")

#-------------------------------------------------------------------------------
# establish relationship with chi-square test on count
chisq.test(public_by_method_t)


#-------------------------------------------------------------------------------
public_by_method_n <-
  public_by_method_count %>%
  filter(is_public == TRUE) %>%
  left_join(public_by_method_total, by = "method") %>%
  mutate(percent = n / total) %>%
  select(method, n_public = n, n_total = total, percent)

# effect size: percentage difference between the two conditions
public_by_method_n$percent[2] - public_by_method_n$percent[1]
