library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

source("r/constants.R")
source("r/io.R")

#===============================================================================

df_url  <-

  # load raw data with URL fields
  load_raw(limesurvey_export_path)  %>%
  lookup_col_names(omit_free_text = FALSE) %>%
  select(id, ends_with("URLPub"), ends_with("URLUni"), ends_with("URLGH"), ends_with("URLPersWeb")) %>%

  # transform and filter only rows with URL
  gather("source", "URL", contains("URL")) %>%
  filter(!is.na(URL)) %>%
  separate(source, into = c("type", "location"), sep = "URL") %>%

  # make values human-readable
  mutate(location = case_when(
    location == "Pub" ~ "public_repo",
    location == "Uni" ~ "university_repo",
    location == "GH" ~ "github",
    location == "PersWeb" ~ "personal_website"
    )) %>%
  left_join(lookup_table_material_types(), by = c("type" = "lime_code")) %>%

  # tidy-up
  select(-type) %>%
  rename(type = target_code) %>%
  select(id, type, location, URL) %>%
  arrange(id)

write_csv(df_url, "output/public_url.csv")
