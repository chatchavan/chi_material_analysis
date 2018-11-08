library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")


source("r/constants.R")
source("r/paper_info.R")
source("r/io.R")


#===============================================================================
# load and convert data to a long format
df_survey <- load_data(limesurvey_export_path, should_exclude_mismatch)


#===============================================================================
# retrieve usable volunteered DOIs
df_postsurvey <-
  load_raw(postsurvey_export_path) %>%
  select(id = starts_with("responseId"), doi = starts_with("doi")) %>%
  filter(!is.na(id), !is.na(doi), str_detect(id, "673185-.+")) %>%
  mutate(
    id = str_match(id, "673185-(.+)")[,2],
    doi = str_extract(doi, "10.+"))


#===============================================================================
# retrieve URLs

df_urls <-
  load_urls(limesurvey_export_path) %>%
  rename(material_url = url)

#===============================================================================
# associate volunteered DOI to response
df_volunteered <-
  df_postsurvey %>%
  left_join(paper_info, by = "doi") %>%
  left_join(df_survey, by = "id") %>%
  left_join(df_urls, by = c("id", "type")) %>%
  select(id, title, type, is_claim_related, is_public, location, location_type, material_url, everything())

#===============================================================================
# save output
write_tsv(df_volunteered, "output/volunteered_dois/responses.tsv")
