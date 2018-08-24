library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")


source("r/constants.R")
source("r/paper_info.R")
source("r/io.R")


#===============================================================================
# load and convert data to a long format
df_survey <-
  load_raw(limesurvey_export_path)  %>%
  lookup_col_names(omit_free_text = FALSE)


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
# associate volunteered DOI to response
df_volunteered <-
  df_postsurvey %>%
  left_join(paper_info, by = "doi") %>%
  left_join(df_survey, by = "id") %>%
  select(-submitdate, -lastpage, -startlanguage, -seed, -startdate, -datestamp)

#===============================================================================
# save output
write_csv(df_volunteered, "output/volunteered_dois/responses.csv")
