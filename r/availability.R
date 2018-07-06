library(tidyverse)
import::from(broom, tidy)
import::from(pairwiseCI, pairwiseCI)
import::from(PropCIs, exactci)
import::from(roomba, roomba) # devtools::install_github("ropenscilabs/roomba")

source("r/persist.R")
source("r/constants.R")
source("r/io.R")

# settings
theme_set(theme_grey(base_size = 7))


#===============================================================================
# load and convert data to a long format
# NOTE: export parameters from LimeSurvey
#  * Export responses as: Answer code
#  * Export questions as: Question code & question text
df_long <- load_data("input/results-survey332449.csv")

persist(df_long)
rm(list = ls())


#===============================================================================
# availability frequency and proportion for each type of materials

# RQ 1: Among the types of materials produced in the course of research,
#       which ones do authors make available outside their lab?

avail_by_type <-
  df_long %>%
  mutate(is_public = if_else(is_public, "public", "private")) %>%
  select(id, type, is_public) %>%
  group_by(type, is_public) %>%
  summarize(n = n()) %>%
  ungroup()

p_tmp <-
  avail_by_type %>%
  ggplot(aes(x = type, y = n, fill = is_public)) +
  geom_col() +
  coord_flip()

ggsave("output/availability_by_type_count.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)


#-------------------------------------------------------------------------------
# proportion and CI

public_by_type_ci <-
  avail_by_type %>%
  spread(key = is_public, value = n, fill = 0L) %>%
  mutate(total = private + public) %>%
  rowwise() %>%
  do({
    row_type <- .$type[[1]]
    exactci(.$public[[1]], .$total[[1]], 0.95) %>%
      tidy() %>%
      mutate(type = row_type)
  })

p_tmp <-
  public_by_type_ci %>%
  mutate(type = factor(type, levels = types_all)) %>%
  mutate(type = fct_rev(type)) %>%
  ggplot(aes(x = type, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0) +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("95% CI of the proportion of publicly available material\n (Clopper-Pearson exact CI)") +
  coord_flip()

ggsave("output/availability_by_type_proportion.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)

rm(list = ls())

#===============================================================================
# group data according to the research questions


# RQ 1.1: Is there a difference in the availability of study materials for
# quantitative and qualitative studies?
pub_study_by_method <-
  df_long %>%
  filter(
    method %in% c("qual", "quan"),
    type == "study") %>%
  select(id, method, is_public) %>%
  mutate(method = str_c("study_", method)) %>%

  # rename for the RQ table
  mutate(is_public = if_else(is_public, "public", "private")) %>%
  rename(group = method) %>%
  mutate(rq = "1.1")


# RQ 1.2: Is there a difference in the availability of data for quantitative and
# qualitative studies?
pub_data_by_method <-
  df_long %>%

  # recode study data regardless of sub-types (remove other types)
  mutate(type = recode(type,
    qualraw       = "data_qual",
    qualcoded     = "data_qual",
    qualcomplete  = "data_qual",
    quanraw       = "data_quan",
    quanprocessed = "data_quan",
    quancomplete  = "data_quan",
    .default      = NA_character_
    )) %>%
  filter(!is.na(type)) %>%

  # if one of the subtype is public, count as public
  select(id, type, is_public) %>%
  group_by(id, type) %>%
  summarize(is_public = any(is_public)) %>%
  ungroup() %>%

  # relabel and rename for the RQ table
  mutate(is_public = if_else(is_public, "public", "private")) %>%
  rename(group = type) %>%
  mutate(rq = "1.2")


# RQ 1.3: Is preprocessed data more frequently available than raw data?
pub_data_by_maturity <-
  df_long %>%

  # recode study data regardless of sub-types (remove other types)
  mutate(type = recode(type,
    qualraw       = "data_raw",
    quanraw       = "data_raw",
    qualcoded     = "data_processed",
    qualcomplete  = "data_processed",
    quanprocessed = "data_processed",
    quancomplete  = "data_processed",
    .default      = NA_character_
    )) %>%
  filter(!is.na(type)) %>%

  # if one of the subtype is public, count as public
  select(id, type, is_public) %>%
  group_by(id, type) %>%
  summarize(is_public = any(is_public)) %>%
  ungroup() %>%

  # relabel and rename for the RQ table
  mutate(is_public = if_else(is_public, "public", "private")) %>%
  rename(group = type) %>%
  mutate(rq = "1.3")


# RQ 1.4: Is software source code more frequently available than hardware specifications?
pub_product_by_type <-
  df_long %>%
  filter(type %in% c("hardware", "software")) %>%
  select(id, type, is_public) %>%

  # relabel and rename for the RQ table
  mutate(is_public = if_else(is_public, "public", "private")) %>%
  rename(group = type) %>%
  mutate(rq = "1.4")


#===============================================================================
# frequency table

freq_table <-
  bind_rows(
    pub_study_by_method,
    pub_data_by_method,
    pub_data_by_maturity,
    pub_product_by_type) %>%
  group_by(rq, group, is_public) %>%
  summarize(n = n()) %>%
  spread(key = is_public, value = n, fill = 0L) %>%
  ungroup()


#===============================================================================
# descriptive statistics: frequency of groups

p_tmp <-
  freq_table %>%
  gather(key = availability, value = n,  private, public) %>%

  ggplot(aes(x = group, y = n, fill = availability)) +
  geom_col() +
  coord_flip() +
  facet_grid(rq ~ ., scales = "free_y")

ggsave("output/availability_pairs_frequency.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)


#===============================================================================
# inferential statistics: CI of proportion difference

pw_results <-
  pairwiseCI(cbind(public, private) ~ group,
  by = "rq",
  data = freq_table,
  method = "Prop.diff",
  CImethod = "NHS")

comp_table <-
  roomba(pw_results$byout, c("estimate", "lower", "upper", "compnames", "method")) %>%
  bind_cols(list(RQ = pw_results$bynames), .)

p_tmp <-
  comp_table %>%
  ggplot(aes(x = compnames, y = estimate, ymin = lower, ymax = upper)) +
  geom_pointrange() +
  coord_flip() +
  xlab(NULL) +
  ylab("Difference of proportion of publicly available materials\n95% CI (Newcombes Hybrid Score)")

ggsave("output/availability_pairs_proportion_difference.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)

rm(list = ls())
