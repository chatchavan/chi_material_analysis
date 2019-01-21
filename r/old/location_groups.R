library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(glue, glue)

source("r/constants.R")
source("r/io.R")

#===============================================================================
# load and convert data to a long format
df_all <-
  load_data(limesurvey_export_path, should_exclude_mismatch)

persist(df_all)
rm_all()


#===============================================================================
# group material locations

df_public <-
  df_all %>%
  filter(is_public) %>%
  select(id, type, location) %>%

  # extract location by type
  mutate(location = str_split(location, ",")) %>%
  unnest(location) %>%

  # group materials into location groups
  mutate(location_group = case_when(
    location == "paper"    ~ "reliable",
    location == "repo_pub" ~ "reliable",
    location == "repo_uni" ~ "unreliable",
    location == "author"   ~ "unreliable",
    location == "repo_github" ~ "unreliable",
    location == "web"         ~ "unreliable",
    location == "acmdl"         ~ "acmdl",
    TRUE ~ NA_character_   # NOTE: these are "other", "prev_work"
  ))

exclusion_count <-
  df_public %>%
  filter(is.na(location_group)) %>%
  nrow()

df_public <-
  df_public %>%
  filter(!is.na(location_group))

# add private materials
df_private <-
  df_all %>%
  filter(!is_public) %>%
  select(id, type) %>%
  mutate(location_group = "private")

df_locations <- bind_rows(df_public, df_private)


# calculate frequency
freq_locationgroups <-
  df_locations %>%
  group_by(type, location_group) %>%
  summarize(n = n()) %>%
  ungroup()

# assign color to each group
scale_fill_location_group <-
  scale_fill_manual(values = c(
    "reliable" = "#3cb44b", # green
    "acmdl" = "#0082c8",  # blue
    "unreliable" = "#ffe119",  # yellow
    "private" = "#e6194b"  # red
  ))

# assign order
freq_locationgroups <-
  freq_locationgroups %>%
  mutate(location_group = factor(location_group, levels = c("reliable", "acmdl", "unreliable", "private")))

# plot
p_tmp <-
  freq_locationgroups %>%
  ggplot(aes(x = type, y = n, fill = location_group)) +
  geom_col() +
  scale_x_material_type +
  scale_fill_location_group +
  ylab("Pieces of material\n(one paper may have multiple types,\nbut at most one piece per type)") +
  xlab(NULL) +
  coord_flip() +
  labs(title = "Distribution of pieces of materials by location for each type",
    caption = glue("Excluded {exclusion_count} pieces of research material that specify locations as 'other' or 'prev_work'")) +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

ggsave("output/public_locationgroup_by_type.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)

