library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(broom, tidy)
import::from(PropCIs, exactci)


source("r/constants.R")
source("r/io.R")

#===============================================================================
# load and convert data to a long format
df_public <-
  load_data(limesurvey_export_path, should_exclude_mismatch) %>%
  filter(is_public)

persist(df_public)
rm_all()

#===============================================================================
# RQ 3: For those that are publicly available, where are they?

locations_long <-
  df_public %>%
  select(id, method, type, location) %>%
  mutate(location = str_split(location, ",")) %>%
  unnest(location)

freq_locations <-
  locations_long %>%
  group_by(type, location) %>%
  summarize(n = n()) %>%
  ungroup()

p_tmp <-
  freq_locations %>%
  ggplot(aes(x = type, y = n, fill = location)) +
  geom_col() +
  scale_x_material_type +
  scale_fill_20 +
  xlab(NULL) +
  coord_flip() +
  ggtitle("RQ 3") +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5))

ggsave("output/public_location_by_type.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)

persist(locations_long)
persist(freq_locations)
rm_all()


#===============================================================================
# RQ 3.1: How frequently are they available on an open-access repository,
# with a persistent identifier and in a format that is time-stamped, immutable,
# and permanent?

# TOOD: need manual annotation


#===============================================================================
# RQ 3.2: How frequently is the ACM’s Digital Library used for additional material?

acm_freq <-
  locations_long %>%
  mutate(is_acm = if_else(location %in% c("acmdl", "paper"), "acmdl", "other")) %>%
  mutate(is_acm = factor(is_acm, levels = c("acmdl", "other"))) %>%
  select(id, type, is_acm) %>%
  group_by(type, is_acm) %>%
  summarize(n = n()) %>%
  spread(key = is_acm, value = n, fill = 0L, drop = FALSE) %>%
  ungroup() %>%
  mutate(total = acmdl + other)

acm_by_type_ci <-
  acm_freq %>%
  rowwise() %>%
  do({
    row_type <- .$type[[1]]
    exactci(.$acmdl[[1]], .$total[[1]], 0.95) %>%
      tidy() %>%
      mutate(type = row_type)
  })



p_tmp <-
  acm_by_type_ci %>%
  ggplot(aes(x = type, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0) +
  scale_x_material_type +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("95% CI of the proportion of materials on ACM DL (in paper or as supplement materials)\n to the all public materials (Clopper-Pearson exact CI)") +
  coord_flip() +
  ggtitle("RQ 3.2")

ggsave("output/public_acm_proportion.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)

rm_all()
