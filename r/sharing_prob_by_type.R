library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(glue, glue)
import::from(broom, tidy)
import::from(PropCIs, exactci)


source("r/constants.R")
source("r/io.R")


#===============================================================================
# retrieve availability frequency
df_all <-
  load_data(limesurvey_export_path, exclude_mismatch = FALSE)

# (For reference: 665 inivitations; 222 responses; of which 29 didn’t generate any research material)

avail_prob_count <-
  df_all %>%
  mutate(is_public = if_else(is_public, "public", "private")) %>%
  mutate(is_public = factor(is_public, levels = c("public", "private"))) %>%
  select(id, type, is_public) %>%
  group_by(type, is_public) %>%
  summarize(n = n_distinct(id)) %>%
  ungroup() %>%
  spread(is_public, n) %>%
  mutate(paper_count = public + private) %>%
  mutate(p_public = public / paper_count) %>%
  mutate(p_private = private / paper_count)


avail_prob <-
  avail_prob_count %>%
  select(type, public = p_public, private = p_private) %>%
  gather("availability", "probability", public, private)

View(avail_prob)

avail_prob <-
  avail_prob %>%
  group_by(type) %>%
  mutate(
    label = sprintf("%2.0f %%", probability * 100),
    label_pos = cumsum(probability) - 0.5 * probability)

p_tmp <-
  avail_prob %>%
  ggplot(aes(x = type, y = probability, fill = availability)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label, y = label_pos), size = 2) +
  coord_flip()

ggsave("output/avail_prob_by_type.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)
