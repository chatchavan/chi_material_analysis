library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(glue, glue)
import::from(broom, tidy)
import::from(PropCIs, exactci)


source("r/constants.R")
source("r/io.R")


#===============================================================================
# summary
df_all <-
  load_data(limesurvey_export_path, exclude_mismatch = FALSE)

# (For reference: 665 inivitations; 222 responses; of which 29 didn’t generate any research material)

# determine research methodolgy types
ids_with_quan <- df_all %>% filter(method == "quan") %>% pull(id) %>% unique()
ids_with_qual <- df_all %>% filter(method == "qual") %>% pull(id) %>% unique()
ids_mix <- intersect(ids_with_quan, ids_with_qual)
ids_quan <- setdiff(ids_with_quan, ids_with_qual)
ids_qual <- setdiff(ids_with_qual, ids_with_quan)

# recode the methdology type for those with mixed method
df_all <-
  df_all %>%
  mutate(method = if_else(id %in% ids_mix, "mix", method))

# summary of the responses by research methodology type
#    (NA means neither quant or qual, e.g., technology contribution)
tbl_total <-
df_all %>%
  group_by(method) %>%
  summarise(n_total = n_distinct(id))

# summary of the private material responses by research methodology
tbl_private <-
  df_all %>%
  filter(!is_public) %>%
  group_by(method) %>%
  summarise(n_private = n_distinct(id))

tbl_summary <- inner_join(tbl_total, tbl_private, by = "method")

#===============================================================================
# determine the probability of each reason
tabulte_reason_prob <- function(df) {
  paper_count <- n_distinct(df$id)

  reasons_long <-
    df %>%
    select(id, method, type, reason) %>%
    mutate(reason = str_split(reason, ",")) %>%
    unnest(reason)

  reasons_prob <-
    reasons_long %>%
    group_by(reason) %>%
    summarize(
      n = n_distinct(id),
      probability = n_distinct(id) / paper_count)

  reasons_prob
}

reason_prob_quan <-
  df_all %>%
  filter(method == "quan" | method == "mix") %>%
  filter(!is_public) %>%
  tabulte_reason_prob()

reason_prob_qual <-
  df_all %>%
  filter(method == "qual" | method == "mix") %>%
  filter(!is_public) %>%
  tabulte_reason_prob()

#===============================================================================
# plot

reason_prob_quan$category = "quan"
reason_prob_qual$category = "qual"
reason_prob <-
  bind_rows(reason_prob_qual, reason_prob_quan) %>%
  mutate(reason = fct_explicit_na(reason, na_level = "(unspecified)"))

# (for plotting)
reason_prob_quan_only <- reason_prob %>% filter(category == "quan")
reason_prob_qual_only <- reason_prob %>% filter(category == "qual")

p_tmp <-
  reason_prob %>%
  ggplot(aes(x = category, y = probability, color = reason)) +
  geom_point() +
  geom_text(aes(label = reason, x = category, y = probability), data = reason_prob_quan_only,
    nudge_x = 0.1, color = "black", hjust = "left", size = 2) +
  geom_text(aes(label = reason, x = category, y = probability), data = reason_prob_qual_only,
    nudge_x = -0.1, color = "black", hjust = "right", size = 2) +
  scale_x_discrete(expand = expand_scale(add = c(2, 2))) +
  scale_color_20 +
  xlab(NULL) +
  guides(
    color = FALSE) # guide_legend(keyheight = 0.5, keywidth = 0.5))

ggsave("output/private_reason_prob.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)

