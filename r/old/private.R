library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(broom, tidy)
import::from(PropCIs, exactci)


source("r/constants.R")
source("r/io.R")


#===============================================================================
# load and convert data to a long format
df_private <-
  load_data(limesurvey_export_path, should_exclude_mismatch) %>%
  filter(!is_public)

df_all <-
  load_data(limesurvey_export_path, exclude_mismatch = FALSE)

paper_count_by_type <-
  df_all %>%
  group_by(type) %>%
  summarize(paper_count = n_distinct(id)) %>%
  ungroup

persist(df_private)
persist(paper_count_by_type)
rm_all()

#===============================================================================
# RQ 2: For those that are private (not publicly available), what are they? what are the reasons?

reasons_long <-
  df_private %>%
  select(id, method, type, reason) %>%
  mutate(reason = str_split(reason, ",")) %>%
  unnest(reason)

freq_reasons <-
  reasons_long %>%
  group_by(type, reason) %>%
  summarize(n = n()) %>%
  ungroup()

p_tmp <-
  freq_reasons %>%
  ggplot(aes(x = reason, y = n)) +
  geom_col() +
  scale_fill_20 +
  xlab(NULL) +
  coord_flip() +
  facet_wrap( ~ type) +
  guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5)) +
  ggtitle("RQ 2")

ggsave("output/private_reason_by_type.pdf", p_tmp, height = 300/72, width = 300/72, unit = "in", dpi = 72)


#-------------------------------------------------------------------------------
# calculate probability

prob_reasons <-
  freq_reasons %>%
  right_join(paper_count_by_type, by = "type") %>%
  mutate(probability = n / paper_count) %>%
  select(type, reason, probability) %>%
  mutate(label = sprintf("%2.0f %%", probability * 100))

p_tmp <-
  prob_reasons %>%
  ggplot(aes(x = reason, y = probability)) +
  geom_col() +
  geom_text(aes(label = label), size = 1, nudge_y = 0.1) +
  scale_fill_20 +
  xlab(NULL) +
  coord_flip() +
  facet_wrap( ~ type) +
  guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5))

ggsave("output/private_reason_by_type_prob.pdf", p_tmp, height = 300/72, width = 300/72, unit = "in", dpi = 72)

#===============================================================================
# RQE: What is the distribution of reasons across types?


p_tmp <-
  freq_reasons %>%
  ggplot(aes(x = reason, y = n)) +
  geom_col() +
  scale_fill_20 +
  xlab(NULL) +
  coord_flip() +
  guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5)) +
  ggtitle("Overall distribtion of reasons")

p_tmp <-
  freq_reasons %>%
  group_by(reason) %>%
  summarise(n = sum(n)) %>%
  mutate(reason = fct_reorder(fct_explicit_na(reason), n)) %>%
  ggplot(aes(x = reason, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("Overall distribtion of reasons")

ggsave("output/private_reason_overall.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)


persist(reasons_long)
persist(freq_reasons)
rm_all()


#===============================================================================
# RQ 2.1: How frequently do the private materials are deemed relevant to the claims in the paper?

relevance_by_type <-
  df_private %>%
  mutate(is_claim_related = if_else(is_claim_related, "related", "unrelated")) %>%
  mutate(is_claim_related = factor(is_claim_related, levels = c("related", "unrelated"))) %>%
  select(id, type, is_claim_related) %>%
  group_by(type, is_claim_related) %>%
  summarize(n = n()) %>%
  ungroup()

p_tmp <-
  relevance_by_type %>%
  ggplot(aes(x = type, y = n, fill = is_claim_related)) +
  geom_col() +
  scale_x_material_type +
  coord_flip() +
  ggtitle("RQ 2.1") +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5)) +
  theme(legend.position = "bottom")

ggsave("output/private_relevance_by_type.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)


#-------------------------------------------------------------------------------
# proportion and CI

relevance_by_type_ci <-
  relevance_by_type %>%
  spread(key = is_claim_related, value = n, fill = 0L, drop = FALSE) %>%
  mutate(total = related + unrelated) %>%
  rowwise() %>%
  do({
    row_type <- .$type[[1]]
    exactci(.$related[[1]], .$total[[1]], 0.95) %>%
      tidy() %>%
      mutate(type = row_type)
  })

p_tmp <-
  relevance_by_type_ci %>%
  ggplot(aes(x = type, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0) +
  scale_x_material_type +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("95% CI of the proportion of claim-related private materials to all private materials\n (Clopper-Pearson exact CI)") +
  coord_flip() +
  ggtitle("RQ 2.1")

ggsave("output/private_relevance_by_type_proportion.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)

rm_all()

#===============================================================================
# RQ 2.2: Is the distribution of reasons different between quantitative and qualitative studies?

freq_reason_method <-
  reasons_long %>%
  filter(reason != "dont_know") %>%
  mutate(type = if_else(type == "study", type, "data")) %>%
  group_by(reason, method, type) %>%
  summarize(n = n()) %>%
  ungroup()

p_tmp <-
  freq_reason_method %>%
  mutate_at(c("reason", "method", "type"), as.factor) %>%
  complete(reason, method, type) %>%
  ggplot(aes(x = reason, y = n, fill = method)) +
  geom_col(position = "dodge") +
  facet_grid(type ~ .) +
  coord_flip() +
  ggtitle("RQ 2.2") +
  guides(fill = guide_legend(keywidth = 0.5, keyheight = 0.5)) +
  theme(legend.position = "bottom")

ggsave("output/private_reason_by_method.pdf", p_tmp, height = 300/72, width = 300/72, unit = "in", dpi = 72)

rm_all()

#===============================================================================
# RQ 2.3: When materials are not publicly available, how frequently is the stated
# reason “I don’t know why we didn’t make this public”?

dontknow_freq <-
  reasons_long %>%
  mutate(is_dontknow = if_else(reason == "dont_know", "dont_know", "other")) %>%
  mutate(is_dontknow = factor(is_dontknow, levels = c("dont_know", "other"))) %>%
  select(id, type, is_dontknow) %>%
  group_by(type, is_dontknow) %>%
  summarize(n = n()) %>%
  spread(key = is_dontknow, value = n, fill = 0L, drop = FALSE) %>%
  ungroup() %>%
  mutate(total = dont_know + other)

dontknow_by_type_ci <-
  dontknow_freq %>%
  rowwise() %>%
  do({
    row_type <- .$type[[1]]
    exactci(.$dont_know[[1]], .$total[[1]], 0.95) %>%
      tidy() %>%
      mutate(type = row_type)
  })


p_tmp <-
  dontknow_by_type_ci %>%
  ggplot(aes(x = type, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0) +
  scale_x_material_type +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("95% CI of the proportion of private materials that were stated as unknown\n to the all private materials (Clopper-Pearson exact CI)") +
  coord_flip() +
  ggtitle("RQ 2.3")

ggsave("output/private_dontknow_proportion.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)

rm_all()
