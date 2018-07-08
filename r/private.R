library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(broom, tidy)
import::from(pairwiseCI, pairwiseCI)
import::from(PropCIs, exactci)
import::from(roomba, roomba) # devtools::install_github("ropenscilabs/roomba")


source("r/constants.R")
source("r/io.R")

# settings
theme_set(theme_grey(base_size = 7))


#===============================================================================
# load and convert data to a long format
df_private <-
  load_data("input/results-survey332449.csv") %>%
  filter(!is_public)

persist(df_private)
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
  ggplot(aes(x = type, y = n, fill = reason)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  ggtitle("RQ 2")

ggsave("output/private_reason_by_type.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)

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
  coord_flip() +
  ggtitle("RQ 2.1")

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
  mutate(type = factor(type, levels = types_all)) %>%
  mutate(type = fct_rev(type)) %>%
  ggplot(aes(x = type, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0) +
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
  ggtitle("RQ 2.2")

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
  mutate(type = factor(type, levels = types_all)) %>%
  mutate(type = fct_rev(type)) %>%
  ggplot(aes(x = type, ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0) +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("95% CI of the proportion of private materials that were stated as unknown to the all private materials\n (Clopper-Pearson exact CI)") +
  coord_flip() +
  ggtitle("RQ 2.3")

ggsave("output/private_dontknow_proportion.pdf", p_tmp, height = 150/72, width = 300/72, unit = "in", dpi = 72)

rm_all()
