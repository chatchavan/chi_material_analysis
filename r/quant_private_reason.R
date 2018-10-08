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

# summary of the responses by research methodology type
df_all %>%
  group_by(method) %>%
  summarise(n = n_distinct(id))

# summary of the private material responses by research methodology
#    (NA means neither quant or qual, e.g., technology contribution)
df_all %>%
  filter(!is_public) %>%
  group_by(method) %>%
  summarise(n = n_distinct(id))


# subsetting only papers with quantitative materials that are private
df_quan <-
  df_all %>%
  filter(method == "quan")

df_quan_private <-
  df_quan %>%
  filter(!is_public)

n_quan_private <- n_distinct(df_quan_private$id)

print(glue("Number of responses with at least one of the predefined material type: {n_distinct(df_all$id)}"))
print(glue("... those of which with quantiative studies: {n_distinct(df_quan$id)}"))
print(glue("...... those of which with at least one material private: {n_quan_private}"))

persist(df_quan_private)
persist(n_quan_private)
rm_all()

#===============================================================================
# determine the probability of each reason

reasons_long <-
  df_quan_private %>%
  select(id, method, type, reason) %>%
  mutate(reason = str_split(reason, ",")) %>%
  unnest(reason)

reasons_prob <-
  reasons_long %>%
  group_by(reason) %>%
  summarize(prob = n_distinct(id) / n_quan_private)

p_tmp <-
  reasons_prob %>%
  mutate(reason = fct_reorder(fct_explicit_na(reason, na_level = "(unspecified)"), prob)) %>%
  ggplot(aes(x = reason, y = prob)) +
  geom_col() +
  xlab(NULL) +
  ylab("Probability") +
  coord_flip() +
  ggtitle("Reasons for private materials (Quantitative studies)",
    subtitle = glue("Among {n_quan_private} papers with at least one type of quantitative materials"))

ggsave("output/private_reason_prob_quan.pdf", p_tmp, height = 200/72, width = 300/72, unit = "in", dpi = 72)
