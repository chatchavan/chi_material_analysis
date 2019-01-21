library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(glue, glue)
import::from(broom, tidy)
import::from(PropCIs, exactci)
import::from(cowplot, plot_grid, get_legend)

source("r/constants.R")
source("r/io.R")


#===============================================================================
# retrieve availability frequency
df_all <-
  load_data(limesurvey_export_path, exclude_mismatch = FALSE) %>%
  mutate(type = factor(type, levels = rev(types_all)))

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

avail_prob <-
  avail_prob %>%
  group_by(type) %>%
  mutate(
    label = sprintf("%2.0f %%", probability * 100),
    label_pos = cumsum(probability) - 0.5 * probability)

p_avail_prob_by_type <-
  avail_prob %>%
  ggplot(aes(x = fct_inorder(type), y = probability, fill = availability)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = label, y = label_pos), size = 2, color = "white") +
  scale_x_material_type +
  scale_fill_availability +
  coord_flip() +
  xlab(NULL) +
  theme(legend.position = "bottom")

# ggsave("output/avail_prob_by_type.pdf", p_avail_prob_by_type, height = 200/72, width = 300/72, unit = "in", dpi = 72)


#===============================================================================
# CI for the proportion


public_by_type_ci <-
  avail_prob_count %>%
  rowwise() %>%
  do({
    row_type <- .$type[[1]]
    exactci(.$public[[1]], .$paper_count[[1]], 0.95) %>%
      tidy() %>%
      mutate(type = row_type)
  })


p_availability_by_type_proportion <-
  public_by_type_ci %>%
  ggplot(aes(x = fct_inorder(type), ymin = conf.low, ymax = conf.high)) +
  geom_errorbar(width = 0.5) +
  geom_text(data = avail_prob_count, inherit.aes = FALSE,
    aes(x = fct_inorder(type), y = 1, label = paste("n =", paper_count)), hjust = "right", size = 3) +
  scale_x_material_type +
  ylim(0, 1) +
  xlab(NULL) +
  ylab("Clopper-Pearson exact 95% CI of the proportion of public materials") +
  coord_flip()

# ggsave("output/availability_by_type_proportion.pdf", p_availability_by_type_proportion, height = 150/72, width = 300/72, unit = "in", dpi = 72)


#===============================================================================
# combine the plots

legend <- get_legend(p_avail_prob_by_type)
p_two_cols <-
  plot_grid(
    p_avail_prob_by_type + theme(legend.position = "none"),
    p_availability_by_type_proportion + theme(legend.position = "none", axis.text.y = element_blank(), axis.ticks.y = element_blank()),
    # p_response_count,
    nrow = 1, rel_widths = c(0.5, 0.45, 0.05))

p_availability <- plot_grid(p_two_cols, legend, nrow = 2, rel_heights = c(3, 0.3))
ggsave("output/availability.pdf", p_availability, height = 150/72, width = 600/72, unit = "in", dpi = 72)
