library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")
import::from(broom, tidy)
import::from(PropCIs, exactci)
import::from(grid, convertWidth, convertHeight)
import::from(egg, set_panel_size)

source("r/constants.R")
source("r/io.R")

#===============================================================================
# prepare data of private materials
df_all <-
  load_data(limesurvey_export_path)

df_public <-
  df_all %>%
  filter(is_public)

public_count_by_type <-
  df_public %>%
  group_by(type) %>%
  summarize(paper_count = n_distinct(id)) %>%
  ungroup

persist(df_public)
persist(public_count_by_type)
rm_all()


#===============================================================================
# calculate probability of each location by material type

locations_long <-
  df_public %>%
  select(id, type, location) %>%
  mutate(location = str_split(location, ",")) %>%
  unnest(location)

locations_count <-
  locations_long %>%
  group_by(type, location) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  right_join(public_count_by_type, by = "type")

locations_prob <-
  locations_count %>%

  # calculate CI
  rowwise() %>%
  do({
    the_ci <- exactci(.$n[[1]], .$paper_count[[1]], 0.95) %>% tidy()
    bind_cols(., the_ci)
  }) %>%

  # calculate percentage probability
  mutate(probability = n / paper_count) %>%
  mutate(label = sprintf("%2.0f %%", probability * 100))



#===============================================================================
# prepare for the plot

# ensure that NA is labeled (we shouldn't need this if scale_x_discrete(na.values = "Unspecified") works)
locations_prob <-
  locations_prob %>%
  replace_na(list(location = "NA"))

# labels and order
locations_dict <- list(
  "paper"       = "In the paper",
  "prev_work"   = "In a cited work",
  "acmdl"       = "On ACM DL",
  "repo_pub"    = "On scientific repo",
  "repo_github" = "On GitHub",
  "repo_uni"    = "On university repo",
  "web"         = "On own website",
  "author"      = "Upon request",
  "other"       = "Other"
)
scale_x_locations <-
  scale_x_discrete(
    drop = FALSE,
    limits = rev(names(locations_dict)),   # rev() is needed when coord_flip()
    labels = rev(unlist(locations_dict)))


# plot configuration
plot_config <- list(
	geom_col(),
	geom_text(aes(label = label, y = 1), size = 1.5, hjust = "right"),
  geom_errorbar(size = 0.3, width = 0.5),
	scale_x_locations,
	ylim(0, 1),
	xlab(NULL),
  ylab(NULL),
	coord_flip(),
	facet_wrap( ~ type, labeller = labeller(type = unlist(types_short_dict)) ),
	guides(fill = guide_legend(ncol = 1, keywidth = 0.5, keyheight = 0.5)),
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_blank()))

# groupped plot function
plot_group <- function(type_levels, plot_path) {
  p_tmp <-
    locations_prob %>%
    filter(type %in% type_levels) %>%
    mutate(type = factor(type, levels = type_levels)) %>%
    ggplot(aes(x = location, y = probability, ymin = conf.low, ymax = conf.high)) +
    plot_config

  gtable_tmp <- set_panel_size(p_tmp, width = unit(3, "cm"), height = unit(3, "cm"))
  overall_width <- convertWidth(sum(gtable_tmp$widths), "in", valueOnly = TRUE)
  overall_height <- convertHeight(sum(gtable_tmp$heights), "in", valueOnly = TRUE)
  ggsave(plot_path, gtable_tmp, width = overall_width + 0.2, height = overall_height)

  # return
  invisible(gtable_tmp)
}


#===============================================================================
# plot for groups of material types

plot_group(c("study"), "output/locations_study.pdf")
plot_group(c("quanraw",  "quanprocessed", "quancode"), "output/locations_quan.pdf")
plot_group(c("qualraw",  "qualcoded", "qualcodebook"), "output/locations_qual.pdf")
plot_group(c("software",  "hardware"), "output/locations_prototypes.pdf")
