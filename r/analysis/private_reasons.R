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

df_private <-
  df_all %>%
  filter(!is_public)

paper_count_by_type <-
  df_all %>%
  group_by(type) %>%
  summarize(paper_count = n_distinct(id)) %>%
  ungroup

persist(df_private)
persist(paper_count_by_type)
rm_all()


#===============================================================================
# calculate probability of each reason by material type

reasons_long <-
  df_private %>%
  select(id, type, reason) %>%
  mutate(reason = str_split(reason, ",")) %>%
  unnest(reason)

reasons_count <-
  reasons_long %>%
  group_by(type, reason) %>%
  summarize(n = n()) %>%
  ungroup() %>%
  right_join(paper_count_by_type, by = "type")

reasons_prob <-
  reasons_count %>%

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
reasons_prob <-
  reasons_prob %>%
  replace_na(list(reason = "NA"))

# labels and order
reasons_dict <- list(
  "sensitive_participant" = "Sensitive data (participants)",
  "sensitive_researcher"  = "Sensitive data (researchers)",
  "sensitive_government"  = "Sensitive data (gov., mil., org.)",
  "no_permission"         = "Haven't obtained participants' permission",
  "resource_annonymize"   = "Lack resources to anonymize",
  "commercial"            = "Commercial value",
  "research"              = "Research value",
  "resource_present"      = "Lack resources (improve presentation)",
  "resource_distribute"   = "Lack resources (distribute & maintain)",
  "off_context"           = "Out of context",
  "no_benefit"            = "Don't see benefit of making public",
  "dont_know"             = "Don't know why haven't made public",
  "other"                 = "Other",
  "NA"                    = "Unspecified"
)
scale_x_reasons <-
  scale_x_discrete(
    drop = FALSE,
    limits = rev(names(reasons_dict)),   # rev() is needed when coord_flip()
    labels = rev(unlist(reasons_dict)))


# plot configuration
plot_config <- list(
	geom_col(),
	geom_text(aes(label = label, y = 1), size = 1.5, hjust = "right"),
  geom_errorbar(size = 0.3, width = 0.5),
	scale_x_reasons,
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
    reasons_prob %>%
    filter(type %in% type_levels) %>%
    mutate(type = factor(type, levels = type_levels)) %>%
    ggplot(aes(x = reason, y = probability, ymin = conf.low, ymax = conf.high)) +
    plot_config

  gtable_tmp <- set_panel_size(p_tmp, width = unit(3.5, "cm"), height = unit(3.5, "cm"))
  overall_width <- convertWidth(sum(gtable_tmp$widths), "in", valueOnly = TRUE)
  overall_height <- convertHeight(sum(gtable_tmp$heights), "in", valueOnly = TRUE)
  ggsave(plot_path, gtable_tmp, width = overall_width + 0.2, height = overall_height)

  # return
  invisible(gtable_tmp)
}


#===============================================================================
# plot for groups of material types

plot_group(c("study"), "output/reasons_study.pdf")
plot_group(c("quanraw",  "quanprocessed", "quancode"), "output/reasons_quan.pdf")
plot_group(c("qualraw",  "qualcoded", "qualcodebook"), "output/reasons_qual.pdf")
plot_group(c("software",  "hardware"), "output/reasons_prototypes.pdf")
