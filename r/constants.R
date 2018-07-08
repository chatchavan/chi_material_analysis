library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

# material types
types_data_qual <- c("qualraw", "qualcoded", "qualcodebook")
types_data_quan <- c("quanraw", "quanprocessed", "quancode")
types_all <- c("study",  types_data_qual, types_data_quan, "software", "hardware")

persist(types_data_qual)
persist(types_data_quan)
persist(types_all)


# overall analysis settings

limesurvey_export_path <- "input/results-survey673185.csv"
persist(limesurvey_export_path)


# plot settings

theme_set(theme_grey(base_size = 7))

# ensure that all material types are shown
scale_x_material_type <- scale_x_discrete(drop = FALSE, limits = rev(types_all))
persist(scale_x_material_type)
