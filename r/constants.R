library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

# material types
types_data_qual <- c("qualraw", "qualcoded", "qualcodebook")
types_data_quan <- c("quanraw", "quanprocessed", "quancode")
types_all <- c("study",  types_data_qual, types_data_quan, "software", "hardware")

persist(types_data_qual)
persist(types_data_quan)
persist(types_all)


# overall analysis settings

limesurvey_export_path <- "input/results-survey.csv"
persist(limesurvey_export_path)

postsurvey_export_path <- "input/results-post-survey.csv"
persist(postsurvey_export_path)


# plot settings

theme_set(theme_grey(base_size = 7))

# ensure that all material types are shown
scale_x_material_type <- scale_x_discrete(drop = FALSE, limits = rev(types_all))
persist(scale_x_material_type)

# color palette: https://sashat.me/2017/01/11/list-of-20-simple-distinct-colors/
scale_fill_20 <-  scale_fill_manual(values = c(
"#e6194b",
"#3cb44b",
"#ffe119",
"#0082c8",
"#f58231",
"#911eb4",
"#46f0f0",
"#f032e6",
"#d2f53c",
"#fabebe",
"#008080",
"#e6beff",
"#aa6e28",
"#fffac8",
"#800000",
"#aaffc3",
"#808000",
"#ffd8b1",
"#000080"))
persist(scale_fill_20)
