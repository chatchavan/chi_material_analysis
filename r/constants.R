library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

# material types
types_data_qual <- c("qualraw", "qualcoded", "qualcodebook")
types_data_quan <- c("quanraw", "quanprocessed", "quancode")
types_all <- c("study",  types_data_qual, types_data_quan, "software", "hardware")
labels_all <- c(
  "Study materials",
  "Raw qualiative data",
  "Coded qualitative data",
  "Coding manual for qualitative data",
  "Raw quantitative data",
  "Processed quantitative data",
  "Data analysis source code",
  "Software prototypes",
  "Hardaware prototypes"
)

persist(types_data_qual)
persist(types_data_quan)
persist(types_all)
persist(labels_all)


# overall analysis settings

limesurvey_export_path <- "input/results-survey.csv"
persist(limesurvey_export_path)

postsurvey_export_path <- "input/results-post-survey.csv"
persist(postsurvey_export_path)

# plot settings

theme_set(theme_grey(base_size = 7))

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

scale_color_20 <-  scale_color_manual(values = c(
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
persist(scale_color_20)


scale_fill_availability <- scale_fill_manual(values = c("#D55E00", "#009E73"), breaks = c("public", "private"))
persist(scale_fill_availability)
