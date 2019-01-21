library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

# material types
types_data_qual <- c("qualraw", "qualcoded", "qualcodebook")
types_data_quan <- c("quanraw", "quanprocessed", "quancode")
persist(types_data_qual)
persist(types_data_quan)

types_dict <- list(
  "study"         = "Study materials",
  "qualraw"       = "Raw qualiative data",
  "qualcoded"     = "Coded qualitative data",
  "qualcodebook"  = "Coding manual for qualitative data",
  "quanraw"       = "Raw quantitative data",
  "quanprocessed" = "Processed quantitative data",
  "quancode"      = "Data analysis source code",
  "software"      = "Software prototypes",
  "hardware"      = "Hardware prototypes"
)
persist(types_dict)


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



