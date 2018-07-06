library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

# material types
types_data_qual <- c("qualraw", "qualcoded", "qualcodebook", "qualcomplete")
types_data_quan <- c("quanraw", "quanprocessed", "quancode", "quancomplete")
types_all <- c("study",  types_data_qual, types_data_quan, "software", "hardware")

persist(types_data_qual)
persist(types_data_quan)
persist(types_all)
