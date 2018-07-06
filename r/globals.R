if (!("g" %in% search())) {

  g <- new.env()

  # material types
  g$types_data_qual <- c("qualraw", "qualcoded", "qualcodebook", "qualcomplete")
  g$types_data_quan <- c("quanraw", "quanprocessed", "quancode", "quancomplete")
  g$types_all <- c("study",  g$types_data_qual, g$types_data_quan, "software", "hardware")

  # persist the variable in the environment
  persist <- function(a_var) {
    assign(deparse(substitute(a_var)), a_var, envir = as.environment("g"))
    rm(list = deparse(substitute(a_var)), pos = ".GlobalEnv")
  }

  attach(g)
  rm(g)

}
