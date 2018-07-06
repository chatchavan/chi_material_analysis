# persist.R
# -------
# Create a separate environment to store variables that should be persisting
# after clearning the interactive environemnt with rm(list = ls()).
#
# by Chat Wacharamanotham

if (!("__persist" %in% search())) {

  g <- new.env()

  g$persist <- function(a_var) {
    assign(deparse(substitute(a_var)), a_var, envir = as.environment("__persist"))
    rm(list = deparse(substitute(a_var)), pos = ".GlobalEnv")
  }

  attach(g, name = "__persist")
  rm(g)

}
