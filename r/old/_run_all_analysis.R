library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")

all_analysis <- function() {
  rm(list = ls())
  source("r/availability.R")
  rm(list = ls())
  source("r/private.R")
  rm(list = ls())
  source("r/public.R")
  rm(list = ls())
}
persist(all_analysis)

# create output path
dir.create("output/all_data", recursive = TRUE, showWarnings = FALSE)
dir.create("output/exclude_expertise_mismatch", recursive = TRUE, showWarnings = FALSE)

# analysis without respondents whose expertise mismatch the material types
should_exclude_mismatch <- FALSE
persist(should_exclude_mismatch)
all_analysis()
output_pdf <- list.files("output", "*.pdf")
target_path <- file.path("output/all_data", output_pdf)
file.rename(file.path("output", output_pdf), target_path)


# analysis of all data
should_exclude_mismatch <- TRUE
persist(should_exclude_mismatch)
all_analysis()
output_pdf <- list.files("output", "*.pdf")
target_path <- file.path("output/exclude_expertise_mismatch", output_pdf)
file.rename(file.path("output", output_pdf), target_path)
