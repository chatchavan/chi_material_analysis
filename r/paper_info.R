# paper metadata
library(tidyverse)
library(cleanslate) # devtools::install_github("chatchavan/cleanslate@v0.1.0")


# data downloaded from ACM DL
# This is manually retrieved by executing a Javascript in Chrome's console on at
#   https://dl.acm.org/citation.cfm?id=3173574&preflayout=flat
#
# JavaScript example:
# let a_array = $$('a[name="FullTextPDF"]')
# for (url in urls) {console.log(url)}

title_ordered <- str_split(read_file("input/title_ordered.txt"), "\n", simplify = TRUE)[1,]
doi_ordered <- str_split(read_file("input/doi_ordered.txt"), "\n", simplify = TRUE)[1,]
url_ordered <- str_split(read_file("input/url_ordered.txt"), "\n", simplify = TRUE)[1,]

paper_info <-
  tibble(
    doi = doi_ordered,
    title = title_ordered,
    url = url_ordered)

persist(paper_info)
rm_all()
