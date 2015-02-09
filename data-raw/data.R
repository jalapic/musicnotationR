dom <- read.csv("data-raw/dom.csv",
                           stringsAsFactors = FALSE)
flies <- read.csv("data-raw/flies.csv",
                stringsAsFactors = FALSE)
devtools::use_data(dom, flies, overwrite = TRUE)
