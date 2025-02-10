library(devtools)
system("git config user.name \"corinnebara\"")
system("git config user.email \"corinne.bara@sipo.gess.ethz.ch\"")
usethis::use_git()
load_all()


x <- "alfa,bravo,charlie,delta"
strsplitcb(x, split = ",")

exists("strsplitcb", where = globalenv(), inherits = FALSE)
system("git add R/strsplitcb.R")

output <- system("git ls-tree HEAD R/strsplitcb.R", intern = TRUE)
print(output)

log_output <- system("git log --oneline", intern = TRUE)
print(log_output)

status_output <- system("git status", intern = TRUE)
print(status_output)

all_files <- system("git ls-tree -r HEAD", intern = TRUE)
print(all_files)

check()
pkgbuild::check_build_tools(debug = TRUE)
install.packages("RTools")
Sys.which("make")
document()
?strsplitcb
check()
devtools::check(quiet = FALSE)

##### Helper function UCDP download via API #####
library(httr)
library(jsonlite)
install.packages("httr")
install.packages("jsonlite")

use_r("ucdpdownload")
usethis::use_package("httr")
system("git add .")
system("git push")


##### Try ACD to monthly, with external download #####
use_r("acdtomonthly")

usethis::use_package("dplyr")
usethis::use_package("tidyr")
usethis::use_package("stringr")
usethis::use_package("readr")
usethis::use_package("lubridate")







