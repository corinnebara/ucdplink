# Whenever I continue developing, start by...
library(devtools)
library(tidyverse)
usethis::use_git()
devtools::load_all(".")

# to check my package
checkresults <- check()
checkresults # see overall results
cat(checkresults$notes, sep = "\n") # see only notes
cat(checkresults$errors)

# To start a new R file
# for now install and load the packages need to make it, they are later in the Description file and import tags
use_r("intersect")
library(future)
library(furrr)

# Push changes to git
system("git add .")
system("git push")

# Update stuff
devtools::document() # update namespace

### Code so far to keep testing in sequence
rm(list = ls())
basedata <- acdtomonthly(postwar_months = 5000)
result <- makezones(basedata, clipcountry = TRUE)
basedata <- result$basedata
episode_zones <- result$episode_zones
gedprepped <- gedprep()
names(gedprepped)
result <- actorlink(gedprepped,basedata)
basedata <- result$basedata
gedtrack <- result$gedtrack
gedintersection <- intersect(gedprepped,episode_zones) # not redo all the time
write_rds(test, "intersect.rds") # not redo all the time
gedintersection <- read_rds("intersect.rds") # not redo all the time

##### temp #####
use_r("makezones")
library(sf)
library(lubridate)
library(cshapes)



##### Stuff I likely don't need to do anymore #####
system("git config user.name \"corinnebara\"")
system("git config user.email \"corinne.bara@sipo.gess.ethz.ch\"")
pkgbuild::check_build_tools(debug = TRUE)
install.packages("RTools")
Sys.which("make")
document()
?strsplitcb
devtools::check(quiet = FALSE)



