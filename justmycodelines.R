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
use_r("gedprep")


# Push changes to git
system("git add .")
system("git push")

# Update stuff
devtools::document() # update namespace


##### temp #####
use_r("makezones")






##### Stuff I likely don't need to do anymore #####
system("git config user.name \"corinnebara\"")
system("git config user.email \"corinne.bara@sipo.gess.ethz.ch\"")
pkgbuild::check_build_tools(debug = TRUE)
install.packages("RTools")
Sys.which("make")
document()
?strsplitcb
devtools::check(quiet = FALSE)



