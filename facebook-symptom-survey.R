# This script functions like a primitive makefile. It downloads and tidies data,
# and does other desired analyses


# Setup
library(tidyverse)

# Data preprocessing
source("src/import-data.R")

# Update README
rmarkdown::render("README.Rmd")


# Update website
rmarkdown::render("docs/index.Rmd")


# Explorations of regional variables. 
source("src/explore-aus-regions.R")