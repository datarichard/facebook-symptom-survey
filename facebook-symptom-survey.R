# This script functions like a primitive makefile. It downloads and tidies data,
# and does other desired analyses


# Setup
library(tidyverse)

# Data preprocessing
require(httr) 
require(jsonlite)
source("src/import-data.R")

# Explorations of regional variables. 
source("src/explore-aus-regions.R")
