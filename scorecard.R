library(renv)
library(tidyverse)
library(readxl)
library(httr2)
library(jsonlite)
library(openxlsx)
library(magrittr)

source("r/api.R")
source("r/api_add.R")
source("r/add.R")
source("r/import_data.R")

# to hold data from internal systems/received from bureaus
data <- list(
  prog_offers = read_xlsx("inputs/program_offers_2025-09-19.xlsx"))


# to hold data read from Scorecard
sc <- list()

# View objects ####
sc$scorecards <- sc_read_objects("scorecards", test_env = FALSE)
sc$containers <- sc_read_objects("containers", test_env = FALSE)
sc$measures <- sc_read_objects("measures", test_env = FALSE)