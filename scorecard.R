library(renv)
library(tidyverse)
library(readxl)
library(httr2)
library(jsonlite)
library(openxlsx)
library(magrittr)

source("r/api.R")
source("r/api_add.R")

data <- list()

# source("O:/Performance Team/Measurement/_ref/helper_functions.R")

# measures <- list(
#   approved = read_excel("C:/Users/lilnguyen/OneDrive - CITY OF PORTLAND, BUREAU OF TECH. SERVICES/Performance/Performance Measurement/FY Planning/Step 3 Finalizing Metrics/Performance Dashboards - Main List.xlsx", "Main") 
# )
# 
# # View objects ####
# scorecards <- sc_read_objects("scorecards")
# containers <- sc_read_objects("containers")
# measures$data <- sc_read_objects("measures", test_env = FALSE, container_id = 10074732)
# 
# measure_status <- measures$data %>%
#   select(ID = id, Title = title, `Measure Type`  = measureType, Calendar = calendarId, Polarity = polarityType, Unit = unitOfMeasure, `Unit Prefix` = uoMisPrefix, Scale = scale, `Data Source` = dataSource, `Graph Type` = graphType, `Previous Time Periods` = previousTimePeriods, `Modify Date` = modifyDate, `Modified By` = modifiedBy) %>%
#   mutate(ID = as.character(ID),
#     Calendar = case_match(Calendar,
#                               7300 ~ "Fiscal Year",
#                               1 ~ "Monthly",
#                               2 ~ "Quarterly",
#                               3 ~ "Annually",
#                               4 ~ "Half Year"),
#          Polarity = case_match(Polarity,
#                               1 ~ "Higher is Better",
#                               2 ~ "Lower is Better",
#                               3 ~ "Target Range",
#                               4 ~ "Do Not Display",
#                               5 ~ "No Polarity"),
#          `Graph Type` = case_match(`Graph Type`,
#                                   1 ~ "Line",
#                                   2 ~ "Bar"))
# measure_template <- measure_status %>%
#   select(ID, Title, `Measure Type`) %>%
#   mutate(`Why Is This Important?` = NA,
#          `What Do The Numbers Show?` = NA,
#          `How Did We Arrive At These Numbers?` = NA,
#          `Where Can I Find More Information?` = NA)
# 
# export_excel(measure_template, "Template", "outputs/Community Econ Dev - Scorecard Template.xlsx", "new",
#              col_width = c(rep("auto", 3), rep(100, 4)))
# export_excel(measure_status, "Status 2024-12-02", "outputs/Community Econ Dev - Scorecard Template.xlsx", "existing")
# # write.xlsx(measures, "outputs/measures.xlsx")
# 
# 
# # Add measures ####
# df <- measures$approved %>%
#   filter(`Service Area` == "Community & Economic Development") %>%
#   select(`Measure Type`, Polarity, Title = `Original Measure`, Notes) %>%
#   mutate(
#     `Measure Type` = case_match(`Measure Type`,
#                                  "Indicator" ~ "indicator",
#                                  "Performance measure" ~ "performance measure"),
#     Polarity = str_trunc(Polarity, 1, "right", ""),
#     Unit = ifelse(grepl("percent", Title, ignore.case = TRUE),
#                   "%", NA))
# 
# map(test, add_sa_measures)
# 
# add_sa_measures <- function(df, measure_row) {
#   
#   measure <- slice(df, measure_row)
#   sc_add_measure(measure_type = measure$`Measure Type`,
#                  title = paste("TEST", measure$Title),
#                  container_id = 10074735,
#                  polarity = measure$Polarity, unit = measure$Unit,
#                  data_source = "TEST Census Data",
#                  test_env = TRUE)
# }
# 
# add_sa_measures(df, 7)
# 
# 
# sc_import_data(measure_id = 101156001, actual_value = 1234, target_value = 4321, date = "2024-07-01", test_env = TRUE)

# parent_id not working
sc_add_scorecard(title = "Portland Children's Levy", parent_id = 89654, test_env = FALSE)
