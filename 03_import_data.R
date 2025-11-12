# historical data in BFM
data$bfm <- read_xlsx("inputs/Citywide_Performance_Measure_Values_and_Metadata_2025-10-31.xlsx") %>%
  select(`BFM ID` = `Performance Measure Cd`, `FY 2016-17 Actuals`:`FY 2024-25 Actuals`) 

# measures currently in Scorecard
sc$measures <- sc_read_objects("measures", test_env = FALSE)
sc$measure_id <- sc$measures %>%
  select(`SC Measure ID` = id, title)

measure_data <- clean_measure_data("Bureau of Fleet & Facilities")

# create csv for a manual batch upload through Scorecard UI, or...
batch_upload_measure_data(measure_data)

# use Scorecard API...
import_measure_data(measure_data)
