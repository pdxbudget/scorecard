clean_measure_data <- function(bureau, check_values = FALSE) {
  
  # check_values: optional boolean, indicate if you want to keep the column showing original values as imported from the template for troubleshooting
  
  df <- read_xlsx(paste0("inputs/bfm_metrics_to_transfer/", bureau, " - Performance Template.xlsx")) %>%
    filter(`SC-Keep Measure?` %in% c("Yes", "New")) %>%
    select(`BFM ID`:`SC-Notes`) %>%
    left_join(data$bfm, by = join_by(`BFM ID`)) %>%
    rename(`BFM FY 2024-25 Actuals` = `FY 2024-25 Actuals`) %>%
    mutate(`FY 2024-25 Actuals` = 
             ifelse((`BFM FY 2024-25 Actuals` %in% c(999999, 0) | is.na(`BFM FY 2024-25 Actuals`)),
                    `SC-FY 2024-25 Actuals`, `BFM FY 2024-25 Actuals`)) %>%
    relocate(`BFM FY 2024-25 Actuals`, .after = `FY 2024-25 Actuals`) %>%
    pivot_longer(`FY 2016-17 Actuals`:`FY 2024-25 Actuals`, names_to = "FY", values_to = "Value") %>%
    mutate(
      `Orig Value` = Value,
      Value = gsub(",", "", Value, fixed = TRUE),
           Value = gsub("$", "", Value, fixed = TRUE),
           Value = gsub("%", "", Value, fixed = TRUE),
           Value = gsub("N/A", "", Value, fixed = TRUE),
           Value = gsub("NA", "", Value, fixed = TRUE),
           Value = as.numeric(Value),
           Date = str_extract(FY, "[0-9]{1,4}"), # extract the first YYYY of an FY,
           Date = ymd(paste0(Date, "-07-01"))) %>%
    left_join(sc$measure_id, by = join_by("Measure Title" == "title"))
  
  if (check_values == FALSE) {
    
    df <- df %>%
      filter(!is.na(Value)) %>%
      select(-`Orig Values`)
    
    return(df)
    
  } else {
    
    return(df)
    
  }
}

batch_upload_measure_data <- function(df) {
  tryCatch({
    
    df <- df %>%
      mutate(target_value = NA,
             comment = NA) %>%
      select(id = `SC Measure ID`,
             actual_value_date = Date,
             target_value,
             actual_value = Value,
             comment) %>%
      mutate(actual_value)
    
    write_excel_csv(df, "outputs/batch_upload.csv", na = "")
    
  },
  
  error = function(cond) {
    
    warning("Error exporting csv for batch upload: ", cond)
    
  }
  )
}

import_measure_data <- function(df) {
  
  tryCatch({
    for (i in unique(df$`SC Measure ID`)) {
      
      measure_df <- df %>%
        filter(`SC Measure ID` == i)
      
      for (j in 1:nrow(measure_df)) {
        
        sc_import_data(
          measure_id = measure_df$`SC Measure ID`[j],
          actual_value = measure_df$Value[j],
          # target_value = NA,
          date = measure_df$Date[j],
          # comment = NA,
          test_env = FALSE)
      }
    }
  },
  
  error = function(cond) {
    
    warning("Error importing measure ", measure_id, ": ", cond)
    
  }
  )
}