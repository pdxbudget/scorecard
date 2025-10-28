sc_read_objects <- function(objects, container_id = NA, test_env = TRUE) {
  
  # objects: 'scorecards','containers', 'indicators', 'perfmeasures', or 'measures'. 'measures' returns both 'indicators' and 'perfmeasures'.
  # container_id: optional for indicators, perfmeaures, or measures if you want to limit the list to those in a specific container
  # test_env: boolean, defaults to TRUE. TRUE sends the API request to the test environment (accessed at https://scorecard-test.clearimpact.com), whereas FALSE sends it to production.
  
  tryCatch({
    
    if (is.null(objects) || !objects %in% c("scorecards","containers", "indicators", "perfmeasures", "measures")) {
      stop("'objects' argument must be: 'scorecards','containers', 'indicators', 'perfmeasures', or 'measures'")
    }
    
    if (objects %in% c("indicators", "perfmeasures", "measures")) {
      url <- ifelse(test_env,
                    "https://api-test.clearimpact.com/api/measures/list",
                    "https://api.resultsscorecard.com/api/measures/list")

      measure_type <- switch(
        objects,
        indicators = "indicator", 
        perfmeasures = "performance measure",
        NA)
      
    } else {

      url <- ifelse(test_env,
                    paste0("https://api-test.clearimpact.com/api/", objects ,"/list"),
                    paste0("https://api.resultsscorecard.com/api/", objects ,"/list"))

      measure_type <- NA
    }
    
    
    url %>%
      request() %>%
      req_body_json(list(siteCode = "Portland",
                         apiKey = Sys.getenv("SC_API_KEY"),
                         measureType = measure_type,
                         containerID = container_id),
                    encode = "json") %>%
      req_perform() %>%
      resp_body_string() %>%
      fromJSON()
  },
  
  error = function(cond) {
    
    warning("Error fetching ", objects, " from Scorecard: ", cond)
    
  }
  )
}




sc_import_data <- function(
    measure_id, actual_value, target_value = NA, date, comment = NA, test_env = TRUE) {
  
    # id [int](Required) - The unique Clear Impact Scorecard ID for the measure that data values are being imported into. Measure IDs can be found by downloading the measure list.
  # actualValue [decimal] (Optional) - Integer data value for the actual value, up to 6 decimals
  # targetValue [decimal] (Optional) - Integer data value for the target value, up to 6 decimals
  # actualValueDate [dateTime] (Required) - Determines the time period to import the data value for, expressed as YYYY-MM-DD. To import values for a specific time period, use a date anywhere within that time period. For example, March 2013 runs from 2013-03-01 through 2013 - 03 - 31. So you can enter any date within this period, such as 2013-03-15.
  # comment [string] (Optional) - Comment for the data value, up to 255 characters (will be truncated after 255)
  # test_env: boolean, defaults to TRUE. TRUE sends the API request to the test environment (accessed at https://scorecard-test.clearimpact.com), whereas FALSE sends it to production.
  
  tryCatch({
    
    if (is.null(measure_id)) {
      stop("'measure_id' must be numeric and length 9")
    }
    
    url <- ifelse(test_env,
                  "https://api-test.clearimpact.com/api/datavalues/import",
                  "https://api.resultsscorecard.com/api/datavalues/import")
    
    url %>%
      request() %>%
      req_body_json(list(siteCode = "Portland",
                         apiKey = Sys.getenv("SC_API_KEY"),
                         id = measure_id,
                         actualValue = actual_value,
                         targetValue = target_value,
                         actualValueDate = lubridate::ymd(date),
                         comment = comment),
                    encode = "json") %>%
      req_perform(verbosity = 2)
  },
  
  error = function(cond) {
    
    warning("Error importing ", measure_id," in Scorecard: ", cond)
    
  }
  )
}