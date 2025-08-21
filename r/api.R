sc_read_objects <- function(objects, container_id = NA, test = TRUE) {
  
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


sc_add_measure <- function(
    measure_type, title, calendar = "fy", data_source, polarity = 1, scale = 0,
    unit, unit_prefix = FALSE, test_env = TRUE) {
  
  # measure_type: string, 'indicator' or 'performance measure'
  # title: string, a name for the measure
  # calendar: string, defaults to 'fy'. 'month', 'quarter', 'annual', 'half year', 'fy quarter', or 'fy'
  # polarity: numeric, defaults to 1. 1 - Higher is Better, 2 - Lower is Better, 3 - Target Range, 4 - Do Not Display,5 - No Polarity
  # scale: numeric, defaults to 0. Number of decimal points to show.
  # unit: string, defaults to blank. Symbol for the unit of measure, typically % or $.
  # unit_prefix: boolean, defaults to FALSE. TRUE moves the unit of measure to the front of the number, like "$1". FALSE keeps the unit symbol behind the number, like "1%". 
  # test_env: boolean, defaults to TRUE. TRUE sends the API request to the test environment (accessed at https://scorecard-test.clearimpact.com), whereas FALSE sends it to production.
  
  tryCatch({
    
    # see 'Scorecard Settings > Object Management > Object Types' for all the measure types in the system
    if (is.null(measure_type) || !measure_type %in% c("indicator", "performance measure", "budget")) {
      stop("'measure_type' argument must be: 'indicator', 'performance measure', or 'budget'")
    }
    
    if (is.null(calendar) || !calendar %in% 
        c("month", "quarter", "annual", "half year", "fy quarter", "fy")) {
      stop("'measure_type' argument must be: 'indicator', 'performance measure', or 'budget'")
    }
    
    if (is.null(title) || !is.character(title)) {
      stop("'title' argument is required and must be a string")
    }
 
    url <- ifelse(test_env,
                  "https://api-test.clearimpact.com/api/measures/add",
                  "https://api.resultsscorecard.com/api/measures/add")

    url %>%
      request() %>%
      req_body_json(list(siteCode = "Portland",
                         apiKey = Sys.getenv("SC_API_KEY"),
                         measureType = measure_type,
                         calculationType = 1,
                         calendarId = case_match(calendar,
                           "month" ~ 1,
                           "quarter" ~ 2,
                           "annual" ~ 3,
                           "half year" ~ 4,
                           "fy quarter" ~  7403,
                           "fy" ~ 7300),
                         colorBands = FALSE,
                         dataSource = data_source,
                         isBestPractice = FALSE,
                         polarityType = polarity,
                         scale = 1,
                         title = title,
                         unitOfMeasure = unit,
                         uoMisPrefix = unit_prefix),
                    encode = "json") %>%
      req_perform() %>%
      resp_body_string() %>%
      fromJSON()
  },
  
  error = function(cond) {
    
    warning("Error creating ", measure_type, ": ", title ," in Scorecard: ", cond)
    
  }
  )
}