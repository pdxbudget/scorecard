sc_read_objects <- function(objects, container_id = NA) {
  
  # objects: 'scorecards','containers', 'indicators', 'perfmeasures', or 'measures'. 'measures' returns both 'indicators' and 'perfmeasures'.
  # container_id: optional for indicators, perfmeaures, or measures if you want to limit the list to those in a specific container
  
  tryCatch({
    
    if (is.null(objects) || !objects %in% c("scorecards","containers", "indicators", "perfmeasures", "measures")) {
      stop("'objects' argument must be: 'scorecards','containers', 'indicators', 'perfmeasures', or 'measures'")
    }
    
    
    if (objects %in% c("indicators", "perfmeasures", "measures")) {
      url <- "https://api.resultsscorecard.com/api/measures/list"
      
      measure_type <- switch(
        objects,
        indicators = "indicator", 
        perfmeasures = "performance measure",
        NA)
      
    } else {
      url <- paste0("https://api.resultsscorecard.com/api/", objects ,"/list")
      
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



sc_create_measure <- function(
    measure_type, title, polarity = 1, scale = 0,
    unit, unit_prefix = FALSE) {
  
  # measure_type: string, 'indicators' or 'perfmeasures'
  # title: string, a name for the measure
  # polarity: numeric, defaults to 1. 1 - Higher is Better, 2 - Lower is Better, 3 - Target Range, 4 - Do Not Display,5 - No Polarity
  # scale: numeric, defaults to 0. Number of decimal points to show.
  # unit: string, defaults to blank. Symbol for the unit of measure, typically % or $.
  # unit_prefix: boolean, defaults to FALSE. TRUE moves the unit of measure to the front of the number, like "$1". FALSE keeps the unit symbol behind the number, like "1%". 
  # container_id: optional for indicators, perfmeaures, or measures if you want to limit the list to those in a specific container
  
  is.tr
  
  tryCatch({
    
    if (is.null(measure_type) || !measure_type %in% c("indicators", "perfmeasures")) {
      stop("'measure_type' argument must be: 'indicators' or 'perfmeasures'")
    }
    
    if (is.null(title) || !is.character(title)) {
      stop("'title' argument is required and must be a string")
    }

    url %>%
      request("https://api.resultsscorecard.com/api/measures/list") %>%
      req_body_json(list(siteCode = "Portland",
                         apiKey = Sys.getenv("SC_API_KEY"),
                         measureType = measure_type,
                         # containerID = container_id,
                         calculationTypeId: 1,
                         calendarId: 1,
                         colorBands: FALSE,
                         isBestPractice: FALSE,
                         polarityId: 1,
                         scale: 1,
                         title: title,
                         measureType: measure_type,
                         unitOfMeasure: unit,
                         uoMisPrefix: unit_prefix),
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