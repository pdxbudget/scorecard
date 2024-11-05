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