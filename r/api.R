sc_read_objects <- function(objects) {
  
  tryCatch({
    
    if (is.null(objects) || !objects %in% c("scorecards","containers", "indicators", "perfmeasures", "measures")) {
      stop("'objects' argument must be: 'scorecards','containers', 'indicators', 'perfmeasures', or 'measures'")
    }
    
    paste0("https://api.resultsscorecard.com/api/", objects ,"/list") %>%
      request() %>%
      req_body_json(list(siteCode = "Portland",
                         apiKey = Sys.getenv("SC_API_KEY"),
                         measureType = "performance measure"),
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