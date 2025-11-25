add_scorecard_bureau <- function(bureau, service_area) {
  if(!is.na(service_area)) {
    service_area_sc_id <- sc$scorecards %>%
      filter(title == service_area) %>%
      pull(id)
  }
  
  sc_add_scorecard(title = bureau, parent_id = service_area_sc_id,
                   description = NA, test_env = FALSE) 
}

add_prog_offers <- function(bureau) {
  po <- prog_offers %>%
    filter(`Bureau Name` == bureau) %>%
    pull(`Program Offer`)
  
  scorecard_id <- sc$scorecards %>%
    filter(title == bureau) %>%
    pull(id)
  
  if (is.null(scorecard_id) | rlang::is_empty(scorecard_id)) {
    stop("No parent scorecard found for ", bureau)
  }
  
  for (i in po) {
    sc_add_container(
      type = "program offer", title = i, scorecard_id = scorecard_id, test_env = FALSE)
  }
}

add_bureau <- function(bureau) {
  
  scorecard_id <- sc$scorecards %>%
    filter(title == bureau) %>%
    pull(id)
  
  if (is.null(scorecard_id)) {
    stop("No parent scorecard found for ", bureau)
  }
  
  sc_add_container(type = "bureau", title = bureau,  scorecard_id = scorecard_id, test_env = FALSE)
  
  message(bureau, " created in Scorecard")
}

add_measures <- function(bureau, rows = NULL) {
  
  df <- read_xlsx(paste0("inputs/bfm_metrics_to_transfer/", bureau, " - Performance Template.xlsx")) %>%
    filter(`SC-Keep Measure?` %in% c("Yes", "New"))
  
  # match to SC container IDs, assign bureau if no PO chosen
  bureau_container_id <- sc$containers %>%
    filter(containerType == "Bureau",
           title ==  bureau) %>%
    pull(id)
  
  df <- df %>%
    mutate(
      `Container Type` = ifelse(
        is.na(`SC-Prog Offer`) & is.na(`Program Offer`),
        "Bureau", "Program Offer"),
      Container = case_when(
        `Container Type` == "Bureau" ~ bureau,
        !is.na(`SC-Prog Offer`) ~ `SC-Prog Offer`,
        is.na(`SC-Prog Offer`) ~ `Program Offer`),
      `PO ID` = ifelse(`Container Type` != "Bureau", str_sub(Container, 1, 6), NA)
    )
  
  pos <- sc$containers %>%
    filter(containerType == "Program Offer") %>%
    mutate(`PO ID` = str_sub(title, start = -6)) %>%
    select(id, `PO ID`)
  
  df <- df %>%
    left_join(pos, "PO ID") %>%
    select(-`PO ID`) %>%
    rename(`Final Container ID` = id)  %>%
    mutate(`Final Container ID` = ifelse(
      `Container Type` == "Bureau",
      bureau_container_id, `Final Container ID`))
  
  if (anyNA(df$`Final Container ID`)) {
    stop("No Scorecard container found for metrics in ", bureau)
  }
  
  # defaults
  
  df <- df %>%
    mutate(
      # metric title defaults to BFM title
      `Final Title` = 
        ifelse(is.na(`SC-Title`),
               `Measure Title`, `SC-Title`),
      # frequency default to FY
      # performance template incorrectly switched the FY and FY QT IDs, so this is corrected here
      `Final Frequency` = 
        case_when(`SC-Frequency` == "7403 - Fiscal Year" ~ "7300 - Fiscal Year",
                  `SC-Frequency` == "7300 - Fiscal Year - Quarter" ~ "7403 - Fiscal Year - Quarter",
                  is.na(`SC-Frequency`) ~ "7300 - Fiscal Year",
                  .default = `SC-Frequency`),
      # data source is kept as-is, with NAs displaying as blank
      # polarity default to Do Not Display
      `Final Polarity` =  
        case_when(
          is.na(`SC-Polarity`) & `Desired Direction` %in% c("UP", "UPWARD") ~ "1 - Higher is Better",
          is.na(`SC-Polarity`) & `Desired Direction` == "DOWN" ~ "2 - Lower is Better",
          is.na(`SC-Polarity`) & `Desired Direction` %in% c("0", "NONE", "NA") ~ "5 - No Polarity",
          .default = `SC-Polarity`,
        ),
      # decimals default to 0
      `Final Decimals` = 
        ifelse(is.na(`SC-Decimals`),
               0, `SC-Decimals`)
      # Unit of Measure is kept as-is, with NA showing as a count or sum
      )

  # if (!is.na(rows)) {
  #   df <- slice(df, rows)
  # }

  for (i in 1:nrow(df)) {

    metric <- df[i,]

    sc_add_measure(
      measure_type = "performance measure",
      title = metric$`Final Title`,
      container_id = metric$`Final Container ID`,
      calendar = str_extract(metric$`Final Frequency`, "[0-9]{1,4}"),
      data_source = metric$`SC-Data Source`,
      polarity = str_sub(metric$`Final Polarity`, 1, 1),
      scale = metric$`Final Decimals`,
      unit = metric$`SC-Unit of Measure`,
      unit_prefix = ifelse(
        !is.na(metric$`SC-Unit of Measure`) &
          metric$`SC-Unit of Measure` == "$", TRUE, FALSE),
      test_env = FALSE)

    message(bureau, ": Added ", metric$`Measure Title`, " metric")
  }
}