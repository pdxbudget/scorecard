# 2a: Add Scorecard dashboard ####

add_scorecard_bureau(
  "Portland Permitting & Development",
  "Community and Economic Development")

# 2c: Add bureaus ####

bureaus <- prog_offers %>%
  distinct(`Bureau Name`) %>%
  pull(`Bureau Name`)

map(bureaus, add_bureau)

# 2b: Add program offers ####

prog_offers <- data$prog_offers %>%
  mutate(`Program Offer` = paste0(`Sub-Program Name`, " - ", `Sub-Program`)) %>%
  filter(`Program Offer` != "NA - NA") %>%
  arrange(`Sub-Program`) %>%
  distinct(`Bureau Name`, `Program Offer`)

sc$scorecards <- sc_read_objects("scorecards", test_env = FALSE) # repull scorecards

add_prog_offers("Portland Permitting & Development")

# 2d: Add measures ####
test <- add_measures("Portland Permitting & Development")
