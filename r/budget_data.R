params <- list(
  account = "Expense",
  bureau = "Fund and Debt Management",
  object = c("EMS - External Materials and Services",
             "IMS - Internal Materials and Services",
             "PERSONAL - Personnel")
  )

---

library(tidyverse)
library(readxl)

data <- readxl::read_xlsx("inputs/AdHoc-FY17to25-2025.01.22.xlsx", "AdHoc-FY17to25-2025.01.22")

`Account Name` = ""
`Bureau Name`
`Major Object - Name`