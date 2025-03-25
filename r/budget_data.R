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

df <- data %>%
  filter(`Account Name` == params$include$account,
         `Bureau Name` != params$exclude$bureau,
         `Major Object - Name` %in% params$include$object)

citywide <- df %>%
  group_by(`NEW Service Area - Name`, `Bureau Name`) %>%
  summarise(across(starts_with("FY20"), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(starts_with("FY20")) %>%
  separate(name, into = c("FY", "Budget Type"), " ", extra = "merge") %>%
  mutate(
    `Budget Type` = case_when(
      `Budget Type` == "Adopted Adopted Budget" ~ "Adopted Budget",
      `Budget Type` == "Revised Revised Budget" ~ "Revised Budget",
      grepl("Actuals", `Budget Type`) ~ "Actuals",
      .default = `Budget Type`)
  )
