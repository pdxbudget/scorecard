params <- list(
  include = list(
    account = "Expense",
    object = c("EMS - External Materials and Services",
               "IMS - Internal Materials and Services",
               "PERSONAL - Personnel")
  ),
  exclude = list(
    bureau = "Fund and Debt Management"
  )
)

#####
  
library(tidyverse)
library(readxl)
library(ggplot2)
library(scales)
library(viridisLite)

data <- list(
  orig = readxl::read_xlsx("inputs/AdHoc-FY17to25-2025.01.22.xlsx", "AdHoc-FY17to25-2025.01.22"))

data$clean <- data$orig %>%
  filter(`Account Name` == params$include$account,
         `Bureau Name` != params$exclude$bureau,
         `Major Object - Name` %in% params$include$object) %>%
  group_by(`Service Area` = `NEW Service Area - Name`, `Bureau Name`) %>%
  summarise(across(starts_with("FY20"), ~ sum(.x, na.rm = TRUE))) %>%
  pivot_longer(starts_with("FY20")) %>%
  separate(name, into = c("FY", "Type"), " ", extra = "merge") %>%
  filter(`Service Area` != "DNU - Do Not Use") %>%
  mutate(
    `Budget Type` =
      case_when(grepl("Revised", Type) ~ "Revised",
                grepl("Adopted", Type) ~ "Adopted"),
    Type = ifelse(is.na(`Budget Type`), "Actuals", "Budget"),
    `Service Area` = str_sub(`Service Area`, 7, -1))

# Summarized tables ####

make_summary_tables <- function(df, grouping) {
  
  suppressMessages({
    if(grouping != "Citywide") {
      df <- df %>%
        group_by(`Service Area`, `Bureau Name`, FY, Type, `Budget Type`) %>%
        summarise(value = sum(value, na.rm = TRUE)) %>%
        filter(`Service Area` == grouping)
      
      tables <- list(
        # tables to populate Scorecard data
        actuals =
          df %>%
          group_by(`Service Area`, FY, Type, `Budget Type`) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(Type == "Actuals"),
        budget =
          df %>%
          group_by(`Service Area`, FY, Type, `Budget Type`) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(Type == "Budget"),
        chart =
          df %>%
          group_by(`Service Area`, `Bureau Name`, FY, Type, `Budget Type`) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(FY == "FY2023-24",
                 `Budget Type` == "Revised" | Type == "Actuals")
      )
      
    } else {
      
      df <- data$clean %>%
        group_by(`Service Area`, FY, Type, `Budget Type`) %>%
        summarise(value = sum(value, na.rm = TRUE))
      
      tables <- list(
        # tables to populate Scorecard data
        actuals =
          df %>%
          group_by(FY, Type, `Budget Type`) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(Type == "Actuals"),
        budget =
          df %>%
          group_by(FY, Type, `Budget Type`) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(Type == "Budget"),
        chart =
          df %>%
          group_by(`Service Area`, FY, Type, `Budget Type`) %>%
          summarise(value = sum(value, na.rm = TRUE)) %>%
          filter(FY == "FY2023-24",
                 `Budget Type` == "Revised" | Type == "Actuals")
      )
    }
  })
  
  tables <- map(tables, ungroup)
  
  message(grouping, " summary table created")
  
  return(tables)
}

service_areas <- unique(data$clean$`Service Area`)

tables <- map(service_areas, ~make_summary_tables(data$clean, .)) %>%
  set_names(service_areas)

tables$Citywide <- make_summary_tables(data$citywide, "Citywide")

# Charts ####

tables$Citywide$chart %>%
  arrange(desc(value)) %>%
  ggplot(aes(fill = Type, x = `Service Area`, y = value)) +
  geom_bar(position = "dodge",
           stat="identity") +
  geom_text(aes(label = label_currency(accuracy = .1, scale_cut = cut_short_scale())(value)),
            position = position_dodge(width = 1), hjust = -.05) +
  scale_y_continuous(
    labels = scales::label_currency(scale_cut = cut_short_scale()),
    expand = expansion(mult = c(0, 0.1))) + 
  scale_fill_manual(values = viridis(3)) +
  labs(title="Citywide FY 23-24 Revised Budget vs. Actuals") +
  coord_flip() +
  theme_minimal()


