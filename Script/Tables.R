library(gtsummary)
library(gt)
library(readxl)
data1 <- read_excel("raw_data/AMR_KAP_Data.xlsx")
#table_1
data1 |> 
  select(1:11) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Tables/table1.docx")
#table_2
data1 |> 
  select(41:49) |> 
  tbl_summary() |> 
  as_gt() |> 
  gtsave("Tables/table2.docx")

#data-table-3
#knowledge
library(dplyr)

knowledge <- data1 |> 
  select(12:23) |> 
  mutate(across(everything(), ~ case_when(
    . == "No" ~ 0,
    . == "Don't Know" ~ 0,
    . == "Yes" ~ 1,
    TRUE ~ NA_real_
  ))) |>  
  # Compute the median of each column, and store it in knowledge_M
  mutate(knowledge_M = apply(across(everything()), 1, function(x) median(x, na.rm = TRUE)) * 100) |> 
  mutate(knowledge_M = paste0(knowledge_M, "%"),
         knowledge_Level = case_when(
           knowledge_M >= 80 ~ "Good",     
           knowledge_M >= 50 & knowledge_M < 80 ~ "Moderate", 
           knowledge_M < 50 ~ "Poor",      
           TRUE ~ "Unknown"               
         ))




#attitude
Attitude <- data1 |> 
  select(24:33) |> 
  mutate(across(everything(), ~case_when(
    . == "Agree" ~ 0,
    . == "Neutral" ~ 0,
    . == "Disagree" ~ 1,
    TRUE ~ NA_real_
  ))) |> 
  mutate(Attitude_M = apply(across(everything()), 1, median, na.rm = TRUE)*100) |>  # Changed here
  mutate(Attitude_M = paste0(Attitude_M, "%"),
         Attitude_Level = case_when(
           Attitude_M >= 80 ~ "Positive",     
           Attitude_M >= 50 & Attitude_M <80 ~ "Uncertain", 
           Attitude_M <50 ~ "Negative",      
           TRUE ~ "Unknown"               
         ))
#practice
Practice <- data1 |> 
  select(34:39) |> 
  mutate(across(everything(), ~case_when(
    . == "No" ~ 1,
    . == "Don't Know" ~ 1,
    . == "Yes" ~ 0,
    TRUE ~ NA_real_
  ))) |>  
  mutate(Practice_M = apply(across(everything()), 1, median, na.rm = TRUE)*100) |>  # Changed here
  mutate(Practice_M = paste0(Practice_M, "%"),
         Practice_Level = case_when(
           Practice_M >=50 ~ "Good",
           Practice_M < 50 ~ "Misuse",      
           TRUE ~ "Unknown"               
         ))
#table_3
Parent_cha <- cbind(Practice, Attitude , knowledge)
Parent_cha |> 
  select(knowledge_Level, Attitude_Level, Practice_Level) |> 
  tbl_summary() |> 
  as_gt()
