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
library(dplyr)
# Modify the 'knowledge' dataframe
knowledge <- data1 |> 
  select(12:23) |> 
  mutate(
    # Apply transformation to specified columns (1, 2, 3, 6, 8, 9, 10, 11, 12)
    across(c(1, 2, 3, 6, 8, 9, 10, 11, 12), ~ case_when(
      . == "No" ~ 0,
      . == "Don't Know" ~ 0,
      . == "Yes" ~ 1,
      TRUE ~ TRUE  # Replace missing with TRUE
    )),
    
    # Apply transformation to columns (4, 5, 7)
    across(c(4, 5, 7), ~ case_when(
      . == "No" ~ 1,
      . == "Don't Know" ~ 0,
      . == "Yes" ~ 0,
      TRUE ~ TRUE  # Replace missing with TRUE
    ))
  )

# Calculate the mean across the transformed columns
knowledge$mean <- rowMeans(knowledge, na.rm = TRUE)

# Calculate the median across the transformed columns
knowledge$median <- apply(knowledge, 1, function(x) median(x, na.rm = TRUE))

# Calculate the score (percentage of 1s across the transformed columns)
knowledge$score <- rowSums(knowledge, na.rm = TRUE) / ncol(knowledge) * 100

# Apply knowledge_M and knowledge_Level calculation
knowledge <- knowledge |> 
  mutate(
    knowledge_M = median * 100,  # Multiply median by 100 for knowledge_M
    knowledge_Level = case_when(
      knowledge_M >= 80 ~ "Good",     
      knowledge_M >= 50 & knowledge_M <= 79 ~ "Moderate", 
      knowledge_M <= 49 ~ "Poor",      
      TRUE ~ "Unknown"               
    )
  )


# Modify the 'Attitude' dataframe
Attitude <- data1 |> 
  select(24:33) |> 
  mutate(across(everything(), ~ case_when(
    . == "Disagree" ~ 0,
    . == "neutral" ~ 0,
    . == "Agree" ~ 1,
    TRUE ~ TRUE  # Replace missing with TRUE
  )))

Attitude$mean <- rowMeans(Attitude[, 1:10], na.rm = TRUE)  
Attitude$median <- apply(Attitude[, 1:10], 1, function(x) median(x, na.rm = TRUE))
Attitude$score <- rowSums(Attitude, na.rm = TRUE) / ncol(knowledge) * 100

Attitude <- Attitude |> 
  mutate(
    Attitude_M = median * 100,  
    Attitude_Level = case_when(
      Attitude_M >= 80 ~ "Positive",     
      Attitude_M >= 50 & Attitude_M < 80 ~ "Uncertain", 
      Attitude_M < 50 ~ "Negative",      
      TRUE ~ "Unknown"               
    )
  )

# Modify the 'Practice' dataframe
Practice <- data1 |> 
  select(34:39) |>  
  mutate(
    # Apply transformation to columns 34, 37, 38
    across(
      .cols = c(1,4,5), 
      .fns = ~ case_when(
        . == "Yes" ~ 0,
        . == "Don't know" ~ 0,
        . == "No" ~ 1,
        TRUE ~ TRUE  # Replace missing with TRUE
      )
    ),
    
    # Apply transformation to columns 35, 36, 39
    across(
      .cols = c(2,3,6),
      .fns = ~ case_when(
        . == "Yes" ~ 1,         
        . == "Don't know" ~ 0,
        . == "No" ~ 0,           
        TRUE ~ TRUE  # Replace missing with TRUE
      )
    )
  )

# Calculate the mean across the selected columns (34 to 39)
Practice$mean <- rowMeans(Practice, na.rm = TRUE)

# Calculate the median across the selected columns (34 to 39)
Practice$median_value <- apply(Practice, 1, function(x) median(x, na.rm = TRUE))

# Calculate the score (percentage of 1s)
Practice$score <- rowSums(Practice, na.rm = TRUE) / ncol(Practice) * 100

# Apply Practice_M and Practice_Level calculation
Practice <- Practice |> 
  mutate(
    Practice_M = median_value * 100,  # Multiply the median by 100 for Practice_M
    Practice_Level = case_when(
      Practice_M >= 80 ~ "Good",     
      Practice_M < 80 ~ "Missuse",      
      TRUE ~ "Unknown"               
    )
  )

Factor <- cbind(Practice,Attitude,knowledge)
library(gtsummary)
library(gt)
Factor |> 
  select(knowledge_Level, Attitude_Level, Practice_Level) |> 
  tbl_summary() |>  
  as_gt() |> 
  gtsave("Tables/Factors contributing to antibiotic misuse among parents of school-going children in Dhaka City, Bangladesh.docx")

#Table-4
data1 |> 
  mutate(Factor = Factor)
data1 |> select(1:9,Factor) |> 
  tbl_uvregression(
    method = lm , 
    y = Factor
  )

