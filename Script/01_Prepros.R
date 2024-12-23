library(tidyverse)
library(readxl)
library(dplyr)
Data <- read_excel("raw_data/AMR_KAP_Data.xlsx")
sum(is.na(Data))
Data <- na.omit(Data)
sum(is.na(Data))

#Demographic_Characteristics
DemG_cha <- Data |>
  select(1:11)
#Distribution of knowledge among parents
do_kno_par <- Data |> 
  select(12:23)
#Attitude towards antibiotic resistance and the misuse of antibiotics among parents
at_parents <- Data |> 
  select(24:33)

library(gtsummary)
library(gt)
library(readxl)
data1 <- read_excel("raw_data/AMR_KAP_Data.xlsx")
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
  mutate(across(everything(), ~case_when(
    . == "No" ~ 0,
    . == "Don't Know" ~ 0,
    . == "Yes" ~ 1,
    TRUE ~ NA_real_
  ))) |>  
  mutate(knowledge_M = rowMeans(across(everything()), na.rm = TRUE)*100) |> 
  mutate(knowledge_M = paste0(knowledge_M, "%"),
         knowledge_Level = case_when(
           knowledge_M >= 80 ~ "Good",     
           knowledge_M >= 50 & knowledge_M <= 79 ~ "Moderate", 
           knowledge_M <= 49 ~ "Poor",      
           TRUE ~ "Unknown"               
         ))
#attitude
Attitude <- data1 |>select(24:33) |> 
  mutate(across(everything(), ~case_when(
    . == "Agree" ~ 0,
    . == "Neutral" ~ 0,
    . == "Disagree" ~ 1,
    TRUE ~ NA_real_
  ))) |> 
  mutate(Attitude_M = rowMeans(across(everything()), na.rm = TRUE)*100) |> 
  mutate(Attitude_M = paste0(Attitude_M, "%"),
         Attitude_Level = case_when(
    Attitude_M >= 80 ~ "Positive",     
    Attitude_M >= 50 & Attitude_M < 80 ~ "Uncertain", 
    Attitude_M < 50 ~ "Negative",      
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
  mutate(Practice_M = rowMeans(across(everything()), na.rm = TRUE)*100) |> 
  mutate(Practice_M = paste0(Practice_M, "%"),
         Practice_Level = case_when(
           Practice_M >= 80 ~ "Good",
           Practice_M < 80 ~ "Misuse",      
           TRUE ~ "Unknown"               
         ))
#table_3
Parent_cha <- cbind(Practice, Attitude , knowledge)
Parent_cha |> 
  select(knowledge_Level, Attitude_Level, Practice_Level) |> 
  tbl_summary() |> 
  as_gt()


library(tidyverse)
library(likert)
library(sjPlot)
library(ISLR)
library(ggplot2)
library(dplyr)
library(ggstats)
library(sjmisc)





#graph_1
# data convert as a factor
data1 $`Antibiotic kills the bacteria(Yes)` <- 
  as.factor(data1 $`Antibiotic kills the bacteria(Yes)`)
data1$` Amoxicillin is an antibiotic (Yes)`<- 
  as.factor(data1$` Amoxicillin is an antibiotic (Yes)`)
data1 $` Azithromycin is an antibiotic(Yes)` <-
  as.factor(data1 $` Azithromycin is an antibiotic(Yes)`)
data1$` Paracetamol is an antibiotic(No)` <- 
  as.factor(data1$` Paracetamol is an antibiotic(No)`)
data1$` Antibiotic kills the virus(No)`<-
  as.factor(data1$` Antibiotic kills the virus(No)`)
data1$` Antibiotics used to treat diarrhoea(Yes)` <- 
  as.factor(data1$` Antibiotics used to treat diarrhoea(Yes)`)
data1$` Antibiotics are useful for flu and cough(No)` <-
  as.factor(data1$` Antibiotics are useful for flu and cough(No)`)
data1 $` Antibiotic resistant bacteria are difficult to treat(Yes)` <-
  as.factor(data1 $` Antibiotic resistant bacteria are difficult to treat(Yes)`)
data1$` Antibiotics can cause allergic reactions(Yes)` <- 
  as.factor(data1$` Antibiotics can cause allergic reactions(Yes)`)
data1 $` Antibiotics can kill normal flora(Yes)` <- 
  as.factor(data1 $` Antibiotics can kill normal flora(Yes)`)
data1 $` Infectious disease are becoming difficult to treat with antibiotics(Yes)`<-
  as.factor(data1 $` Infectious disease are becoming difficult to treat with antibiotics(Yes)`)

#Graph_1
select(data1,12 :23)
G1 <- select(data1,12 :23)
gglikert(G1) + labs(x="Percentage",fill= "Response") + theme(legend.position = "top")


# graph_2 
## data convert as a factor
data1 $ ` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)` <- 
  as.factor(data1 $ ` I will see another doctor if the first one has not been prescribed antibiotics(Disagree)`)
data1 $ ` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)` <-
  as.factor(data1 $ ` I am not satisfied if the doctor does not prescribe an antibiotic to me(Disagree)`)
data1 $` Antibiotics are safe and hence can be used commonly(Disagree)` <-
  as.factor(data1 $` Antibiotics are safe and hence can be used commonly(Disagree)`)
data1$ ` Sick child is given antibiotics, even there is no indication(Disagree)` <-
  as.factor(data1 $ ` Sick child is given antibiotics, even there is no indication(Disagree)`)
data1 $` Antibiotics can improve fever in children(Disagree)` <-
  as.factor(data1 $` Antibiotics can improve fever in children(Disagree)`)
data1 $`A child with cold is given antibiotics(Disagree)`<- 
  as.factor(data1 $`A child with cold is given antibiotics(Disagree)`)
data1 $ `I stop antibiotics when my child condition improves(Disagree)` <-
  as.factor(data1 $ `I stop antibiotics when my child condition improves(Disagree)`)
data1 $  `I reusing the same antibiotics for similar symptoms(Disagree)` <-
  as.factor(data1 $  `I reusing the same antibiotics for similar symptoms(Disagree)`)
data1 $ `Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)` <-
  as.factor(data1 $ `Leftover antibiotics are good to keep at home in case I might need them for my child later on(Disagree)`)
data1 $ `Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)` <-
  as.factor(data1 $ `Doctors often take time to inform parents how antibiotics should be used for their children(Disagree)`)

#Graph-2
select(data1,24:33)
G2 <- select(data1,24 :33)
gglikert(G2) + labs(x= "Percentage",fill= "Response")+ theme(legend.position = "top")


# Graph3
## data convert as a factor
data1 $` My child should complete a given dose, even he improve after 2 dose(Yes)` <- 
  as.factor(data1 $` My child should complete a given dose, even he improve after 2 dose(Yes)`)
data1 $ ` I seek medical advice before giving antibiotic to my children(Yes)` <- as.factor(data1 $ ` I seek medical advice before giving antibiotic to my children(Yes)`)
data1 $ ` I like to take antibiotic from pharmacy instead of taking from doctor(No)` <-as.factor(data1 $ ` I like to take antibiotic from pharmacy instead of taking from doctor(No)`)
data1 $` I give my children antibiotics(No)` <-as.factor(data1 $` I give my children antibiotics(No)`)
data1 $ ` I give my children antibiotics when they get cough(No)` <- as.factor(data1 $ ` I give my children antibiotics when they get cough(No)`)
data1 $ ` I check expiring date of antibiotic before giving to children(Yes)` <- as.factor(data1 $ ` I check expiring date of antibiotic before giving to children(Yes)`)

# figure 3
G3 <-select(data1,34:39)
gglikert(G3) + labs(x= "Percentage", fill= "Response") + 
  theme(legend.position = "top" )

  