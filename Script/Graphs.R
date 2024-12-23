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
gglikert(G1) + labs(x="Percentage",fill= "Response",) + theme(legend.position = "top")
ggsave("Figs/Figure-1.png", dpi = 300)

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
ggsave("Figs/Figure-2.png", dpi = 300)


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
gglikert(G3) + labs(x= "Percentage",
fill= "Response",
) + 
  theme(legend.position = "top" )
ggsave("Figs/Figure_3_Attitude_Antibiotics.png", dpi = 300)

