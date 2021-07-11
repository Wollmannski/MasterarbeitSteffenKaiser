library(zoo)
library(tidyverse)
source("./helper_functions.R")

#Parcus Unten--------------------------------------------------------------

#Nr1 
parcus_untenNR1 <- parcus_untenNR1 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_untenNR1),
    RH <= 60 ~ humidity_correction_below(parcus_untenNR1)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_untenNR1 <- adjust_PM_180sec(parcus_untenNR1,53, "NR1")

parcus_untenNR1 <- parcus_untenNR1 %>% 
  na.omit()



#Nr2
parcus_untenNR2 <- parcus_untenNR2 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_untenNR2),
    RH <= 60 ~ humidity_correction_below(parcus_untenNR2)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%  
  ungroup() 

parcus_untenNR2 <- adjust_PM_180sec(parcus_untenNR2, 53, "NR2")

parcus_untenNR2 <- parcus_untenNR2 %>% 
  na.omit()



#Nr 4 
parcus_untenNR4 <- parcus_untenNR4 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_untenNR4),
    RH <= 60 ~ humidity_correction_below(parcus_untenNR4)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_untenNR4 <- adjust_PM_180sec(parcus_untenNR4, 53, "NR4")

parcus_untenNR4 <- parcus_untenNR4 %>% 
  na.omit()



#Nr 7
parcus_untenNR7 <- parcus_untenNR7 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_untenNR7),
    RH <= 60 ~ humidity_correction_below(parcus_untenNR7)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) 

parcus_untenNR7 <- parcus_untenNR7 %>% 
  na.omit()



#Parcus mitte-----------------------------------------------------

#Nr1 
parcus_mitteNR1 <- parcus_mitteNR1 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_mitteNR1),
    RH <= 60 ~ humidity_correction_below(parcus_mitteNR1)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_mitteNR1 <- adjust_PM_180sec(parcus_mitteNR1, 53, "NR1")

parcus_mitteNR1 <- parcus_mitteNR1 %>% 
  na.omit()



#Nr2 ---NO DATA---
parcus_mitteNR2 <- parcus_mitteNR2 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_mitteNR2),
    RH <= 60 ~ humidity_correction_below(parcus_mitteNR2)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_mitteNR2 <- adjust_PM_180sec(parcus_mitteNR2, 53, "NR2")

parcus_mitteNR2 <- parcus_mitteNR2 %>% 
  na.omit()



#Nr 4
parcus_mitteNR4 <- parcus_mitteNR4 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_mitteNR4),
    RH <= 60 ~ humidity_correction_below(parcus_mitteNR4)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_mitteNR4 <- adjust_PM_180sec(parcus_mitteNR4, 53, "NR4")

parcus_mitteNR4 <- parcus_mitteNR4 %>% 
  na.omit()



#Nr 7
 parcus_mitteNR7 <- parcus_mitteNR7 %>% 
   mutate(PM2.5_HumCor = case_when(
     RH > 60 ~ humidity_correction_above(parcus_mitteNR7),
     RH <= 60 ~ humidity_correction_below(parcus_mitteNR7)
   )) %>% 
   group_by(date) %>% 
   mutate(Mittel180 = rollmean(
     PM2.5_HumCor, k = 180, fill = NA, align = "center"
   ))

parcus_mitteNR7 <- parcus_mitteNR7 %>% 
  na.omit()



#Parcus Oben--------------------------------------------------------------------

#Nr1 
parcus_obenNR1 <- parcus_obenNR1 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_obenNR1),
    RH <= 60 ~ humidity_correction_below(parcus_obenNR1)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_obenNR1 <- adjust_PM_180sec(parcus_obenNR1, 53, "NR1")

parcus_obenNR1 <- parcus_obenNR1 %>% 
  na.omit()



#Nr2 
parcus_obenNR2 <- parcus_obenNR2 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_obenNR2),
    RH <= 60 ~ humidity_correction_below(parcus_obenNR2)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_obenNR2 <- adjust_PM_180sec(parcus_obenNR2, 53, "NR2")

parcus_obenNR2 <- parcus_obenNR2 %>% 
  na.omit()



#Nr 4
parcus_obenNR4 <- parcus_obenNR4 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_obenNR4),
    RH <= 60 ~ humidity_correction_below(parcus_obenNR4)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

parcus_obenNR4 <- adjust_PM_180sec(parcus_obenNR4, 53, "NR4")

parcus_obenNR4 <- parcus_obenNR4 %>% 
  na.omit()



#Nr 7
parcus_obenNR7 <- parcus_obenNR7 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(parcus_obenNR7),
    RH <= 60 ~ humidity_correction_below(parcus_obenNR7)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) 

parcus_obenNR7 <- parcus_obenNR7 %>% 
  na.omit()



#Zitadelle unten--------------------------------------------

#Nr3 
zitadelle_untenNR3 <- zitadelle_untenNR3 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_untenNR3),
    RH <= 60 ~ humidity_correction_below(zitadelle_untenNR3)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_untenNR3 <- adjust_PM_180sec(zitadelle_untenNR3, 53, "NR3")

zitadelle_untenNR3 <- zitadelle_untenNR3 %>% 
  na.omit()



#Nr 6
zitadelle_untenNR6 <- zitadelle_untenNR6 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_untenNR6),
    RH <= 60 ~ humidity_correction_below(zitadelle_untenNR6)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_untenNR6 <- adjust_PM_180sec(zitadelle_untenNR6, 53, "NR6")

zitadelle_untenNR6 <- zitadelle_untenNR6 %>% 
  na.omit()



#Nr 8
zitadelle_untenNR8 <- zitadelle_untenNR8 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_untenNR8),
    RH <= 60 ~ humidity_correction_below(zitadelle_untenNR8)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_untenNR8 <- adjust_PM_180sec(zitadelle_untenNR8, 53, "NR8")

zitadelle_untenNR8 <- zitadelle_untenNR8 %>% 
  na.omit()



#Zitadelle mitte-----------------------------------------------------

#Nr3
zitadelle_mitteNR3 <- zitadelle_mitteNR3 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_mitteNR3),
    RH <= 60 ~ humidity_correction_below(zitadelle_mitteNR3)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_mitteNR3 <- adjust_PM_180sec(zitadelle_mitteNR3, 53, "NR3")

zitadelle_mitteNR3 <- zitadelle_mitteNR3 %>% 
  na.omit()



#Nr 6
zitadelle_mitteNR6 <- zitadelle_mitteNR6 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_mitteNR6),
    RH <= 60 ~ humidity_correction_below(zitadelle_mitteNR6)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_mitteNR6 <- adjust_PM_180sec(zitadelle_mitteNR6, 53, "NR6")

zitadelle_mitteNR6 <- zitadelle_mitteNR6 %>% 
  na.omit()



#Nr 8
zitadelle_mitteNR8 <- zitadelle_mitteNR8 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_mitteNR8),
    RH <= 60 ~ humidity_correction_below(zitadelle_mitteNR8)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup()

zitadelle_mitteNR8 <- adjust_PM_180sec(zitadelle_mitteNR8, 53, "NR8")

zitadelle_mitteNR8 <- zitadelle_mitteNR8 %>% 
  na.omit()



#Zitadelle Oben--------------------------------------------------------------------

#Nr3
zitadelle_obenNR3 <- zitadelle_obenNR3 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_obenNR3),
    RH <= 60 ~ humidity_correction_below(zitadelle_obenNR3)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_obenNR3 <- adjust_PM_180sec(zitadelle_obenNR3, 53, "NR3")

zitadelle_obenNR3 <- zitadelle_obenNR3 %>% 
  na.omit()



#Nr 6
zitadelle_obenNR6 <- zitadelle_obenNR6 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_obenNR6),
    RH <= 60 ~ humidity_correction_below(zitadelle_obenNR6)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_obenNR6 <- adjust_PM_180sec(zitadelle_obenNR6, 53, "NR6")

zitadelle_obenNR6 <- zitadelle_obenNR6 %>% 
  na.omit()



#Nr 8
zitadelle_obenNR8 <- zitadelle_obenNR8 %>% 
  mutate(PM2.5_HumCor = case_when(
    RH > 60 ~ humidity_correction_above(zitadelle_obenNR8),
    RH <= 60 ~ humidity_correction_below(zitadelle_obenNR8)
  )) %>% 
  group_by(date) %>% 
  mutate(Mittel180 = rollmean(
    PM2.5_HumCor, k = 180, fill = NA, align = "center"
  )) %>%
  ungroup() 

zitadelle_obenNR8 <- adjust_PM_180sec(zitadelle_obenNR8, 53, "NR8")

zitadelle_obenNR8 <- zitadelle_obenNR8 %>% 
  na.omit()


