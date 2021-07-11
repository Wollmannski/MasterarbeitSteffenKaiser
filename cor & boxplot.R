#Coeficient of variation CV--------------------------------------------------------------------------------------------
#CV ZIMEN
zimen_clean <- zimen %>% 
  na.omit() 
cv_zimen <- sd(zimen_clean$Z_PM25) / mean(zimen_clean$Z_PM25) * 100

#CV Parcus
cv_parcus <- sd(parcus_alles$PM25) / mean(parcus_alles$PM25) * 100

#CV Zitadelle
cv_zitadelle <- sd(zitadelle_alles$PM25) / mean(zitadelle_alles$PM25) * 100

#Correlation ----------------------------------------------------------------------------------------------------------

#correlations
#Parcus

parcus_join <- left_join(zimen_clean, parcus_alles, by="DATETIME_UTC")
parcus_join_clean <- parcus_join %>% 
  na.omit()

#Overall correlation 
cor(parcus_join_clean$P_PM25, parcus_join_clean$PM25)

#Zitadelle
zitadelle_join <- left_join(zimen_clean, zitadelle_alles, by="DATETIME_UTC")
zitadelle_join_clean <- zitadelle_join %>% 
  na.omit()

#Overall correlation 
cor(zitadelle_join_clean$Z_PM25, zitadelle_join_clean$PM25)

#Device Correlation
#Cor devices parcus 1/2/7/4
parcus_join_nr1 <- parcus_join_clean %>% 
  filter(unit == "NR1")

cor(parcus_join_nr1$P_PM25, parcus_join_nr1$PM25)

#NR2
parcus_join_nr2 <- parcus_join_clean %>% 
  filter(unit == "NR2")

cor(parcus_join_nr2$P_PM25, parcus_join_nr2$PM25)

#NR4
parcus_join_nr4 <- parcus_join_clean %>% 
  filter(unit == "NR4")

cor(parcus_join_nr4$P_PM25, parcus_join_nr4$PM25)

#NR7
parcus_join_nr7 <- parcus_join_clean %>% 
  filter(unit == "NR7")

cor(parcus_join_nr7$P_PM25, parcus_join_nr7$PM25)

#Cor devices zitadelle 3/6/8
#NR3
zitadelle_join_nr3 <- zitadelle_join_clean %>% 
  filter(unit == "NR3")

cor(zitadelle_join_nr3$Z_PM25, zitadelle_join_nr3$PM25)

#NR6
zitadelle_join_nr6 <- zitadelle_join_clean %>% 
  filter(unit == "NR6")

cor(zitadelle_join_nr6$Z_PM25, zitadelle_join_nr6$PM25)
#NR8
zitadelle_join_nr8 <- zitadelle_join_clean %>% 
  filter(unit == "NR8")

cor(zitadelle_join_nr8$Z_PM25, zitadelle_join_nr8$PM25)


#Boxplots -------------------------------------------------------------------------------------------------------------

#2020-11-17 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR12020.11.17, aes(x=unit, y=ADJ_180sec.NR1, color="1.00m No1")) +
  geom_boxplot(data = parcus_untenNR22020.11.17, aes(x=unit, y=ADJ_180sec.NR2, color="1.00m No2")) +
  geom_boxplot(data = parcus_mitteNR72020.11.17, aes(x=unit, y=Mittel180, color="1.60m No7")) +
  geom_boxplot(data = parcus_obenNR42020.11.17, aes(x=unit, y=ADJ_180sec.NR4, color="4.00m No4")) +
  scale_color_manual(values = c(`1.00m No1` = "#ff5010", `1.00m No2` = "#700808", `1.60m No7` = "#4286ff", `4.00m No4` = "#ff1597")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-11-17")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2020-11-17 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR62020.11.17, aes(x=unit, y=ADJ_180sec.NR6, color="1.00m No6")) +
  geom_boxplot(data = zitadelle_mitteNR32020.11.17, aes(x=unit, y=ADJ_180sec.NR3, color="1.60m No3")) +
  geom_boxplot(data = zitadelle_obenNR82020.11.17, aes(x=unit, y=ADJ_180sec.NR8, color="4.00m No8")) +
  scale_color_manual(values = c(`1.00m No6` = "#ffbb42", `1.60m No3` = "#0ff1ce", `4.00m No8` = "#d47fff")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-11-17")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020-11-18 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR12020.11.18, aes(x=unit, y=ADJ_180sec.NR1, color="1.00m No1")) +
  geom_boxplot(data = parcus_untenNR22020.11.18, aes(x=unit, y=ADJ_180sec.NR2, color="1.00m No2")) +
  geom_boxplot(data = parcus_obenNR42020.11.18, aes(x=unit, y=ADJ_180sec.NR4, color="4.00m No4")) +
  geom_boxplot(data = parcus_mitteNR72020.11.18, aes(x=unit, y=Mittel180, color="1.60m No7")) +
  scale_color_manual(values = c(`1.00m No1` = "#ff5010", `1.00m No2` = "#700808", `1.60m No7` = "#4286ff", `4.00m No4` = "#ff1597")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-11-18")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2020-11-18 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR62020.11.18, aes(x=unit, y=ADJ_180sec.NR6, color="1.00m No6")) +
  geom_boxplot(data = zitadelle_mitteNR32020.11.18, aes(x=unit, y=ADJ_180sec.NR3, color="1.60m No3")) +
  geom_boxplot(data = zitadelle_obenNR82020.11.18, aes(x=unit, y=ADJ_180sec.NR8, color="4.00m No8")) +
  scale_color_manual(values = c(`1.00m No6` = "#ffbb42", `1.60m No3` = "#0ff1ce", `4.00m No8` = "#d47fff")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-11-18")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020-11-19 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR12020.11.19, aes(x=unit, y=ADJ_180sec.NR1, color="1.00m No1")) +
  geom_boxplot(data = parcus_untenNR22020.11.19, aes(x=unit, y=ADJ_180sec.NR2, color="1.00m No2")) +
  geom_boxplot(data = parcus_mitteNR72020.11.19, aes(x=unit, y=Mittel180, color="1.60m No7")) +
  geom_boxplot(data = parcus_obenNR42020.11.19, aes(x=unit, y=ADJ_180sec.NR4, color="4.00m No4")) +
  scale_color_manual(values = c(`1.00m No1` = "#ff5010", `1.00m No2` = "#700808", `1.60m No7` = "#4286ff", `4.00m No4` = "#ff1597")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-11-19")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2020-11-18 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR62020.11.19, aes(x=unit, y=ADJ_180sec.NR6, color="1.00m No6")) +
  geom_boxplot(data = zitadelle_mitteNR32020.11.19, aes(x=unit, y=ADJ_180sec.NR3, color="1.60m No3")) +
  geom_boxplot(data = zitadelle_obenNR82020.11.19, aes(x=unit, y=ADJ_180sec.NR8, color="4.00m No8")) +
  scale_color_manual(values = c(`1.00m No6` = "#ffbb42", `1.60m No3` = "#0ff1ce", `4.00m No8` = "#d47fff")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-11-19")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020-12-02 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR42020.12.02, aes(x=unit, y=ADJ_180sec.NR4, color="1.00m No4")) +
  geom_boxplot(data = parcus_mitteNR12020.12.02, aes(x=unit, y=ADJ_180sec.NR1, color="1.60m No1")) +
  geom_boxplot(data = parcus_obenNR72020.12.02, aes(x=unit, y=Mittel180, color="4.00m No7")) +
  scale_color_manual(values = c(`1.00m No4` = "#ff1597", `1.60m No1` = "#ff5010", `4.00m No7` = "#4286ff")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-12-02")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2020-12-02 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR82020.12.02, aes(x=unit, y=ADJ_180sec.NR8, color="1.00m No8")) +
  geom_boxplot(data = zitadelle_mitteNR62020.12.02, aes(x=unit, y=ADJ_180sec.NR6, color="1.60m No6")) +
  geom_boxplot(data = zitadelle_obenNR32020.12.02, aes(x=unit, y=ADJ_180sec.NR3, color="4.00m No3")) +
  scale_color_manual(values = c(`1.00m No8` = "#d47fff", `1.60m No6` = "#ffbb42", `4.00m No3` = "#0ff1ce")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-12-02")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020-12-03 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR42020.12.03, aes(x=unit, y=ADJ_180sec.NR4, color="1.00m No4")) +
  geom_boxplot(data = parcus_mitteNR12020.12.03, aes(x=unit, y=ADJ_180sec.NR1, color="1.60m No1")) +
  geom_boxplot(data = parcus_obenNR72020.12.03, aes(x=unit, y=Mittel180, color="4.00m No7")) +
  scale_color_manual(values = c(`1.00m No4` = "#ff1597", `1.60m No1` = "#ff5010", `4.00m No7` = "#4286ff")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-12-03")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2020-12-03 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR82020.12.03, aes(x=unit, y=ADJ_180sec.NR8, color="1.00m No8")) +
  geom_boxplot(data = zitadelle_mitteNR62020.12.03, aes(x=unit, y=ADJ_180sec.NR6, color="1.60m No6")) +
  geom_boxplot(data = zitadelle_obenNR32020.12.03, aes(x=unit, y=ADJ_180sec.NR3, color="4.00m No3")) +
  scale_color_manual(values = c(`1.00m No8` = "#d47fff", `1.60m No6` = "#ffbb42", `4.00m No3` = "#0ff1ce")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-12-03")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020-12-04 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR42020.12.04, aes(x=unit, y=ADJ_180sec.NR4, color="1.00m No4")) +
  geom_boxplot(data = parcus_mitteNR12020.12.04, aes(x=unit, y=ADJ_180sec.NR1, color="1.60m No1")) +
  geom_boxplot(data = parcus_obenNR72020.12.04, aes(x=unit, y=Mittel180, color="4.00m No7")) +
  scale_color_manual(values = c(`1.00m No4` = "#ff1597", `1.60m No1` = "#ff5010", `4.00m No7` = "#4286ff")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-12-04")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2020-12-04 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR82020.12.04, aes(x=unit, y=ADJ_180sec.NR8, color="1.00m No8")) +
  geom_boxplot(data = zitadelle_mitteNR62020.12.04, aes(x=unit, y=ADJ_180sec.NR6, color="1.60m No6")) +
  geom_boxplot(data = zitadelle_obenNR32020.12.04, aes(x=unit, y=ADJ_180sec.NR3, color="4.00m No3")) +
  scale_color_manual(values = c(`1.00m No8` = "#d47fff", `1.60m No6` = "#ffbb42", `4.00m No3` = "#0ff1ce")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-12-04")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2021-02-23 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR72021.02.23, aes(x=unit, y=Mittel180, color="1.00m No7")) +
  geom_boxplot(data = parcus_mitteNR42021.02.23, aes(x=unit, y=ADJ_180sec.NR4, color="1.60m No4")) +
  geom_boxplot(data = parcus_obenNR12021.02.23, aes(x=unit, y=ADJ_180sec.NR1, color="4.00m No1")) +
  geom_boxplot(data = parcus_obenNR22021.02.23, aes(x=unit, y=ADJ_180sec.NR2, color="4.00m No2")) +
  scale_color_manual(values = c(`1.00m No7` = "#4286ff", `1.60m No4` = "#ff1597", `4.00m No1` = "#ff5010",`4.00m No2` = "#700808")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-02-23")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2021-02-23 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR32021.02.23, aes(x=unit, y=ADJ_180sec.NR3, color="1.00m No3")) +
  geom_boxplot(data = zitadelle_mitteNR82021.02.23, aes(x=unit, y=ADJ_180sec.NR8, color="1.60m No8")) +
  geom_boxplot(data = zitadelle_obenNR62021.02.23, aes(x=unit, y=ADJ_180sec.NR6, color="4.00m No6")) +
  scale_color_manual(values = c(`1.00m No3` = "#0ff1ce", `1.60m No8` = "#d47fff", `4.00m No6` = "#ffbb42")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2021-02-23")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2021-02-24 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR72021.02.24, aes(x=unit, y=Mittel180, color="1.00m No7")) +
  geom_boxplot(data = parcus_mitteNR42021.02.24, aes(x=unit, y=ADJ_180sec.NR4, color="1.60m No4")) +
  geom_boxplot(data = parcus_obenNR12021.02.24, aes(x=unit, y=ADJ_180sec.NR1, color="4.00m No1")) +
  geom_boxplot(data = parcus_obenNR22021.02.24, aes(x=unit, y=ADJ_180sec.NR2, color="4.00m No2")) +
  scale_color_manual(values = c(`1.00m No7` = "#4286ff", `1.60m No4` = "#ff1597", `4.00m No1` = "#ff5010",`4.00m No2` = "#700808")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-02-24")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2021-02-24 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR32021.02.24, aes(x=unit, y=ADJ_180sec.NR3, color="1.00m No3")) +
  geom_boxplot(data = zitadelle_mitteNR82021.02.24, aes(x=unit, y=ADJ_180sec.NR8, color="1.60m No8")) +
  geom_boxplot(data = zitadelle_obenNR62021.02.24, aes(x=unit, y=ADJ_180sec.NR6, color="4.00m No6")) +
  scale_color_manual(values = c(`1.00m No3` = "#0ff1ce", `1.60m No8` = "#d47fff", `4.00m No6` = "#ffbb42")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2021-02-24")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2021-02-25 Parcus
ggplot() +
  geom_boxplot(data = parcus_untenNR72021.02.25, aes(x=unit, y=Mittel180, color="1.00m No7")) +
  geom_boxplot(data = parcus_mitteNR42021.02.25, aes(x=unit, y=ADJ_180sec.NR4, color="1.60m No4")) +
  geom_boxplot(data = parcus_obenNR12021.02.25, aes(x=unit, y=ADJ_180sec.NR1, color="4.00m No1")) +
  geom_boxplot(data = parcus_obenNR22021.02.25, aes(x=unit, y=ADJ_180sec.NR2, color="4.00m No2")) +
  scale_color_manual(values = c(`1.00m No7` = "#4286ff", `1.60m No4` = "#ff1597", `4.00m No1` = "#ff5010",`4.00m No2` = "#700808")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-02-25")+
  theme_minimal() +
  theme(legend.title = element_blank())

#2021-02-25 Zitadelle
ggplot() +
  geom_boxplot(data = zitadelle_untenNR32021.02.25, aes(x=unit, y=ADJ_180sec.NR3, color="1.00m No3")) +
  geom_boxplot(data = zitadelle_mitteNR82021.02.25, aes(x=unit, y=ADJ_180sec.NR8, color="1.60m No8")) +
  geom_boxplot(data = zitadelle_obenNR62021.02.25, aes(x=unit, y=ADJ_180sec.NR6, color="4.00m No6")) +
  scale_color_manual(values = c(`1.00m No3` = "#0ff1ce", `1.60m No8` = "#d47fff", `4.00m No6` = "#ffbb42")) +
  labs(x="Measuring device" ,y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2021-02-25")+
  theme_minimal() +
  theme(legend.title = element_blank())

