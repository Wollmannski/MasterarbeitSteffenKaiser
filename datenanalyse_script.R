library(zoo)
library(tidyverse)

#2020.11.17 PLOTS---------------------------------------------------------------------------------------------------------------
#parcus unten NR1 Tag 2020.11.17-----------------------------
parcus_untenNR12020.11.17 <- parcus_untenNR1 %>% 
  filter(date == "2020-11-17") %>%
mutate(DATETIME_UTC = DATETIME_UTC - minutes(17) - seconds(46))

#parcus unten NR2 Tag 2020.11.17-----------------------------
parcus_untenNR22020.11.17 <- parcus_untenNR2 %>% 
  filter(date == "2020-11-17") %>%
mutate(DATETIME_UTC = DATETIME_UTC - minutes(27) - seconds(30))

#parcus mitte NR7 2020.11.17----------------------------------
parcus_mitteNR72020.11.17 <- parcus_mitteNR7 %>% 
  filter(date == "2020-11-17") %>%
mutate(DATETIME_UTC = DATETIME_UTC - minutes(14) - seconds(47))

#parcus oben Nr 4 2020.11.17----------------------------------
parcus_obenNR42020.11.17<- parcus_obenNR4 %>% 
  filter(date == "2020-11-17") %>%
mutate(DATETIME_UTC = DATETIME_UTC - minutes(7) - seconds(47))

#ZIMEN PM25 Tag 2020.11.17-------------------------------------
zimen2020.11.17 <- zimen %>% 
  filter(date == "2020-11-17")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-11-17 16:00:00", "2020-11-17 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR12020.11.17, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="1.00m No1")) +
  geom_line(data = parcus_untenNR22020.11.17, aes(x=DATETIME_UTC, y=ADJ_180sec.NR2, color="1.00m No2")) +
  geom_line(data = parcus_mitteNR72020.11.17, aes(x=DATETIME_UTC, y=Mittel180, color="1.60m No7")) +
  geom_line(data = parcus_obenNR42020.11.17, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="4.00m No4")) +
  geom_line(data = zimen2020.11.17, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No1` = "#ff5010", `1.00m No2` = "#700808", `1.60m No7` = "#4286ff", `4.00m No4` = "#ff1597", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-11-17")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR6 Tag 2020.11.17-----------------------------
zitadelle_untenNR62020.11.17 <- zitadelle_untenNR6 %>% 
  filter(date == "2020-11-17") %>% 
  mutate(DATETIME_UTC = DATETIME_UTC - minutes(2) - seconds(47))

#zitadelle mitte NR3 2020.11.17----------------------------------
zitadelle_mitteNR32020.11.17 <- zitadelle_mitteNR3 %>% 
  filter(date == "2020-11-17") %>% 
  mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#zitadelle oben NR8 2020.11.17----------------------------------
zitadelle_obenNR82020.11.17 <- zitadelle_obenNR8 %>% 
  filter(date == "2020-11-17") %>% 
  mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2020.11.17-------------------------------------
zimen2020.11.17 <- zimen %>% 
  filter(date == "2020-11-17")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-11-17 16:00:00", "2020-11-17 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR62020.11.17, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="1.00m No6")) +
  geom_line(data = zitadelle_mitteNR32020.11.17, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="1.60m No3")) +
  geom_line(data = zitadelle_obenNR82020.11.17, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="4.00m No8")) +
  geom_line(data = zimen2020.11.17, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No6` = "#ffbb42", `1.60m No3` = "#0ff1ce", `4.00m No8` = "#d47fff", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-11-17")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020.11.18 PLOTS---------------------------------------------------------------------------------------------------------------
#parcus unten NR1 2020.11.17
parcus_untenNR12020.11.18 <- parcus_untenNR1 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(30))

#parcus unten NR2 Tag 2020.11.17-----------------------------
parcus_untenNR22020.11.18 <- parcus_untenNR2 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(47))

#parcus mitte NR7 2020.11.18----------------------------------
parcus_mitteNR72020.11.18 <- parcus_mitteNR7 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) + seconds(3))

#parcus oben Nr 4 2020.11.18----------------------------------
parcus_obenNR42020.11.18<-parcus_obenNR4 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(2))

#ZIMEN PM25 Tag 2020.11.18-------------------------------------
zimen2020.11.18 <- zimen %>% 
  filter(date == "2020-11-18")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-11-18 16:00:00", "2020-11-18 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR12020.11.18, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="1.00m No1")) +
  geom_line(data = parcus_untenNR22020.11.18, aes(x=DATETIME_UTC, y=ADJ_180sec.NR2, color="1.00m No2")) +
  geom_line(data = parcus_mitteNR72020.11.18, aes(x=DATETIME_UTC, y=Mittel180, color="1.60m No7")) +
  geom_line(data = parcus_obenNR42020.11.18, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="4.00m No4")) +
  geom_line(data = zimen2020.11.18, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No1` = "#ff5010", `1.00m No2` = "#700808", `1.60m No7` = "#4286ff", `4.00m No4` = "#ff1597", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-11-18")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR6 Tag 2020.11.18-----------------------------
zitadelle_untenNR62020.11.18 <- zitadelle_untenNR6 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(46))

#zitadelle mitte NR3 2020.11.18----------------------------------
zitadelle_mitteNR32020.11.18 <- zitadelle_mitteNR3 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(47))

#zitadelle oben NR8 2020.11.18----------------------------------
zitadelle_obenNR82020.11.18<-zitadelle_obenNR8 %>% 
  filter(date == "2020-11-18") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2020.11.18-------------------------------------
zimen2020.11.18 <- zimen %>% 
  filter(date == "2020-11-18")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-11-18 16:00:00", "2020-11-18 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR62020.11.18, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="1.00m No6")) +
  geom_line(data = zitadelle_mitteNR32020.11.18, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="1.60m No3")) +
  geom_line(data = zitadelle_obenNR82020.11.18, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="4.00m No8")) +
  geom_line(data = zimen2020.11.18, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No6` = "#ffbb42", `1.60m No3` = "#0ff1ce", `4.00m No8` = "#d47fff", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-11-18")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020.11.19 PLOTS---------------------------------------------------------------------------------------------------------------
#parcus unten NR1 2020.11.19
parcus_untenNR12020.11.19 <- parcus_untenNR1 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(11) - seconds(30))

#parcus unten NR2 Tag 2020.11.19-----------------------------
parcus_untenNR22020.11.19 <- parcus_untenNR2 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(11) - seconds(46))

#parcus mitte NR7 2020.11.19----------------------------------
parcus_mitteNR72020.11.19 <- parcus_mitteNR7 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(7) - seconds(30))

#parcus oben Nr 4 2020.11.19----------------------------------
parcus_obenNR42020.11.19<-parcus_obenNR4 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(4) - seconds(47))

#ZIMEN PM25 Tag 2020.11.19-------------------------------------
zimen2020.11.19 <- zimen %>% 
  filter(date == "2020-11-19")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-11-19 16:00:00", "2020-11-19 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR12020.11.19, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="1.00m No1")) +
  geom_line(data = parcus_untenNR22020.11.19, aes(x=DATETIME_UTC, y=ADJ_180sec.NR2, color="1.00m No2")) +
  geom_line(data = parcus_mitteNR72020.11.19, aes(x=DATETIME_UTC, y=Mittel180, color="1.60m No7")) +
  geom_line(data = parcus_obenNR42020.11.19, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="4.00m No4")) +
  geom_line(data = zimen2020.11.19, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No1` = "#ff5010", `1.00m No2` = "#700808", `1.60m No7` = "#4286ff", `4.00m No4` = "#ff1597", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-11-19")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR6 Tag 2020.11.19-----------------------------
zitadelle_untenNR62020.11.19 <- zitadelle_untenNR6 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(13) - seconds(46))

#zitadelle mitte NR3 2020.11.19----------------------------------
zitadelle_mitteNR32020.11.19 <- zitadelle_mitteNR3 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(12) - seconds(46))

#zitadelle oben NR8 2020.11.19----------------------------------
zitadelle_obenNR82020.11.19<-zitadelle_obenNR8 %>% 
  filter(date == "2020-11-19") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(12) - seconds(46))

#ZIMEN PM25 Tag 2020.11.19-------------------------------------
zimen2020.11.19 <- zimen %>% 
  filter(date == "2020-11-19")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-11-19 16:00:00", "2020-11-19 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR62020.11.19, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="1.00m No6")) +
  geom_line(data = zitadelle_mitteNR32020.11.19, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="1.60m No3")) +
  geom_line(data = zitadelle_obenNR82020.11.19, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="4.00m No8")) +
  geom_line(data = zimen2020.11.19, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No6` = "#ffbb42", `1.60m No3` = "#0ff1ce", `4.00m No8` = "#d47fff", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-11-19")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020.12.02 PLOTS---------------------------------------------------------------------------------------------------------------

#parcus unten NR4 Tag 2020.12.02-----------------------------
parcus_untenNR42020.12.02 <- parcus_untenNR4 %>% 
  filter(date == "2020-12-02") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(7) - seconds(46))

#parcus mitte NR1 2020.12.02----------------------------------
parcus_mitteNR12020.12.02 <- parcus_mitteNR1 %>% 
  filter(date == "2020-12-02") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(7) - seconds(46))

#parcus oben Nr 7 2020.12.02----------------------------------
parcus_obenNR72020.12.02<-parcus_obenNR7 %>% 
  filter(date == "2020-12-02") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(6) - seconds(46))

#ZIMEN PM25 Tag 2020.12.02-------------------------------------
zimen2020.12.02 <- zimen %>% 
  filter(date == "2020-12-02")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-12-02 16:00:00", "2020-12-02 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR42020.12.02, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="1.00m No4")) +
  geom_line(data = parcus_mitteNR12020.12.02, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="1.60m No1")) +
  geom_line(data = parcus_obenNR72020.12.02, aes(x=DATETIME_UTC, y=Mittel180, color="4.00m No7")) +
  geom_line(data = zimen2020.12.02, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No4` = "#ff1597", `1.60m No1` = "#ff5010", `4.00m No7` = "#4286ff", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-12-02")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR8 Tag 2020.12.02-----------------------------
zitadelle_untenNR82020.12.02 <- zitadelle_untenNR8 %>% 
  filter(date == "2020-12-02") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(47))

#zitadelle mitte NR6 2020.12.02----------------------------------
zitadelle_mitteNR62020.12.02 <- zitadelle_mitteNR6 %>% 
  filter(date == "2020-12-02") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(59))

#zitadelle oben NR3 2020.12.02----------------------------------
zitadelle_obenNR32020.12.02<-zitadelle_obenNR3 %>% 
  filter(date == "2020-12-02") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2020.12.02-------------------------------------
zimen2020.12.02 <- zimen %>% 
  filter(date == "2020-12-02")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-12-02 16:00:00", "2020-12-02 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR82020.12.02, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="1.00m No8")) +
  geom_line(data = zitadelle_mitteNR62020.12.02, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="1.60m No6")) +
  geom_line(data = zitadelle_obenNR32020.12.02, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="4.00m No3")) +
  geom_line(data = zimen2020.12.02, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No8` = "#d47fff", `1.60m No6` = "#ffbb42", `4.00m No3` = "#0ff1ce", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-12-02")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020.12.03 PLOTS---------------------------------------------------------------------------------------------------------------

#parcus unten NR4 Tag 2020.12.03-----------------------------
parcus_untenNR42020.12.03 <- parcus_untenNR4 %>% 
  filter(date == "2020-12-03") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(31))

#parcus mitte NR1 2020.12.03----------------------------------
parcus_mitteNR12020.12.03 <- parcus_mitteNR1 %>% 
  filter(date == "2020-12-03") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(48))

#parcus oben Nr 7 2020.12.03----------------------------------
parcus_obenNR72020.12.03<-parcus_obenNR7 %>% 
  filter(date == "2020-12-03") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2020.12.03-------------------------------------
zimen2020.12.03 <- zimen %>% 
  filter(date == "2020-12-03")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-12-03 16:00:00", "2020-12-03 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR42020.12.03, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="1.00m No4")) +
  geom_line(data = parcus_mitteNR12020.12.03, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="1.60m No1")) +
  geom_line(data = parcus_obenNR72020.12.03, aes(x=DATETIME_UTC, y=Mittel180, color="4.00m No7")) +
  geom_line(data = zimen2020.12.03, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No4` = "#ff1597", `1.60m No1` = "#ff5010", `4.00m No7` = "#4286ff", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-12-03")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR8 Tag 2020.12.03-----------------------------
zitadelle_untenNR82020.12.03 <- zitadelle_untenNR8 %>% 
  filter(date == "2020-12-03") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(47))

#zitadelle mitte NR6 2020.12.03----------------------------------
zitadelle_mitteNR62020.12.03 <- zitadelle_mitteNR6 %>% 
  filter(date == "2020-12-03") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(45))

#zitadelle oben NR3 2020.12.03----------------------------------
zitadelle_obenNR32020.12.03<-zitadelle_obenNR3 %>% 
  filter(date == "2020-12-03") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(44))

#ZIMEN PM25 Tag 2020.12.03-------------------------------------
zimen2020.12.03 <- zimen %>% 
  filter(date == "2020-12-03")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-12-03 16:00:00", "2020-12-03 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR82020.12.03, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="1.00m No8")) +
  geom_line(data = zitadelle_mitteNR62020.12.03, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="1.60m No6")) +
  geom_line(data = zitadelle_obenNR32020.12.03, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="4.00m No3")) +
  geom_line(data = zimen2020.12.03, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No8` = "#d47fff", `1.60m No6` = "#ffbb42", `4.00m No3` = "#0ff1ce", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-12-03")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2020.12.04 PLOTS---------------------------------------------------------------------------------------------------------------

#parcus unten NR4 Tag 2020.12.04-----------------------------
parcus_untenNR42020.12.04 <- parcus_untenNR4 %>% 
  filter(date == "2020-12-04") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(47))

#parcus mitte NR1 2020.12.04----------------------------------
parcus_mitteNR12020.12.04 <- parcus_mitteNR1 %>% 
  filter(date == "2020-12-04") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(47))

#parcus oben Nr 7 2020.12.04----------------------------------
parcus_obenNR72020.12.04<-parcus_obenNR7 %>% 
  filter(date == "2020-12-04") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2020.12.04-------------------------------------
zimen2020.12.04 <- zimen %>% 
  filter(date == "2020-12-04")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-12-04 16:00:00", "2020-12-04 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR42020.12.04, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="1.00m No4")) +
  geom_line(data = parcus_mitteNR12020.12.04, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="1.60m No1")) +
  geom_line(data = parcus_obenNR72020.12.04, aes(x=DATETIME_UTC, y=Mittel180, color="4.00m No7")) +
  geom_line(data = zimen2020.12.04, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No4` = "#ff1597", `1.60m No1` = "#ff5010", `4.00m No7` = "#4286ff", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2020-12-04")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR8 Tag 2020.12.04-----------------------------
zitadelle_untenNR82020.12.04 <- zitadelle_untenNR8 %>% 
  filter(date == "2020-12-04") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(45))

#zitadelle mitte NR6 2020.12.04----------------------------------
zitadelle_mitteNR62020.12.04 <- zitadelle_mitteNR6 %>% 
  filter(date == "2020-12-04") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(2))

#zitadelle oben NR3 2020.12.03----------------------------------
zitadelle_obenNR32020.12.04<-zitadelle_obenNR3 %>% 
  filter(date == "2020-12-04") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2020.12.03-------------------------------------
zimen2020.12.04 <- zimen %>% 
  filter(date == "2020-12-04")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2020-12-04 16:00:00", "2020-12-04 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR82020.12.04, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="1.00m No8")) +
  geom_line(data = zitadelle_mitteNR62020.12.04, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="1.60m No6")) +
  geom_line(data = zitadelle_obenNR32020.12.04, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="4.00m No3")) +
  geom_line(data = zimen2020.12.04, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No8` = "#d47fff", `1.60m No6` = "#ffbb42", `4.00m No3` = "#0ff1ce", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2020-12-04")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2021.02.23 PLOTS---------------------------------------------------------------------------------------------------------------

#parcus unten NR7 Tag 2021.02.23-----------------------------
parcus_untenNR72021.02.23 <- parcus_untenNR7 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(46))

#parcus mitte NR4 2021.02.23----------------------------------
parcus_mitteNR42021.02.23 <- parcus_mitteNR4 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(46))

#parcus oben Nr1 2021.02.23----------------------------------
parcus_obenNR12021.02.23<-parcus_obenNR1 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(54))

#parcus oben Nr2 2021.02.23----------------------------------
parcus_obenNR22021.02.23<-parcus_obenNR2 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2021.02.23-------------------------------------
zimen2021.02.23 <- zimen %>% 
  filter(date == "2021-02-23")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2021-02-23 16:00:00", "2021-02-23 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR72021.02.23, aes(x=DATETIME_UTC, y=Mittel180, color="1.00m No7")) +
  geom_line(data = parcus_mitteNR42021.02.23, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="1.60m No4")) +
  geom_line(data = parcus_obenNR12021.02.23, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="4.00m No1")) +
  geom_line(data = parcus_obenNR22021.02.23, aes(x=DATETIME_UTC, y=ADJ_180sec.NR2, color="4.00m No2")) +
  geom_line(data = zimen2021.02.23, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No7` = "#4286ff", `1.60m No4` = "#ff1597", `4.00m No1` = "#ff5010",`4.00m No2` = "#700808", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2021-02-23")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR3 Tag 2021.02.23-----------------------------
zitadelle_untenNR32021.02.23 <- zitadelle_untenNR3 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(4) - seconds(47))

#zitadelle mitte NR8 2021.02.23----------------------------------
zitadelle_mitteNR82021.02.23 <- zitadelle_mitteNR8 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(4) - seconds(46))

#zitadelle oben Nr6 2021.02.23----------------------------------
zitadelle_obenNR62021.02.23<-zitadelle_obenNR6 %>% 
  filter(date == "2021-02-23") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(2) - seconds(44))

#ZIMEN PM25 Tag 2021.02.23-------------------------------------
zimen2021.02.23 <- zimen %>% 
  filter(date == "2021-02-23")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2021-02-23 16:00:00", "2021-02-23 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR32021.02.23, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="1.00m No3")) +
  geom_line(data = zitadelle_mitteNR82021.02.23, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="1.60m No8")) +
  geom_line(data = zitadelle_obenNR62021.02.23, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="4.00m No6")) +
  geom_line(data = zimen2021.02.23, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No3` = "#0ff1ce", `1.60m No8` = "#d47fff", `4.00m No6` = "#ffbb42", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2021-02-23")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2021.02.24 PLOTS---------------------------------------------------------------------------------------------------------------

#parcus unten NR7 Tag 2021.02.24-----------------------------
parcus_untenNR72021.02.24 <- parcus_untenNR7 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(29) - seconds(46))

#parcus mitte NR4 2021.02.24----------------------------------
parcus_mitteNR42021.02.24 <- parcus_mitteNR4 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(25) - seconds(30))

#parcus oben Nr1 2021.02.24----------------------------------
parcus_obenNR12021.02.24<-parcus_obenNR1 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#parcus oben Nr2 2021.02.23----------------------------------
parcus_obenNR22021.02.24<-parcus_obenNR2 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(45))

#ZIMEN PM25 Tag 2021.02.24-------------------------------------
zimen2021.02.24 <- zimen %>% 
  filter(date == "2021-02-24")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2021-02-24 16:00:00", "2021-02-24 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR72021.02.24, aes(x=DATETIME_UTC, y=Mittel180, color="1.00m No7")) +
  geom_line(data = parcus_mitteNR42021.02.24, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="1.60m No4")) +
  geom_line(data = parcus_obenNR12021.02.24, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="4.00m No1")) +
  geom_line(data = parcus_obenNR22021.02.24, aes(x=DATETIME_UTC, y=ADJ_180sec.NR2, color="4.00m No2")) +
  geom_line(data = zimen2021.02.24, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No7` = "#4286ff", `1.60m No4` = "#ff1597", `4.00m No1` = "#ff5010",`4.00m No2` = "#700808", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2021-02-24")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR3 Tag 2021.02.24-----------------------------
zitadelle_untenNR32021.02.24 <- zitadelle_untenNR3 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(46))

#zitadelle mitte NR8 2021.02.24----------------------------------
zitadelle_mitteNR82021.02.24 <- zitadelle_mitteNR8 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(46))

#zitadelle oben Nr6 2021.02.23----------------------------------
zitadelle_obenNR62021.02.24<-zitadelle_obenNR6 %>% 
  filter(date == "2021-02-24") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2021.02.24-------------------------------------
zimen2021.02.24 <- zimen %>% 
  filter(date == "2021-02-24")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2021-02-24 16:00:00", "2021-02-24 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR32021.02.24, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="1.00m No3")) +
  geom_line(data = zitadelle_mitteNR82021.02.24, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="1.60m No8")) +
  geom_line(data = zitadelle_obenNR62021.02.24, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="4.00m No6")) +
  geom_line(data = zimen2021.02.24, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No3` = "#0ff1ce", `1.60m No8` = "#d47fff", `4.00m No6` = "#ffbb42", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2021-02-24")+
  theme_minimal() +
  theme(legend.title = element_blank())



#2021.02.25 PLOTS---------------------------------------------------------------------------------------------------------------

#parcus unten NR7 Tag 2021.02.25-----------------------------
parcus_untenNR72021.02.25 <- parcus_untenNR7 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(3) - seconds(30))

#parcus mitte NR4 2021.02.25----------------------------------
parcus_mitteNR42021.02.25 <- parcus_mitteNR4 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(2) - seconds(46))

#parcus oben Nr1 2021.02.25----------------------------------
parcus_obenNR12021.02.25<-parcus_obenNR1 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#parcus oben Nr2 2021.02.25----------------------------------
parcus_obenNR22021.02.25<-parcus_obenNR2 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(1) - seconds(30))

#ZIMEN PM25 Tag 2021.02.25-------------------------------------
zimen2021.02.25 <- zimen %>% 
  filter(date == "2021-02-25")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2021-02-25 16:00:00", "2021-02-25 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = parcus_untenNR72021.02.25, aes(x=DATETIME_UTC, y=Mittel180, color="1.00m No7")) +
  geom_line(data = parcus_mitteNR42021.02.25, aes(x=DATETIME_UTC, y=ADJ_180sec.NR4, color="1.60m No4")) +
  geom_line(data = parcus_obenNR12021.02.25, aes(x=DATETIME_UTC, y=ADJ_180sec.NR1, color="4.00m No1")) +
  geom_line(data = parcus_obenNR22021.02.25, aes(x=DATETIME_UTC, y=ADJ_180sec.NR2, color="4.00m No2")) +
  geom_line(data = zimen2021.02.25, aes(x=DATETIME_UTC, y=P_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No7` = "#4286ff", `1.60m No4` = "#ff1597", `4.00m No1` = "#ff5010",`4.00m No2` = "#700808", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 55)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Parcusstraße", subtitle = "2021-02-25")+
  theme_minimal() +
  theme(legend.title = element_blank())



#zitadelle unten NR3 Tag 2021.02.25-----------------------------
zitadelle_untenNR32021.02.25 <- zitadelle_untenNR3 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(6) - seconds(46))

#zitadelle mitte NR8 2021.02.25----------------------------------
zitadelle_mitteNR82021.02.25 <- zitadelle_mitteNR8 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - minutes(5) - seconds(46))

#zitadelle oben Nr6 2021.02.25----------------------------------
zitadelle_obenNR62021.02.25 <- zitadelle_obenNR6 %>% 
  filter(date == "2021-02-25") %>% 
mutate(DATETIME_UTC = DATETIME_UTC - seconds(46))

#ZIMEN PM25 Tag 2021.02.25-------------------------------------
zimen2021.02.25 <- zimen %>% 
  filter(date == "2021-02-25")

#plotten---------------------------------------------------------
lim <- as.POSIXct(c("2021-02-25 16:00:00", "2021-02-25 21:00:00"),  origin = "1970-01-01")
ggplot() +
  geom_line(data = zitadelle_untenNR32021.02.25, aes(x=DATETIME_UTC, y=ADJ_180sec.NR3, color="1.00m No3")) +
  geom_line(data = zitadelle_mitteNR82021.02.25, aes(x=DATETIME_UTC, y=ADJ_180sec.NR8, color="1.60m No8")) +
  geom_line(data = zitadelle_obenNR62021.02.25, aes(x=DATETIME_UTC, y=ADJ_180sec.NR6, color="4.00m No6")) +
  geom_line(data = zimen2021.02.25, aes(x=DATETIME_UTC, y=Z_PM25, color="4.00m ZIMEN")) +
  scale_color_manual(values = c(`1.00m No3` = "#0ff1ce", `1.60m No8` = "#d47fff", `4.00m No6` = "#ffbb42", `4.00m ZIMEN`="#000000")) +
  scale_x_datetime(limits = lim) +
  scale_y_continuous(limits = c(-5, 50)) +
  labs(x="Time (24h)",y="PM2.5 (µg/m3)",title = "Zitadelle", subtitle = "2021-02-25")+
  theme_minimal() +
  theme(legend.title = element_blank())


