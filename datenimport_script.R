library(tidyverse)
library(lubridate)

# Kalibrationsdaten lesen (nach Lorenz Harr)
cali_180 <- read_delim("./DataCorrectionByLorenzHarr/CalibrationskoeffizientenApr21_180.csv",
                       delim = ";",
                       skip = 2) 

cali_180 <- cali_180 %>% 
  rename(var = Device) %>% 
  slice(1:(n()-2)) %>% 
  pivot_longer(!var, names_to = "device", values_to = "value")


cali_20 <- read_delim("./DataCorrectionByLorenzHarr/CalibrationskoeffizientenApr21_20.csv",
                       delim = ";",
                       skip = 2) 

cali_20 <- cali_20 %>% 
  rename(var = Device) %>% 
  slice(1:(n()-2)) %>% 
  pivot_longer(!var, names_to = "device", values_to = "value")


#Parcusstraße Daten einlesen -------------
#Nach Messhoehen und Gerätenummern

#Parcus Unten nach Nummern----------------------------------------------------------------------------------------------

#NR1
parcus_untenNR1 <-
  list.files(path = "./data/parcusStr/",
             pattern = "UntenNR1", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "unten",
         unit = "NR1")

parcus_untenNR1 <- parcus_untenNR1 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_untenNR1$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_untenNR1 <- parcus_untenNR1 %>% 
  mutate(date = ymd(parcus_untenNR1$date),
         time = hms(parcus_untenNR1$time))



#NR2
parcus_untenNR2 <-
  list.files(path = "./data/parcusStr/",
             pattern = "UntenNR2", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "unten",
         unit = "NR2")

parcus_untenNR2 <- parcus_untenNR2 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_untenNR2$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_untenNR2 <- parcus_untenNR2 %>% 
  mutate(date = ymd(parcus_untenNR2$date),
         time = hms(parcus_untenNR2$time))



#NR4
parcus_untenNR4 <-
  list.files(path = "./data/parcusStr/",
             pattern = "UntenNR4", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "unten",
         unit = "NR4")

parcus_untenNR4 <- parcus_untenNR4 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_untenNR4$DATETIME_UTC))) %>%
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_untenNR4 <- parcus_untenNR4 %>% 
  mutate(date = ymd(parcus_untenNR4$date),
         time = hms(parcus_untenNR4$time))



#NR7
parcus_untenNR7 <-
  list.files(path = "./data/parcusStr/",
             pattern = "UntenNR7", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "unten",
         unit = "NR7")

parcus_untenNR7 <- parcus_untenNR7 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_untenNR7$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_untenNR7 <- parcus_untenNR7 %>% 
  mutate(date = ymd(parcus_untenNR7$date),
         time = hms(parcus_untenNR7$time))



#Parcus Mitte nach Nummern----------------------------------------
#NR1
parcus_mitteNR1 <-
  list.files(path = "./data/parcusStr/",
             pattern = "MitteNR1", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "mitte",
         unit = "NR1")

parcus_mitteNR1 <- parcus_mitteNR1 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_mitteNR1$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_mitteNR1 <- parcus_mitteNR1 %>% 
  mutate(date = ymd(parcus_mitteNR1$date),
         time = hms(parcus_mitteNR1$time))



#NR2
parcus_mitteNR2 <-
  list.files(path = "./data/parcusStr/",
             pattern = "MitteNR2", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "mitte",
         unit = "NR2")

parcus_mitteNR2 <- parcus_mitteNR2 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_mitteNR2$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_mitteNR2 <- parcus_mitteNR2 %>% 
  mutate(date = ymd(parcus_mitteNR2$date),
         time = hms(parcus_mitteNR2$time))



#NR4
parcus_mitteNR4 <-
  list.files(path = "./data/parcusStr/",
             pattern = "MitteNR4", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "mitte",
         unit = "NR4")

parcus_mitteNR4 <- parcus_mitteNR4 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_mitteNR4$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_mitteNR4 <- parcus_mitteNR4 %>% 
  mutate(date = ymd(parcus_mitteNR4$date),
         time = hms(parcus_mitteNR4$time))



#NR7
parcus_mitteNR7 <-
  list.files(path = "./data/parcusStr/",
             pattern = "MitteNR7", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "mitte",
         unit = "NR7")

parcus_mitteNR7 <- parcus_mitteNR7 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_mitteNR7$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_mitteNR7 <- parcus_mitteNR7 %>% 
  mutate(date = ymd(parcus_mitteNR7$date),
         time = hms(parcus_mitteNR7$time))



#Parcus Oben nach Nummern-------------------------------------

#NR1
parcus_obenNR1 <-
  list.files(path = "./data/parcusStr/",
             pattern = "ObenNR1", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "oben",
         unit = "NR1")

parcus_obenNR1 <- parcus_obenNR1 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_obenNR1$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_obenNR1 <- parcus_obenNR1 %>% 
  mutate(date = ymd(parcus_obenNR1$date),
         time = hms(parcus_obenNR1$time))



#NR2
parcus_obenNR2 <-
  list.files(path = "./data/parcusStr/",
             pattern = "ObenNR2", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "oben",
         unit = "NR2")

parcus_obenNR2 <- parcus_obenNR2 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_obenNR2$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_obenNR2 <- parcus_obenNR2 %>% 
  mutate(date = ymd(parcus_obenNR2$date),
         time = hms(parcus_obenNR2$time))



#NR4
parcus_obenNR4 <-
  list.files(path = "./data/parcusStr/",
             pattern = "ObenNR4", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "oben",
         unit = "NR4")

parcus_obenNR4 <- parcus_obenNR4 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_obenNR4$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_obenNR4 <- parcus_obenNR4 %>% 
  mutate(date = ymd(parcus_obenNR4$date),
         time = hms(parcus_obenNR4$time))



#NR7
parcus_obenNR7 <-
  list.files(path = "./data/parcusStr/",
             pattern = "ObenNR7", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "parcusstraße",
         height = "oben",
         unit = "NR7")

parcus_obenNR7 <- parcus_obenNR7 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(parcus_obenNR7$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

parcus_obenNR7 <- parcus_obenNR7 %>% 
  mutate(date = ymd(parcus_obenNR7$date),
         time = hms(parcus_obenNR7$time))

parcus_unten <- rbind(parcus_untenNR1, parcus_untenNR2, parcus_untenNR4, parcus_untenNR7)
parcus_mitte <- rbind(parcus_mitteNR1, parcus_mitteNR4, parcus_mitteNR7)
parcus_oben <- rbind(parcus_obenNR1, parcus_obenNR2, parcus_obenNR4, parcus_obenNR7)

parcus_alles <- rbind(parcus_unten, parcus_mitte, parcus_oben)


#Zitadelle Daten einlesen--------------------------------------------------------------------------------------------------
#Nach Messhoehen und Gerätenummern

#Zitadelle Unten

#NR3
zitadelle_untenNR3 <-
  list.files(path = "./data/zitadelle/",
             pattern = "UntenNR3", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "unten",
         unit = "NR3")

zitadelle_untenNR3 <- zitadelle_untenNR3 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_untenNR3$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_untenNR3 <- zitadelle_untenNR3 %>% 
  mutate(date = ymd(zitadelle_untenNR3$date),
         time = hms(zitadelle_untenNR3$time))



#NR6
zitadelle_untenNR6 <-
  list.files(path = "./data/zitadelle/",
             pattern = "UntenNR6", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "unten",
         unit = "NR6")

zitadelle_untenNR6 <- zitadelle_untenNR6 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_untenNR6$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_untenNR6 <- zitadelle_untenNR6 %>% 
  mutate(date = ymd(zitadelle_untenNR6$date),
         time = hms(zitadelle_untenNR6$time))



#NR8
zitadelle_untenNR8 <-
  list.files(path = "./data/zitadelle/",
             pattern = "UntenNR8", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "unten",
         unit = "NR8")

zitadelle_untenNR8 <- zitadelle_untenNR8 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_untenNR8$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_untenNR8 <- zitadelle_untenNR8 %>% 
  mutate(date = ymd(zitadelle_untenNR8$date),
         time = hms(zitadelle_untenNR8$time))



#Zitadelle Mitte nach Nummern-----------------------------

#NR3
zitadelle_mitteNR3 <-
  list.files(path = "./data/zitadelle/",
             pattern = "MitteNR3", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "mitte",
         unit = "NR3")

zitadelle_mitteNR3 <- zitadelle_mitteNR3 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_mitteNR3$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_mitteNR3 <- zitadelle_mitteNR3 %>% 
  mutate(date = ymd(zitadelle_mitteNR3$date),
         time = hms(zitadelle_mitteNR3$time))



#NR6
zitadelle_mitteNR6 <-
  list.files(path = "./data/zitadelle/",
             pattern = "MitteNR6", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "mitte",
         unit = "NR6")

zitadelle_mitteNR6 <- zitadelle_mitteNR6 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_mitteNR6$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_mitteNR6 <- zitadelle_mitteNR6 %>% 
  mutate(date = ymd(zitadelle_mitteNR6$date),
         time = hms(zitadelle_mitteNR6$time))



#NR8
zitadelle_mitteNR8 <-
  list.files(path = "./data/zitadelle/",
             pattern = "MitteNR8", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "mitte",
         unit = "NR8")

zitadelle_mitteNR8 <- zitadelle_mitteNR8 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_mitteNR8$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_mitteNR8 <- zitadelle_mitteNR8 %>% 
  mutate(date = ymd(zitadelle_mitteNR8$date),
         time = hms(zitadelle_mitteNR8$time))



#Zitadelle oben nach Nummern----------------------------------------

#NR3
zitadelle_obenNR3 <-
  list.files(path = "./data/zitadelle/",
             pattern = "ObenNR3", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "oben",
         unit = "NR3")

zitadelle_obenNR3 <- zitadelle_obenNR3 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_obenNR3$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_obenNR3 <- zitadelle_obenNR3 %>% 
  mutate(date = ymd(zitadelle_obenNR3$date),
         time = hms(zitadelle_obenNR3$time))



#NR6
zitadelle_obenNR6 <-
  list.files(path = "./data/zitadelle/",
             pattern = "ObenNR6", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "oben",
         unit = "NR6")

zitadelle_obenNR6 <- zitadelle_obenNR6 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_obenNR6$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_obenNR6 <- zitadelle_obenNR6 %>% 
  mutate(date = ymd(zitadelle_obenNR6$date),
         time = hms(zitadelle_obenNR6$time))



#NR8
zitadelle_obenNR8 <-
  list.files(path = "./data/zitadelle/",
             pattern = "ObenNR8", 
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "??dddddddddddddddddddddddddddddddddddddddddddd")) %>% 
  mutate(place = "zitadelle",
         height = "oben",
         unit = "NR8")

zitadelle_obenNR8 <- zitadelle_obenNR8 %>% 
  mutate(DATETIME_UTC = as.POSIXct(dmy_hms(zitadelle_obenNR8$DATETIME_UTC))) %>% 
  separate(DATETIME_UTC, into = c('date', 'time'), sep=' ', remove = FALSE)

zitadelle_obenNR8 <- zitadelle_obenNR8 %>% 
  mutate(date = ymd(zitadelle_obenNR8$date),
         time = hms(zitadelle_obenNR8$time))

zitadelle_unten <- rbind(zitadelle_untenNR3, zitadelle_untenNR6, zitadelle_untenNR8)
zitadelle_mitte <- rbind(zitadelle_mitteNR3, zitadelle_mitteNR6, zitadelle_mitteNR8)
zitadelle_oben <- rbind(zitadelle_obenNR3, zitadelle_obenNR6, zitadelle_obenNR8)

zitadelle_alles <- rbind(zitadelle_unten, zitadelle_mitte, zitadelle_oben)



# Zimen Daten einlesen --------------------------------------------------------------------------------------------------------

zimen <-
  list.files(path = "./data/Zimen/",
             full.names = T) %>% 
  map_df(~read_delim(., delim = ";", col_types = "?ddddddddddddddddd",na = c("#", "NA")))

zimen <- zimen %>% 
  mutate(Zeitpunkt = as.POSIXct(dmy_hm(zimen$Zeitpunkt))) %>% 
  separate(Zeitpunkt, into = c('date', 'time'), sep=' ', remove = FALSE)

zimen <- zimen %>% 
  mutate(date = ymd(zimen$date),
         time = hms(zimen$time)) %>% 
  rename(DATETIME_UTC = Zeitpunkt)


