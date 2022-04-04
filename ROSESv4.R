install.packages("sf")
library(sf)
library(haven)
library(dplyr)
library(data.table)
library(stringr)
library(readxl)
library(tidyr)
library(ggplot2)
library(forcats)
roses <- read_sav("ROSES.sav")
woj <- read.csv2("woj.csv",fileEncoding = "UTF-8")
coord <- read_xlsx("Koordynaty.xlsx")
kordadd <- read.csv2("Koordynaty_add1.csv")
coordimportant <- read_xlsx("exportImportant.xlsx")
kordadd <- kordadd %>% 
  filter(Województwo>0) %>% 
  select(ResponseId, Województwo)

### Porządzkuję bałagan z miastami, żeby móc pogrupować do województw
# Czyszczenie długości i szerokości geograficznej
coord <- coord[2:2319,] %>%
  mutate(Długość1 = str_remove(Długość, "\'E$"), Szerokość1 = str_remove(Szerokość, "\'N$")) %>% 
  mutate(Długość1 = str_replace(Długość1, "°", "\\."), Szerokość1 = str_replace(Szerokość1, "°", "\\.")) %>% 
  mutate(Długość1 = as.numeric(Długość1), Szerokość1 = as.numeric(Szerokość1))
coord <- coord %>% 
  select(Miejscowość,Długość=Długość1,Szerokość=Szerokość1)

# Zaokrąglanie dł. i szer. w Roses

r1 <- roses %>% 
  mutate(LocationLatitude = round(as.numeric(LocationLatitude), 4), 
         LocationLongitude = round(as.numeric(LocationLongitude), 4)) %>% 
  filter(LocationLongitude >0) %>% 
  select(ResponseId,LocationLongitude, LocationLatitude)

r2 <- merge.data.frame(r1, coord, by = NULL) %>%
  group_by(ResponseId) %>%
  mutate(min_d = min((LocationLatitude-Szerokość)^2+(LocationLongitude - Długość)^2,na.rm=TRUE)) %>% 
  mutate(d=(LocationLatitude-Szerokość)^2+(LocationLongitude - Długość)^2) %>% 
  filter(min_d==d & min_d <5) %>%
  select(ResponseId,Miejscowość) %>% 
  left_join(coordimportant, by.x="Miejscowość",by.y="Miejscowość") %>% 
  filter(is.null(nazwa.województwa)==FALSE) %>% 
  select(ResponseId,Województwo=nazwa.województwa) %>% 
  bind_rows(kordadd) # !!!! TU JEST EL PROBLEMOOOO

roses_q <- left_join(r2,roses,by="ResponseId")

### 3 wykres:

# rosesw3<- roses_q %>% 
#   select(Q13_1:Q13_10, Województwo, ResponseId) %>% 
#   group_by(Województwo) %>% 
#   mutate(rankQ1 = mean(Q13_1, na.rm = TRUE),
#          rankQ2 = mean(Q13_2, na.rm = TRUE),
#          rankQ3 = mean(Q13_3, na.rm = TRUE),
#          rankQ4 = mean(Q13_4, na.rm = TRUE),
#          rankQ5 = mean(Q13_5, na.rm = TRUE),
#          rankQ6 = mean(Q13_6, na.rm = TRUE),
#          rankQ7 = mean(Q13_7, na.rm = TRUE),
#          rankQ8 = mean(Q13_8, na.rm = TRUE),
#          rankQ9 = mean(Q13_9, na.rm = TRUE),
#          rankQ10 = mean(Q13_10, na.rm = TRUE)) %>%
#   select(rankQ1:rankQ10, Województwo) %>%
#   mutate(maks = max(rankQ1:rankQ10)) %>% 
#   distinct(Województwo, .keep_all = TRUE) %>% 
#   filter(is.na(Województwo)==FALSE)

# opcja globalna jakbyśmy stwierdzili że województwa nas nie obchodzą
pomoce<-read_xlsx("pomoce.xlsx")
rosesw3<- roses_q %>%
  ungroup() %>%
  select(Q13_1:Q13_10) %>%
  mutate(rankQ1 = mean(Q13_1, na.rm = TRUE),
        rankQ2 = mean(Q13_2, na.rm = TRUE),
        rankQ3 = mean(Q13_3, na.rm = TRUE),
        rankQ4 = mean(Q13_4, na.rm = TRUE),
        rankQ5 = mean(Q13_5, na.rm = TRUE),
        rankQ6 = mean(Q13_6, na.rm = TRUE),
        rankQ7 = mean(Q13_7, na.rm = TRUE),
        rankQ8 = mean(Q13_8, na.rm = TRUE),
        rankQ9 = mean(Q13_9, na.rm = TRUE),
        rankQ10 = mean(Q13_10, na.rm = TRUE)) %>%
  select(rankQ1:rankQ10) 
rosesw3_1 <- bind_cols(t(rosesw3[1,]), pomoce) %>% 
  rename(Użycie=...1) %>% 
  mutate(Pomoc = forcats::fct_reorder(Pomoc,Użycie))


rosesw3_1 %>% 
  ggplot(aes(y = Pomoc, x = Użycie))+
  geom_col(color="black", fill = "LightBlue")+
  theme_bw()+
  scale_x_continuous(n.breaks = 10)+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "lemonchiffon1"))+
  labs(x = "Współczynnik użycia")+
  ggtitle("Popularność wybranych pomocy naukowych")

# Wykres 2 a)
roses_q$ilena <- rowSums(is.na(roses_q[,163:172]))
rosesw2 <- roses_q %>% 
  select(Q13_1:Q13_10, ResponseId, ilena) %>% 
  group_by(ResponseId) %>% 
  mutate(sum_pom = sum(Q13_1, Q13_2, Q13_3, Q13_4, Q13_5, Q13_6, Q13_7, Q13_8, Q13_9, Q13_10, na.rm = TRUE),
         cunt = 10-ilena,
         wsp_pom = sum_pom/cunt) %>% 
  select(ResponseId,wsp_pom)

rosesw2_z <- roses_q %>% 
  select(ResponseId,Q5_1:Q9_31) %>% 
  ungroup() %>% 
  mutate(wsp_z = rowMeans(select(.,starts_with("Q")), na.rm = TRUE)) %>% 
  select(ResponseId, wsp_z)
  
rosesw2_xd <- bind_cols(rosesw2, rosesw2_z)

plot2a <- rosesw2_xd %>% 
  ggplot(aes(x = wsp_pom, y = wsp_z))+
  geom_point()

plot2a



### Plan na Rurze ###
# UZUPEŁNIĆ coordimportant - brakuje kilku miejscowości
# Jakie pytania chcemy zada?? 
# Basic theme - podział na województwa
# 1. Mapa Polski - Współczynnik wykorzystania pomocy naukowych (Q13) + liczba recordów
# 2. Punktowy - "x" zainteresowanie uczniów przedmiotami, "y" wykorzystanie pomocy (a. jednostki, b.woj)
# 3. Słupkowy - ranking pomocy naukowych (Q13)



