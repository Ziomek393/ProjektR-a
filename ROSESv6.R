library(sf)
library(haven)
library(dplyr)
library(stringr)
library(readxl)
library(tidyr)
library(ggplot2)
library(forcats)
library(RColorBrewer)


roses <- read_sav("ROSES.sav")
woj <- read.csv2("woj.csv", fileEncoding = "UTF-8") # Ramka znaleziona w internecie z miastami z przypisanymi województwami
coord <- read_xlsx("Koordynaty.xlsx") # Miasta do których mamy współrzędne geograficzne (też z neta)
coordimportant <- read_xlsx("exportImportant.xlsx") # Ramka przypisująca miasta do województw, których nie było w woj.csv
kordadd <- read_xlsx("Koordynaty_add1.xlsx") # ResponseId, Województwo dla ankiet bez lokalizacji odczytane z Q19
kordadd <- kordadd %>% 
  filter(Województwo > 0) %>% 
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

r1 <- roses %>% # Wyciągnięcie z roses interesujących nas obserwacji (tj. podana lokalizacja)
  mutate(LocationLatitude = round(as.numeric(LocationLatitude), 4), 
         LocationLongitude = round(as.numeric(LocationLongitude), 4)) %>% 
  filter(LocationLongitude > 0) %>% 
  select(ResponseId,LocationLongitude, LocationLatitude)

r2 <- merge.data.frame(r1, coord, by = NULL) %>% # Przypisanie do ~każdego ResponseId województwa
  group_by(ResponseId) %>%
  mutate(min_d = min( abs(LocationLatitude-Szerokość) + abs(LocationLongitude - Długość), na.rm=TRUE)) %>% 
  mutate(d = abs(LocationLatitude - Szerokość) + abs(LocationLongitude - Długość) ) %>% 
  filter(min_d == d & min_d < 5) %>%
  select(ResponseId, Miejscowość) %>% 
  left_join(coordimportant, by.x="Miejscowość", by.y="Miejscowość") %>% 
  filter(is.na(nazwa.województwa) == FALSE) %>% 
  select(ResponseId, Województwo = nazwa.województwa) %>% 
  bind_rows(kordadd)

roses_q <- left_join(r2, roses, by="ResponseId") # Dodanie do roses kolumny województw

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
pomoce <- read_xlsx("pomoce.xlsx")

rosesw3 <- roses_q %>%
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
  ggplot(aes(y = Pomoc, x = Użycie)) +
  geom_col(color="black", fill = "LightBlue") +
  theme_bw() +
  scale_x_continuous(breaks = seq(1, 4, 0.5))+
  coord_cartesian(xlim = c(1,4))+
  theme(axis.title.y = element_blank(),
        plot.title = element_text(face = "bold", hjust = 0.5),
        panel.background = element_rect(fill = "lemonchiffon1")) +
  labs(x = "Współczynnik użycia") +
  scale_y_discrete(labels = function(x) str_wrap(str_replace_all(x, "foo" , " "), width = 18)) +
  ggtitle("Popularność wybranych pomocy naukowych")


# Wykres 2 a)


rosesw2 <- roses_q %>%
  ungroup() %>%
  mutate(wsp_pom = rowMeans(select(., starts_with("Q13")), na.rm = TRUE)) %>% 
  select(Q13_1:Q13_10,ResponseId, wsp_pom)

# roses_q$ilena <- rowSums(is.na(roses_q[,163:172]))
# rosesw2 <- roses_q %>% 
#   select(Q13_1:Q13_10, ResponseId, ilena) %>% 
#   group_by(ResponseId) %>% 
#   mutate(sum_pom = sum(Q13_1, Q13_2, Q13_3, Q13_4, Q13_5, Q13_6, Q13_7, Q13_8, Q13_9, Q13_10, na.rm = TRUE),
#          cunt = 10-ilena,
#          wsp_pom = sum_pom/cunt) %>% 
#   select(ResponseId,wsp_pom)

rosesw2_z <- roses_q %>% 
  select(ResponseId,Q5_1:Q9_31) %>%
  ungroup() %>% 
  mutate(wsp_z = rowMeans(select(.,starts_with("Q")), na.rm = TRUE)) %>% 
  select(ResponseId, wsp_z)

rosesw2_xd <- bind_cols(rosesw2, rosesw2_z) %>%
  group_by(Q13_4) %>% 
  mutate(wsp_z_med_4 = median(wsp_z)) %>% 
  group_by(Q13_6) %>% 
  mutate(wsp_z_med_6 = median(wsp_z)) %>% 
  ungroup() %>% 
  filter(is.na(wsp_pom) == FALSE | is.na(wsp_z) == FALSE)


rosesw2_xd %>% 
  ggplot(aes(x = wsp_pom)) +
  geom_smooth(aes(y = wsp_z,color = "Wszystkie pomoce \nnaukowe")) +
  geom_line(data = rosesw2_xd, aes(x = Q13_4, y = wsp_z_med_4, color = "E-Podręczniki"),
            size = 1) +
  geom_line(data = rosesw2_xd, aes(x = Q13_6, y = wsp_z_med_6, color = "Gry Komputerowe"), 
            size = 1) +
  scale_colour_manual("", 
                      breaks = c("Wszystkie pomoce \nnaukowe",
                                 "E-Podręczniki", 
                                 "Gry Komputerowe"),
                      values = c("blue", "green", "red")) +
  labs(x = "Stopień użycia pomocy \nnaukowych", y = "Zainteresowanie nauką")

# rosesw2_xd %>% 
#   group_by(Q13_10) %>% 
#   mutate(wsp_z_med = median(wsp_z)) %>% 
#   ggplot(aes(x = Q13_10, y = wsp_z_med)) +
#   geom_line() +
#   coord_cartesian(ylim = c(2,3))


# Wykres 1


polandmap <- read_sf("wojewodztwa.shp") %>% 
  select(nazwa = JPT_NAZWA_, geometry) %>% 
  arrange(nazwa)

# ggplot(data = polandmap) + geom_sf()

tmp <- roses_q %>% ungroup() %>% 
  filter(is.na(Województwo) == FALSE) %>%
  mutate(Meanvalue = rowMeans(select(., starts_with("Q13")), na.rm = TRUE)) %>% 
  mutate(Województwo = str_remove(Województwo, " ")) %>% 
  group_by(Województwo) %>% 
  summarise(MeanWoj = mean(Meanvalue, na.rm = TRUE)) %>% 
  distinct(Województwo, .keep_all = TRUE) %>% 
  arrange(Województwo) %>% 
  mutate(Województwo = str_to_lower(Województwo)) %>% 
  inner_join(polandmap, by = c("Województwo" = "nazwa")) %>% 
  st_as_sf()

ggplot(data = tmp) + geom_sf(aes(fill = MeanWoj)) + 
  scale_fill_gradient(low = "skyblue1", high = "skyblue4", 
                      name = "Współczynnik wykorzystania\n pomocy naukowych")
  


### Plan na Rurze ###
# UZUPEŁNIĆ coordimportant - brakuje kilku miejscowości
# Jakie pytania chcemy zada?? 
# Basic theme - podział na województwa
# 1. Mapa Polski - Współczynnik wykorzystania pomocy naukowych (Q13) + liczba recordów
# 2. Punktowy - "x" zainteresowanie uczniów przedmiotami, "y" wykorzystanie pomocy (a. jednostki, b.woj)
# 3. Słupkowy - ranking pomocy naukowych (Q13)



