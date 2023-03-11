#### Paczki do przetwarzania/wizualizacji danych ####
require(eurostat)
require(tidyverse)
remove(list = ls())

#### Wczytywanie danych ####

# Plik do łączenia kodów GUS z NUTS3
GUS_NUTS <- read.csv("./data/GUS_NUTS.csv", sep = ";",
					 colClasses = c("character","character","character")) %>% 
	select(Kod_NUTS, Kod_GUS) %>% as_tibble

# TFR - Total Fertility Rate - Współczynnik dzietności
TFR <- read.csv("./data/wspolczynnik_dzietnosci//LUDN_2346_CREL_20230311183211.csv",
				sep = ";", dec = ",", header = T, fill = TRUE,
				colClasses = c("character","character","character",
							   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "TFR") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

TFR_P <- read.csv("./data/wspolczynnik_dzietnosci//LUDN_2346_CREL_20230311183211.csv",
				  sep = ";", dec = ",", header = T, fill = TRUE,
				  colClasses = c("character","character","character",
				  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "TFR") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

TFR_P

# Shapefile'e dla Polski
POL <- get_eurostat_geospatial(output_class = "sf",
							   resolution = "01",
							   nuts_level = 3,
							   year = 2021) %>% 
	filter(CNTR_CODE == "PL")


#### Łączenie danych do pliku shapefile ####
POL <- left_join(POL,TFR_P, by = "NUTS_ID")

