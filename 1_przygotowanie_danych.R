#### Paczki do przetwarzania/wizualizacji danych ####
require(eurostat)
require(tidyverse)
remove(list = ls())

#### Wczytywanie danych ####

# Plik do łączenia kodów GUS z NUTS3
GUS_NUTS <- read.csv("./data/GUS_NUTS.csv", sep = ";",
					 colClasses = c("character","character","character")) %>% 
	select(Kod_NUTS, Kod_GUS) %>% as_tibble

# TFR - Total Fertility Rate - Współczynnik dzietności - wspolczynnik_dzietnosci
TFR <- read.csv("./data/wspolczynnik_dzietnosci/LUDN_2346_CREL_20230311183211.csv",
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
				values_from = Wartosc) %>% 
	filter(Rok != 2022)


TFR_P <- read.csv("./data/wspolczynnik_dzietnosci//LUDN_2346_CREL_20230311183211.csv",
				  sep = ";", dec = ",", header = T, fill = TRUE,
				  colClasses = c("character","character","character",
				  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>%
	filter(Rok != 2022) %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "TFR") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc) 

TFR_P

# bezrobotni_wg_wieku_i_plci
bezrobotni_wg_wieku_i_plci <- read.csv("./data/bezrobotni_wg_wieku_i_plci/RYNE_1946_CREL_20230312143348.csv",
				sep = ";", dec = ",", header = T, fill = TRUE,
				colClasses = c("character","character","character", "character",
							   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Płeć, Rok, Wartosc) %>% 
	mutate(zmienna = "bezrobotni") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Płeć),
				values_from = Wartosc)

bezrobotni_wg_wieku_i_plci_P <- read.csv("./data/bezrobotni_wg_wieku_i_plci/RYNE_1946_CREL_20230312143348.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE,
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Płeć, Rok, Wartosc) %>% 
	mutate(zmienna = "bezrobotni") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Płeć, Rok),
				values_from = Wartosc)

# cena_mieszkan_metr_mediana
cena_mieszkan_metr_mediana <- read.csv("./data/cena_mieszkan_metr_mediana/RYNE_3787_CREL_20230312175819.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE,
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "cena_mieszkan_metr_mediana") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

cena_mieszkan_metr_mediana_P <- read.csv("./data/cena_mieszkan_metr_mediana/RYNE_3787_CREL_20230312175819.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE,
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "cena_mieszkan_metr_mediana") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# cena_mieszkan_metr_srednia
cena_mieszkan_metr_srednia <- read.csv("./data/cena_mieszkan_metr_srednia/RYNE_3788_CREL_20230312175926.csv",
									   sep = ";", dec = ",", header = T, fill = TRUE,
									   colClasses = c("character","character","character", "character",
									   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "cena_mieszkan_metr_srednia") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

cena_mieszkan_metr_srednia_P <- cena_mieszkan_metr_srednia %>% 
	mutate(zmienna = "cena_mieszkan_metr_srednia") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = cena_mieszkan_metr_srednia)


cena_mieszkan_metr_srednia_P <- read.csv("./data/cena_mieszkan_metr_srednia/RYNE_3788_CREL_20230312175926.csv",
										 sep = ";", dec = ",", header = T, fill = TRUE,
										 colClasses = c("character","character","character", "character",
										 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "cena_mieszkan_metr_srednia") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# ludnosc_kobiety
ludnosc_kobiety <- read.csv("./data/ludnosc_kobiety/LUDN_2137_CREL_20230312180724.csv",
							sep = ";", dec = ",", header = T, fill = TRUE,
							colClasses = c("character","character","character", "character",
										   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Wiek, Rok, Wartosc) %>% 
	mutate(zmienna = "ludnosc_kobiety") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Wiek),
				values_from = Wartosc) %>% 
	mutate(ludnosc_kobiety_wiek_rozrodczy = `ludnosc_kobiety_15-19` +
		   	`ludnosc_kobiety_20-24` +
		   	`ludnosc_kobiety_25-29` +
		   	`ludnosc_kobiety_30-34` +
		   	`ludnosc_kobiety_35-39` +
		   	`ludnosc_kobiety_40-44` +
		   	`ludnosc_kobiety_45-49` )

ludnosc_kobiety_ogolem_P <- ludnosc_kobiety %>% 
	select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_kobiety_ogółem) %>% 
	mutate(zmienna = "ludnosc_kobiety_ogółem") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = ludnosc_kobiety_ogółem) 

ludnosc_kobiety_rozrodczy_P <- ludnosc_kobiety %>% 
	select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_kobiety_wiek_rozrodczy) %>% 
	mutate(zmienna = "ludnosc_kobiety_wiek_rozrodczy") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = ludnosc_kobiety_wiek_rozrodczy) 


# ludnosc_ogolem
ludnosc_ogolem <- read.csv("./data/ludnosc_ogolem/LUDN_2137_CREL_20230312180341.csv",
							sep = ";", dec = ",", header = T, fill = TRUE,
							colClasses = c("character","character","character", "character",
										   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Wiek, Rok, Wartosc) %>% 
	mutate(zmienna = "ludnosc_ogolem") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Wiek),
				values_from = Wartosc) %>% 
	mutate(ludnosc_ogolem_wiek_rozrodczy = 
		   	`ludnosc_ogolem_15-19` +
		   	`ludnosc_ogolem_20-24` +
		   	`ludnosc_ogolem_25-29` +
		   	`ludnosc_ogolem_30-34` +
		   	`ludnosc_ogolem_35-39` +
		   	`ludnosc_ogolem_40-44` +
		   	`ludnosc_ogolem_45-49` )

ludnosc_ogolem_ogolem_P <- ludnosc_ogolem %>% 
	select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_ogolem_ogółem) %>% 
	mutate(zmienna = "ludnosc_ogolem_ogolem") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = ludnosc_ogolem_ogółem) 

ludnosc_ogolem_rozrodczy_P <- ludnosc_ogolem %>% 
	select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_ogolem_wiek_rozrodczy) %>% 
	mutate(zmienna = "ludnosc_ogolem_wiek_rozrodczy") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = ludnosc_ogolem_wiek_rozrodczy) 

# ludnosc_mezczyzni
ludnosc_mezczyzni <- read.csv("./data/ludnosc_mezczyzni/LUDN_2137_CREL_20230312180626.csv",
						   sep = ";", dec = ",", header = T, fill = TRUE,
						   colClasses = c("character","character","character", "character",
						   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Wiek, Rok, Wartosc) %>% 
	mutate(zmienna = "ludnosc_mezczyzni") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna, Wiek),
				values_from = Wartosc) %>% 
	mutate(ludnosc_mezczyzni_wiek_rozrodczy = 
		   	`ludnosc_mezczyzni_15-19` +
		   	`ludnosc_mezczyzni_20-24` +
		   	`ludnosc_mezczyzni_25-29` +
		   	`ludnosc_mezczyzni_30-34` +
		   	`ludnosc_mezczyzni_35-39` +
		   	`ludnosc_mezczyzni_40-44` +
		   	`ludnosc_mezczyzni_45-49` )

ludnosc_mezczyzni_ogolem_P <- ludnosc_mezczyzni %>% 
	select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_mezczyzni_ogółem) %>% 
	mutate(zmienna = "ludnosc_mezczyzni_ogolem") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = ludnosc_mezczyzni_ogółem) 

ludnosc_mezczyzni_rozrodczy_P <- ludnosc_mezczyzni %>% 
	select(NUTS_ID, Kod_GUS, Nazwa, Rok, ludnosc_mezczyzni_wiek_rozrodczy) %>% 
	mutate(zmienna = "ludnosc_mezczyzni_wiek_rozrodczy") %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = ludnosc_mezczyzni_wiek_rozrodczy) 

# malzenstwa_zawarte
malzenstwa_zawarte <- read.csv("./data/malzenstwa_zawarte/LUDN_3430_CREL_20230312142004.csv",
				sep = ";", dec = ",", header = T, fill = TRUE,
				colClasses = c("character","character","character",
							   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "malzenstwa_zawarte") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	filter(Rok != 2022) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc) %>% 

malzenstwa_zawarte_P <- read.csv("./data/malzenstwa_zawarte/LUDN_3430_CREL_20230312142004.csv",
							   sep = ";", dec = ",", header = T, fill = TRUE,
							   colClasses = c("character","character","character",
							   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "malzenstwa_zawarte") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	filter(Rok != 2022) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# mieszkania_uzytkowe
mieszkania_uzytkowe <- read.csv("./data/mieszkania_uzytkowe/PRZE_3820_CREL_20230312174157.csv",
							   sep = ";", dec = ",", header = T, fill = TRUE,
							   colClasses = c("character","character","character",
							   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(Wskaźniki == "mieszkania oddane do użytkowania na 1000 ludności" ~ "mieszkania_oddane",
							   Wskaźniki == "przeciętna powierzchnia użytkowa 1 mieszkania oddanego do użytkowania" ~ "mieszkania_przecietna_powierzchnia",
							   Wskaźniki == "nowe budynki mieszkalne na 1000 ludności" ~ "mieszkania_nowe",
							   Wskaźniki == "przeciętna liczba izb w 1 mieszkaniu oddanym do użytkowania" ~ "mieszkania_izby")
	) %>% 
	select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	filter(Rok != c(2002,2003,2022)) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

mieszkania_uzytkowe_P <- read.csv("./data/mieszkania_uzytkowe/PRZE_3820_CREL_20230312174157.csv",
								sep = ";", dec = ",", header = T, fill = TRUE,
								colClasses = c("character","character","character",
											   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(Wskaźniki == "mieszkania oddane do użytkowania na 1000 ludności" ~ "mieszkania_oddane",
							   Wskaźniki == "przeciętna powierzchnia użytkowa 1 mieszkania oddanego do użytkowania" ~ "mieszkania_przecietna_powierzchnia",
							   Wskaźniki == "nowe budynki mieszkalne na 1000 ludności" ~ "mieszkania_nowe",
							   Wskaźniki == "przeciętna liczba izb w 1 mieszkaniu oddanym do użytkowania" ~ "mieszkania_izby")
	) %>% 
	select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	filter(Rok != c(2002,2003,2022)) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)


# rozwody_i_separacje
rozwody_i_separacje <- read.csv("./data/rozwody_i_separacje/LUDN_1971_CREL_20230312142529.csv",
								sep = ";", dec = ",", header = T, fill = TRUE,
								colClasses = c("character","character","character",
											   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(`Rozwody.i.separacje` == "rozwody na 10 tys. ludności" ~ "rozwody_na_10_tys",
							   `Rozwody.i.separacje` == "separacje" ~ "separacje",
							   `Rozwody.i.separacje` == "rozwody" ~ "rozwody",
							   `Rozwody.i.separacje` == "separacje na 10 tys. ludności" ~ "separacje_na_10_tys")
	) %>% 
	select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

rozwody_i_separacje_P <- read.csv("./data/rozwody_i_separacje/LUDN_1971_CREL_20230312142529.csv",
								sep = ";", dec = ",", header = T, fill = TRUE,
								colClasses = c("character","character","character",
											   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	mutate(zmienna = case_when(`Rozwody.i.separacje` == "rozwody na 10 tys. ludności" ~ "rozwody_na_10_tys",
							   `Rozwody.i.separacje` == "separacje" ~ "separacje",
							   `Rozwody.i.separacje` == "rozwody" ~ "rozwody",
							   `Rozwody.i.separacje` == "separacje na 10 tys. ludności" ~ "separacje_na_10_tys")
	) %>% 
	select(Kod, Nazwa, Rok, Wartosc, zmienna) %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# swiadczenia_500_plus
swiadczenia_500_plus <- read.csv("./data/swiadczenia_500_plus/OCHR_3803_CREL_20230312173651.csv",
				sep = ";", dec = ",", header = T, fill = TRUE,
				colClasses = c("character","character","character",
							   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "swiadczenia_500_plus") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

swiadczenia_500_plus_P <- read.csv("./data/swiadczenia_500_plus/OCHR_3803_CREL_20230312173651.csv",
								 sep = ";", dec = ",", header = T, fill = TRUE,
								 colClasses = c("character","character","character",
								 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "swiadczenia_500_plus") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# swiadczenia_spoleczne
swiadczenia_spoleczne <- read.csv("./data/swiadczenia_spoleczne/OCHR_2993_CREL_20230312173337.csv",
								 sep = ";", dec = ",", header = T, fill = TRUE,
								 colClasses = c("character","character","character",
								 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "swiadczenia_spoleczne") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

swiadczenia_spoleczne_P <- read.csv("./data/swiadczenia_spoleczne/OCHR_2993_CREL_20230312173337.csv",
								  sep = ";", dec = ",", header = T, fill = TRUE,
								  colClasses = c("character","character","character",
								  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "swiadczenia_spoleczne") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# wspolczynnik_feminizacji
wspolczynnik_feminizacji <- read.csv("./data/wspolczynnik_feminizacji/LUDN_3429_CREL_20230312094222.csv",
								  sep = ";", dec = ",", header = T, fill = TRUE,
								  colClasses = c("character","character","character",
								  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "wspolczynnik_feminizacji") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

wspolczynnik_feminizacji_P <- read.csv("./data/wspolczynnik_feminizacji/LUDN_3429_CREL_20230312094222.csv",
									 sep = ";", dec = ",", header = T, fill = TRUE,
									 colClasses = c("character","character","character",
									 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "wspolczynnik_feminizacji") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)


# wynagrodzenia
wynagrodzenia <- read.csv("./data/wynagrodzenia/WYNA_2497_CREL_20230312151155.csv",
									 sep = ";", dec = ",", header = T, fill = TRUE,
									 colClasses = c("character","character","character",
									 			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "wynagrodzenia") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)

wynagrodzenia_P <- read.csv("./data/wynagrodzenia/WYNA_2497_CREL_20230312151155.csv",
						  sep = ";", dec = ",", header = T, fill = TRUE,
						  colClasses = c("character","character","character",
						  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "wynagrodzenia") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)


# zlobki
zlobki <- read.csv("./data/zlobki/OCHR_3227_CREL_20230312181227.csv",
						  sep = ";", dec = ",", header = T, fill = TRUE,
						  colClasses = c("character","character","character",
						  			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "zlobki") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa, Rok), names_from = c(zmienna),
				values_from = Wartosc)
zlobki_P <- read.csv("./data/zlobki/OCHR_3227_CREL_20230312181227.csv",
				   sep = ";", dec = ",", header = T, fill = TRUE,
				   colClasses = c("character","character","character",
				   			   NA,NA,NA,NA,NA)) %>% 
	as_tibble %>% 
	select(Kod, Nazwa, Rok, Wartosc) %>% 
	mutate(zmienna = "zlobki") %>% 
	rename(Kod_GUS = Kod) %>% 
	left_join(GUS_NUTS, by = "Kod_GUS") %>% 
	rename(NUTS_ID = Kod_NUTS) %>% 
	relocate(NUTS_ID) %>% 
	pivot_wider(id_cols = c(NUTS_ID, Kod_GUS, Nazwa), names_from = c(zmienna, Rok),
				values_from = Wartosc)

# Shapefile'e dla Polski
POL <- get_eurostat_geospatial(output_class = "sf",
							   resolution = "01",
							   nuts_level = 3,
							   year = 2021) %>% 
	filter(CNTR_CODE == "PL")


#### Łączenie danych do pliku shapefile ####
POL <- left_join(POL,TFR_P, by = "NUTS_ID")

