library(readxl) # načtení xlsx souboru
library(dplyr) # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2) # pro hezčí grafiku
library(tibble) # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(ggmosaic) # geometrie geom_mosaic() pro mozaikový graf pomocí ggplot2
library(tidyr) # funkce uncount() pro převod long formátu kont.tabulky do standardního datového formátu
library(lsr) # funkce cramersV()
library(nortest) # funkce pearson.test() 
library(epiR) # funkce epi.2by2()
library(svglite) # pro export obrázků ve formátu svg


# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Nastavení pracovního adresáře
setwd("C:/Users/hab0065/Downloads/hab0065/WD_15")
getwd()

data = read_excel("data_zkouska_vzor.xlsx")

data$rychlost = ifelse(data$pred < 18, "nizka", "optimalni")
data = data %>% mutate(rychlost = ifelse(pred < 18, "nizka", "optimalni")) 


data5 = data %>% filter(typ %in% c("ADSL", "SATELIT"))
data5

tab = table(data5$typ, data5$rychlost)
tab

# sance  = uspesne / neuspesne
# riziko = uspesne / celku

# Zmena poradi radku, pripadne sloupcu
data5$typ = factor(data5$typ, levels = c("SATELIT", "ADSL"))
data5$typ = factor(data5$typ, levels = c("ADSL", "SATELIT"))

# sumarni radky
addmargins(tab)

# b)
mosaicplot(tab)
cramersV(tab)

# c) Sance a Riziko je v sesite

# d) Pomer sanci je v sesite + moznost chi-kvadrat

# Oboustranny IO s vypisem bodoveho odhadu
prop.test(65, 130, alternative = "two.sided", conf.level = 0.95)
prop.test(45, 122, alternative = "two.sided", conf.level = 0.95)


epi.2by2(tab)

# chi test
pom = chisq.test(tab)
pom$expected
pom











