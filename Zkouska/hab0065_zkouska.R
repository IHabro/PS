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

library(readxl)
library(moments)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(forcats)
library(BSDA)     # funkce SGN.test() pro test o mediánu
library(lawstat)
library(car)
library(FSA) # funkce dunnTest()
# library(multcompView)
library(rcompanion)
library(EnvStats)

install.packages("EnvStats")

# 2)
x = c(5,6,7,8,9)
p = c(0.2, 0.2, 0.4, 0.1, 0.1)
sum(x)
sum(p)

x*p
sum(x*p)

x*x*p
sum(x*x*p)

x = x*5
x

# 3)
x = seq(0,1250,0.01)
f = dweibull(x,shape = 1.2, scale = 300)
F = pweibull(x,shape = 1.2, scale = 300)
data = data.frame(x,f,F) # příprava data.frame

# Generování grafů
a = ggplot(data = data, aes(x = x, y = f)) +
  geom_line() +
  theme_bw() +
  labs(x="rychlost větru (m/s)",
       y = "hustota p-sti")
b = ggplot(data = data, aes(x = x, y = F)) +
  geom_line() +
  theme_bw() +
  labs(x="rychlost větru (m/s)",
       y = "distribuční funkce")

# Zkombinujeme grafy dle svých představ
ggarrange(a,b,
          nrow = 2)

# 3) b
pweibull(249, shape = 1.2, scale = 300)


# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Nastavení pracovního adresáře
setwd("C:/Users/hab0065/Downloads/hab0065/Zkouska")
getwd()

# Nacteni dat a uprava
data = read_excel("data_20231220.xlsx")
data$pokles = 100 - data$spotreba.2022 / (data$spotreba.2020 / 100)

boxplot(data$spotreba.2020)

#* Vlastnosti
summary = data %>%
  group_by(typ) %>%
  summarise(rozsah = length(na.omit(narust)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(narust)),
            minimum = min(narust, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(narust, 0.25, na.rm=T),
            prumer = mean(narust, na.rm=T),
            median = median(narust, na.rm=T),
            Q3 = quantile(narust, 0.75, na.rm=T),
            maximum = max(narust, na.rm=T),
            rozptyl = var(narust, na.rm=T),
            smerodatna_odchylka = sd(narust,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(narust, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(narust, na.rm=T)-3),
            Rfps = Q3 - Q1,
            dolni_hradba = Q1 - 1.5*Rfps,
            horni_hradba = Q3 + 1.5*Rfps)

# Boxplot
ggplot(data,
       aes(x = fct_reorder(typ, spotreba.2020),   # seřazení faktoru dle numerické proměnné (vzestupně), f-ce je součástí balíčku forcats
           y = spotreba.2020))+ # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "Typ vytapeni",
       y = "Spotreba 2020") +
  theme_bw()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

# Odlehla pozorovani
outliers = 
  data %>% 
  group_by(typ) %>% 
  identify_outliers(spotreba.2020)

data$spotreba.2020.clean = ifelse(data$id %in% outliers$id,NA,data$spotreba.2020) 

dataPlyn = data[data$typ %in% c("plynove"), ]
dataTuha = data[data$typ %in% c("tuha_paliva"), ]
dataCerp = data[data$typ %in% c("tepelne_cerpadlo"), ]
dataElek = data[data$typ %in% c("elektricke"), ]

shapiro.test(dataPlyn$spotreba.2020)
shapiro.test(dataTuha$spotreba.2020)
shapiro.test(dataCerp$spotreba.2020)
shapiro.test(dataElek$spotreba.2020)

data4A = rbind(dataElek, dataCerp)

t.test(dataCerp$spotreba.2020.clean,
       conf.level = 0.95,
       alternative = "greater")

t.test(dataElek$spotreba.2020.clean,
       conf.level = 0.95,
       alternative = "greater")

mean(na.omit(dataCerp$spotreba.2020.clean))

varTest(dataCerp$spotreba.2020, # nutno mít aktivován balíček EnVStats
        sigma.squared = 25^2,    # pozor - jde o test rozptylu
        conf.level = 0.95,
        alternative = "greater") #alternativa je ve tvaru "<"

# Přepočet int. odhadu sm. odchylky (odmocnění mezí int. odhadu rozptylu)
sqrt(varTest(dataCerp$spotreba.2020, 
             sigma.squared = 25^2, 
             conf.level = 0.95,
             alternative = "less")$conf.int)


















