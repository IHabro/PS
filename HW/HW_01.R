# .....................................................................................
## 1. Priprava prostredi
install.packages("readxl")  # načtení xlsx souborů
install.packages("moments") # pro výpočet šikmosti a špičatosti
install.packages("dplyr")   # pro efektivní práci s datovým souborem
install.packages("tidyr")   # pro efektivní práci s datovým souborem (pivot_longer)
install.packages("ggplot2") # pro hezčí grafiku
install.packages("ggpubr")  # pro kombinování grafů z ggplot2
install.packages("rstatix") # pro identifikaci odlehlých pozorování
install.packages("forcats") # obsahuje funkci fct_infreq(), která seřadí úrovně faktorů podle četností
install.packages("BSDA")
install.packages("lawstat")
install.packages("FSA")
install.packages("multcompView")
install.packages("rcompanion")

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

options(OutDec= ",",           # nastavení des. čárky v grafických výstupech
        digits = 7,            # nastavení počtu des. míst ve výstupech základního Rka
        pillar.sigfig = 10)    # nastavení počtu platných cifer ve výstupech balíčku dplyr

getwd()
setwd("C:/Users/Sarum/source/repos/School/Ing/PS/HW_01")

# .....................................................................................
## 2. Nacteni dat
Nvidia_2080 = data = read_excel("PS_data.xlsx",
                                sheet = "Nvidia RTX 2080 Ti")

Nvidia_3070 = data = read_excel("PS_data.xlsx",
                                sheet = "Nvidia RTX 3070 Ti")

Amd_6800 =data = read_excel("PS_data.xlsx",
                            sheet = "AMD Radeon RX 6800 XT")

Amd_7700 = data = read_excel("PS_data.xlsx",
                             sheet = "AMD Radeon RX 7700 XT")

class(Nvidia_2080)
class(Nvidia_3070)
class(Amd_6800)
class(Amd_7700)

Nvidia_2080 = as.data.frame(Nvidia_2080)
Nvidia_3070 = as.data.frame(Nvidia_3070)
Amd_6800 = as.data.frame(Amd_6800)
Amd_7700 = as.data.frame(Amd_7700)

## 3. Uprava dat na standardni format
colnames(Nvidia_3070) = c("id", "release", "patched")
colnames(Amd_7700) = c("id", "release", "patched")

Nvidia_3070$narust = Nvidia_3070$patched - Nvidia_3070$release
Amd_7700$narust = Amd_7700$patched - Amd_7700$release

Nvidia_3070$typ = "Nvidia RTX 3070 Ti"
Amd_7700$typ = "AMD Radeon RX 7700 XT"

colnames(Nvidia_2080) = c("id", "release", "patched")
colnames(Amd_6800) = c("id", "release", "patched")

Nvidia_2080$narust = Nvidia_2080$patched - Nvidia_2080$release
Amd_6800$narust = Amd_6800$patched - Amd_6800$release

Nvidia_2080$typ = "Nvidia RTX 2080 Ti"
Amd_6800$typ = "AMD Radeon RX 6800 XT"


data = rbind(Nvidia_3070, Nvidia_2080, Amd_7700, Amd_6800)

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

#* Vnitrni hradby

data$order = 1:length(data$narust)

outliers = 
  data %>% 
  group_by(typ) %>% 
  identify_outliers(narust)

data$out = ifelse(data$order %in% outliers$order,NA,data$narust) 


#* Vlastnosti po odstraneni outliers
summary = data %>%
  group_by(typ) %>%
  summarise(rozsah = length(na.omit(out)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(out)),
            minimum = min(out, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(out, 0.25, na.rm=T),
            prumer = mean(out, na.rm=T),
            median = median(out, na.rm=T),
            Q3 = quantile(out, 0.75, na.rm=T),
            maximum = max(out, na.rm=T),
            rozptyl = var(out, na.rm=T),
            smerodatna_odchylka = sd(out,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(out, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(out, na.rm=T)-3),
            Rfps = Q3 - Q1,
            dolni_hradba = Q1 - 1.5*Rfps,
            horni_hradba = Q3 + 1.5*Rfps,
            horni_mez = prumer + 3*smerodatna_odchylka,
            dolni_mez = prumer - 3*smerodatna_odchylka)

## 4. Grafy

# Fill NA values with mean values
data$filled_out = data$out
data$filled_out[data$order == 33] = 5.1
data$filled_out[data$order == 120] = 4.4
data$filled_out[data$order == 148] = 5.5
data$filled_out[data$order == 252] = 5.9

class(data$out)
class(data$filled_out)

#* Boxplot
ggplot(data,
       aes(x = fct_reorder(typ,narust),   # seřazení faktoru dle numerické proměnné (vzestupně), f-ce je součástí balíčku forcats
           y = narust))+ # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "Grafická karta",
       y = "Nárůst (FPS)",
       title = "Srovnání nárůstů FPS po patchi 1.5") +
  theme_bw()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

ggplot(data,
       aes(x = fct_reorder(typ,filled_out),
           y = filled_out))+   # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "Grafická karta",
       y = "Nárůst (FPS)") +
  theme_bw()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

#* Histogram
ggplot(filter(data, typ == "Nvidia RTX 3070 Ti"),
aes(x = filled_out))+
  geom_histogram(color = "grey",
                 fill = "lightblue")+
  labs(x = "Nárust (FPS)",
       y = "Četnost",
       title = "Histogram pro nárust FPS")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(hjust = 0.5))

Nvidia_histogram = ggplot(filter(data, typ == "Nvidia RTX 3070 Ti"),
       aes(x = filled_out))+
  geom_histogram(bins = 15,
                 aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 color = "black",           # barva ohraničení
                 fill = "lightgray")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  stat_function(fun = dnorm,
                args = list(mean = mean(data$filled_out), sd = sd(data$filled_out)),
                color = "red")+      # Gaussovka
  labs(x = "Nárůst (FPS)",
       y = "Hustota pravděpodobnosti",
       title = "Histogram pro nárůst FPS pro grafickou kartu Nvidia RTX 3070 Ti")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

AMD_histogram = ggplot(filter(data, typ == "AMD Radeon RX 7700 XT"),
                       aes(x = filled_out))+
  geom_histogram(bins = 15,
                 aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 color = "black",           # barva ohraničení
                 fill = "lightgray")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  stat_function(fun = dnorm,
                args = list(mean = mean(data$filled_out), sd = sd(data$filled_out)),
                color = "red")+      # Gaussovka
  labs(x = "Nárůst (FPS)",
       y = "Hustota pravděpodobnosti",
       title = "Histogram pro nárůst FPS pro grafickou kartu 'AMD Radeon RX 7700 XT'")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13)) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

ggarrange(Nvidia_histogram, AMD_histogram,  # vypsání objektů k vykreslení
          ncol = 2,   # počet sloupců, řádky jsou dopočítány automaticky
          heights = c(2.5,1))


#* Experimentovani
data$typ = fct_reorder(data$typ,data$filled_out) # faktor vyrobce seřadíme dle proměnné kap5

box =
  ggplot(data,
         aes(x = fct_reorder(typ,filled_out),
             y = filled_out))+   # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "Grafická karta",
       y = "Nárůst (FPS)",
       title = "Nárůst FPS po patchi 1.5") +
  theme_bw()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed


hist =
  ggplot(data,
         aes(x = filled_out))+
  geom_histogram(bins = 15,
                 aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 color = "black",           # barva ohraničení
                 fill = "lightgray")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  stat_function(fun = dnorm,
                args = list(mean = mean(data$filled_out), sd = sd(data$filled_out)),
                color = "red")+      # Gaussovka
  labs(x = "Nárůst (FPS)",
       y = "Hustota pravděpodobnosti")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 12)) +
  facet_wrap("typ",nrow = 2) # "facety" umožňují přípravu sad grafů stejného typu v závislosti na kategoriální proměnné


qq =
  ggplot(data, 
         aes(sample = filled_out))+
  stat_qq()+
  stat_qq_line()+
  labs(x = "Teoretické normované kvantily",
       y = "Výběrové kvantily")+
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12)) +
  facet_wrap("typ",nrow = 2,
             scales = "free")  # nepožadujeme sjednocení rozsahů os

pom = ggarrange(hist, qq,
                ncol = 2)
ggarrange(pom,
          ncol = 2,
          heights = c(4,4))



### HW 2
shapiro.test(data$out)

NVIDIA_30 = data[data$typ %in% c("Nvidia RTX 3070 Ti"), ]
NVIDIA_20 = data[data$typ %in% c("Nvidia RTX 2080 Ti"), ]
AMD_77 = data[data$typ %in% c("AMD Radeon RX 7700 XT"), ]
AMD_68 = data[data$typ %in% c("AMD Radeon RX 6800 XT"), ]

shapiro.test(NVIDIA_20$out)
shapiro.test(NVIDIA_30$out)
shapiro.test(AMD_77$out)
shapiro.test(AMD_68$out)

symmetry.test(NVIDIA_20$out, boot = FALSE)
symmetry.test(NVIDIA_30$out, boot = FALSE)
symmetry.test(AMD_77$out, boot = FALSE)
symmetry.test(AMD_68$out, boot = FALSE)

wilcox.test(NVIDIA_20$out, conf.int = 0.95, alternative = "two.sided")
wilcox.test(NVIDIA_30$out, conf.int = 0.95, alternative = "two.sided")
wilcox.test(AMD_77$out, conf.int = 0.95, alternative = "two.sided")
wilcox.test(AMD_68$out, conf.int = 0.95, alternative = "two.sided")

POJMENOVANI = rbind(NVIDIA_30, NVIDIA_20, AMD_77, AMD_68)

# Mannův-Whitney test
wilcox.test(AMD$out, NVIDIA$out,
            alternative = "two.sided",
            conf.level = 0.95,
            conf.int = T)


# HW 3
# Leveneho a cochran test

# Test pomeru
s2=POJMENOVANI %>% 
  group_by(typ) %>% 
  summarise(var = var(filled_out,na.rm = T))
max(s2$var)/min(s2$var)

POJMENOVANI$typ = as.factor(POJMENOVANI$typ)
leveneTest(filled_out ~ typ, data=POJMENOVANI)

POJMENOVANI %>%
  group_by(typ) %>%
  wilcox.test(filled_out, conf.int = 0.95, alternative = "greater")


kruskal.test(filled_out ~ typ, data=POJMENOVANI)

DT = dunnTest(filled_out ~ typ,
              data=data,
              method="bonferroni")
DT
PT = DT$res # úprava výstupu Dunnové testu do podoby vhodné jako vstup
cldList(P.adj ~ Comparison, data = PT,
        threshold = 0.01)

