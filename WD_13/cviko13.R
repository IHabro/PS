# ......................................................................................
# ................Cvičení 12 - Testování hypotéz (vícevýběrové testy)...................
# ................................... Doplňující příklady ..............................
# ........................... Martina Litschmannová, Adéla Vrtková .....................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

install.packages("dunn.test")
install.packages("FSA")

# Aktivace všech potřebných balíčků (v případě potřeby nutno nainstalovat)
library(readxl) # načtení xlsx souboru
library(dplyr) # pro efektivní preprocessing (práci s datovým rámcem)
library(ggplot2) # pro hezčí grafiku
library(tibble) # umožňuje nastavení počtu des. míst ve výstupech balíčku dplyr
library(moments) # funkce skewness(), kurtosis()
library(lawstat) # funkce symmetry.test() pro ověření symetrie
library(FSA) # funkce dunnTest()
library(rstatix) # funkce identify_outliers()
library(rcompanion) # písmenkové schéma
library(dunn.test)

# Nastavení výstupů
options(scipen = 100, # číselné hodnoty zobrazovat jako desetinná čísla
        OutDec = ",", # oddělovač desetinných míst v tabulkách a grafech
        signif = 5, # počet platných cifer ve výstupech klasického Rka
        pillar.sigfig=5) # počet platných cifer ve výstupech balíčku dplyr

# Příklad 1. ####
# Data potřebná pro vlastní analýzu (data_dvouvyberove.xlsx, list hraci) najdete 
# v LMS (11. týden). Data jsou inspirována herní platformou Steam, kterou 
# pravděpodobně znáte. V souboru najdete ID sledovaných hráčů, jejich celkové 
# doby hraní za rok 2018 a 2019 a používané verze operačních systémů (OS). 
# *a) ####
# Na hladině významnosti 0,01 rozhodněte, zda se střední celkové odehrané doby (v roce 2018) 
# hráčů (popř. mediány celkových dob odehraných hráči) statisticky významně liší v závislosti 
# na používané verzi operačního systému. Pokud ano, zjistěte, zda lze některé skupiny hráčů považovat 
# (z hlediska celkové odehrané doby v roce 2018) za homogenní. Výsledky doplňte bodovými 
# a intervalovými odhady příslušných středních hodnot, popř. mediánů. Nezapomeňte na ověření 
# předpokladů pro použití zvolených metod!

# Nastavte si pracovní adresář a do něj si nahrajte z LMS potřebná data
setwd("C:/Users/hab0065/Downloads/hab0065/WD_13")
getwd()

# Data načteme
data = read_excel("data_vicevyberove.xlsx",
                  sheet = "hraci")

#*Preprocessing ####
# Ověříme, zda v datech nejsou odlehlá pozorování
boxplot(data$odehrane_hod_2018~data$system)

# Data obsahují odlehlá pozorování, z dalšího zpracování je vyřadíme
outliers = data %>% 
  group_by(system) %>% 
  identify_outliers(odehrane_hod_2018)

data = data %>% 
  mutate(odehrane_hod_2018_out = ifelse(IDhrace %in% outliers$IDhrace,
                                        NA,
                                        odehrane_hod_2018))

boxplot(data$odehrane_hod_2018_out~data$system)

#*Ověření předpokladů ####
# Data jsou nezávislá (každá hodnota byla měřena na jíné statistické jednotce, tj. pro jiného hráče)

# Ověření normality
# Q-Q grafy
ggplot(data,
       aes(sample = odehrane_hod_2018_out)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  facet_wrap("system",nrow = 3)

# Histogramy
ggplot(data,aes(x=odehrane_hod_2018_out)) +
  geom_histogram(col = "black",
                 fill = gray.colors(10)[10]) +  # odstíny šedi jsou v paletě gray.colors() řazeny od nejtmavší k nejsvětlejší
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "počet odehraných hodin v roce 2018",
       y = "četnost") +
  facet_wrap(~system,nrow = 3)

# Základní výběrové charakteristiky, míry šikmosti a špičatosti, Shapirův-Wilkův test
stat = data %>% 
  group_by(system) %>% 
  summarise(n = length(na.omit(odehrane_hod_2018_out)),
            sd = sd(odehrane_hod_2018_out,na.rm = T),
            rozptyl = var(odehrane_hod_2018_out,na.rm = T),
            prumer = mean(odehrane_hod_2018_out,na.rm = T),
            medián = median(odehrane_hod_2018_out,na.rm = T),
            šikmost = moments::skewness(odehrane_hod_2018_out,na.rm = T),
            špičatost = moments::kurtosis(odehrane_hod_2018_out,na.rm = T)-3,
            SW.test = shapiro.test(odehrane_hod_2018_out)$p.value) 

t(stat)

# Na hladině významnosti 0,01 nezamítáme předpoklad normality (viz ...).

# Ověření homoskedasticity (shody rozptylů)
boxplot(data$odehrane_hod_2018_out~data$system) # výsledek odhadujeme dle "výšky" krabic

max(stat$rozptyl)/min(stat$rozptyl)  # poměr je větší než 2, tj.očekáváme, že rozptyly se významně liší

bartlett.test(data$odehrane_hod_2018_out~data$system) # nezamítli jsme předpoklad normality, tj. volíme Bartletův test
# Na hladině významnosti 0,01 zamítáme předpoklad o shodě rozptylů.

# Ověření shody tvaru rozdělení
# Pohledem na histogramy kontrolujeme, zda se jedná o výběry z symetrického / poz. zešikmeného /
# neg. zešikmeného rozdělení.
ggplot(data,aes(x=odehrane_hod_2018_out)) +
  geom_histogram() +
  facet_wrap(~system,nrow = 3)

# Na základě ověření předpokladů volíme pro analýzy Kruskalův-Wallisův test (srovnání mediánů).

# Kruskalův-Wallisův test
kruskal.test(data$odehrane_hod_2018_out~data$system)

# Na hladině významnosti 0,01 zamítáme předpoklad o shodě mediánů 
# (Kruskalův-Wallisův test, X2 = 157,5, df = 2, p-hodnota < 0,001),
# tj. používaný typ OS statisticky významně souvisí s celkovým počtem odehraných hodin v roce 2018.

# Post-hoc analýza pro Kruskalův-Wallisův test
DT = dunnTest(odehrane_hod_2018_out ~ system,       # knihovna FSA          
              data=data,
              method="bonferroni")
DT
PT = DT$res # úprava výstupu Dunnové testu do podoby vhodné jako vstup
# pro tvorbu písmenkového schématu

# písmenkové schéma
cldList(P.adj ~ Comparison, data = PT,
        threshold = 0.01)

# Statisticky významně nejvyšší medián celkového počtu odehraných hodin v roce 2018 vykazují hráči 
# na OS Linux a OS Windows. Hráči na OSX vykazují statisticky významně nižší medián celkového počtu 
# odehraných hodin v roce 2018. Statistická významnost je posuzována na hladině významnosti 0,01.
# Ověření předpokladů pro použití int. odhadů mediánů
data %>% 
  group_by(system) %>% 
  summarise(
    šikmost = moments::skewness(odehrane_hod_2018_out,na.rm = T),
    p.hodnota = symmetry.test(odehrane_hod_2018_out,boot = F)$p.value)

# Na hladině významnosti 0,01 nezamítáme předpoklad symetrie.

# Bodové a intervalové odhady mediánů celkových odehraných dob v roce 2018
t(stat)
wilcox.test(data$odehrane_hod_2018_out[data$system == "Linux"],
            alternative = "two.sided", # oboustranný odhad
            conf.int = T,
            conf.level = 0.99)

wilcox.test(data$odehrane_hod_2018_out[data$system == "WIN"],
            alternative = "two.sided", # oboustranný odhad
            conf.int = T,
            conf.level = 0.99)

wilcox.test(data$odehrane_hod_2018_out[data$system == "OSX"],
            alternative = "two.sided", # oboustranný odhad
            conf.int = T,
            conf.level = 0.99)

# Rozsahy výběrů >30 a <2 000, proto zaokrouhlujeme směr. odchylku na 3 platné cifry, tj. na desetiny.
# Všechny míry polohy zaokrouhlujeme stejně.

# *b) ####
# Na hladině významnosti 0,01 rozhodněte, zda se střední celkové odehrané doby (v roce 2019) 
# hráčů (popř. mediány celkových dob odehraných hráči) statisticky významně liší v závislosti 
# na používané verzi operačního systému. Pokud ano, zjistěte, zda lze některé skupiny hráčů považovat 
# (z hlediska celkové odehrané doby v roce 2019) za homogenní. Výsledky doplňte bodovými 
# a intervalovými odhady příslušných středních hodnot, popř. mediánů. Nezapomeňte na ověření 
# předpokladů pro použití zvolených metod!

#*Preprocessing ####

# Ověříme, zda v datech nejsou odlehlá pozorování
boxplot(data$odehrane_hod_2019~data$system)

# Data obsahují odlehlá pozorování, z dalšího zpracování je vyřadíme
outliers = data %>% 
  group_by(system) %>% 
  identify_outliers(odehrane_hod_2019)

data = data %>% 
  mutate(odehrane_hod_2019_out = ifelse(IDhrace %in% outliers$IDhrace,
                                        NA,
                                        odehrane_hod_2019))

boxplot(data$odehrane_hod_2019_out~data$system)

#*Ověření předpokladů ####
# Data jsou nezávislá (každá hodnota byla měřena na jíné statistické jednotce, tj. pro jiného hráče)

# Ověření normality
# Q-Q grafy
ggplot(data,
       aes(sample = odehrane_hod_2019_out)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  facet_wrap("system",nrow = 3)

# Histogramy
ggplot(data,aes(x=odehrane_hod_2019_out)) +
  geom_histogram(col = "black",
                 fill = gray.colors(10)[10]) +  # odstíny šedi jsou v paletě gray.colors() řazeny od nejtmavší k nejsvětlejší
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "počet odehraných hodin v roce 2019",
       y = "četnost") +
  facet_wrap(~system,nrow = 3)

# Základní výběrové charakteristiky, míry šikmosti a špičatosti, Shapirův-Wilkův test
stat = data %>% 
  group_by(system) %>% 
  summarise(n = length(na.omit(odehrane_hod_2019_out)),
            sd = sd(odehrane_hod_2019_out,na.rm = T),
            rozptyl = var(odehrane_hod_2019_out,na.rm = T),
            prumer = mean(odehrane_hod_2019_out,na.rm = T),
            medián = median(odehrane_hod_2019_out,na.rm = T),
            šikmost = moments::skewness(odehrane_hod_2019_out,na.rm = T),
            špičatost = moments::kurtosis(odehrane_hod_2019_out,na.rm = T)-3,
            SW.test = shapiro.test(odehrane_hod_2019_out)$p.value) 

t(stat)

# Na hladině významnosti 0,01 zamítáme předpoklad normality (viz ...).

# Ověření homoskedasticity (shody rozptylů)
boxplot(data$odehrane_hod_2019_out~data$system) # výsledek odhadujeme dle "výšky" krabic

max(stat$rozptyl)/min(stat$rozptyl)  # poměr je větší než 2, tj.očekáváme, že rozptyly se významně liší

levene.test(data$odehrane_hod_2019_out,data$system) # zamítli jsme předpoklad normality, tj. volíme Leveneho test
# Na hladině významnosti 0,01 nezamítáme předpoklad o shodě rozptylů.

# Ověření shody tvaru rozdělení
# Pohledem na histogramy kontrolujeme, zda se jedná o výběry z symetrického / poz. zešikmeného /
# neg. zešikmeného rozdělení.
ggplot(data,aes(x=odehrane_hod_2019_out)) +
  geom_histogram() +
  facet_wrap(~system,nrow = 3)

# Na základě ověření předpokladů volíme pro analýzy Kruskalův-Wallisův test (srovnání mediánů).

# Kruskalův-Wallisův test
kruskal.test(data$odehrane_hod_2019_out~data$system)

# Na hladině významnosti 0,01 nezamítáme předpoklad o shodě mediánů
# (Kruskalův-Wallisův test, X2 = 3,1, df = 2, p-hodnota = 0,209),
# tj. používaný typ OS statisticky významně nesouvisí s celkovým počtem odehraných hodin v roce 2019.

# Ověření předpokladů pro použití IO mediánů
data %>% 
  group_by(system) %>% 
  summarise(sym.test = symmetry.test(odehrane_hod_2019_out,boot = F)$p.value)

# Na hladině významnosti 0,01 nezamítáme předpoklad symetrie.

# Bodové a intervalové odhady mediánů celkových odehraných dob v roce 2019
t(stat)
wilcox.test(data$odehrane_hod_2019_out[data$system == "Linux"],
            alternative = "two.sided", # oboustranný odhad
            conf.int = T,
            conf.level = 0.99)

wilcox.test(data$odehrane_hod_2019_out[data$system == "WIN"],
            alternative = "two.sided", # oboustranný odhad
            conf.int = T,
            conf.level = 0.99)

wilcox.test(data$odehrane_hod_2019_out[data$system == "OSX"],
            alternative = "two.sided", # oboustranný odhad
            conf.int = T,
            conf.level = 0.99)

# Rozsahy výběrů >30 a <2 000, proto zaokrouhlujeme směr. odchylku na 3 platné cifry, tj. na desetiny.
# Všechny míry polohy zaokrouhlujeme stejně.

# *c) ####
# Na hladině významnosti 0,01 rozhodněte, zda se střední meziroční nárůsty odehrané doby (v období 201á-2019) 
# hráčů (popř. mediány nárůstů) statisticky významně liší v závislosti 
# na používané verzi operačního systému. Pokud ano, zjistěte, zda lze některé skupiny hráčů považovat 
# (z hlediska meziročního nárůstu odehrané doby) za homogenní. Výsledky doplňte bodovými 
# a intervalovými odhady příslušných středních hodnot, popř. mediánů. Nezapomeňte na ověření 
# předpokladů pro použití zvolených metod!

#*Preprocessing ####
# Výpočet nárůstů
data = data %>% 
  mutate(narust = odehrane_hod_2019-odehrane_hod_2018)

# Ověříme, zda v datech nejsou odlehlá pozorování
boxplot(data$narust~data$system)

# Data obsahují odlehlá pozorování, z dalšího zpracování je vyřadíme
outliers = data %>% 
  group_by(system) %>% 
  identify_outliers(narust)

data = data %>% 
  mutate(narust_out = ifelse(IDhrace %in% outliers$IDhrace,
                             NA,
                             narust))

boxplot(data$narust_out~data$system)

#*Ověření předpokladů ####
# Data jsou nezávislá (každá hodnota byla měřena na jíné statistické jednotce, tj. pro jiného hráče)

# Ověření normality
# Q-Q grafy
ggplot(data,
       aes(sample = narust_out)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() +
  facet_wrap("system",nrow = 3)

# Histogramy
ggplot(data,aes(x=narust_out)) +
  geom_histogram(col = "black",
                 fill = gray.colors(10)[10]) +  # odstíny šedi jsou v paletě gray.colors() řazeny od nejtmavší k nejsvětlejší
  theme_bw() +
  theme(legend.position = "none") +
  labs(x = "nárůst počtu odehraných hodin",
       y = "četnost") +
  facet_wrap(~system,nrow = 3)

# Základní výběrové charakteristiky, míry šikmosti a špičatosti, Shapirův-Wilkův test
stat = data %>% 
  group_by(system) %>% 
  summarise(n = length(na.omit(narust_out)),
            sd = sd(narust_out,na.rm = T),
            rozptyl = var(narust_out,na.rm = T),
            prumer = mean(narust_out,na.rm = T),
            medián = median(narust_out,na.rm = T),
            šikmost = moments::skewness(narust_out,na.rm = T),
            špičatost = moments::kurtosis(narust_out,na.rm = T)-3,
            SW.test = shapiro.test(narust_out)$p.value) 

t(stat)

# Na hladině významnosti 0,01 nezamítáme předpoklad normality (viz ...).

#*Ověření předpokladů ####
# Data jsou nezávislá (každá hodnota byla měřena na jíné statistické jednotce, tj. pro jiného hráče)

# Ověření normality
# Q-Q grafy
ggplot(data,
       aes(sample = narust_out)) +
  stat_qq() +
  stat_qq_line() +
  facet_wrap("system",nrow = 3)

# Histogramy
ggplot(data,aes(x=narust_out)) +
  geom_histogram() +
  facet_wrap(~system,nrow = 3)

# Míry šikmosti a špičatosti a Shapirův-Wilkův test
t(stat)

# Na hladině významnosti 0,01 nezamítáme předpoklad normality (viz ...).

# Ověření homoskedasticity (shody rozptylů)
boxplot(data$narust_out~data$system) # výsledek odhadujeme dle "výšky" krabic

max(stat$rozptyl)/min(stat$rozptyl)  # poměr je menší než 2, tj.očekáváme, že rozptyly se významně neliší

bartlett.test(data$narust_out~data$system) # nezamítli jsme předpoklad normality, tj. volíme Bartletův test
# Na hladině významnosti 0,01 nezamítáme předpoklad o shodě rozptylů (Bartletův test, K2 = 4,1, df = 2, p-hodnota = 0,132).

# Na základě ověření předpokladů volíme pro analýzy test ANOVA (srovnání středních hodnot).

# ANOVA test
vysledky=aov(narust_out~system, data = data) # POZOR! Nestačí použít příkaz aov(). Výstup příkazu musíme uložit do pomocné proměnné
# a na tu aplikovat příkaz summary().
summary(vysledky)  

# Na hladině významnosti 0,01 zamítáme hypotézu o shodě středních hodnot (ANOVA, p-hodnota<<0,001) 
# tj. používaný typ OS statisticky významně souvisí s meziročním nárůstem odehraných hodin v období 2018-2019.

# Post hoc analýza
TT = TukeyHSD(vysledky,"system",
              conf.level = 0.99, 
              ordered = TRUE)
TT
### Konverze TukeyHSD na standardní data.frame
TUK = as.data.frame(TT$system)
names(TUK) = gsub(" ", ".", names(TUK))

HSD = data.frame(Comparison=row.names(TUK), 
                 diff=TUK$diff, lwr=TUK$lwr, lwr=TUK$lwr, p.adj=TUK$p.adj)
# písmenkové schéma
cldList(p.adj ~ Comparison, data = HSD,
        threshold = 0.01)

# průměry
t(stat)

# Statisticky významně nejvyšší průměrný meziroční nárůst celkového počtu odehraných hodin v období 2018-2019
# vykazují hráči na OSX. Hráči na OS Linux a OS Windows vykazují statisticky významně nižší průměrný meziroční
# nárůst odehraných hodin (mezi těmito průměry není st. významný rozdíl). Statistická významnost je posuzována 
# na hladině významnosti 0,01.

# Ověření předpokladů pro použití IO stř. hodnot
# Předpoklad normality byl ověřen výše.

# Bodové a intervalové odhady stř. hodnot meziročních nárůstů odehraných dob v období 2018-2019
t(stat)
t.test(data$narust_out[data$system == "Linux"],
       alternative = "two.sided", # oboustranný odhad
       conf.level = 0.99)

t.test(data$narust_out[data$system == "WIN"],
       alternative = "two.sided", # oboustranný odhad
       conf.level = 0.99)

t.test(data$narust_out[data$system == "OSX"],
       alternative = "two.sided", # oboustranný odhad
       conf.level = 0.99)

# Rozsahy výběrů >30 a <2 000, proto zaokrouhlujeme směr. odchylku na 3 platné cifry, tj. na desetiny.
# Všechny míry polohy zaokrouhlujeme stejně.
