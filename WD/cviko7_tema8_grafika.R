# .....................................................................................
# ............    Explorační analýza dat - Grafika s ggplot2      .....................
# ............       Adéla Vrtková, Martina Litschmannová    ..........................
# .....................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

# Základní skript obsahuje nezbytné minimum potřebné ke zvládnutí principů práce
# s jazykem R v kontextu Explorační analýzy dat.

# Lehkou nádstavbu k Základnímu skriptu tvoří skript "Grafika s ggplot2",
# kde jsou vytaženy ukázky grafiky s tímto balíčkem. Ač to tak na první pohled nepůsobí,
# ggplot2 ve výsledku dokáže ušetřit spousty práce a času při vytváření pěkných grafů
# pro domácí úkoly, závěrečné práce, diplomky apod.

# .....................................................................................
## Příprava prostředí ####

# Instalace knihoven (lze také provést přes okno Packages -> Install)
# Provádíme pouze jednou na daném počítači
install.packages("readxl")  # načtení xlsx souborů
install.packages("moments") # pro výpočet šikmosti a špičatosti
install.packages("dplyr")   # pro efektivní práci s datovým souborem
install.packages("tidyr")   # pro efektivní práci s datovým souborem (pivot_longer)
install.packages("ggplot2") # pro hezčí grafiku
install.packages("ggpubr")  # pro kombinování grafů z ggplot2
install.packages("rstatix") # pro identifikaci odlehlých pozorování
install.packages("forcats") # obsahuje funkci fct_infreq(), která seřadí úrovně faktorů podle četností

# Aktivace knihoven (nutno opakovat při každém novém spuštění Rka, vhodné mít na začátku skriptu)
library(readxl)
library(moments)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(rstatix)
library(forcats)  

# Výpis pracovního adresáře (také je vidět v záhlaví Console)
getwd()

# Nastavení pracovního adresáře dle potřeby (lze také přes Session -> Set Working Directory -> Choose Directory)
# POZOR! Cesta musí obsahovat lomítka "/", nikoliv obrácené lomítko (backslash). 
setwd("C:/Users/hab0065/Downloads/hab0065/WD")

# Připravujeme-li publikaci v češtině, lze spustit příkaz, který ve VÝSTUPECH změní des. tečku na des.čárku.
# Stále ale píšeme v příkazech desetinné tečky, příkaz změní jen VÝSTUP.
# Zároveň je vhodné nastavit požadovaný počet des. míst ve výstupech generovaných základním Rkem
# i ve výstupech z balíčku dplyr.

options(OutDec= ",",           # nastavení des. čárky v grafických výstupech
        digits = 7,            # nastavení počtu des. míst ve výstupech základního Rka
        pillar.sigfig = 10)    # nastavení počtu platných cifer ve výstupech balíčku dplyr

# .....................................................................................
# Import dat ####
dataS = read_excel("aku.xlsx", 
                   sheet = "Standard",
                   skip = 2)

colnames(dataS) = c("ID", "kap5", "kap100", "vyrobce")

# .....................................................................................
## Vysvětlení principu práce s knihovnou ggplot2 ####

# Základní fungování ggplot2 - pomocí vrstev:
# Nejprve definujeme "estetiku" (aesthetics)
#           - důležitá část, kde specifikujeme proměnnou na ose x a/nebo na ose y
#           - lze ale i určit proměnnou, která ovlivní velikost (size) nebo barvu (color) vykreslených objektů (např. bodů)
# Následuje určení "geometrie" (geometries)
#           - tato část definuje, jak se mají data znázornit
#           - jako body (geom_point), čáry (geom_line), krabicové grafy (geom_boxplot), sloupcové grafy (geom_bar),...
#           - je třeba uvážit typ dat a na základě toho, jakou chceme informaci předat, zvolit geometrii
#           - různé "geom" lze i kombinovat, má-li to smysl
# Samozřejmostí je změna popisků pomocí vrstvy "labs", grafická témata (themes), přidání textu (geom_text) apod.
# https://www.rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf

# Ukládání grafů lze např. pomocí funkce dev.print, jpeg, pdf a dalších.
# Jednodušeji pak v okně Plots -> Export

# Pro dobrou orientaci jsou kapitoly číslovány dle ZÁKLADNÍHO skriptu.

# .....................................................................................
#* 5.2 Sloupcový graf (ggplot2) ####

# Sloupcový graf pomocí ggplot2 - ze std. datového formátu
ggplot(dataS, 
       aes(x = vyrobce))+  # estetika
  geom_bar()

# V tuto chvíli je na zvážení seřazení výrobců dle četnosti (od nejčetnějšího).
dataS$vyrobce = as.factor(dataS$vyrobce)
dataS$vyrobce = fct_infreq(dataS$vyrobce)

# Další parametry je ale jednodušší nastavit, když je graf vytvořen z tabulky, která obsahuje 
# absolutní a relativní četnosti.

# Takovou tabulku lze připravit pomocí funkcí knihovny dplyr
tab_cetnosti_dplyr = 
  dataS %>%
  group_by(vyrobce) %>%
  dplyr::summarise(cetnost = n()) %>%                                  
  mutate(rel.cetnost = round(100*(cetnost / sum(cetnost)), 1))    # příprava tabulky
tab_cetnosti_dplyr[1,3] = 100 - sum((tab_cetnosti_dplyr[2:4, 3])) # ohlídání zaokrouhlovací chyby
tab_cetnosti_dplyr

# Vytvoření popisků (např. pro tvorbu grafů)
tab_cetnosti_dplyr =
  tab_cetnosti_dplyr %>% 
  mutate(popisky = paste0(cetnost," (",rel.cetnost," %)"))
tab_cetnosti_dplyr

sum(tab_cetnosti_dplyr[,3])

# Vytvoření grafu
ggplot(tab_cetnosti_dplyr, 
       aes(x = vyrobce,        # estetika
           y = cetnost))+      
  geom_bar(stat = "identity")    # parametr stat nastavený kvůli vstupní tabulce

# Můžeme si vyhrát s dalšími grafickými parametry a nastavením popisků 
ggplot(tab_cetnosti_dplyr, 
       aes(x = vyrobce, 
           y = cetnost))+  # estetika
  geom_bar(stat = "identity",         # parametr stat nastavený kvůli tabulce
           fill = "darkolivegreen3",  # barva sloupců
           width = 0.7)+              # šířka sloupců
  labs(x = "Výrobce",                 # názvy os a název grafu
       y = "Četnost",
       title = "Struktura souboru dle výrobce")+
  theme_classic()+                                   # vzhled grafu (další např. theme_classic, theme_dark, theme_light,...)
  theme(plot.title = element_text(hjust = 0.5),                  # zarovnání názvu grafu
        axis.text = element_text(color = "black", size = 13),    # barva a velikost popisků os
        axis.title = element_text(size = 13))+                   # velikost názvů os
  geom_text(aes(y = cetnost, 
                label = paste0(cetnost," (", rel.cetnost," %)")),     # jaký text a kam umístit
            size = 6,                                   # velikost textu
            position = position_stack(vjust = 0.5))+  # nastavení pozice na střed sloupce
  coord_flip()  # horizontální orientace

# Mimo klasického sloupcového grafu lze sestrojit i 100% skládaný sloupcový graf, což je perfektní alternativa ke koláčovým grafům.
ggplot(tab_cetnosti_dplyr, 
       aes(x = "",
           y = rel.cetnost,
           fill = vyrobce))+  # jiné nastavení estetiky
  geom_bar(stat="identity")+
  labs(x = "",
       y = "Kumulativní relativní četnost (%)",
       fill = "Výrobce",
       title = "Struktura souboru dle výrobce")+
  theme_light()+
  theme(plot.title = element_text(hjust = 0.5),                  # zarovnání názvu grafu
        axis.text = element_text(color = "black", size = 13),    # barva a velikost popisků os
        axis.title = element_text(size = 13))+
  geom_text(aes(y = rel.cetnost, 
                label = paste0(cetnost," (", rel.cetnost," %)")),
            size = 5,
            position = position_stack(vjust = 0.5))        # parametr kvůli správnému vykreslení popisků

# Použijeme-li následující funkci, varianty faktoru seřadíme dle četností sestupně.
dataS$vyrobce = fct_rev(fct_infreq(dataS$vyrobce))
# Vyzkoušejte nyní znovu spustit kód pro výpočet tab_cetnosti_dplyr a následně kód pro tvorbu 100% skládaného sloupcového grafu.

# Zkuste využít předešlého kódu a vytvořit si sloupcový graf pro proměnnou Výrobce podle sebe.

# .....................................................................................
#* 6.2 Krabicový graf (ggplot2) ####
ggplot(dataS,
       aes(x = "",
           y = kap5))+   # estetika
  geom_boxplot()   # specifikace způsobu vykreslení - boxplot

# Chceme-li vykreslit pouze pro jednoho výrobce a nechceme si definovat další nový objekt, lze to udělat následovně
ggplot(dataS[dataS$vyrobce=="A",],
       aes(x = "",
           y = kap5))+ # estetika
  geom_boxplot()

# ... a graficky si pohrát s výstupem
ggplot(dataS[dataS$vyrobce=="A",],
       aes(x = "",
           y = kap5))+ # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "",
       y = "(mAh)",
       title = "Kapacita po 5 cyklech (mAh), výrobce A")+  # popisky
  theme_bw()+
  geom_point(aes(x = "",
                 y = mean(dataS$kap5[dataS$vyrobce=="A"], na.rm=T)),  # vykreslení průměru
             color = "red",
             shape = 3)

# Vícenásobný krabicový graf v ggplot2
ggplot(dataS,
       aes(x = vyrobce,
           y = kap5))+ # estetika
  geom_boxplot()+
  labs(x = "",
       y = "(mAh)",
       title = "Kapacita po 5 cyklech (mAh) podle výrobce") # popisky

# ... s trochou grafiky a seřazením výrobců dle kap5
ggplot(dataS,
       aes(x = fct_reorder(vyrobce,kap5),   # seřazení faktoru dle numerické proměnné (vzestupně), f-ce je součástí balíčku forcats
           y = kap5))+ # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "",
       y = "Kapacita po 5 cyklech (mAh)") +
  theme_light()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) +
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

# .....................................................................................
#* 6.3 Histogram (ggplot2) ####
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(binwidth = 20)   # Lze nastavit parametr bins (počet všech sloupců) nebo binwidth (šířka jednoho sloupce)

#...s pokročilou úpravou výstupu
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(binwidth = 40,
                 color = "grey",
                 fill = "lightblue")+
  labs(x = "kapacita (mAh)",
       y = "četnost",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

#... se změnou měřítka osy y a přidáním empirické hustoty
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 binwidth = 40,
                 color = "grey",           # barva ohraničení
                 fill = "lightblue")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

# s přidáním hustoty pravděpodobnosti normálního rozdělení pomocí stat_function()
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 binwidth = 40,
                 color = "grey",           # barva ohraničení
                 fill = "lightblue")+     # barva výplně
  geom_density()+                        # křivka odhadu hustoty
  stat_function(fun = dnorm,
                args = list(mean = mean(dataS$kap5), sd = sd(dataS$kap5)),
                color = "red")+      # Gaussovka
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13))

# .....................................................................................
#* 6.4 QQ-graf (ggplot2) ####

ggplot(dataS, 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()

# Výstup ggplot lze upravit analogicky předchozím ggplot výstupům.

# .....................................................................................
#* 6.5 Kombinace grafických výstupů do jednoho obrázku (ggplot2) ####

# Kombinaci více objektů z ggplot vyžaduje jiný postup
# Jednotlivé objekty si uložit a na konci zavolat funkci ggarrange z balíčku ggpubr

hist_kap5 =
  ggplot(dataS,
         aes(x = kap5))+
  geom_histogram(binwidth = 40,
                 color = "grey",
                 fill = "lightgreen")+
  labs(x = " ",
       y = "četnost")+
  theme_test()

box_kap5 =
  ggplot(dataS,
         aes(x = "",
             y = kap5))+
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   
  geom_boxplot()+
  labs(x = "\n",
       y = "kapacita (mAh)")+
  theme_test()+
  coord_flip()

ggarrange(hist_kap5, box_kap5,  # vypsání objektů k vykreslení
          ncol = 1,   # počet sloupců, řádky jsou dopočítány automaticky
          heights = c(2.5,1))

# Návrh na úpravu - k dokonalosti chybí sjednotit měřítka na osách x, případně zvětšit popisky pro lepší čitelnost hodnot

# Pokud je však cílem SROVNÁNÍ, pak není vhodné výše uvedené generovat pro každého výrobce zvlášť.
# V takovém případě je dobré sáhnout po jednom vícenásobném krabicovém grafu 
# a k němu vytvořit sadu histogramů a sadu QQ-grafů.

# Dostat ggplot do for-cyklu už vyžaduje pokročilejší znalosti jazyka R, tudíž tato možnost nebude zde už uvedena.
# Nicméně skvělá funkce facet_wrap() velice příhodně pomůžou s vykreslováním sady
# histogramů, kdy automaticky pohlídají sjednocené osy...

# Jednotlivé objekty si uložíme (pojmenujeme) a na konci zavoláme funkci ggarrange z balíčku ggpubr

dataS$vyrobce = fct_reorder(dataS$vyrobce,dataS$kap5) # faktor vyrobce seřadíme dle proměnné kap5

box =
  ggplot(dataS,
         aes(x = vyrobce,
             y = kap5))+ # estetika
  stat_boxplot(geom = "errorbar", 
               width = 0.2) +   # ohraničení fousů (musí být před geom_boxplot)
  geom_boxplot()+
  labs(x = "",
       y = "kapacita \npo 5 cyklech (mAh)",
       title = "")+  # popisky
  theme_bw()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) +
  theme(axis.text = element_text(color = "black", size = 12))


hist =
  ggplot(dataS,
         aes(x = kap5))+
  geom_histogram(color = "black",           # barva ohraničení
                 fill = "lightgray") +      # barva výplně
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 12)) +
  facet_wrap("vyrobce",nrow = 4) # "facety" umožňují přípravu sad grafů stejného typu v závislosti na kategoriální proměnné


qq =
  ggplot(dataS, 
         aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()+
  labs(x = "teoretické normované kvantily",
       y = "výběrové kvantily")+
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 12)) +
  facet_wrap("vyrobce",nrow = 4,
             scales = "free")  # nepožadujeme sjednocení rozsahů os

pom = ggarrange(hist,qq,
                nrow = 1)
ggarrange(box,pom,
          nrow = 2,
          heights = c(2,4))

ggsave("aku_total.svg",
       height = 24, width = 17,units = "cm")


