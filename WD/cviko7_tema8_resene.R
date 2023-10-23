# ......................................................................................
# ............................. Cvičení 7 - Explorační analýza .........................
# .......................... Martina Litschmannová, Adéla Vrtková ......................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

# .....................................................................................
## 1. Příprava prostředí (knihovny, pracovní adresář) ####

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
## 2. Import datového souboru ####

# Základní funkce - read.table, read.csv, read.csv2, ...
# Záleží hlavně na formátu souboru (.txt, .csv), na tzv. oddělovači jednotlivých hodnot, desetinné čárce/tečce...

# Načtení a uložení datového souboru ve formátu csv2 z pracovního adresáře
data = read.csv2(file = "aku_st.csv")

# Načtení a uložení datového souboru ve formátu csv2 z lokálního disku do datového rámce data
data = read.csv2(file = "C:/Users/Moje/aku_st.csv")

# Načtení a uložení datového souboru ve formátu csv2 z internetu do datového rámce data
data = read.csv2(file = "http://am-nas.vsb.cz/lit40/DATA/aku.csv")

# Načtení a uložení datového souboru ve formátu xlsx z pracovního adresáře do datového rámce data
# Používáme funkci z balíčku readxl, který jsme v úvodu rozbalili
# Samotný xlsx soubor nesmí být otevřený, jinak spuštění importu hodí error
data = read_excel("aku.xlsx", 
                  sheet = "Data",           # specifikace listu v xlsx souboru
                  skip = 3)                 # řádky, které je potřeba přeskočit

# Přejmenování sloupců - je-li nutné
colnames(data) = c("A_5","B_5","C_5","D_5","A_100","B_100","C_100","D_100") 

## Poznámka (kterou je dobré dočíst až do konce....)
# Vždy je možné importovat pomocí "Import Dataset" z okna Environment bez nutnosti psát kód
# V tom případě ale nesmí být v "cestě" k souboru žádné speciální znaky (háčky, čárky). Jinak se objeví error.
# Objekt importovaný touto cestou bude v novém RStudiu jako typ "tibble".
# Jedná se o modernější "data.frame" a v některých funkcích může dělat problémy a házet errory!
# Jednoduše lze tento objekt převést na typ data.frame pomocí as.data.frame().

# Objekt typu "tibble" vznikne i importem dat pomocí funkce read_excel()
class(data)

# Proto provedeme změnu datové struktury na datový rámec - data.frame
data = as.data.frame(data)
class(data)

# .....................................................................................
## 3. Preprocessing dat aneb cesta ke standardnímu datovému formátu ####

#* 3.1 Výběr proměnných ####

#.....................................................
# Zobrazení 3. sloupce - několik způsobů
data[ , 3]
# nebo (víme-li, jak se jmenuje proměnná zapsána ve 3. sloupci)
data[["C_5"]]
# nebo
data$C_5 # nejpoužívanější
# nebo pomocí funkce select balíčku dplyr, která vybere zvolené sloupce
select(data, C_5)
# nebo
data %>%                   # symbol %>% (pipe) lze vypsat pomocí klávesové zkratky CTRL+SHIFT+M
  select(C_5)

# %>% -> Ctrl+Shift+M

#.....................................................
# Uložení prvního a pátého sloupce dat. rámce data do dat. rámce pokus
pokus = data[ , c(1, 5)]
# nebo pomocí funkce z dplyr
pokus = data %>% 
  select(1, 5)
# nebo pomocí názvů
pokus = data %>% 
  select(A_5, A_100)

#.....................................................
# Vyloučení prvního a pátého sloupce z dat. rámce data a uložení do dat. rámce pokus
pokus = data[ ,-c(1, 5)]
# nebo pomocí dplyr
pokus = data %>% 
  select(-1,-5)
# nebo pomocí názvů
pokus = data %>% 
  select(-A_5,-A_100)

pokus %>% select(A_5:D_5)

# .....................................................................................
#* 3.2 Uložení menších logických celků ####

# Často se hodí provést vytvoření několika menších logických celků (např. uložit si zvlášť měření po 5 cyklech).
# Tyto menší logické celky pak můžou pomoct právě při vytváření std. datového formátu.
# Pozn. při ukládání dat mysleme na přehlednost v názvech!

data5 = data[,1:4] # z dat vybereme ty sloupce, které odpovídají měřením po 5 cyklech
colnames(data5) = c("A","B","C","D") # přejmenujeme sloupce

# Totéž provedeme pro měření provedené po 100 cyklech
data100 = data[,5:8] # z dat vybereme ty sloupce, které odpovídají měřením po 100 cyklech
colnames(data100) = c("A","B","C","D") # přejmenujeme sloupce

# Výše vytvořené soubory neodpovídají std. datovému formátu.
# O převedení se jednoduše postará funkce pivot_longer(), která je součástí balíčku tidyverse.

data5S = data5 %>% 
  pivot_longer(
    cols = A:D,
    names_to = "vyrobce",
    values_to = "kap5")

data100S = data100 %>% 
  pivot_longer(
    cols = A:D,
    names_to = "vyrobce",
    values_to = "kap100")


# .....................................................................................
#* 3.3 Sestavení std. datového formátu ####

# Sloučením dostaneme std. datový formát se všemi údaji (jupí!)
dataS = cbind(data5S, data100S)

# Provedeme ještě kosmetické úpravy (odstranění nadbytečného sloupce a vyřešení NA)
dataS = dataS[,-3] # vynecháme nadbytečný třetí sloupec
dataS = na.omit(dataS) # vynecháme řádky s NA hodnotami
# !!! S funkci na.omit zacházejte extrémně opatrně, abyste nechtěně nepřišli o data !!!

# Možná se ptáte, proč nepoužít funkci pivot_longer() na původní importovaná data (data).
# Funkce pivot_longer() funguje tak, že všechny sloupce seřadí do jednoho sloupce
# a vytvoří k nim nový sloupec jako identifikátor toho, odkud původně (z jakého sloupce)
# pocházely. Chcete-li standardní datový soubor vytvořit přímo z původního datového souboru (data),
# musíte se seznámít s pokročilejšími technikami "pivotování" tabulek (viz např. 
# https://dcl-wrangle.stanford.edu/pivot-advanced.html):

dataS = data %>% pivot_longer(
  cols = A_5:D_100,
  names_to = c("vyrobce",".value"),
  names_sep = "_"
)

colnames(dataS) = c("vyrobce","kap5","kap100")  
dataS = na.omit(dataS) # odstranění řádků s NA z datového rámce dataS
# !!! s funkcí na.omit zacházejte extrémně opatrně, aby jste nechtěně nepřišli o data !!!

# Na rovinu je potřeba přiznat, že tento preprocessing bychom zvládli i ručně v Excelu.
# To znamená není vůbec špatnou cestou si potřebné úpravy do std. datového formátu provést přímo v něm.

# .....................................................................................
#* 3.4 Definování nové proměnné ve std. datovém formátu ####

# Definování nové proměnné pokles
dataS$pokles = dataS$kap5 - dataS$kap100
# nebo pomocí funkce z balíčku dplyr
dataS = dataS %>% 
  mutate(pokles=kap5-kap100)

# .....................................................................................
#* 3.5 Výběr ze std. datového formátu na základě dané podmínky ####

# Prozkoumejte následující příkazy a pochopte jejich strukturu
dataS[dataS$vyrobce == "A", ]

dataS %>% 
  filter(vyrobce == "A")

# Prozkoumejte následující příkazy a pochopte jejich strukturu
dataS$kap5[dataS$vyrobce == "A"]

dataS %>% 
  filter(vyrobce == "A") %>% 
  select(kap5)

# Prozkoumejte následující příkazy a pochopte jejich strukturu
dataS[dataS$vyrobce %in% c("A", "B"), c("kap100", "vyrobce")]

dataS %>% 
  filter(vyrobce %in% c("A", "B")) %>% 
  select(kap100, vyrobce)

# Výše uvedené je vhodné, pokud si potřebujeme vytvořit zcela samostatné proměnné
# Např. proměnnou a5, která obsahuje kapacity po 5 cyklech akumulátorů od výrobce A
a5 = dataS$kap5[dataS$vyrobce=="A"]

# Analogicky lze vytvořit další různé potřebné "podvýběry" a uložit je.

# .....................................................................................
#* 3.6 Podrobnější okénko do funkcí knihovny dplyr ####
# Je nutné aplikovat na data ve st. datovém formátu !!!
# Operátor pipe %>% - pomáhá při řetězení funkcí - v novém RStudiu klávesová zkratka Ctrl+Shift+M

#.....................................................
# filter - vybere řádky na základě daných podmínek
# Výběr výrobků od výrobce A
dataS %>% 
  filter(vyrobce=="A")

# Výběr výrobků od výrobce A nebo B
dataS %>% 
  filter(vyrobce=="A" | vyrobce=="B")  # | oddělující podmínky odpovídá logickému "nebo"

# Výběr všech výrobků s poklesem o 200 mAh a větším od výrobce C
dataS %>% 
  filter(pokles>=200, vyrobce=="C")  # čárka oddělující podmínky odpovídá logickému "a zároveň"

#.....................................................
# select - vybere sloupce podle jejich názvu nebo čísla
# Výběr sloupce s údaji o výrobci podle názvu sloupce
dataS %>% 
  select(vyrobce)

# Výběr sloupce s údaji o výrobci podle čísla sloupce
dataS %>% 
  select(3)
# Co je bezpečnější/lepší?

#.....................................................
# mutate - přidá novou proměnnou nebo transformuje existující
# Vytvoření nového sloupce pokles_Ah, který údává pokles kapacit v Ah (původní data v mAh, 1 Ah = 1000 mAh)
dataS %>% 
  mutate(pokles_Ah=pokles/1000)

#.....................................................
# summarise - generuje sumární charakteristiky různých proměnných
# Výpočet průměru a mediánu všech hodnot proměnné kap5
dataS %>% 
  summarise(prum=mean(kap5),
            median=median(kap5))

#.....................................................
# arrange - seřadí řádky podle zvolené proměnné
# Vzestupné a sestupné seřazení řádků podle hodnoty poklesu
dataS %>%
  arrange(pokles)

dataS %>%
  arrange(desc(pokles))

#.....................................................
# group_by - seskupí hodnoty do skupin podle zvolené proměnné - samotné v podstatě "k ničemu"
dataS %>%
  group_by(vyrobce)
# Ideální pro spočítání sumárních charakteristik pro každého výrobce zvlášť, např. průměru
dataS %>%
  group_by(vyrobce) %>% 
  summarise(prum=mean(kap5))

#.....................................................
# Závěrečná poznámka k dplyr (kterou je dobré dočíst až do konce...)
# Některé operace mohou vyhodit objekt typu "tibble".
# Jedná se o modernější data.frame, nicméně v některých funkcích může dělat problémy a házet errory!
# Jednoduše lze tento "tibble" objekt převést na typ data.frame pomocí as.data.frame().


# .....................................................................................
# 4. Grafika v R - obecně ####

#* 4.1 Základní R ####

# Základem jsou tzv. high-level funkce, které vytvoří graf (tj. otevřou grafické oknou a vykreslí 
# dle zadaných parametrů).
# Na ně navazují tzv. low-level funkce, které něco do aktivního grafického okna přidají, 
# samy o sobě neotevřou nové, př. low-level funkcí - např. abline, points, lines, legend, 
# title, axis ... které přidají přímku, body, legendu...
# Tzn. před použitím "low-level" funkce je potřeba, volat "high-level" funkci (např. plot, boxplot, hist, barplot,...)

# Další grafické parametry naleznete v nápovědě
# nebo např. zde http://www.statmethods.net/advgraphs/parameters.html
# nebo zde https://flowingdata.com/2015/03/17/r-cheat-sheet-for-graphical-parameters/
# nebo http://bcb.dfci.harvard.edu/~aedin/courses/BiocDec2011/2.Plotting.pdf

## Barvy v R
# http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf
# https://www.nceas.ucsb.edu/~frazier/RSpatialGuides/colorPaletteCheatsheet.pdf

#* 4.2 Knihovna ggplot2 ####

## Ukážeme si i modernější nástroje pro mnohem hezčí výstupy - balíček ggplot2
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

# .....................................................................................
## 5. Explorační analýza a vizualizace kategoriální proměnné ####

class(dataS$vyrobce) # kontrola datového typu

# Pro práci s kategoriální proměnnou je nutné uložení proměnné jako typ factor
dataS$vyrobce = as.factor(dataS$vyrobce)

# Prohlédněme si varianty/kategorie proměnné
levels(dataS$vyrobce) # v uvedeném pořadí a podobě se budou také automaticky řadit v tabulkách i grafech

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést přejmenování/překódování kategorií, lze využít příkazu
# dataS$vyrobce = factor(dataS$vyrobce,
#                        levels = c("A", "B", "C", "D"),  # současné varianty
#                        labels = c("Acko", "Becko", "Cecko", "Decko")) # nové varianty

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést změnu pořadí kategorií bez překódování, lze využít příkazu
# dataS$vyrobce = factor(dataS$vyrobce,
#                        levels = c("D", "C", "B", "A")) # současné varianty v novém pořadí

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést změnu pořadí kategorií dle četností (sestupně) bez překódování, lze využít příkazu
# data$vyrobce = fct_infreq(data$vyrobce)  # funkce je součástí balíčku forcats

# NESPOUŠTĚJTE! Pokud by bylo potřeba provést změnu pořadí kategorií i překódování, lze využít příkazu
# dataS$vyrobce = factor(dataS$vyrobce,
#                        levels = c("D", "C", "B", "A"), # současné varianty v novém pořadí
#                        labels = c("Decko", "Cecko", "Becko", "Acko")) # nové varianty v novém pořadí

# .....................................................................................
#* 5.1 Tabulka četností (základní R) ####

# Tabulka absolutních četností kategoriální proměnné výrobce
cetnosti = table(dataS$vyrobce)
cetnosti

# V případě nominální kategoriální proměnné je vhodné varianty proměnné seřadit dle jejich četností.
# Buď lze využít předchozího kódu a ručně si nastavit pořadí kategorií, nebo lze toto seřazení "zautomatizovat".

dataS$vyrobce = fct_infreq(dataS$vyrobce) # funkce seřadí varianty proměnné dle četností (sestupně)

cetnosti = table(dataS$vyrobce)
cetnosti # Nyní jsou výstupem četnosti variant analyzované proměnné v požadovaném pořadí.

# Tabulka relativních četností a její přepočet na procenta
rel.cetnosti = prop.table(cetnosti)*100
rel.cetnosti # výpis

# U relativních četností v procentech je potřeba pohlídat zaokrouhlení a s ním spojené riziko zaokrouhlovací chyby.
rel.cetnosti = round(rel.cetnosti, digits = 1) # zaokrouhlení na 1 desetinné místo
rel.cetnosti
# Vysvětlete, proč bylo zvoleno toto zaokrouhlení!

rel.cetnosti[4] = 100 - sum(rel.cetnosti[1:3]) # ohlídání zaokrouhlovací chyby
rel.cetnosti

#* 5.2 Tabulka četností (dplyr) ####
dataS %>% 
  group_by(vyrobce) %>% 
  summarise(abs_cetnost = n())

# Jednoduchým doplněním přidáme sloupec s relativními četnostmi.Tabulku uložíme pod názvem tabulka_dplyr
tabulka_dplyr = 
  dataS %>% 
  group_by(vyrobce) %>% 
  summarise(abs_cetnost = n()) %>% 
  mutate(rel_cetnost_proc = round(100*(abs_cetnost/sum(abs_cetnost)),1))

tabulka_dplyr

# Ošetření případné zaokrouhlovací chyby
tabulka_dplyr[1,3] = 100 - sum(tabulka_dplyr[2:4,3])
tabulka_dplyr

# Vytvoření popisků (např. pro tvorbu grafů)
tabulka_dplyr =
  tabulka_dplyr %>% 
  mutate(popisky = paste0(abs_cetnost," (",rel_cetnost_proc," %)"))
tabulka_dplyr

# Uložení tabulky do csv souboru pro export do MS Excel
write.csv2(tabulka_dplyr, file="tabulka_dplyr.csv")

# .....................................................................................
#* 5.3 Sloupcový graf ####

# Základní (tzn. nevyžadující žádný balíček) sloupcový graf vychází z tabulky četností, kterou máme nachystanou

cetnosti = table(dataS$vyrobce)
cetnosti
barplot(cetnosti) # high-level funkce pro vykreslení sloupcového grafu

# Základní sloupcový graf pomocí knihovny ggplot2 vytvoříme následujícím příkazem:
tabulka_dplyr

ggplot(tabulka_dplyr,
       aes(x = vyrobce, 
           y = abs_cetnost))+  # estetika
  geom_bar(stat = "identity")  # geom_? nastavuje typ grafu


# Pro další úpravy grafu je vhodné základní grafický výstup pojmenovat a poté dále doplňovat.
graf_vyrobce = 
  ggplot(tabulka_dplyr,
         aes(x = vyrobce, 
             y = abs_cetnost))+  # estetika
  geom_bar(stat = "identity", 
           fill = "grey55") # parametr fill umožňuje nastavit jinou barvu sloupců

# Parametrem fill lze nastavit i různé barvy pro každý sloupec.
# Při jednoduché analýze kvalitativní proměnné ale nebývá důvod pro barevné rozlišení sloupců.
# Méně je někdy více... (posuďte sami)

ggplot(tabulka_dplyr,
       aes(x = vyrobce, 
           y = abs_cetnost))+  # estetika
  geom_bar(stat = "identity", 
           fill = c("forestgreen", "indianred", "dodgerblue", "orange"))

# Pokračujme v úpravách připraveného grafu
graf_vyrobce

graf_vyrobce +            
  theme_bw()   # vzhled grafu (další "témata" viz https://ggplot2.tidyverse.org/reference/ggtheme.html)    

# Sloupcový graf pro nominální proměnnou vhodný k publikaci 
ggplot(tabulka_dplyr,
       aes(x = vyrobce, 
           y = abs_cetnost)) +        # estetika
  geom_bar(stat = "identity",         # geom_? nastavuje typ grafu
           fill = "grey55") +         # parametr fill umožňuje nastavit jinou barvu sloupců
  theme_bw()+                         # vzhled grafu (další "témata" viz https://ggplot2.tidyverse.org/reference/ggtheme.html)
  ylim(0,90)+                                               # nastavení rozsahu osy y
  labs(x = "výrobce",                                       # popis osy x
       y = "počet akumulátorů",                             # popis osy y
       title = "Struktura akumulátorů dle výrobce")+        # název grafu
  theme(plot.title = element_text(hjust = 0.5),             # zarovnání názvu grafu na střed
        axis.text = element_text(color="black", size=12),   # černá barva pro hodnoty na osách a zvětšení velikosti písma
        axis.title = element_text(size=14))+                # zvětšení velikosti písma pro názvy os
  geom_text(aes(y = abs_cetnost, 
                label = popisky),     # jaký text a kam umístit (umisťujeme popisky na sloupce)
            size = 4,                 # velikost textu
            vjust = -0.5)             # nastavení popisků nad sloupce

# Funkce ggsave() uloží aktuální obrázek vytvořený pomocí ggplot2 do pracovního adresáře
ggsave("graf_vyrobce.png", width = 13, height = 10, units = "cm")            

# Doporučení - v grafech by se neměla zbytečně duplikovat informace, máme-li jednoduchý sloupcový graf
# s řádně popsanými sloupci, není důvod vkládat např. legendu.
# Pokud graf vložíme do dokumentu, kde jej opatříme titulkem (např. Obr. 1: .....), pak není důvod mít 
# v samotném grafu velký název, jelikož bude ta stejná informace v titulku.

# .....................................................................................
## 6. Explorační analýza a vizualizace kvantitativní proměnné ####

# .....................................................................................
#* 6.1 Výpočet číselných charakteristik ####
summary(dataS$kap5)

# Výpočet průměru jedné proměnné
mean(dataS$kap5)

# Pozor na chybějící hodnoty
mean(data$C_5)
mean(data$C_5,na.rm=TRUE)

# Výpočet mediánu jedné proměnné
quantile(dataS$kap5, probs=0.5)

# Určení rozsahu výběru (tj. počet statistických jednotek - počet akumulátorů)
length(dataS$kap5) # POZOR! Funkce počítá i NA.

# Další charakteristiky -> var(), sd(), min(), max(),...(viz R-tahák)

# Pozor! Funkce pro výpočet šikmosti (skewness) a špičatosti (kurtosis) nejsou součástí základního R, najdete je v balíčku moments
# Normálnímu rozdělení odpovídá špičatost 3, resp. špičatost v intervalu (1,5)
# Pro standardizaci špičatosti je nutno od vypočtené hodnoty odečíst 3.
# Napíšete-li před název funkce název balíčku a "::", zajistíte tím, že bude použita funkce z daného balíčku
# Nutno ohlídat, když jsou v různých balíčcích definovány různé funkce pod stejným jménem
moments::skewness(dataS$kap5)
moments::kurtosis(dataS$kap5)-3

# Chceme-li spočítat danou charakteristiku pro proměnnou kapacita po 5 cyklech podle výrobců, můžeme použít funkci tapply
tapply(dataS$kap5, dataS$vyrobce, mean, na.rm = T)

# Efektní výpočet číselných charakteristik (ať už s ohledem, či bez ohledu na výrobce) je v základním R
# poměrně "krkolomné" vytvořit. Z toho důvodu si zde vypůjčime funkce z balíčku dplyr.

# Výpočet číselných charakteristik proměnné kap5 bez ohledu na výrobce
dataS %>% 
  summarise(rozsah = length(na.omit(kap5)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(kap5)),
            minimum = min(kap5, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(kap5, 0.25, na.rm=T),
            prumer = mean(kap5, na.rm=T),
            median = median(kap5, na.rm=T),
            Q3 = quantile(kap5, 0.75, na.rm=T),
            maximum = max(kap5, na.rm=T),
            rozptyl = var(kap5, na.rm=T),
            smerodatna_odchylka = sd(kap5,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(kap5, na.rm=T)),              # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(kap5, na.rm=T)-3))

# Nezapoměňte na správné zaokrouhlení (viz Manuál) !!!
# Použijeme funkci group_by a dostaneme charakteristiky pro kapacitu po 5 cyklech podle výrobců
# Vzhledem k neúplnému výpisu je vhodné si výstup uložit a prohlédnout si jej v novém okně,
# popřípadě v transformované podobě
charakteristiky_dle_vyrobce = 
  dataS %>%
  group_by(vyrobce) %>% 
  summarise(rozsah = length(na.omit(kap5)),   # funkce length() nemá atribut na.rm, toto lze řešit např. použitím f-ce na.omit()
            pocet_NA = sum(is.na(kap5)),
            minimum = min(kap5, na.rm=T),     # preventivní na.rm=T
            Q1 = quantile(kap5, 0.25, na.rm=T),
            prumer = mean(kap5, na.rm=T),
            median = median(kap5, na.rm=T),
            Q3 = quantile(kap5, 0.75, na.rm=T),
            maximum = max(kap5, na.rm=T),
            rozptyl = var(kap5, na.rm=T),
            smerodatna_odchylka = sd(kap5,na.rm=T),
            variacni_koeficient = (100*(smerodatna_odchylka/prumer)),  # variační koeficient v procentech
            sikmost = (moments::skewness(kap5, na.rm=T)),       # preventivní specifikace balíčku moments
            spicatost = (moments::kurtosis(kap5, na.rm=T)-3))

t(charakteristiky_dle_vyrobce)

# .....................................................................................
#* 6.2 Krabicový graf ####
boxplot(dataS$kap5)

# A ještě vykreslení vícenásobného krabicového grafu (který by šel dále graficky upravit)
boxplot(dataS$kap5~dataS$vyrobce)

# Pro vykreslení vícenásobného krabicového grafu je na místě zvážit seřazení výrobců dle kapacity po 5 cyklech.

# Krabicový graf (dplyr)
ggplot(dataS,
       aes(x = fct_reorder(vyrobce,kap5),   # seřazení faktoru dle numerické proměnné (vzestupně), f-ce je součástí balíčku forcats
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
       y = "Kapacita po 5 cyklech (mAh)",
       title = "Výrobce A")+  # popisky
  theme_light()+
  stat_summary(geom = "point", fun = "mean", colour = "red", size = 2, shape = 3) + # vykreslení průměru
  theme(plot.title = element_text(hjust = 0.5))      # zarovnání názvu grafu na střed

# Vícenásobný krabicový graf v ggplot2 s trochou grafiky
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
#* 6.3 Histogram ####

# Jednoduché a rychlé vykreslení
hist(dataS$kap5)
hist(dataS$kap5, breaks=10) # Co dělají různé hodnoty parametru breaks s grafem?

# Takto bychom mohli vykreslit histogramy pro ostatní výrobce.
# POZOR! Mají-li být histogramy mezi sebou srovnatelné, musí mít sjednocené osy!!!

# Jednoduché pohlídání sjednocených os umí knihovna ggplot2.
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
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5)) # zarovnání názvu grafu na střed

#... se změnou měřítka osy y (a přidáním empirické hustoty pravděpodobnosti)
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(aes(y = ..density..),          # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 binwidth = 40,
                 color = "grey",           # barva ohraničení
                 fill = "lightblue")+      # barva výplně
  geom_density()+                          # empirická hustota pravděpodobnosti (tj. odhad na základě dat)
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5)) # zarovnání názvu grafu na střed

# s přidáním hustoty pravděpodobnosti normálního rozdělení pomocí stat_function()
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(aes(y = ..density..),     # říkáme, že chceme změnu měřítka osy y kvůli hustotě
                 binwidth = 40,
                 color = "grey",           # barva ohraničení
                 fill = "lightblue")+      # barva výplně
  geom_density()+                          # empirická hustota pravděpodobnosti (tj. odhad na základě dat)
  stat_function(fun = dnorm,
                args = list(mean = mean(dataS$kap5), sd = sd(dataS$kap5)),
                color = "red")+      # Gaussova křivka
  labs(x = "kapacita (mAh)",
       y = "hustota pravděpodobnosti",
       title = "Histogram pro kapacitu po 5 cyklech")+
  theme_test()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5)) # zarovnání názvu grafu na střed

# sada histogramů pro jednotlivé výrobce
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(binwidth = 20,
                 color = "black",          # barva ohraničení
                 fill = "lightblue")+      # barva výplně
  geom_density() +
  labs(x = "kapacita (mAh)",
       y = "počet akumulátorů",
       title = "Histogram pro kapacitu akumulátorů po 5 cyklech dle výrobce")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5)) + # zarovnání názvu grafu na střed
  facet_wrap(~vyrobce,nrow = 4) # "facety" umožňují přípravu sad grafů stejného typu v závislosti na kategoriální proměnné

# sada histogramů pro jednotlivé výrobce seřazené dle kapacity po 5 cyklech
ggplot(dataS,
       aes(x = kap5))+
  geom_histogram(binwidth = 20,
                 color = "black",          # barva ohraničení
                 fill = "lightblue")+      # barva výplně
  geom_density() +
  labs(x = "kapacita (mAh)",
       y = "počet akumulátorů",
       title = "Histogram pro kapacitu akumulátorů po 5 cyklech dle výrobce")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 13),
        axis.title = element_text(size = 13),
        plot.title = element_text(hjust = 0.5)) + # zarovnání názvu grafu na střed
  facet_wrap(~fct_reorder(vyrobce,kap5),nrow = 4) # "facety" umožňují přípravu sad grafů stejného typu v závislosti na kategoriální proměnné
# funkce fct_reorder seřadí varianty faktoru dle kvantitativní proměnné

# .....................................................................................
#* 6.4 QQ-graf ####
#* 
# Jednoduché a velmi rychlé vykreslení, např. pro kapacitu po 5 cyklech pro výrobce A
qqnorm(dataS$kap5[dataS$vyrobce == "A"])
qqline(dataS$kap5[dataS$vyrobce == "A"])

# S využitím balíčku ggplot2
ggplot(dataS[dataS$vyrobce == "A",], 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()

# Výstup ggplot lze upravit analogicky předchozím ggplot výstupům.

# Pro vykreslení sady QQ-grafů lze opět využít facety s tím, že zde již nepotřebujeme dodržet sjednocené osy
ggplot(dataS, 
       aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()+
  theme_bw() +
  facet_wrap("vyrobce", 
             nrow = 4,
             scales = "free") # nepožadujeme sjednocení rozsahů os

# .....................................................................................
#* 6.5 Kombinace grafických výstupů do jednoho obrázku (ggplot2) ####

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
  geom_histogram(binwidth = 20,
                 color = "black",          # barva ohraničení
                 fill = "lightgrey")+      # barva výplně
  geom_density() +
  labs(x = "kapacita (mAh)",
       y = "počet akumulátorů",
       title = "")+
  theme_bw()+
  theme(axis.text = element_text(color = "black", size = 12)) +
  facet_wrap(~vyrobce,nrow = 4) # "facety" umožňují přípravu sad grafů stejného typu v závislosti na kategoriální proměnné


qq =
  ggplot(dataS, 
         aes(sample = kap5))+
  stat_qq()+
  stat_qq_line()+
  labs(x = "teoretické normované kvantily",
       y = "výběrové kvantily",
       title = "")+
  theme_bw() +
  facet_wrap("vyrobce", 
             nrow = 4,
             scales = "free") + # nepožadujeme sjednocení rozsahů os
  theme(axis.text = element_text(color = "black", size = 12))

pom = ggarrange(hist,qq,
                nrow = 1)
ggarrange(box,pom,
          nrow = 2,
          heights = c(2,4))

ggsave("aku_total.svg",
       height = 24, width = 17,units = "cm")

# .....................................................................................
## 7. Identifikace odlehlých pozorování a jejich odstranění ####

# Využití funkce boxplot s parametrem plot = FALSE - s ohledem na výrobce
boxplot(dataS$kap5~dataS$vyrobce,plot=FALSE)
# v $out jsou uložena odlehlá pozorování detekována metodou vnitřních hradeb,
# v $group je info, ze které skupiny odlehlá pozorování jsou

# Využití funkce identify_outliers() z balíčku rstatix

# Nejprve je nutno datový soubor doplnit o jednoznačný identifikátor statistických jednotek (id)
dataS$id = 1:length(dataS$vyrobce)

# Funkce identify_outliers() vyfiltruje z původního datového souboru odlehlá pozorování
outliers_kap5 = 
  dataS %>% 
  group_by(vyrobce) %>% 
  identify_outliers(kap5)

# V datovém souboru si definujeme novou proměnnou, do níž zapíšeme data po odstranění odlehlých pozorování.
# POZOR! Nikdy si nepřepisujeme původní data!!!

dataS$kap5_out = ifelse(dataS$id %in% outliers_kap5$id,NA,dataS$kap5) 
# Funkce ifelse() funguje stejně jako funkce když() v MS Excel.

# Podívejme se, na efekt odstranění odlehlých pozorování:
boxplot(dataS$kap5~dataS$vyrobce)
boxplot(dataS$kap5_out~dataS$vyrobce)

# V grafickém výstupu po odstranění odlehlých pozorování můžete vidět "nová" odlehlá pozorování.
# To však již nejsou odlehlá pozorování z původních dat, tj. tato odlehlá pozorování již neodstraňujeme!

# Analytik může vždy říct, že odlehlá pozorování odstraňovat nebude, 
# ale tuto informaci musí do zápisu o analýze uvést a patřičně zdůvodnit!!!

# .....................................................................................
## Shrnutí aneb Jak postupovat při analýze kvantitativní proměnné tříděné dle proměnné kvalitativní ####
# .....................................................................................
# 1.) Ověříme, zda jsou proměnné správného datového typu,
#     zejména u kvalitativní proměnné zkontrolovat, zda je nastaven typ factor
#     a zda jsou varianty proměnné rozumně seřazeny a pojmenovány.
# 2.) Vizuálně posoudíme data dle vícenásobného krabicového grafu (odlehlá pozorování, rozpětí pozorovaných dat, ...)
# 3.) Pokud uznáme, že je vhodné data "vyčistit" (opravit či odstranit odlehlá pozorování, nesmyslné údaje apod.),
#     učiníme tak a NEZAPOMENEME o tom učinit záznam do výzkumné zprávy.
# 4.) Posoudíme základní číselné charakteristiky pro původní i očištěná data.
# 5.) Připravíme grafické výstupy (vícenásobný krabicový graf - zpravidla pro původní data, sadu histogramů, 
#     popř. sadu histogramů a sadu Q-Q grafů - zpravidla pro očištěná data).
# 6.) Na základě histogramů, Q-Q grafů, šikmostí a špičatostí se pokusíme posoudit možnost modelování
#     analyzovaných dat normálním rozdělením (pro každou třídu/skupinu zvlášť).
# 7.) Výsledky naší analýzy shrneme do výzkumné zprávy (protokolu, reportu). Při prezentaci číselných
#     charakteristik NEZAPOMENEME zaokrouhlit číselné charakteristiky podle
#     pravidel zaokrouhlování (viz manuál). Zaokrouhlené číselné charakteristiky
#     uvádíme nejen v tabulkách, ale i v komentářích (v textu) s odpovídajícími jednotkami.

