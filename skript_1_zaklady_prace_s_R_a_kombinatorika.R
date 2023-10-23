#--------------------------------------------------------------------------------------
#--------------- Cvièení 1 - Struèný úvod do R, Kombinatorika -------------------------
#---------------- Adéla Vrtková, Michal Béreš, Martina Litschmannová ------------------
#--------------------------------------------------------------------------------------

# Nezobrazuje-li se vám text korektnì, 
# nastavte File \ Reopen with Encoding... na UTF-8

# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O

# Pro spouštìní pøíkazù v jednotlivých øádcích použijte CTRL+ENTER

# Jednoduché poèetní operace ----
2+4
5/2

# POZOR na závorky! Pro poèítání se používají pouze kulaté! Hranaté a složené mají v R jinou funkci!
(((10+2)*(340-33))-2)/3

#* Kombinaèní èíslo, faktoriál ----
choose(10,2)
factorial(4)

# Datové typy -> numeric, character, logical, (complex) ----
# funkcí class zjišujeme typ objektu
a = 2+3
class(a)

b="pismenko"
class(b)

c=(1>3)
class(c)

d=3+1i
class(d)

# Datové struktury v R ----
# vector (rozumíme sloupcový vektor)
# factor (speciální pøípad vektoru)
# matrix (matice s rozmìry n x m)
# data.frame (datový rámec s rozmìry n x p)
# list (seznam)


#* Definování vektoru (sloupcový=column) ----
a = c(3,4,6,7)
a <- c(3,4,6,7) # jiný (pùvodní) zpùsob pøiøazení
a[2] # druhá položka vektoru a

# další možnosti
rep(1,4) # vytvoøí vektor se ètyømi jednièkami
seq(1,10,2) # posloupnost od 1 do 10 s krokem 2
1:10  # posloupnost od 1 do 10 s krokem 1

b=c("A","B","C","D")
class(b)

# pøedefinování objektu na jiný typ - napø. as.vector, as.matrix, as.factor,...
b = as.factor(b)
b

#* Práce s vektory - sluèování podle sloupcù/øádkù ----
cbind(a,b)
rbind(a,b)

# Definování matice ----
A = matrix(c(3,4,6,7,3,2),nrow=2,ncol=3)
B = matrix(c(3,4,6,7,3,2),nrow=2,ncol=3,byrow=TRUE)
C = matrix(c(3,4,6,7,3,2),nrow=3,ncol=2)

A
A[1,3] # prvek matice A v prvním øádku, tøetím sloupci
A[1,] # první øádek matice A
A[,2:3] # druhý a tøetí sloupec matice A
A[,c(1,3)] # první a tøetí sloupec matice A

# diagonální matice
diag(4)
diag(4,2)

#* Operace s maticemi ----
# pozor na maticové násobení -> %*%
A
B
A+B
A-B
A*B # POZOR! Násobí se prvky na stejných pozicích.
A%*%C # Klasické násobení matic

# Definování vlastní funkce ----
# nazev = function(promìnné){tìlo funkce}
funkce = function(x,y){
  return((x^2)+(y^2)) # to, co funkce vrací se dává jako argument return()
}
funkce(10,15)

# Shrnutí, doporuèení, rady ----
# 1. pozor na závorky -> ( ) pøi poèítání, [ ] odkazují na prvek ve vektoru/matici/data.framu, { } používají se pøi cyklech 
# 2. brát v úvahu typ objektu
# 3. používat nápovìdu - napø. ?seq, nebo help(matrix)
# 4. nastavení Working Directory
# 5. pozor na mezery v názvu promìnných, rozlišování velkých a malých písmen

# Kombinatorika ----
 
#* V(n,k) - variace bez opakování ---- 
# první argument bude celkový poèet entit, druhý argument velikost výbìru

variace = function(n,k) 
  {
  citatel = factorial(n)  
  jmenovatel = factorial(n-k)
  return(citatel/jmenovatel)
  }

#* V*(n,k) - variace s opakováním ----

variace_opak = function(n,k) {
  return(n^k)
}

#* P(n) - permutace (pøesmyèky) ----

permutace = function(n){
  return(factorial(n))
}

#* P*(n1,n2,n3,....,nk) - permutace s opakováním ---- 
# vstupní promìnné tvoøí vektor s jednotlivými poèty unikátních entit

permutace_opak = function(vec_n) # vec_n je vektor poètù hodnot pø.: vec_n = c(2,2,2,4,3)
  {
  n = sum(vec_n) # spoèteme kolik máme hodnot celkem
  citatel = factorial(n) 
  jmenovatel = prod(factorial(vec_n)) # výstupem funkce prod() je souèin položek vektoru, který je argumentem této funkce
  return(citatel/jmenovatel)
  }

#* C(n,k) - kombinace ----

kombinace = function(n,k)
  {
  return(choose(n,k)) 
  }

#* C*(n,k) - kombinace s opakováním ----

kombinace_opak = function(n,k)
  {
  return(choose(n+k-1,k)) 
  }

# Pøíklady z pracovních listù ----

#* Pøíklad 1 ----

# V prodejnì mají k dispozici tøi typy zámkù. Pro otevøení prvního
# zámku je nutno zmáèknout ètyøi z deseti tlaèítek oznaèených èíslicemi 0 až 9. (Na poøadí nezáleží - tlaèítka zùstávají zmáèknuta.)
# Druhý zámek se otevøe pokud zmáèkneme šest tlaèítek z deseti.
# Pro otevøení tøetího zámku je nutno nastavit správnou kombinaci
# na ètyøech kotouèích. Který z tìchto zámkù nejlépé chrání pøed
# zlodìji?

z1 = kombinace(10,4)
z2 = kombinace(10,6)
z3 = variace_opak(10,4)
paste0("poèet kombinací: zámek 1: ",z1,", zámek 2: ",z2,", zámek 3: ",z3) # funkce paste0() sluèuje textové øetìzce
paste0("pravdìpodobnost náhodného otevøení: zámek 1: ",1/z1,", zámek 2: ",1/z2,", zámek 3:",1/z3)

#* Pøíklad 2 ----

# V prodejnì nabízejí dva druhy zamykání kuføíku. První kuføík se zamyká šifrou, která se skládá
# z šesti èíslic. Druhý kuføík se zamyká dvìma zámky, které se otevírají souèasnì. Šifra každého
# z nich se skládá ze tøí èíslic. Urèete pro každý kuføík pravdìpodobnost otevøení zlodìjem pøi
# prvním pokusu. Který typ zámku je bezpeènìjší?

z1 = variace_opak(10,6)
z2 = variace_opak(10,3)*variace_opak(10,3)

paste("poèet kombinací: kufr 1: ",z1,", kufr 2: ",z2)

#* Pøíklad 3 ----

# V urnì je 40 koulí - 2 èervené a 38 bílých. Z urny náhodnì vytáhneme 2 koule. S jakou pravdìpodobností budou obì èervené?

poc_moz = kombinace(40,2)
poc_priz = kombinace(2,2)*kombinace(38,0)
prob=poc_priz/poc_moz;
paste("pravdìpodobnost je: ",prob)

#* Pøíklad 4 ----

# Student si mìl ke zkoušce pøipravit odpovìdi na 40 otázek. Na dvì otázky, které mu dal zkoušející, neumìl odpovìdìt a tak øekl „To mám smùlu! To jsou jediné dvì otázky, na které neumím odpovìdìt.“ S jakou pravdìpodobností mluví pravdu?
# viz Pøíklad 3

#* Pøíklad 5 ----

# Test z chemie žák složí, pokud v seznamu 40 chemických slouèenin podtrhne jediné dva aldehydy, které v seznamu jsou. Jaká je pravdìpodobnost, že test složí žák, který provede výbìr
# slouèenin náhodnì?
# viz Pøíklad 3

#* Pøíklad 6 ----

# Ze zahranièí se vracela skupina 40 turistù a mezi nimi byli 2 pašeráci. Na hranici celník 2 turisty
# vyzval k osobní prohlídce a ukázalo se, že oba dva jsou pašeráci. Zbylí turisté na to reagovali:
# „Celník mìl opravdu štìstí!“, „Pašeráky nìkdo udal!“, . . .. Jak se postavit k tìmto výrokùm?
# Je oprávnìné podezøení, že pašeráky nìkdo udal?
# viz Pøíklad 3

#* Pøíklad 7 ----

# Z urny se tøemi koulemi, dvìma èervenými a jednou bílou, budou souèasnì vybrány dvì koule.
# Student a uèitel uzavøou sázku. Pokud budou obì koule stejné barvy, vyhraje student. Pokud
# budou mít koule rùznou barvu, vyhraje uèitel. Je hra férová? Jaké jsou pravdìpodobnosti výhry
# uèitele a studenta?

# funkce combn vyrobí kombinace o pøedepsané velikosti - první parametr je vektor hodnot, druhý velikost výbìru
combn(c('cervena','cervena','bila'),2)

#* Pøíklad 8 ----

# Hra popsaná v pøíkladu 7 nebyla férová. Jakou kouli (èervenou nebo bílou) musíme do urny pøidat, aby hra férová byla?

combn(c('cervena','cervena','cervena','bila'),2)
combn(c('cervena','cervena','bila','bila'),2)

#* Pøíklad 9 ----

# Chcete hrát Èlovìèe nezlob se, ale ztratila se hrací kostka. Èím a jak lze nahradit hrací kostku,
# máte-li k dispozici hrací karty (balíèek 32 karet) a 4 rùznobarevné kulièky?
# viz Pracovní listy

#* Pøíklad 10 ----

# Chcete hrát Èlovìèe nezlob se, ale ztratila se hrací kostka. Jak lze nahradit hrací kostku, máte-li
# k dispozici 3 rùznobarevné kulièky?
# viz Pracovní listy


#* Pøíklad 11 ----

# V prodejnì vozù Škoda mají v mìsíci únoru prodejní akci. Ke standardnímu vybavení nabízejí
# 3 položky z nadstandardní výbavy zdarma. Nadstandardní výbava zahrnuje 7 položek:
# - tempomat, vyhøívání sedadel, zadní airbagy, xenonová svìtla, stropní okénko, bezpeènostní
# zámek pøevodovky, speciální odolný metalízový lak.
 
# Kolik možností má zákazník, jak zvolit 3 položky z nadstandardní výbavy?

kombinace(7,3)

#* Pøíklad 12 ----

#  Pøi zkoušce si do 5. øady sedlo 12 studentù. Zkoušející chce urèit sám, jak tyto studenty v øadì
# rozesadit.
# Kolik je možností jak studenty rozesadit?
# Student Brahý žádá, aby mohl sedìt na kraji a odejít døíve, aby stihl vlak. Kolik je možností jak studenty rozesadit, chce-li zkoušející vyhovìt požadavku studenta Brahého?
# Kolik je možností jak studenty rozesadit, nesmìjí-li Pažout a Horáèek sedìt vedle sebe?

#* a) ----
permutace(12)

#* b) ----
2*permutace(11)

#* c) ----
vedle_sebe=2*permutace(11)
permutace(12)-vedle_sebe

#* Pøíklad 13 ----

# Kolik anagramù lze vytvoøit ze slova STATISTIKA?

permutace_opak(c(2,3,2,2,1))

#* Pøíklad 14 ----

# V Tescu dostali nové zboží – 6 druhù chlapeckých trik. Od každého druhu mají alespoò 7 kusù.
# Maminka chce synovi koupit 4 trika. Kolik je možností, jak je vybrat
# a) mají-li být všechna rùzná?
# b) pøipouští-li, že mohou být všechna stejná?

#* a) ----
kombinace(6,4)
#* b) ----
kombinace_opak(6,4)

#* Pøíklad 15 ----
# Kolik hesel délky 5 mùžeme vytvoøit ze znakù abecedy
# a) nejsou-li rozlišována velká a malá písmena?
# b) jsou-li rozlišována velká a malá písmena?

#* a) ----
variace_opak(26,5)

#* b) ----
variace_opak(52,5)
