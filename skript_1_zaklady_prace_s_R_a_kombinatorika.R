#--------------------------------------------------------------------------------------
#--------------- Cvi�en� 1 - Stru�n� �vod do R, Kombinatorika -------------------------
#---------------- Ad�la Vrtkov�, Michal B�re�, Martina Litschmannov� ------------------
#--------------------------------------------------------------------------------------

# Nezobrazuje-li se v�m text korektn�, 
# nastavte File \ Reopen with Encoding... na UTF-8

# Pro zobrazen� obsahu skriptu pou�ijte CTRL+SHIFT+O

# Pro spou�t�n� p��kaz� v jednotliv�ch ��dc�ch pou�ijte CTRL+ENTER

# Jednoduch� po�etn� operace ----
2+4
5/2

# POZOR na z�vorky! Pro po��t�n� se pou��vaj� pouze kulat�! Hranat� a slo�en� maj� v R jinou funkci!
(((10+2)*(340-33))-2)/3

#* Kombina�n� ��slo, faktori�l ----
choose(10,2)
factorial(4)

# Datov� typy -> numeric, character, logical, (complex) ----
# funkc� class zji��ujeme typ objektu
a = 2+3
class(a)

b="pismenko"
class(b)

c=(1>3)
class(c)

d=3+1i
class(d)

# Datov� struktury v R ----
# vector (rozum�me sloupcov� vektor)
# factor (speci�ln� p��pad vektoru)
# matrix (matice s rozm�ry n x m)
# data.frame (datov� r�mec s rozm�ry n x p)
# list (seznam)


#* Definov�n� vektoru (sloupcov�=column) ----
a = c(3,4,6,7)
a <- c(3,4,6,7) # jin� (p�vodn�) zp�sob p�i�azen�
a[2] # druh� polo�ka vektoru a

# dal�� mo�nosti
rep(1,4) # vytvo�� vektor se �ty�mi jedni�kami
seq(1,10,2) # posloupnost od 1 do 10 s krokem 2
1:10  # posloupnost od 1 do 10 s krokem 1

b=c("A","B","C","D")
class(b)

# p�edefinov�n� objektu na jin� typ - nap�. as.vector, as.matrix, as.factor,...
b = as.factor(b)
b

#* Pr�ce s vektory - slu�ov�n� podle sloupc�/��dk� ----
cbind(a,b)
rbind(a,b)

# Definov�n� matice ----
A = matrix(c(3,4,6,7,3,2),nrow=2,ncol=3)
B = matrix(c(3,4,6,7,3,2),nrow=2,ncol=3,byrow=TRUE)
C = matrix(c(3,4,6,7,3,2),nrow=3,ncol=2)

A
A[1,3] # prvek matice A v prvn�m ��dku, t�et�m sloupci
A[1,] # prvn� ��dek matice A
A[,2:3] # druh� a t�et� sloupec matice A
A[,c(1,3)] # prvn� a t�et� sloupec matice A

# diagon�ln� matice
diag(4)
diag(4,2)

#* Operace s maticemi ----
# pozor na maticov� n�soben� -> %*%
A
B
A+B
A-B
A*B # POZOR! N�sob� se prvky na stejn�ch pozic�ch.
A%*%C # Klasick� n�soben� matic

# Definov�n� vlastn� funkce ----
# nazev = function(prom�nn�){t�lo funkce}
funkce = function(x,y){
  return((x^2)+(y^2)) # to, co funkce vrac� se d�v� jako argument return()
}
funkce(10,15)

# Shrnut�, doporu�en�, rady ----
# 1. pozor na z�vorky -> ( ) p�i po��t�n�, [ ] odkazuj� na prvek ve vektoru/matici/data.framu, { } pou��vaj� se p�i cyklech 
# 2. br�t v �vahu typ objektu
# 3. pou��vat n�pov�du - nap�. ?seq, nebo help(matrix)
# 4. nastaven� Working Directory
# 5. pozor na mezery v n�zvu prom�nn�ch, rozli�ov�n� velk�ch a mal�ch p�smen

# Kombinatorika ----
 
#* V(n,k) - variace bez opakov�n� ---- 
# prvn� argument bude celkov� po�et entit, druh� argument velikost v�b�ru

variace = function(n,k) 
  {
  citatel = factorial(n)  
  jmenovatel = factorial(n-k)
  return(citatel/jmenovatel)
  }

#* V*(n,k) - variace s opakov�n�m ----

variace_opak = function(n,k) {
  return(n^k)
}

#* P(n) - permutace (p�esmy�ky) ----

permutace = function(n){
  return(factorial(n))
}

#* P*(n1,n2,n3,....,nk) - permutace s opakov�n�m ---- 
# vstupn� prom�nn� tvo�� vektor s jednotliv�mi po�ty unik�tn�ch entit

permutace_opak = function(vec_n) # vec_n je vektor po�t� hodnot p�.: vec_n = c(2,2,2,4,3)
  {
  n = sum(vec_n) # spo�teme kolik m�me hodnot celkem
  citatel = factorial(n) 
  jmenovatel = prod(factorial(vec_n)) # v�stupem funkce prod() je sou�in polo�ek vektoru, kter� je argumentem t�to funkce
  return(citatel/jmenovatel)
  }

#* C(n,k) - kombinace ----

kombinace = function(n,k)
  {
  return(choose(n,k)) 
  }

#* C*(n,k) - kombinace s opakov�n�m ----

kombinace_opak = function(n,k)
  {
  return(choose(n+k-1,k)) 
  }

# P��klady z pracovn�ch list� ----

#* P��klad 1 ----

# V prodejn� maj� k dispozici t�i typy z�mk�. Pro otev�en� prvn�ho
# z�mku je nutno zm��knout �ty�i z deseti tla��tek ozna�en�ch ��slicemi 0 a� 9. (Na po�ad� nez�le�� - tla��tka z�st�vaj� zm��knuta.)
# Druh� z�mek se otev�e pokud zm��kneme �est tla��tek z deseti.
# Pro otev�en� t�et�ho z�mku je nutno nastavit spr�vnou kombinaci
# na �ty�ech kotou��ch. Kter� z t�chto z�mk� nejl�p� chr�n� p�ed
# zlod�ji?

z1 = kombinace(10,4)
z2 = kombinace(10,6)
z3 = variace_opak(10,4)
paste0("po�et kombinac�: z�mek 1: ",z1,", z�mek 2: ",z2,", z�mek 3: ",z3) # funkce paste0() slu�uje textov� �et�zce
paste0("pravd�podobnost n�hodn�ho otev�en�: z�mek 1: ",1/z1,", z�mek 2: ",1/z2,", z�mek 3:",1/z3)

#* P��klad 2 ----

# V prodejn� nab�zej� dva druhy zamyk�n� kuf��ku. Prvn� kuf��k se zamyk� �ifrou, kter� se skl�d�
# z �esti ��slic. Druh� kuf��k se zamyk� dv�ma z�mky, kter� se otev�raj� sou�asn�. �ifra ka�d�ho
# z nich se skl�d� ze t�� ��slic. Ur�ete pro ka�d� kuf��k pravd�podobnost otev�en� zlod�jem p�i
# prvn�m pokusu. Kter� typ z�mku je bezpe�n�j��?

z1 = variace_opak(10,6)
z2 = variace_opak(10,3)*variace_opak(10,3)

paste("po�et kombinac�: kufr 1: ",z1,", kufr 2: ",z2)

#* P��klad 3 ----

# V urn� je 40 koul� - 2 �erven� a 38 b�l�ch. Z urny n�hodn� vyt�hneme 2 koule. S jakou pravd�podobnost� budou ob� �erven�?

poc_moz = kombinace(40,2)
poc_priz = kombinace(2,2)*kombinace(38,0)
prob=poc_priz/poc_moz;
paste("pravd�podobnost je: ",prob)

#* P��klad 4 ----

# Student si m�l ke zkou�ce p�ipravit odpov�di na 40 ot�zek. Na dv� ot�zky, kter� mu dal zkou�ej�c�, neum�l odpov�d�t a tak �ekl �To m�m sm�lu! To jsou jedin� dv� ot�zky, na kter� neum�m odpov�d�t.� S jakou pravd�podobnost� mluv� pravdu?
# viz P��klad 3

#* P��klad 5 ----

# Test z chemie ��k slo��, pokud v seznamu 40 chemick�ch slou�enin podtrhne jedin� dva aldehydy, kter� v seznamu jsou. Jak� je pravd�podobnost, �e test slo�� ��k, kter� provede v�b�r
# slou�enin n�hodn�?
# viz P��klad 3

#* P��klad 6 ----

# Ze zahrani�� se vracela skupina 40 turist� a mezi nimi byli 2 pa�er�ci. Na hranici celn�k 2 turisty
# vyzval k osobn� prohl�dce a uk�zalo se, �e oba dva jsou pa�er�ci. Zbyl� turist� na to reagovali:
# �Celn�k m�l opravdu �t�st�!�, �Pa�er�ky n�kdo udal!�, . . .. Jak se postavit k t�mto v�rok�m?
# Je opr�vn�n� podez�en�, �e pa�er�ky n�kdo udal?
# viz P��klad 3

#* P��klad 7 ----

# Z urny se t�emi koulemi, dv�ma �erven�mi a jednou b�lou, budou sou�asn� vybr�ny dv� koule.
# Student a u�itel uzav�ou s�zku. Pokud budou ob� koule stejn� barvy, vyhraje student. Pokud
# budou m�t koule r�znou barvu, vyhraje u�itel. Je hra f�rov�? Jak� jsou pravd�podobnosti v�hry
# u�itele a studenta?

# funkce combn vyrob� kombinace o p�edepsan� velikosti - prvn� parametr je vektor hodnot, druh� velikost v�b�ru
combn(c('cervena','cervena','bila'),2)

#* P��klad 8 ----

# Hra popsan� v p��kladu 7 nebyla f�rov�. Jakou kouli (�ervenou nebo b�lou) mus�me do urny p�idat, aby hra f�rov� byla?

combn(c('cervena','cervena','cervena','bila'),2)
combn(c('cervena','cervena','bila','bila'),2)

#* P��klad 9 ----

# Chcete hr�t �lov��e nezlob se, ale ztratila se hrac� kostka. ��m a jak lze nahradit hrac� kostku,
# m�te-li k dispozici hrac� karty (bal��ek 32 karet) a 4 r�znobarevn� kuli�ky?
# viz Pracovn� listy

#* P��klad 10 ----

# Chcete hr�t �lov��e nezlob se, ale ztratila se hrac� kostka. Jak lze nahradit hrac� kostku, m�te-li
# k dispozici 3 r�znobarevn� kuli�ky?
# viz Pracovn� listy


#* P��klad 11 ----

# V prodejn� voz� �koda maj� v m�s�ci �noru prodejn� akci. Ke standardn�mu vybaven� nab�zej�
# 3 polo�ky z nadstandardn� v�bavy zdarma. Nadstandardn� v�bava zahrnuje 7 polo�ek:
# - tempomat, vyh��v�n� sedadel, zadn� airbagy, xenonov� sv�tla, stropn� ok�nko, bezpe�nostn�
# z�mek p�evodovky, speci�ln� odoln� metal�zov� lak.
 
# Kolik mo�nost� m� z�kazn�k, jak zvolit 3 polo�ky z nadstandardn� v�bavy?

kombinace(7,3)

#* P��klad 12 ----

#  P�i zkou�ce si do 5. �ady sedlo 12 student�. Zkou�ej�c� chce ur�it s�m, jak tyto studenty v �ad�
# rozesadit.
# Kolik je mo�nost� jak studenty rozesadit?
# Student Brah� ��d�, aby mohl sed�t na kraji a odej�t d��ve, aby stihl vlak. Kolik je mo�nost� jak studenty rozesadit, chce-li zkou�ej�c� vyhov�t po�adavku studenta Brah�ho?
# Kolik je mo�nost� jak studenty rozesadit, nesm�j�-li Pa�out a Hor��ek sed�t vedle sebe?

#* a) ----
permutace(12)

#* b) ----
2*permutace(11)

#* c) ----
vedle_sebe=2*permutace(11)
permutace(12)-vedle_sebe

#* P��klad 13 ----

# Kolik anagram� lze vytvo�it ze slova STATISTIKA?

permutace_opak(c(2,3,2,2,1))

#* P��klad 14 ----

# V Tescu dostali nov� zbo�� � 6 druh� chlapeck�ch trik. Od ka�d�ho druhu maj� alespo� 7 kus�.
# Maminka chce synovi koupit 4 trika. Kolik je mo�nost�, jak je vybrat
# a) maj�-li b�t v�echna r�zn�?
# b) p�ipou�t�-li, �e mohou b�t v�echna stejn�?

#* a) ----
kombinace(6,4)
#* b) ----
kombinace_opak(6,4)

#* P��klad 15 ----
# Kolik hesel d�lky 5 m��eme vytvo�it ze znak� abecedy
# a) nejsou-li rozli�ov�na velk� a mal� p�smena?
# b) jsou-li rozli�ov�na velk� a mal� p�smena?

#* a) ----
variace_opak(26,5)

#* b) ----
variace_opak(52,5)
