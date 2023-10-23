# ......................................................................................
# ........................Cvičení 3 - Diskrétní náhodná veličina........................
# ..................Martina Litschmannová, Adéla Vrtková, Michal Béreš..................
# ......................................................................................

# Nezobrazuje-li se vám text korektně, nastavte File \ Reopen with Encoding... na UTF-8
# Pro zobrazení obsahu skriptu použijte CTRL+SHIFT+O
# Pro spouštění příkazů v jednotlivých řádcích použijte CTRL+ENTER

#  Příklady ####
# * Příklad 1 ####
# Majitel servisního střediska nabídl prodejně automobilů, která si zřídila autopůjčovnu
# své služby. Za každý automobil zapůjčený jeho prostřednictvím obdrží od autopůjčovny 
# 500,- Kč. Zároveň se však zavázal, že každý den investuje do údržby zapůjčených 
# automobilů 800,- Kč. Počet automobilů zapůjčených prostřednictvím servisního střediska 
# za 1 den je popsán následující pravděpodobnostní funkcí:
# Obrázek viz sbírka úloh.

# ** 1a) ####
# Hodnota pravděpodobnostní funkce pro 5 automobilů byla špatně čitelná. Určete ji:
x = c(0,1,2,3,4,5,6)
p = c(0.01,0.40,0.25,0.15,0.10,0,0.03) # za neznámou hodnotu p-stní funkce P(5) dosadíme v prvním kroku 0
p[6] = 1 - sum(p) # zápis pro x=5 je 6. pozice

# Vizualizace p-stní f-ce aneb začínáme s grafikou
plot(x, p)

# ** 1b) ####
# Určete a zakreslete distribuční funkci náhodné veličiny X, která je definována jako
# počet zapůjčených automobilů.
p
F = cumsum(p)
F

plot(x, F, type="s") # zjednodušený graf distribuční funkce

# ** 1c)  ####
# Určete střední hodnotu, rozptyl, směrodatnou odchylku a modus počtu zapůjčených
# automobilů během jednoho dne.

# Střední hodnota
EX = sum(x*p)

# Rozptyl
EX2 = sum(x*x*p)  # druhý obecný moment
DX = EX2 - EX^2

# Směrodatná odchylka
sigma.X = sqrt(DX)

# ** 1d) ####
# Určete pravděpodobnostní funkci a distribuční funkci náhodné veličiny Y, která je
# definována jako denní příjem majitele servisu.

y = 500*x
plot(y, p)

# Distribuční funkce
plot(y,cumsum(p),type = "s")

# ** 1e)  ####
# Určete střední hodnotu, směrodatnou odchylku a modus příjmu majitele servisu ze
# zapůjčených automobilů během jednoho dne.

# Střední hodnota
EY = sum(y*p)

# Rozptyl
EY2 = sum(y*y*p)  # druhý obecný moment
DY = EY2 - EY^2

# Směrodatná odchylka
sigma.Y = sqrt(DY)

# ** 1f)  ####
# Určete pravděpodobnost, že příjem majitele servisu (náhodná veličina Y) z půjčování
# automobilů převýší jeho výdaje.

# zisk
z=500*x-800
z

# příjem převýší výdaje, když je zisk kladný
z > 0
p
sum(p[z>0])

# ** 1g)  ####
# Určete střední hodnotu, směrodatnou odchylku a modus náhodné veličiny Z, která je
# definována jako zisk majitele servisu ze zapůjčených automobilů během jednoho dne.

# Střední hodnota
EZ = sum(z*p)

# Rozptyl
EZ2 = sum(z*z*p)  # druhý obecný moment
DZ = EZ2 - EZ^2

# Směrodatná odchylka
sigma.Z = sqrt(DZ)

# * Příklad 2 ####
# Pro distribuční funkci náhodné veličiny X platí:
# 
# $F(x)=\begin{cases}
# 0   &      x \leq -1 \\
# 0.3 & -1 < x \leq  0 \\
# 0.7 &  0 < x \leq  1 \\
# 1   & -1 < x
# \end{cases}$
# 
# ** 2a)  ####
# Určete pravděpodobnostní funkci náhodné veličiny X, její střední hodnotu a směrodatnou
# odchylku.

F = c(0, 0.3, 0.7, 1)
x = c(-1,0,1)


p = diff(F) # F[i+1]-F[i]

plot(x,p,pch = 3) # pravděpodobnostní funkce
plot(x,cumsum(p),"s") # distribuční funkce

# Všimněte si vlivu "počítačové aritmetiky na výpočet souhrnných charakteristik"
# Střední hodnota
EX = sum(x*p)

# Rozptyl
EX2 = sum(x*x*p)  # druhý obecný moment
DX = EX2 - EX^2

# Směrodatná odchylka
sigma.X = sqrt(DX)

# Pro odstranění nedostatku raději nastavíme přesnost vstupních údajů
x= round(x,0)
p = round(p,1)
# Střední hodnota
EX = sum(x*p)

# Rozptyl
EX2 = sum(x*x*p)  # druhý obecný moment
DX = EX2 - EX^2

# Směrodatná odchylka
sigma.X = sqrt(DX)

# ** 2b)  ####
# Náhodná veličina Y = 1 − 3X, určete P(y), F(y), E(Y), D(Y).

y = 1 - 3*x
plot(y,p,pch = 3) # všimněte si automaticky ořezaného rozsahu osy y

plot(y,cumsum(p),type = "s")   # Nesmyslný výstup - čím je to způsobeno?
y
p

# Setřídění pravděpodobnostní funkce podle hodnot NV Y (vzestupně)
y
sort(y) # setřídění hodnot NV Y

# Jak setřídit f-ci p podle NV Y?
idx_sorted = order(y) # funkce order vrátí indexy setříděného pořadí
idx_sorted
y = y[idx_sorted] # hodnoty NV Y setříděny vzestupně
p = p[idx_sorted] # hodnoty p setříděny dle hodnot NV Y
p

plot(y,cumsum(p),type = "s") # opět pozor na automatické oříznutí osy y

# Střední hodnota
EY = sum(y*p)

# Rozptyl
EY2 = sum(y*y*p)  # druhý obecný moment
DY = EY2 - EY^2

# Směrodatná odchylka
sigma.Y = sqrt(DY)

# ** 2c)  ####
# Náhodná veličina W = $3X^2$, určete P(w), F(w), E(W), D(W).

w = 3*x*x
w

plot(w,p,pch = 3)
plot(w,cumsum(p),type = "s") # Opět nesmyslný výstup. Kde se stala chyba?

# V tomto případě lze opravit "ručně":
w = c(0,3)
p = c(0.4,0.6)

plot(w,p,pch = 3)
plot(w,cumsum(p),type = "s")
# Střední hodnota
EW = sum(w*p)

# Rozptyl
EW2 = sum(w*w*p)  # druhý obecný moment
DW = EW2 - EW^2

# Směrodatná odchylka
sigma.W = sqrt(DW)

# * Příklad 3 ####
# V dílně jsou dva stroje pracující nezávisle na sobě. Pravděpodobnost poruchy prvního
# stroje je 0,2, pravděpodobnost poruchy druhého stroje je 0,3. Náhodná veličina X je
# definována jako počet současně porouchaných strojů. Určete:

# ** 3a)  ####
# pravděpodobnostní funkci náhodné veličiny X,

x = c(0, 1, 2)
p1 = 0.2 # pravděpodobnost poruchy stroje 1
p2 = 0.3 # pravděpodobnost poruchy stroje 2

p = x*0 # inicializace pole o stejné velikosti jako je velikost x
# spočteme jednotlivé pravděpodobnosti počtu porouchaných strojů
p[1] = (1 - p1)*(1 - p2) # 0 porouchaných tedy oba v provozu
p[3] = p1*p2 # 2 tedy porouchané oba

# p[2] lze vypočítat na základě úvahy, že součet všech p-stí musí být 1
p
p[2] = 1 - sum(p)
p
# nebo na základě úvahy, že právě jeden stroj (buď první nebo druhý) bude porouchaný
p[2] = (1 - p1)*p2 + p1*(1 - p2) 
p

plot(x,p,pch = 3)

# ** 3b) ####
# distribuční funkci náhodné veličiny X,

plot(x,cumsum(p),type = "s")

# ** 3c) ####
# střední hodnotu a rozptyl náhodné veličiny X.

# Střední hodnota
EX = sum(x*p)

# Rozptyl
EX2 = sum(x*x*p)  # druhý obecný moment
DX = EX2 - EX^2

# Směrodatná odchylka
sigma.X = sqrt(DX)


