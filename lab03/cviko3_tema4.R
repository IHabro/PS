# Ve městě, které má 150 000 obyvatel, bydlí v určité ulici 250 osob. Za účelem sociologického
# průzkumu bylo náhodně vybráno 300 osob z daného města. S jakou pravděpodobností jsou mezi
# vybranými dva nebo více osob ze zmiňované ulice?
# Hypergeometricke rozdeleni
1 - phyper(1, 250, 150000-250,300)



# 2.a
x = 0:4
y = dbinom(x, 4, 0.02)
y = round(y, 3)
y[1] = 0.923
sum(y)
F = cumsum(y)
plot(x, F, type = "s")

# 2.d
hodnoty= x
hodnoty
pravdep= dbinom(x, 4, 0.02)
pravdep[1] = 0.923
pravdep
stredniHodnota = sum(hodnoty*pravdep)
stredniHodnota
druhyObecny = sum(hodnoty*hodnoty*pravdep)
druhyObecny
rozptyl = druhyObecny - stredniHodnota*stredniHodnota
rozptyl
smerodatnaOdchylka = sqrt(rozptyl)
smerodatnaOdchylka

# 3
1-pnbinom(10,5,0.2)

# 4
# Poison
1 - ppois(6,5)

# 5
1 - ppois(0,0.1)
1 - dpois(0,0.1)

# 6.a
pnbinom(60-4,4,0.05)

pnbinom(19-4,4,0.25) - pnbinom(10-4,4,0.25)

# 7.b
dbinom(0,10,0.25)

# 8.a
dpois(0,4)


# Modelovani ggPlot
x = 0:50
p = dpois(hodnoty,4)
plot(x, p)

# instalace:
install.packages("ggplot2")

# pouziti: musi se inicializovat pri kazdem zapnuti
library(ggplot2)

data = data.frame(x,p) # Příprava dat do formátu data.frame
plt = ggplot(data = data, aes(x = x, y = p)) + geom_point(pch = 3)
plt + theme_light()
plt


