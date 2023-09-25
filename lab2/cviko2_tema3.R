#
x = c(5:9)
x

p = c(0.1,0.2,0.4,0.2,0.1)
p
sum(p)
sum(p[3:5])
sum(p[x>=7])

p1 = x*p
p1
ex = sum(p1)
ex

p2 = x*x*p
p2

sum(p2)

rozptyl = sum(p2) - ex*ex
rozptyl

sqrt(rozptyl)


# Program na vypocet stredni hodnotz, rozptylu a smerodatne odchylky
hodnoty= c(0:6)
hodnoty
pravdep= choose(6,hodnoty)*(0.49^hodnoty)*(0.51^(6-hodnoty))
pravdep
stredniHodnota = sum(hodnoty*pravdep)
stredniHodnota
druhyObecny = sum(hodnoty*hodnoty*pravdep)
druhyObecny
rozptyl = druhyObecny - stredniHodnota*stredniHodnota
rozptyl
smerodatnaOdchylka = sqrt(roz)
smerodatnaOdchylka


choose(6,hodnoty)*(0.49^hodnoty)*(0.51^(6-hodnoty)) == pbinom(hodnoty, 6, p=0.49)


