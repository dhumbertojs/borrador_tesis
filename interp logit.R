#Interpretacion de coeficientes

c <- 0.635
y <- -1.003
i <- -0.178
p <- -0.007
e <- 0.002

# IM promedio -0.01
# Promedio electricidad 77.8
# población promedio 40715
# drenaje promedio 35.05
im = i*(-0.01)
pob = p *log(40715)
elec = e*77.8

sum = c + y + im + pob +elec
calc <- exp(sum)

calc*100/(1+calc)


a <- exp(1.522-0.064-0.096*0.01-0.082*log(40715)+0.002*35.05)
a*100./(1+a)


b <- exp(0.191-2.234+0.237*0.01+0.06*log(40715)+0.002*77.8)
b*100/(1+b)

c <- exp(3.319+1.797+0.168*0.01-0.294*log(40715)-0.003*35.05)
c*100/(1+c)