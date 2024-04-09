#Elias Eduardo Cardona Rodríguez - LITC 4°A

InfoBase=read.csv(file=file.choose(),header=TRUE)

x=as.vector(InfoBase[,1])
y=as.vector(InfoBase[,2])

cor(x,y)

regresion=lm(y~x)
print(regresion)

print(summary(regresion))

plot(x, y, xlab="peso", ylab="longitud media", main="Longitud media según el peso")
abline(regresion, col="red")


cat(" ", "THIS IS CONF", ".........................................", sep="\n")
print(confint(regresion))
print(confint(regresion,level=0.90))

cat(" ", "THIS WAS CONF", ".........................................", sep="\n")


anova=aov(regresion)
print(anova)
print(summary(anova))


residuos=rstandard(regresion)
vajustados=fitted(regresion)
plot(vajustados,residuos)



plot(qqnorm(residuos, main="Tabla de normalidad de residuos"))
plot(qqline(residuos, main="Tabla de linealidad de residuos"))

t=2.101
xn=3.4
yn=5.0343 + (0.2025*xn)

xbarra=mean(x)
m=(xn*xbarra)^2

d=m/96.9

r=0.0042





