#Elias Eduardo Cardona Rodríguez - LITC 4°A
InfoBase <- read.csv(file = file.choose(), header = TRUE)
y <- as.vector(InfoBase[,2])
x <- as.vector(InfoBase[,3])

# correlación entre datos
relacion <- cor(x, y)
print(relacion)

# reporte de regresión lineal
regresion <- lm(y~x)
print(summary(regresion))


plot(x, y, type="p", main="Resistencia al corte según la edad del propelente")
abline(lm(y~x), col="red")


AnovaDeRegresion <- aov(formula = regresion)
print(summary(AnovaDeRegresion))


