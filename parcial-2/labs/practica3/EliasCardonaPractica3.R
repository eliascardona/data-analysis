#Elias Eduardo Cardona Rodríguez - LITC 4°A

InfoBase <- read.csv(file = file.choose(), header = TRUE)

x <- as.vector(InfoBase[,1])
y <- as.vector(InfoBase[,2])

# Correlación entre datos
relacion <- cor(x, y)
print(relacion)

# Reporte de regresión lineal
regresion <- lm(y ~ x)
print(summary(regresion))

# Gráfico de dispersión y regresión lineal
plot(x, y, xlab="Peso", ylab="Longitud Media", type="p", main="Longitud Media según el Peso")
abline(regresion, col="red")

# Análisis de varianza
AnovaDeRegresion <- anova(regresion)
print(summary(AnovaDeRegresion))

# Residuos y valores ajustados
residuos <- resid(regresion)
vajustados <- fitted(regresion)

# Gráfico de residuos
plot(vajustados, residuos, main="Gráfico de Residuos")
abline(h = 0, col = "red")

# Gráfico Q-Q para evaluar la normalidad de los residuos
qqnorm(residuos)
qqline(residuos)



