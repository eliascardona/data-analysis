# Elias Eduardo Cardona Rodríguez - LITC 4°A

InfoBase <- read.csv(file=file.choose(), header = TRUE)

x = as.vector(InfoBase[,3])
y = as.vector(InfoBase[,2])


correlacion.xy = cor(x, y)
correlacion.xy

print("Coeficiente de correlación")
print(correlacion.xy)




plot(x, y, type="p", col=rainbow(10), main="Dispersión de cajas entregadas con respecto al tiempo", xlab="Cajas transportadas", ylab="Tiempo en min.")

abline(lm(y~x), col="red")


modelo <- lm(formula=y~x)
print(modelo)

