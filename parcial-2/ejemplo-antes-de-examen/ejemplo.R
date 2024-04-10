#### ABRIR LOS DATOS ####
ruta=file.choose()
base=read.csv(file=ruta,header=TRUE)

#### DEFINIR ELEMENTOS ####
y=as.vector(base[,1])
x=as.vector(base[,2])

## correlación ##
cor(x,y)

## regresion ##
regresion=lm(y~x)
print(regresion)

print(summary(regresion))

## gráfico ##
plot(x,y,main="Salario por Especialidad", xlab="Especialidad", ylab="Salario")
abline(regresion,col="red")

## intervalos de confianza ##
print(confint(regresion))
print(confint(regresion,level=0.90))
print(confint(regresion,level=0.99))


## ANOVA ##
anova=aov(regresion)
print(anova)
print(summary(anova))

## comparar residuos y valores ajustados ##
residuos=rstandard(regresion)
vajustados=fitted(regresion)
# plot(vajustados,residuos)

# normalidad de residuos
# qqnorm(residuos)
# qqline(residuos)

