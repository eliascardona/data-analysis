#### DECLARACION DE LIBRERIAS A UTILIZAR ####
library(car)
library(plot3D)
library(rgl)
library(plot3Drgl)
library(lmtest)

#### ABRIR LOS DATOS ####
base=read.csv(file=file.choose(),header=TRUE)

#### DEFINIR ELEMENTOS ####

x1=as.vector(base[,1])
x2=as.vector(base[,2])
x3=as.vector(base[,3])
x4=as.vector(base[,4])
x5=as.vector(base[,5])
y=as.vector(base[,6])


#### MATRIZ DE GRAFICOS DE DISPERSIÓN ####

#### AJUSTANDO NUESTRA INFORMACION BASE ####
plot(base)


#### MATRIZ DE CORRELACIÓN ####
print(round(cor(base),3))


#### MODELO DE REGRESIÓN LINEAL MÚLTIPLE ###
modelo = lm(y ~ x1 + x2 + x3 + x4 + x5)
print(coef(modelo))
print(summary(modelo))


#### INTERVALOS DE CONFIANZA ####
print(confint(modelo,conf.level=0.95))


#### PRUEBA DE NORMALIDAD DE RESIUDOS ####
residuos = rstandard(modelo)
residuos
qqnorm(residuos, col="blue")
qqline(residuos, col="red")


#### VALORES AJUSTADOS ####
vajustados=fitted(modelo)
plot(vajustados,residuos)


#### BOX TIDWELL   |   LINEALIDAD   ####
boxTidwell(y~x1+x3+x4+x5)

#### DURBIN-WATSON   |   INDEPENDENCIA ####
durbinWatsonTest(modelo, simulate = TRUE)

####  NCV TEST     ####
####  p-value > alfa, entonces hay linealidad  ####

ncvTest(modelo)
spreadLevelPlot(modelo)

#### HOMOCEDASTICIDAD ####
bptest(modelo, ~I(x1^x2) + I(x1^x2) + I(x1*x2))


#print("====================== CORRELACION DE 'base2' - Info base sin la variable Y ==========================")
base2 = base[-5]
print(round(cor(base2), 3))



## Comparacion de nuestro modelo original con nuestros nuevos modelos

m0 = lm(y~x4)
m1 = lm(y~x4+x1)
m2 = lm(y~x4+x2)
m3 = lm(y~x4+x3)
m4 = lm(y~x4+x5)

m5 = lm(y~x4+x3+x2)
m6 = lm(y~x4+x2+x1)
m7 = lm(y~x4+x2+x5)
m8 = lm(y~x4+x1+x5)
m9 = lm(y~x4+x3+x5)

m10 = lm(y~x4+x1+x2+x3)
m11 = lm(y~x4+x1+x2+x5)
m12 = lm(y~x4+x3+x2+x5)
m13 = lm(y~x4+x1+x3+x5)



#print("=================================== summary de todos los modelos ================================================")
print(summary(modelo))
print(summary(m0))
print(summary(m1))
print(summary(m2))
print(summary(m3))
print(summary(m4))
print(summary(m5))
print(summary(m6))
print(summary(m7))
print(summary(m8))
print(summary(m9))
print(summary(m10))
print(summary(m11))
print(summary(m12))
print(summary(m13))



#print("=================================== Pruebas akaike ================================================")
print(AIC(modelo))
print(AIC(m0))
print(AIC(m1))
print(AIC(m2))
print(AIC(m3))
print(AIC(m4))
print(AIC(m5))
print(AIC(m6))
print(AIC(m7))
print(AIC(m8))
print(AIC(m9))
print(AIC(m10))
print(AIC(m11))
print(AIC(m12))
print(AIC(m13))
print(AIC(m14))



#print("=================================== anova final  ================================================")
anova = aov(modelo)
print(anova(m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, m10, m11, m12, m13, modelo))



#### FIT POINTS ####
fitpoints = predict(modelo)
print(fitpoints)
fit = lm(y ~ x1 + x2 + x3 + x4 + x5)



#broom:argument(fit)
leveragePlots(fit, layout = c(2,2))















