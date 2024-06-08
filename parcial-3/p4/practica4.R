#### DECLARACION DE LIBRERIAS A UTILIZAR ####
library(broom)
library(car)
library(rgl)
library(lmtest)

#### ABRIR LOS DATOS ####
base=read.csv(file=file.choose(),header=TRUE)

#### DEFINIR ELEMENTOS ####
x1=as.vector(base[,1])
x2=as.vector(base[,2])
x3=as.vector(base[,3])
x4=as.vector(base[,4])
y=as.vector(base[,5])

#### MATRIZ DE GRAFICOS DE DISPERSIÓN ####
plot(base)
base

#### MATRIZ DE CORRELACIÓN ####
print(round(cor(base),3))


#### MODELO DE REGRESIÓN LINEAL MÚLTIPLE ###
modelo = lm(y ~ x1 + x2 + x3 + x4)
print(coef(modelo))
print(summary(modelo))


#### INTERVALOS DE CONFIANZA ####
print(confint(modelo, level=0.95))
print(confint(modelo, level=0.99))

#### PRUEBA DE NORMALIDAD DE RESIUDOS ####
residuos = rstandard(modelo)
residuos
qqnorm(residuos, col="blue")
qqline(residuos, col="red")
#### PRUEBA SHAPIRO PARA PROBAR NORMALIDAD DE RESIUDOS ####
shapiro.test(modelo$residuals)



#### VALORES AJUSTADOS ####
vajustados=fitted(modelo)
plot(vajustados,residuos)

#### DURBIN-WATSON   |   INDEPENDENCIA ####
durbinWatsonTest(modelo, simulate = TRUE)


#### BOX TIDWELL   |   LINEALIDAD   ####
boxTidwell(y~x1+x2)

####  NCV TEST     ####
####  p-value > alfa, entonces hay linealidad  ####
ncvTest(modelo)
spreadLevelPlot(modelo)

#### HOMOCEDASTICIDAD ####
bptest(modelo, ~I(x1^2) + I(x2^2) + I(x3^2) + I(x4^2) + I(x1*x2) + I(x1*x3) + I(x1*x4) + I(x2*x3) + I(x2*x4) + I(x3*x4))



#print("====================== CORRELACION DE 'base2' - Info base sin la variable Y ==========================")
base2 = base[-5]
print(round(cor(base2), 3))

## Comparacion de nuestro modelo original con nuestros nuevos modelos

m0 = lm(y~x1)
m1 = lm(y~x1+x2)
m2 = lm(y~x1+x3)
m3 = lm(y~x1+x4)
m4 = lm(y~x1+x2+x3)
m5 = lm(y~x1+x2+x4)
m6 = lm(y~x1+x3+x4)
m7 = lm(y~x1+x2+x3+x4)
coef(m5)



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
#print("=================================== anova final  ================================================")
anova = aov(modelo)
print(anova(m0, m1, m2, m3, m4, m5, m6, m7, modelo))


#### FIT POINTS ####
fitpoints = predict(modelo)
print(fitpoints)
fit = lm(y~x1+x2+x3+x4)
broom::augment(fit)
leveragePlots(fit, layout = c(2,2))


base3=base[-3-6-8,]

#### DEFINIR ELEMENTOS ####
nx1=as.vector(base3[,1])
nx2=as.vector(base3[,2])
nx3=as.vector(base3[,3])
nx4=as.vector(base3[,4])
ny=as.vector(base3[,5])

#### MODELO ####
nm5 = lm(ny~nx1+nx2+nx4)

#### REPORTES ####
summary(nm5)
AIC(nm5)











