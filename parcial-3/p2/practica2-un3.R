#### DECLARACION DE LIBRERIAS A UTILIZAR ####
library(car)
library(plot3D)
library(rgl)
library(plot3Drgl)
#library(lmtest)

#### ABRIR LOS DATOS ####
base=read.csv(file=file.choose(),header=TRUE)

#### DEFINIR ELEMENTOS ####

x1=as.vector(base[,1])
x2=as.vector(base[,2])
x3=as.vector(base[,3])
x4=as.vector(base[,4])
y=as.vector(base[,5])


#### MATRIZ DE GRAFICOS DE DISPERSIÓN ####

#### AJUSTANDO NUESTRA INFORMACION BASE ####
#plot(base)


#### MATRIZ DE CORRELACIÓN ####
#print(round(cor(base),5))


#### MODELO DE REGRESIÓN LINEAL MÚLTIPLE ###
modelo=lm(y~x1+x2+x3+x4)
#print(coef(modelo))

#### ANOVA SE REALIZA EN EXCEL ####


#### PRUEBA INDIVIDUAL CON t0 ####
#print(summary(modelo))


#### INTERVALOS DE CONFIANZA ####
#print(confint(modelo,conf.level=0.97))


#### PRUEBA DE NORMALIDAD DE RESIUDOS ####
#residuos = rstandard(modelo)

#qqnorm(residuos, col="blue")
#qqline(residuos, col="red")


#### VALORES AJUSTADOS ####
vajustados=fitted(modelo)
#plot(vajustados,residuos)

#x1.pred=seq(min(x1),max(x1),length.out=40)
#x2.pred=seq(min(x2),max(x2),length.out=40)
#x3.pred=seq(min(x3),max(x3),length.out=40)
#x4.pred=seq(min(x4),max(x4),length.out=40)





## scatter3D(x=x1,
##          y=x2,
##          z=y,
##          theta=30,
##          phi=8,pch=20,
##          bty="g",
##          colkey=TRUE,
##          surf=list(x=x1.pred,y=x2.pred,z=y.pred,fit=fitpoints))


#### BOX TIDWELL   |   LINEALIDAD   ####
#boxTidwell(y~x1+x2)

#### DURBIN-WATSON   |   INDEPENDENCIA DE RESIUDOS ####
#durbinWatsonTest(modelo, simulate = TRUE)

#### NCV TEST ####
#ncvTest(modelo)
#spreadLevelPlot(modelo)
##Si el valor P es mayor a alfa, entonces hay linealidad

#### HOMOCEDASTICIDAD ####
#bptest(modelo, ~I(x1^x2) + I(x1^x2) + I(x1*x2))


## Una vez eliminadas las variables que no son significativas,
## probamos la multicolinealidad de nuestro modelo (con las variables restantes)

base2 = base[-4]

print(round(cor(base2), 2))
print(cor(base2))



## Comparacion de nuestro modelo original con nuestros nuevos modelos

m0 = lm(y~x1)
m1 = lm(y~x1+x2)
m2 = lm(y~x1+x3)
m3 = lm(y~x1+x4)
m4 = lm(y~x1+x2+x3)
m5 = lm(y~x1+x2+x4)
m6 = lm(y~x1+x3+x1)




print("=================================== summary de todos los modelos ================================================")
print(summary(modelo))
print(summary(m0))
print(summary(m1))
print(summary(m2))
print(summary(m3))
print(summary(m4))
print(summary(m5))
print(summary(m6))





print("=================================== Pruebas akaike ================================================")
print(AIC(modelo))
print(AIC(m0))
print(AIC(m1))
print(AIC(m2))
print(AIC(m3))
print(AIC(m4))
print(AIC(m5))
print(AIC(m6))



print("=================================== anova final  ================================================")
anova = aov(modelo)
print(anova(m0, m1, m2, m3, m4, m5, m6, modelo))


#### FIT POINTS ####
fitpoints = predict(modelo)
#print(fitpoints)

fit = lm(y ~ x1 + x2 + x3 + x4)

broom:argument(fit)
leveragePlots(fit, layout = c(2,2) )



























