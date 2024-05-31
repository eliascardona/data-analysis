#### DECLARACION DE LIBRERIAS A UTILIZAR ####
library(car)
library(plot3D)
library(rgl)
library(plot3Drgl)

#### ABRIR LOS DATOS ####
InfoBase=read.csv(file=file.choose(),header=TRUE)

#### DEFINIR ELEMENTOS ####
y=as.vector(InfoBase[,5])
x1=as.vector(InfoBase[,2])
x2=as.vector(InfoBase[,3])
x3=as.vector(InfoBase[,4])

#### MATRIZ DE GRAFICOS DE DISPERSIÓN ####
InfoBase1=InfoBase[,-1]

#### AJUSTANDO NUESTRA INFORMACION BASE ####
# plot(InfoBase1)


#### MATRIZ DE CORRELACIÓN ####
# print(round(cor(InfoBase1),4))


#### MODELO DE REGRESIÓN LINEAL MÚLTIPLE ###
modelo=lm(y~x1+x2+x3)
print(coef(modelo))

#### ANOVA SE REALIZA EN EXCEL ####


#### PRUEBA INDIVIDUAL CON t0 ####
print(summary(modelo))


#### INTERVALOS DE CONFIANZA ####
print(confint(modelo,conf.level=0.95))


#### PRUEBA DE NORMALIDAD DE RESIUDOS ####
residuos = rstandard(modelo)
qqnorm(residuos, col="blue")
#qqline(residuos, col="red")


#### VALORES AJUSTADOS ####
vajustados=fitted(modelo)
#plot(vajustados,residuos)


x1.pred=seq(min(x1),max(x1),length.out=40)
x2.pred=seq(min(x2),max(x2),length.out=40)
x3.pred=seq(min(x2),max(x2),length.out=20)

#### MATRIZ PREDICTORA ####
x1x2= expand.grid(x1=x1.pred, x2=x2.pred, x3=x3.pred)
##y.pred = matrix(predict(modelo,newdata=x1x2),nrow=40,ncol=40)

#### FIT POINTS ####
fitpoints = predict(modelo)
print(fitpoints)


## scatter3D(x=x1,
##          y=x2,
##          z=y,
##          theta=30,
##          phi=8,pch=20,
##          bty="g",
##          colkey=TRUE,
##          surf=list(x=x1.pred,y=x2.pred,z=y.pred,fit=fitpoints))

#### BOX TIDWELL ####
boxTidwell(y~x1+x2)

#### PRUEBA DE DURBIN-WATSON ####
durbinWatsonTest(modelo, simulate = TRUE)

#### NCV TEST ####
ncvTest(modelo)
spreadLevelPlot(modelo)


##  Si el valor P es mayor a alfa, entonces hay linealidad













