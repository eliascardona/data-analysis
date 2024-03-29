# ----------------------------------------------------
# ELIAS EDUARDO CARDONA RODR�GUEZ
# DISE�O DE EXPERIENTOS POR MEDIO DE DISE�O FACTORIAL
# LIC. EN INFORM�RICA  4�A
# ----------------------------------------------------
InfoBase=read.csv(file=file.choose(),header=TRUE)

## DEFINIR ELEMENTOS ##
profundidad=as.vector(InfoBase[,1])
velocidad=as.vector(InfoBase[,2])
acabado=as.vector(InfoBase[,3])
tratamientos=as.vector(InfoBase[,4])

efecA=factor(profundidad)
efecB=factor(velocidad)

print(InfoBase)

### PROBAR HOMOCEDASTICIDAD ###

bltt <- bartlett.test(acabado~tratamientos)
print(bltt)


## ANOVA ##
anova = aov(acabado~(efecA+efecB)^2)
print(summary(anova))

anova2 = aov(acabado~(efecA*efecB))
print(summary(anova2))

### INTERACCION DE FACTORES ###
library(phia)
Grafica=interactionMeans(anova)
plot(Grafica)

### PRUEBA PARA VERIFICAR LA NORMALIDAD DE LOS RESIDUOS ###
sha2 <- shapiro.test(residuals(anova))
print(sha2)



## TUKEY ##
tuk <- TukeyHSD(anova,conf.level=0.95)
print(tuk)




