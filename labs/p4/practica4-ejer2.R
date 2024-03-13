# Elias Eduardo Cardona Rodríguez
# Lic.en Informática
# Práctica 4 - Ejercicio 2

InfoBase2=read.csv(file.choose(), header=TRUE)
valores=as.vector(InfoBase2[,1])
tratamientos=as.vector(InfoBase2[,2])
bloques=as.vector(InfoBase2[,3])


# Anova
anova2=aov(valores~bloques+tratamientos)
summary(anova2)

tukey2<-TukeyHSD(anova2, conf.level = 0.95)
tukey2
