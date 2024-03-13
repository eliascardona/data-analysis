# Elias Eduardo Cardona Rodríguez
# Lic.en Informática
# Práctica 4 - Ejercicio 1

InfoBase=read.csv(file.choose(), header=TRUE)
valor=as.vector(InfoBase[,1])
tratamiento=as.vector(InfoBase[,2])

# Anova
anova=aov(valor~tratamiento)
summary(anova)

tukey<-TukeyHSD(anova, conf.level = 0.95)
tukey

