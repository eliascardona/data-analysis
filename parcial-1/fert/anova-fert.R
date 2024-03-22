# ELIAS CARDONA RODRIGUEZ - LITC 4A
# Anova por dos vías (problema del fertilizante)
# FECHA -> 20/FEB/2024

InfoBase=read.csv(file.choose(), header=TRUE)
# Definir las variables
valor = as.vector(InfoBase[,1])
tratamiento = factor(as.vector(InfoBase[,2]))
bloque = as.vector(InfoBase[,3])


# Analisis de varianza
anova1=aov(valor~tratamiento+bloque)


# print fmt
cat(" ", " ", "FINAL ANOVA SUMMARY", ".........................................", sep="\n")
print(summary(anova1))





# Diagrama de caja y brazos para los tratamientos
boxplot(valor~bloque, xlab="Valores", ylab="Bloque", main="Distribución por bloque")


# Prueba Tukey
tukey = TukeyHSD(anova1, conf.level=0.95)


cat(" ", " ", "TUKEY REPORT", ".........................................", sep="\n")
print(tukey)


