# ELIAS CARDONA RODRIGUEZ - LITC 4A
# FECHA -> 15/FEB/2024

InfoBase=read.csv(file.choose(), header=TRUE)
# Definir las variables
valor=as.vector(InfoBase[,1])
tratamiento=as.vector(InfoBase[,2])

# Realizar prueba de homocedasticidad
hip1 <- bartlett.test(valor~tratamiento)
print(hip1)


# Analisis de varianza
anova1=aov(valor~tratamiento)

cat("------------- ANOVA SUMMARY OF tratamiento ----------------", "  ", sep="\n")
print(summary(anova1))



# Diagrama de caja y brazos para los tratamientos
boxplot(valor~tratamiento, main="Resistencia ~ Tipo de plástico",
xlab="Tipo de plástico", ylab="Resistencia del plástico", col="#10A5F5")


# Prueba Tukey
tukey <- TukeyHSD(anova1, conf.level=0.95)
print(tukey)


