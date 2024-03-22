# ELIAS CARDONA RODRIGUEZ - LITC 4A
# Anova por dos vías (problema del fertilizante)
# FECHA -> 20/FEB/2024
library(gplots)

base=read.csv(file=file.choose(), header=TRUE)
RENDIMIENTO=as.vector(base[,1])
METODO=factor(as.vector(base[,2]))
FERTILIZANTE=factor(as.vector(base[,3]))

# Prueba de homocedasticidad
barlett1 <- bartlett.test(RENDIMIENTO~FERTILIZANTE)
barlett2 <- bartlett.test(RENDIMIENTO~METODO)

# Anova
anova=aov(RENDIMIENTO~FERTILIZANTE+METODO)
print(summary(anova))

# GRAFICO DE MEDIAS de los metodos de ensamble y de los operarios
#plotmeans(RENDIMIENTO~FERTILIZANTE)
plotmeans(RENDIMIENTO~METODO)

# Probar la normalidad de los residuos ####
sha1 <- shapiro.test(residuals(anova))
print(sha1)

# Caja y brazo
# boxplot(RENDIMIENTO~FERTILIZANTE,col=rainbow(4),main="Rendimiento de ferts", xlab="Bloque", ylab="Rendimiento")

# Prueba Tukey
tukey = TukeyHSD(anova, conf.level=0.95)
print(tukey)


