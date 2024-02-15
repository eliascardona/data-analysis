# ==========================0
#  Elias Cardona Rodríguez
#  Lic. en Informática 4°A
#  Métodos estadísticos
#  Fecha: 14/feb/2024
# ==========================0

#  Objetivo, realizar el análsis de varianza (prueba
#  anova) a un solo factor de los datos
#  mostrados a continuación

datos <- data.frame(
	Plastico = rep(c("Plastico 1", "Plastico 2", "Plastico 3", "Plastico 4"), each=4),
	Resistencia = c(
		# Plástico 1
		135, 175, 97, 169, 213, 171, 115, 143,
		# Plástico 2
		275, 170, 154, 133, 219, 187, 220, 185,
		# Plástico 3
		169, 239, 184, 222, 253, 179, 280, 193,
		# Plástico 4
		115, 105, 93, 85, 120, 74, 87, 63
	)
)


modelo_anova <- aov(Resistencia ~ Plastico, data = datos)
print(summary(modelo_anova))






