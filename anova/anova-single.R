# ==========================0
#  Elias Cardona Rodríguez
#  Lic. en Informática 4°A
#  Métodos estadísticos
#  Fecha: 12/feb/2024
# ==========================0

#  Objetivo, realizar el análsis de varianza (prueba
#  anova) a un solo factor de los datos
#  mostrados a continuación

datos <- data.frame(
	A = c(23, 28, 21, 27, 95, 41, 37, 30, 32, 36),
	B = c(35, 36, 29, 40, 43, 49, 51, 28, 50, 52),
	C = c(50, 43, 36, 34, 45, 52, 52, 43, 44, 34)
)

modelo_anova <- aov(A ~ datos)
summary(modelo_anova)

