#### ABRIR LOS DATOS ####
base=read.csv(file=file.choose(),header=TRUE)

#### DEFINIR ELEMENTOS ####
x1=as.vector(base[,1])
x2=as.vector(base[,2])
x3=as.vector(base[,3])
x4=as.vector(base[,4])
y=as.vector(base[,5])

#### MODELO ####
m5 = lm(y~x1+x2+x4)

#### REPORTES ####
summary(m5)
AIC(m5)

