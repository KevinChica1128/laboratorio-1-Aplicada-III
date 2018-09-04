# Kevin García - 1533173
# Alejandro Vargas -
# Laboratorio 1 - Aplicada III
# Datos Importaciones

#Se carga la base de datos
library(readxl)
Importaciones <- read_excel("Importaciones.xlsx", 
                            col_types = c("numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric"))
View(Importaciones)

## Función para sd (1/n)
sd2 <- function (x) {
  
  sqrt(sum((x - mean(x))^2) / (length(x)))
  
} 

x0<-Importaciones$Año
x1<-Importaciones$Colombia
x2<-Importaciones$Brasil
x3<-Importaciones$Chile
x4<-Importaciones$Argentina
x5<-Importaciones$Ecuador
x6<-Importaciones$Peru

#Estandarización

z1 <- (x1 - mean(x1))/sd2(x1) 
z2 <- (x2 - mean(x2))/sd2(x2)
z3 <- (x3 - mean(x3))/sd2(x3)
z4 <- (x4 - mean(x4))/sd2(x4)
z5 <- (x5 - mean(x5))/sd2(x5)
z6 <- (x6 - mean(x6))/sd2(x6)

Z <- matrix(c(z1,z2,z3,z4,z5,z6),20,6)


R <- (t(Z)%*%Z) *(1/20) ## Matriz de Correlación Manual

R1<- cor(Importaciones[,-1]) ## Matriz de correlación con función

# Descomposición en valores y vectores propios

dv <- eigen(R) ## Valores y Vectores propios

u <- dv$vectors
l <- dv$values

sum(l)

t <- Z%*%u  ## Componentes en Rp

## Representación de los Individuos en el primer Plano Factorial

x11()
plot(t[,1],t[,2],type = "b",xlab="Componente 1",ylab="Componente 2",main = "Representación de los individuos en el primer plano principal con sus trayectorias")
text(t[,1],t[,2],x0, cex=0.6, pos=1, col=4)
#Con la función de R
library(FactoMineR)
x11()
PCA.results <- PCA(Importaciones[,-1])

library(factoextra)

x11()
fviz_pca_biplot(PCA.results)


#Varianza explicada por las componentes principales
library(ggplot2)
l
prop_varianza<-l/sum(l)
x11()
ggplot(data = data.frame(prop_varianza, pc = 1:6),
       aes(x = pc, y = prop_varianza)) +
  geom_col(width = 0.3) +
  scale_y_continuous(limits = c(0,1)) +
  theme_bw() +
  labs(x = "Componente principal",
       y = "Prop. de varianza explicada")