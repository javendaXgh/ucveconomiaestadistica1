# libro 2.1.1 al 2.1.4

library(ggplot2)
library(openintro)
library(DescTools)

#### dotplots
loan50$interest_rate
sort(loan50$interest_rate)
max(loan50$interest_rate)
min(loan50$interest_rate)
mean(loan50$interest_rate)
median(loan50$interest_rate)
Mode(loan50$interest_rate)

round(loan50$interest_rate,0)
plot(x= loan50$interest_rate, 
     y=rep(0,length(loan50$interest_rate)),
     main='Plot de puntos: Tasa de interés',
     xlab='%',
     ylab='',
     pch= 1)

par(new=TRUE)
points(mean(loan50$interest_rate),
       -.05, 
       pch = 17, 
       col = "red",
       cex = 2)

# balanza en el promedio que es el triángulo rojo

# stacked -apilado
loan50_redondeado <- loan50%>%
  mutate(interest_rate= round(interest_rate,0 ))

loan50_redondeado$interest_rate
sort(loan50_redondeado$interest_rate)
max(loan50_redondeado$interest_rate)
min(loan50_redondeado$interest_rate)
mean(loan50_redondeado$interest_rate)
median(loan50_redondeado$interest_rate)
Mode(loan50_redondeado$interest_rate)

ggplot(loan50_redondeado, 
       aes(x = interest_rate)) + 
  geom_dotplot()+
  scale_y_continuous(NULL, breaks = NULL)

# cuál es el valor más común (moda)
# donde se presenta una mayor densidad

ggplot(loan50_redondeado, 
       aes(x = interest_rate)) + 
  geom_dotplot()+
  scale_y_continuous(NULL, breaks = NULL)+
  annotate("pointrange",
           x = mean(loan50$interest_rate),
           y = -.01, 
           ymin = -.01,
           ymax = .2,
           colour = "red")

ggplot(loan50_redondeado, 
       aes(x = interest_rate)) + 
  geom_dotplot()+
  scale_y_continuous(NULL, breaks = NULL)+
  annotate("pointrange",
         x = min(loan50_redondeado$interest_rate),
         y = -.01, 
         ymin = -.01,
         ymax = .5,
         colour = "blue")+
  annotate("pointrange",
           x = max(loan50_redondeado$interest_rate),
           y = -.01, 
           ymin = -.01,
           ymax = .5,
           colour = "blue")

max(loan50_redondeado$interest_rate)-min(loan50_redondeado$interest_rate)

# sesgo a la derecha
ggplot(data= loan50_redondeado, aes(x=interest_rate))+
  geom_histogram(bins=8, 
                 fill='blue')

ggplot(data= loan50_redondeado, aes(x=interest_rate))+
  geom_histogram(bins=12, 
                 fill='blue')


mean(loan50$interest_rate)
sd(loan50$interest_rate)
summary(loan50$interest_rate)
####################################################

set.seed(5313)
babies_subset <- babies%>%
  sample_n(50)

babies_subset$age
min(babies_subset$age)
max(babies_subset$age)
max(babies_subset$age)-min(babies_subset$age)

ggplot(data= babies_subset) + 
  geom_dotplot(aes(x = age))+
  scale_y_continuous(NULL, breaks = NULL)

# ggplot(data= babies_subset) + 
#   geom_dotplot(aes(x = age))+
#   scale_y_continuous(NULL, breaks = NULL)+
#   annotate("point",
#            x = mean(babies_subset$age),
#            y = -.01, 
#            col = "red")


ggplot(data= babies_subset) +
  geom_dotplot(aes(x = age),
               fill = "steelblue") + 
  scale_y_continuous(NULL, breaks = NULL)+
  annotate('point',
           x=mean(babies_subset$age),
           y=-.01,
           col='red')


ggplot(data=babies_subset, 
       aes(x=age))+
  geom_histogram()

ggplot(data=babies_subset, 
       aes(x=age))+
  geom_histogram(bins = 15)

ggplot(data=babies_subset, 
       aes(x=age))+
  geom_histogram(bins = 10)

ggplot(data=babies, 
       aes(x=age))+
  geom_histogram()

# qué tipo de ..modal es?

hist(babies$age, breaks = 21)



#### simulacion datos no agrupados. variable cualitativa
set.seed(5431) # generación de semi
# generar muestra aleatoria simulada para 12 personas que tienen entre 0 y 4 hermanos
library(dplyr)
calificacion <- sample(c('⭐️',
                          '⭐️⭐️',
                          '⭐️⭐️⭐️',
                          '⭐️⭐️⭐️⭐️',
                          '⭐️⭐️⭐️⭐️⭐')
                       , size= 30, 
                       replace= TRUE,
                       prob = c(.1,.1,.2,.4,.2))
calificacion
# crear las clases (intervalos) para 1 y 2\\

table(calificacion)%>%
  as_data_frame()%>%
  dplyr::rename(fi=n)%>%
  dplyr::rename(Xi=1)%>%
  mutate(hi= (fi/sum(fi))*100)%>%
  mutate(Fi= cumsum(fi))%>%
  mutate(Hi= cumsum(hi))

df_calificacion <- table(calificacion)%>%
  as_data_frame()%>%
  dplyr::rename(fi=n)%>%
  dplyr::rename(Xi=1)%>%
  mutate(hi= (fi/sum(fi))*100)%>%
  mutate(Fi= cumsum(fi))%>%
  mutate(Hi= cumsum(hi))

library(plyr)

calificacion2 <- as.numeric( mapvalues(calificacion,
                                       c('⭐️','⭐️⭐️','⭐️⭐️⭐️','⭐️⭐️⭐️⭐️',"⭐️⭐️⭐️⭐️⭐"),
                                       1:5))

calificacion2

hist(calificacion2, breaks = 4)


#####
profesor1 <- rnorm(35, mean=12,sd=2)
max(profesor1)
min(profesor1)
mean(profesor1)
median(profesor1)

profesor2 <-  rnorm(35, mean=12.7,sd=2.8)
max(profesor2)
min(profesor2)
mean(profesor2)
median(profesor2)



hist(profesor2, col='blue',,breaks = 8)
par(new=TRUE)
hist(profesor1, col='red',breaks = 8)
(profesor2)



####
# Fijamos una semilla para la reproducibilidad de los datos
set.seed(123)

# Definimos los parámetros de las distribuciones
media1 <- 13
media2 <- 14
desviacion_estandar1 <- 1.5
desviacion_estandar2 <- 3

# Generamos datos simulados para ambas distribuciones
n_datos <- 1000
datos1 <- rnorm(n_datos, mean = media1, sd = desviacion_estandar1)
datos2 <- rnorm(n_datos, mean = media2, sd = desviacion_estandar2)

# Filtramos los datos para que no excedan el límite superior de 20
datos1_filtrados <- datos1[datos1 <= 20]
datos2_filtrados <- datos2[datos2 <= 20]

# Creamos un rango de valores para graficar las distribuciones teóricas
x <- seq(min(datos1_filtrados, datos2_filtrados), 20, length.out = 200)

# Calculamos las densidades de probabilidad para cada distribución
densidad1 <- dnorm(x, mean = media1, sd = desviacion_estandar1)
densidad2 <- dnorm(x, mean = media2, sd = desviacion_estandar2)

# Graficamos las distribuciones
plot(x, densidad1, type = "l", col = "blue", lwd = 2,
     xlab = "Valores", ylab = "Densidad",
     main = "Comparación de Distribuciones Normales",
     ylim = c(0, max(densidad1, densidad2) * 1.1)) # Ajustamos el límite superior del eje y

lines(x, densidad2, col = "red", lwd = 2)

# Agregamos una leyenda para identificar las distribuciones
legend("topright",
       legend = c(paste("DE =", desviacion_estandar1), paste("DE =", desviacion_estandar2)),
       col = c("blue", "red"), lty = 1, lwd = 2)

# Podemos agregar líneas verticales en la media para referencia
abline(v = media1, lty = 2, col = "gray")
abline(v = media2, lty = 2, col = "orange")
text(media1 + 0.5, max(densidad1, densidad2) * 0.9, paste("Media =", media1), pos = 4, col = "gray")

