#usar https://ucveconomia-estadistica1.netlify.app/clase_08_apoyo
# https://colab.research.google.com/github/javendaXgh/ucveconomiaestadistica1/blob/main/notebooks/Clase_10.ipynb#scrollTo=XRZ96eUPvW2i
# extraer crecimiento gdppercap venezuela
# recomendación promedio a usar
indicador <- 'NY.GDP.DEFL.KD.ZG' #'FP.CPI.TOTL.ZG'
### 
library(WDI)  
inflacion_jp <- WDI(indicator = indicador, 
                     country = "JP",   
                     start = 1960,
                     end = 2024) 


inflacion_japon <- inflacion_jp%>%
  janitor::clean_names()%>%
  filter(between(year, 1961,2023))%>%
  arrange(year)%>%
  mutate(inflacion= round(ny_gdp_defl_kd_zg,1))%>%
  select(4,6)

write.csv(inflacion_japon,'data/inflacion_japon.csv')

View(inflacion_japon)

# fuente https://data.worldbank.org/indicator/NY.GDP.DEFL.KD.ZG
# Banco Mundial
inflacion_japon <- read.csv(...)

# media, mediana, varianza, desviacion tipica. 

# señalar ^ para elevar al cuadrado
inflacion_japon$inflacion[1:9]

inflacion_japon$year[1:9]

# media recomendada a usar
inflacion_japon$inflacion[2:5]

inflacion_japon$year[2:5]

hist(inflacion_japon$inflacion)

library(ggplot2)
ggplot(data=inflacion_japon, aes(x=inflacion))+
  geom_dotplot()+
  annotate('point',
           x=mean(inflacion_japon$inflacion),
           y=-.01,
           col='red', 
           size=2)


ggplot(data=inflacion_japon,
       aes(y=inflacion))+
  geom_boxplot()

ggplot(data=inflacion_japon, aes(x=inflacion))+
  geom_dotplot()+
  annotate('point',
           x=median(inflacion_japon$inflacion),
           y=-.01,
           col='red', 
           size=2)

ggplot(data=inflacion_japon,
       aes(y=inflacion))+
  geom_boxplot()+
  coord_flip()
  
boxplot(inflacion_japon$inflacion)

# relacion entre boxplot y dotplot?


# cuartiles
# primer percentil.  cuántos datos hay
summary(inflacion_jp_sub$inflacion) # no usar esta
# RIC
# calcular los valores de la línea de valores atípicos superior e inferior
# valores atípicos observados en la serie


valores <- c(3,3,5,6,7,8,8,8,9,9,9,10,12,15,18,18)
median(valores)

sum(valores)
length(valores)

promedio <- sum(valores)/length(valores)
mean(valores)

sd(valores)
hist(valores, beans=3)



cdad_hermanos <- sample(0:4, 12 ,replace=TRUE)
table(cdad_hermanos)


table(cdad_hermanos)%>%
  as_data_frame()

# se expresan en intervalos o en clases
table(cdad_hermanos)%>%
  as_data_frame()%>%
  dplyr::rename(clases= cdad_hermanos, fi= n)%>%
  mutate(clases=as.numeric(clases))%>%
  mutate(pto_medio = (lag(clases)+clases)/2)


cdad_hermanos <- sample(0:4, 12 ,replace=TRUE)
table(cdad_hermanos)


table(cdad_hermanos)%>%
  as_data_frame()

# se expresan en intervalos o en clases
table(cdad_hermanos)%>%
  as_data_frame()%>%
  dplyr::rename(clases= cdad_hermanos, fi= n)%>%
  mutate(clases=as.numeric(clases))%>%
  mutate(pto_medio = (lag(clases)+clases)/2)
mutate(clases=as.numeric(clases),
       pto_medio = (lag(clases)+clases)/2,
       clases2= paste(lag(clases),'-',clases),
       hi= (fi/sum(fi))*100)



cdad_hermanos%>%
  as_data_frame()%>%
  group_by(value) %>%
  dplyr::summarise(frequency = n()) %>%
  arrange(desc(frequency))
# caso 2
muestra_clases <- hist(cdad_hermanos, breaks=4, plot=FALSE)
muestra_clases
# group_by(clases)%>%
# mutate()%>%
# mutate()%>%
# slice(-1)
muestra_clases <- hist(cdad_hermanos, breaks=4, plot=FALSE)
muestra_clases
table(cdad_hermanos)


####
datos <- c(1, 2, 3, 4, 5)
media_aritmetica <- mean(datos)
media_aritmetica 


#media_ponderada
valores <- c(10, 20, 30, 40)
pesos <- c(0.4, 0.3, 0.2, 0.1)
media_ponderada <- weighted.mean(valores, pesos)
media_ponderada

## media armónica
harmonic_mean <- function(x) {
  n <- length(x)
  n / sum(1/x)
}

datos <- c(40, 60)
media_armonica <- harmonic_mean(datos)
media_armonica


### media geométrica
geometric_mean <- function(x) {
  prod(x)^(1/length(x))
}

prod(c(1.10, 1.20, 1.05))

tasas <- c(1.10, 1.20, 1.05)
media_geometrica <- geometric_mean(tasas)
print(media_geometrica) 

geometric.mean(tasas)



########
library(gapminder)
library(dplyr)
gap_2007 <- gapminder%>%
  filter(year==2007)

sd(gap_2007$lifeExp)
densityPlot(gap_2007$lifeExp,histo='faded')
hist(gap_2007$lifeExp)

summary(gap_2007$lifeExp)

IQR(gap_2007$lifeExp)

quantile(gap_2007$lifeExp, 0.25)
quantile(gap_2007$lifeExp, c(0.1,.9))
quantile(gap_2007$lifeExp, c(0.01,.99))
quantile(gap_2007$lifeExp, seq(.1:1,by=.1))

quantile(gap_2007$lifeExp, 0.75) +1.5*IQR(gap_2007$lifeExp) 
quantile(gap_2007$lifeExp, 0.25) -1.5*IQR(gap_2007$lifeExp) 
min(gap_2007$lifeExp)

boxplot(data= gapminder,
        gdpPercap~ continent, 
        col='orange')

boxplot(data= gapminder,
        gdpPercap~ continent, 
        col='orange')

boxplot(data= gapminder,
        gdpPercap~ continent, 
        subset = year==2007,
        col='orange')


boxplot(data= gap_2007,
        gdpPercap~ continent, 
        col='orange')

boxplot(data= gap_2007,
        gdpPercap~ continent, 
        col='orange',
        titl)

# graficar con ggplot
p <- ggplot(mpg, aes(class, hwy))
p + geom_boxplot()

ggplot(data=gap_2007,aes(continent,lifeExp))+
  geom_boxplot(fill='orange')

ggplot(data=gap_2007,aes(continent,lifeExp))+
  geom_boxplot(fill='orange',
               outlier.colour = "red", 
               outlier.shape = 1)



df <- data.frame(
  x = 1,
  y0 = min(gap_2007$lifeExp),
  y25 = quantile(gap_2007$lifeExp, 0.25),
  y50 = median(gap_2007$lifeExp),
  y75 = quantile(gap_2007$lifeExp, 0.75),
  y100 = max(gap_2007$lifeExp)
)
ggplot(df, aes(x)) +
  geom_boxplot(
    aes(ymin = y0, 
        lower = y25, 
        middle = y50, 
        upper = y75, 
        ymax = y100),
    stat = "identity")


library(gapminder)

boxplot(data= gapminder, 
        x=gapminder$lifeExp , 
        subset = gapminder$year == 2007, 
        col = 'orange')
