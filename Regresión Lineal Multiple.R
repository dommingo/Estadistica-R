############################################################
# Variables en el conjunto de datos:
#
# year - year
# hour - hour of the day (0-23)
# season -  1 = spring, 2 = summer, 3 = fall, 4 = winter 
# holiday - whether the day is considered a holiday
# workingday - whether the day is neither a weekend nor holiday
# weather - 1: Clear, Few clouds, Partly cloudy, Partly cloudy
#           2: Mist + Cloudy, Mist + Broken clouds, Mist + Few clouds, Mist
#           3: Light Snow, Light Rain + Thunderstorm + Scattered clouds, Light Rain + Scattered clouds
#           4: Heavy Rain + Ice Pallets + Thunderstorm + Mist, Snow + Fog 
# temp - temperature in Celsius
# atemp - "feels like" temperature in Celsius
# humidity - relative humidity
# windspeed - wind speed
# variable respuesta: count - number of total rentals
#
############################################################

####### PARTE 1: CONSTRUCCION DEL MODELO ########

## 1

rm(list=ls())  # Si ejecutamos rm(list=ls())  borramos todos los objetos de la memoria

############################################################
# Lectura de datos y inspeccion
############################################################
## 2

setwd('/Users/Domm/Desktop/MBD/2_Estadistica/20_11_18/Practica_1/Datos')                                         
datos <- read.table('p1_train.csv',sep = ';',header=TRUE,dec=".")

## 3

datos <- datos[ ,!colnames(datos) =="id"]  # Borramos la columna id ya que no se va a utilizar en la practica.

dim(datos)      # Contamos las dimensiones (numero de filas y columnas)
head(datos)     # Visualizamos la cabezera de los datos
summary(datos)  # Hacemos una descriptiva de los datos

############################################################
# Convertir variables no numericas a factores
############################################################
## 4

# Existen variables que deben ser transformadas a categoricas

datos$year <- factor(datos$year) 
datos$season <- factor(datos$season) 
datos$holiday <- factor(datos$holiday) 
datos$workingday <- factor(datos$workingday) 
datos$weather <- factor(datos$weather) 

## 5

par(mfrow=c(2,2))

boxplot(datos$count~datos$hour, main="count vs franja horaria",xlab="Horas del dia",ylab="count")
with(datos,lines(lowess(count~hour),col=2))
plot(count~hour,datos,xlab="Horas del dia",ylab="count",
     main="count por franja horaria",col=4,pch=19,cex=0.8,las=1)  # Numero de bicicletas alquiladas en funcion de la franja horaria
with(datos,lines(lowess(count~hour),col=2))

intervals <- c(0,7,9,16,20,24)  # Creamos los puntos de corte para las franjas horarias

datos$hour_intervals <- cut(datos$hour,breaks = intervals,right = FALSE)
boxplot(datos$count~datos$hour_intervals, main="count vs franja horaria",xlab="Intervalo de horas",ylab="count")
with(datos,lines(lowess(count~hour_intervals),col=2))

datos$hour2 <- ifelse (datos$hour >= 7 & datos$hour < 21,'[7,21)','[21,7)') # Creamos dos intervalos para comparar con
datos$hour2 <- factor(datos$hour2)                                          # nuestro modelo de intervalos
boxplot(datos$count~datos$hour2, main="count vs franja horaria",xlab="Intervalo de horas",ylab="count")
with(datos,lines(lowess(count~hour2),col=2))

############################################################
# Descriptiva
############################################################
## 6

pairs(datos[,c(2,7,8,9,10,11)]) # Hacemos una descriptiva seleccionando las columnas que son variables numericas

############################################################
# Eliminamos variables muy correlacionadas
############################################################
## 7

# Existen 2 variables muy correlacionadas, la variable temp y la variable atemp, como las 2 contienen gran parte de informacion de la otra
# vamos a hacer 2 plots, count vs temp y count vs atemp de esta manera observaremos que variable predice peor la respuesta, entonces,
# podremos descartar con el valor de R^2 mas bajo.

par(mfrow=c(1,2))

mod.lm0 <- lm(count~temp,datos)    # Hacemos un ajuste del modelo lineal con la intruccion lm, temp en funcion de atemp, los dos en ºC                                  
mod.lm1 <- lm(count~atemp,datos)   # Hacemos un ajuste del modelo lineal con la intruccion lm, atemp en funcion de temp, los dos en ºC  

mod.lm0                            # Vemos los coeficientes de los 2 modelos
mod.lm1                           

plot(count~temp,datos)             # Hacemos los plots correspondientes a cada modelo
abline(mod.lm0,col="red")
plot(count~atemp,datos)
abline(mod.lm1,col="red")

summary(mod.lm0)                   # count = Bo + Bi·temp + Ei      Bo = 4.00 ;  Bi = 9.247   Ei = 167.2   R^2 = 0.1578                                        
summary(mod.lm1)                   # count = Bo + Bi·atemp + Ei     Bo = -7.30 ; Bi = 8.386   Ei = 167.6   R^2 = 0.1537              

## Como podemos observar la variable atemp predice peor la respuesta con un R^2 = 0.1537, entonces vamos a prescindir de ella eliminandola.

datos2 <- datos[ ,!colnames(datos) =="atemp"]  # Borramos la variable atemp y guardamos los datos en otra variable datos2.

############################################################
# Descriptiva bivariante con la respuesta
############################################################
## 8a

par(mfrow=c(2,2))

for(i in c(2,7,8,9)){
  plot(datos2$count~datos2[,i],main=names(datos2)[i],xlab=names(datos2)[i],ylab="count")
  with(datos2,lines(lowess(count~datos2[,i]),col=2))
}

# Podemos observar que en el plot de count vs hour no presenta una relacion lineal ya que observamos una curvatura en el suavizado, mientras que en los
# otros 3 plot, count vs temps, count vs humidity y count vs windspeed se observa que hay presente una relacion lineal, aun asi, se tendria que hacer
# un pequeño ajuste para que el suavizado fuera mas una recta ya que como se observa presentan un curvatura muy poco pronunciada.
 
## 8b

par(mfrow=c(1,6))

boxplot(datos$count~datos$year,main = 'count vs años',xlab = 'year',ylab = 'count')
boxplot(datos$count~datos$season,main = 'count vs estaciones',xlab = 'season',ylab = 'count')
boxplot(datos$count~datos$holiday,main = 'count vs dias festivos',xlab = 'holiday',ylab = 'count')
boxplot(datos$count~datos$workingday,main = 'count vs dias laborables',xlab = 'workingday',ylab = 'count')
boxplot(datos$count~datos$weather,main = 'count vs condicion del tiempo',xlab = 'weather',ylab = 'count')
boxplot(datos$count~datos$hour_intervals,main = 'count vs intervalos de horas',xlab = 'Intervalos de horas',ylab = 'count')

# En el boxplot de holiday no influye si el dia es festivo o no, lo mismo pasa en el bloxplot de workingday ya que no influye si es dia laborable o no,
# en canvio en el bloxplot de year hubo un incremento en el alquiler de bicis en 2012 respecto a 2011, lo mismo pasa en el bloxplot de season y weather 
# que influye en la respuesta (alquiler de bicis) dependiendo en que estacion del año estamos o que climatologia nos encontramos.

############################################################
# Seleccion de modelo y variables
############################################################
## 9

datos3 <- subset(datos2, select = -c(2,12))               # Borramos la variable hour y hour2 y nos quedamos con hour_intervals 

datos4 <- subset(datos2, select = -c(2,11))               # Borramos la variable hour y hour_intervals y nos quedamos con hour2


# Haremos 2 comprovaciones con intervalos diferentes en las franjas horarias y observaremos como predicen las variables con el R^2
# y nos quedaremos con el intervalo que prediga mejor

mod.var <- lm(count~.,datos3)      # Hacemos una ajuste lineal con todas las variables
mod.var                            # Vemos los coeficientes de las variables
summary(mod.var)                   # Hacemos un summary para ver todos los datos (R^2, error residual, coeficientes,etc)

mod.var0 <- lm(count~.,datos4)   
mod.var0                         
summary(mod.var0)      

# Podemos observar que nos predice mejor la variable hour_intervals de los datos3 con un R^2 = 0.6334,
# por tanto nos quedamos con la variable mod.var

## 10

mod.var1 <- step(mod.var, k = 2)                     # Seleccionamos automaticamente las variables con la funcion step().
mod.var2 <- step(mod.var, k = log(nrow(datos3)))     # <none> nos dice que variables quedan si no hacemos nada, podemos utilizar
summary(mod.var1)                                    # la instrucción: step(mod.var, k = 2) para el metodo AIC con  k = 2 o
summary(mod.var2)                                    # step(mod.var, k = log(datos3) con k= log(datos3). 
                                                    
# Observamos que con el metodo AIC o BIC la prediccion no tiene un cambio significativo, con AIC el R^2 = 0.6333  y con BIC = 0.6328,
# por este motivo escojeremos la variable mod.var2 que utiliza el metodo BIC ya que las variables descartadas no son relevantes y
# nos rejimos por el principio de parsimonia.

# En la teoria de la informacion nos dice que hay metodos como el AIC y BIC que nos hacen una seleccion de variables que mas se adecuan al modelo,
# con la funcion step() hacemos uso de estos metodos. Hemos utilizado la funcion step() con k = 2 y k = log(n) el sistema nos indica 
# que variables deben ser eliminadas del modelo con un AIC mas pequeño. En este caso las variables workingday, windspeed y holiday. Entonces,
# el modelo sin estas variables queda:  count ~ year + season  + weather + temp + humidity + hour_intervals. De hecho si nos fijamos en el
# summary vemos que todas las variables son muy significativas con un p-valor muy pequeño a excepcion de weather 2, ademas el t-valor tambien es grande
# lo que nos indica que el error asociado a la variable modifica en poca magnitud el valor de dicha vaiable.

############################################################
# Colinealidad
############################################################
## 11

library(car)
vif(mod.var2)

# Podemos unos criterios para el VIF:

# VIF = 1: No existe colinealidad, total ausencia
# 1 < VIF < 5: La regresión se puede ver afectada por cierta colinealidad.
# 5 < VIF < 10: Variable debe se suprimida ya que un 80% su informacion esta contenida en las otras

# Todas las variables estan contenidas en un rango VIF < 5, por consiguiente no eliminaria ninguna de las variables, 
# aunque podemos observar que la variable temp y season estan en el rango 1 < VIF < 5 pero mas cerca del 5. 

############################################################
# Validacion
############################################################
## 12

par(mfrow=c(2,2))            # Graficos para valorar las 3 primeras premisas
plot(mod.var2) 

# Plot para valorar la premisa de independencia seleccionando 500 valores

par(mfrow=c(1,1))
plot(residuals(mod.var2)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')

# Podemos observar que no se cumplen algunas premisas. El plot de Residuals vs Fitted values se observa una curvatura en la linia,
# ademas presenta una distribucion de puntos en forma de embudo, por tanto no cumple las premisas de linealidad ni homoscedasticidad.
# Por otra parte tampoco cumple la premisa de normalidad porque los puntos no se ajustan a la recta de Normalidad. Sin embargo,
# cumple la premisa de independencia de los residuos ya que son uniformes en el tiempo. Entonces deberemos probar algunas transformaciones.

## Vamos a ver una 2 maneras de ver la descriptiva de residuos vs variables predictoras

## Alternativa 1
par(mfrow=c(2,3))
for(i in c(1,2,5,6,7,10)){
  plot(resid(mod.var2)~datos3[,i],main=names(datos3)[i],xlab=names(datos3)[i],ylab="Residuals")
  with(datos3,lines(lowess(resid(mod.var2)~datos3[,i]),col=2))
}

## Alternativa 2
library(car)
residualPlots(mod.var2)  

############################################################
# Transformacion de boxCox
############################################################
## 13

##  Calculamos la lambda mas adecuada para la transformacion de Boxcox y guardamos la nueva variable a countBC

par(mfrow=c(1,1))
bc <- boxCox(mod.var2)
bc
bc$x[which.max(bc$y)]
lamb <- bc$x[which.max(bc$y)]           # Nos da una lambda = 0.3030303
datos3$countBC <- datos3$count^lamb 

## Hacemos otra transformacion a la variable respuesta aplicando el logaritmo y lo guardamos en la variable countLog
datos3$countLog <- log(datos3$count)

############################################################
# Nuevos modelos con respuestas transformadas
############################################################
## 14

# Volvemos a ajustar los modelos a las nuevas respuesta creadas anteriormente countBC y countLog

mod.var3 <- lm(countBC~year + season + weather + temp + humidity + hour_intervals, datos3)
summary(mod.var3)  # R^2 = 0.7461

mod.var4 <- lm(countLog~year + season + weather + temp + humidity + hour_intervals, datos3)
summary(mod.var4)  # R^2 = 0.7218

# Como podemos observar la variable respuesta que nos predice mejor y por consiguiente nos da un R^2 mas grande
# es la variable countBC con un R^2 = 0.7461 y una lambda = 0.3030303.

############################################################
# Validacion
############################################################
## 15

### Validacion de las premisas para el modelo con la variable respuesta countBC

par(mfrow=c(2,2)) # Graficos para valorar las 3 premisas
plot(mod.var3)  

# Plot para valorar la premisa de independencia seleccionando 500 valores

par(mfrow=c(1,1))
plot(residuals(mod.var3)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')

# Podemos observar que hemos ajustado bastante las tres premisas, presenta mejor linealidad, hemos ajustado la linia a la recta de Normalidad 
# y los residuos presentan una distribucion homogenea, por consiguiente tambien se ha ajustado la homoscedasticidad. Se mantienen constantes
# los residuos con el tiempo, es decir, no presenta ningun crecimiento, por consiguiente tambien cumple la independencia.

## Volvemosa hacer la descriptiva de residuos vs variables predictoras

## Alternativa 1
par(mfrow=c(2,3))
for(i in c(1,2,5,6,7,10)){
  plot(resid(mod.var3)~datos3[,i],main=names(datos3)[i],xlab=names(datos3)[i],ylab="Residuals")
  with(datos3,lines(lowess(resid(mod.var3)~datos3[,i]),col=2))
}

## Alternativa 2. Residuos versus variables predictoras
residualPlots(mod.var3)   

# Podemos ver que aun se necesita un ajuste en la linealidad de los residuos vs variables predictoras.

### Validacion de las premisas para el modelo con la variable respuesta countLog

par(mfrow=c(2,2)) # Graficos para valorar las 3 premisas
plot(mod.var4) 

# Plot para valorar la premisa de independencia seleccionando 500 valores

par(mfrow=c(1,1))
plot(residuals(mod.var4)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')

# En los graficos observamos que hemos arreglado la premisa de linealidad, sin embargo no se ha ajustado la linia a la recta de Normalidad
# y aun presenta homoscedasticidad. Los residuos son constantes en el tiempo, cumplen independencia.

## Volvemosa hacer la descriptiva de residuos vs variables predictoras

## Alternativa 1
par(mfrow=c(2,3))
for(i in c(1,2,5,6,7,10)){
  plot(resid(mod.var4)~datos3[,i],main=names(datos3)[i],xlab=names(datos3)[i],ylab="Residuals")
  with(datos3,lines(lowess(resid(mod.var4)~datos3[,i]),col=2))
}

## Alternativa 2
residualPlots(mod.var4)

# Podemos observar que aun se necesita un ajuste en la linealidad de los residuos vs variables predictoras.

############################################################
# Transformaciones polinomicas
############################################################
## 16 y 17

# Vamos a realizar unas transformaciones polinomicas añadiendo polinomios de grado superior a las variables predictoras para ver
# como se comporta el R^2.

mod.var3 <- lm(countBC~year + season + weather + poly(temp,2) + poly(humidity,2) + hour_intervals, datos3)
summary(mod.var3)  # R^2 = 0.7489

mod.var4 <- lm(countLog~year + season + weather + poly(temp,2) + poly(humidity,2) + hour_intervals, datos3)
summary(mod.var4)  # R^2 = 0.7269

# Hacemos unas transformaciones logaritmicas a las variables predictoras temp y humidity, 
# de esta manera podremos ver si ajustamos mejor las premisas y como se comporta el R^2.

datos3$temp_Log <- log(datos3$temp + 0.1)         
datos3$humidity_Log <- log(datos3$humidity +0.1)

mod.var5 <- lm(count~year + season + weather + temp_Log + humidity_Log + hour_intervals, datos3)
summary(mod.var5)  # R^2 = 0.6232 ; Esta opcion la descartaremos directamente pq nos da una prediccion en peor.

# Como podemos ver el modelo mod.var3 nos da un R^2 mas alto con un R^2 = 0.7489, por consiguiente nos quedamos
# con este modelo.

# Nota: Hemos probado varias transformaciones de tipo 1/Y,Y/X, con potencias y raices a la variable respuesta count y el mejor ajuste nos lo da
# el modelo mod.var3.

##### Validacion #####

# Validacion de las premisas para el modelo con la variable respuesta countBC y polinomios de grado 2 en las variables
# temp y humidity.

par(mfrow=c(2,2)) # Graficos para valorar 3 premisas
plot(mod.var3)  

# Plot para valorar la premisa de independencia seleccionando 500 valores

par(mfrow=c(1,1))
plot(residuals(mod.var3)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')

## Alternativa 1. Residuos versus variables predictoras
par(mfrow=c(2,3))
for(i in c(1,2,5,6,7,10)){
  plot(resid(mod.var3)~datos3[,i],main=names(datos3)[i],xlab=names(datos3)[i],ylab="Residuals")
  with(datos3,lines(lowess(resid(mod.var3)~datos3[,i]),col=2))
}
## Alternativa 2. Residuos versus variables predictoras
residualPlots(mod.var3)   

# Podemos observar que se mantienen las tres premisas, presenta buena linealidad, buen ajuste a la linia a la recta de Normalidad 
# y buena homoscedasticidad. Con los ploinomios de grado 2 ajustamos los residuos a cada una de las variables predictoras numericas,
# es decir, son lineales. La premisa de independencia tambien se cumple.

# Validacion de las premisas para el modelo con la variable respuesta countLog

par(mfrow=c(2,2)) # Graficos para valorar las 3 premisas
plot(mod.var4) 

# Plot para valorar la premisa de independencia seleccionando 500 valores

par(mfrow=c(1,1))
plot(residuals(mod.var4)[1:500], col="black", type="l", xlab = 'Order', ylab = 'Residuals')
abline(h=0,col='red')

## Alternativa 1. Residuos versus variables predictoras
par(mfrow=c(1,6))
for(i in c(1,2,5,6,7,10)){
  plot(resid(mod.var4)~datos3[,i],main=names(datos3)[i],xlab=names(datos3)[i],ylab="Residuals")
  with(datos3,lines(lowess(resid(mod.var4)~datos3[,i]),col=2))
}
## Alternativa 2. Residuos versus variables predictoras
residualPlots(mod.var4)

# En los graficos observamos que se mantiene la premisa de linealidad, sin embargo la linia a la recta de Normalidad no se ajusta
# y aun presenta homoscedasticidad. Con los polinomios de grado 2 ajustamos los residuos a cada una de la variables predictoras
# numericas. La premisa de independencia se cumple.

## La conclusion es que nos quedaremos con la variable respuesta countBC y unas trasnformaciones polinomicas de grado 2 en las
## variables predictoras temp y humidity, entonces nos quedamos con el modelo mod.var3.

############################################################
# Observaciones influyentes
############################################################
# 18

datos5 <- subset(datos3, select = -c(3,4,8,12,13,14))

par(mfrow=c(1,1))
influenceIndexPlot(mod.var3)           # Las observaciones 6076 y 5064 en principio tienen mucha influencia a posteriori. 
influencePlot(mod.var3)                # La 352 y 2715 tienen mucha influencia a priori.
obs.rm <- c(6076,5064)
View(datos5[obs.rm,])
col.points <- rep(rgb(0,0,0,0.1),nrow(datos5))        # Vector de colores
col.points[obs.rm] <- 2:3                             # Colores diferentes para las observaciones influyentes
pairs(datos5[,-10],col=col.points,pch=19,cex=0.8)     # Dibujo por pares de las observaciones influyentes
datos.rm <- datos5[-obs.rm,]                          # Nos creamos un nuevo data.frame sin estas observaciones

## 19

##--Ajuste del modelo sin observaciones influyentes

mod.var6 <-  lm(countBC~year + season + weather + poly(temp,2) + poly(humidity,2) + hour_intervals, datos.rm)

## Comparacion del nuevo modelo con el antiguo

summary(mod.var6)   # Modelo nuevo sin outliers y observaciones influyentes
summary(mod.var3)   # Modelo antiguo con todos los puntos

# Podemos observar que no existe ninguna variacion significativa en la prediccion del R^2. En el metodo mod.var6 tenemos un R^2 = 0.7495 y
# en el metodo mod.var3 tenemos un R^2 = 0.7489, por consiguiente nos quedamos con el modelo antiguo mod.var3.

##-- Modelo final
mod.final <- mod.var3

# Efectos sobre cada una de las variables

library(effects)
plot(allEffects(mod.final))

######## PARTE 2. HACER PREDICCIONES CON LA MUESTRA TEST ########

# 20

datos_test <- read.table('p1_test.csv',sep = ';',header=TRUE,dec=".")

############################################################
# Transformaciones en variables
############################################################
# 21

# Transformamos las variables a categorias con la funcion factor, solo las que vamos a necesitar porque las otras seran eliminadas
# ya que en el modelo entrenamiento no estan.

datos_test$year <- factor(datos_test$year) 
datos_test$season <- factor(datos_test$season) 
datos_test$weather <- factor(datos_test$weather) 

# Creamos los mismos intervalos para las franjas horarias

intervals <- c(0,7,9,16,20,24)
datos_test$hour_intervals <- cut(datos_test$hour,breaks = intervals,right = FALSE)

# Eliminamos las columnas que no necesitamos para contruir el modelo y nos quedamos con las mismas columnas del modelo de entrenamiemnto 
# mas la columns id.

datos_test <- subset(datos_test, select = -c(3,5,6,9,11))

############################################################
# Calcular predicciones
############################################################
# 22

## Predicciones

datos_test$countBC_pred <- predict(mod.final,datos_test)   # Predicciones para los nuevos valores
datos_test$count_pred <- datos_test$countBC_pred^(1/lamb)  # Hacemos la inversa de la lambda para obtener el Nº de bicicletas alquiladas count_pred
datos_test$count_pred_redondeado <- round(datos_test$count_pred,digits = 0)  # Redondeamos el valor

############################################################
# Guardar fichero
############################################################
# 23

# Escribimos el fichero final en 2 columnas: la primera contiene el id y la segunda el count predicho.

datos_final <- subset(datos_test,select = -c(2,3,4,5,6,7,8,9))
write.table(datos_final,"p1.txt",quote=FALSE,row.names = FALSE,col.names = FALSE,sep=",")
  






