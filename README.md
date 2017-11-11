# neuralnetworks
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)

library(moments)
library(base)
library(forecast)
library(readxl)
library(ggplot2)
setwd("~/Desktop/EIA/2017-2/1. Modelos y Simulacion Financiera/Taller Exoticos")
info_nn <- as.data.frame(read_excel("~/Desktop/EIA/2017-2/1. Modelos y Simulacion Financiera/Taller Exoticos/petroleo_util.xlsx"))
info_nn<-info_nn[1:261,c(3,6,7,8)]
dataexotico <- as.data.frame(read_excel("~/Desktop/EIA/2017-2/1. Modelos y Simulacion Financiera/Taller Exoticos/datos exotico.xlsx"))

index<-1:209
train<-info_nn[index,]
test<-info_nn[-index,]
```



## Punto 1

El subyacente a negociar para el taller es el petroleo WTI

### Descripcion General

El petroleo WTI (West Texas Intermediate) es una "corriente" de petroleo crudo que se produce en EUA y especificamente en Texas y el sur de Oklahoma (US Energy Administration). Este tipo de petroleo es de baja densidad y bajo contenido de azufres, lo que facilita su refinamiento, es la segunda referencia de petroleo mas usada en el trading, detras del Brent (IG Espana). 

### Descripcion Estadistica

Recientemente, el petroleo ha tenido una alta volatilidad y baja en los precios, esto probablemente como reaccion de los mercados al exceso de oferta y a la incertidumbre sobre la produccion de grupos como la Organizacion de Paises Exportadores de Petroleo (OPEP).

En este taller seran usados los precios spot semanales disponibles entre el 6 de Enero de 2012 y el 30 de Diciembre de 2016.

La desviacion estandar del WTI en ese rango de tiempo fue de `r sd(train[,4])`, un valor bastante alto, considerando que al primero de Enero del 2016, el precio spot era de `r train[209,4]`.

La distribucion de los precios del WTI en el espacio de tiempo determinado es la siguiente:

```{r histograma, echo=FALSE}
hist(train[,4],nclass=150,main=" ",xlab="Precios",ylab="Frecuencia")
```

Si se quitan de la distribucion los precios superiores a 70, es posible analizar los precios mas recientes del petroleo, despues de la fuerte baja en 2014:

```{r ,echo=FALSE}
hist(train[,4][train[,4]<70],nclass=20,main=" ",xlab="Precios",ylab="Frecuencia")
```

El coeficiente de asimetria de los datos del petroleo es de `r skewness(train[,4])`, lo que indica un claro sesgo de los datos, concentrados en la derecha de la distribucion, esto ocasionado por la gran baja del precio y la presencia de precios en el nivel cercano a 100 usd/barril.

La kurtosis es de `r kurtosis(train[,4])`, lo que indica que los datos de la funcion estan concentrados y no tienen colas pesadas.

## Punto 2

Se establecieron contratos de la siguiente manera: opcion call a 1 mes (strike 30), forward a 3 meses, opcion call a 6 meses (strike 46) y forward a 12 meses, sobre el petroleo WTI y asumiendo una tasa del 0.34% EA, la tasa de intervencion de la Reserva Federal para el 1 de Enero de 2016.

Se realizaron pronosticos para la valoracion de las opciones y forwards con el uso de una red neuronal entrenada con los datos de: produccion del petroleo en EU, precio del futuro de WTI y el precio del WTI rezagado un periodo.

```{r, include=FALSE}
lm.fit<-glm(spot~.,data=train)
summary(lm.fit)

pr.lm<-predict(lm.fit,test)
MSE.lm<-sum((pr.lm-test$spot)^2)/nrow(test)

maxs<-apply(info_nn,2,max)
mins<-apply(info_nn,2,min)

scaled<-as.data.frame(scale(info_nn,center=mins,scale=maxs-mins))

train_<-scaled[index,]
test_<-scaled[-index,]

library(neuralnet)
n<-names(train_)
f<-as.formula(paste("spot ~",paste(n[!n %in% "spot"],collapse=" + ")))

nn<-neuralnet(f,data=train_,hidden=c(5,3),linear.output = T)
plot(nn)

pr.nn<-compute(nn,test_[,1:3])
pr.nn_<-pr.nn$net.result*(max(info_nn$spot)-min(info_nn$spot))+min(info_nn$spot)

test.r<-(test_$spot)*(max(info_nn$spot)-min(info_nn$spot))+min(info_nn$spot)

MSE.nn<-sum((test.r-pr.nn_)^2)/nrow(test_)

tasa<-((1+0.0034)^(1/12))-1

# A 1 mes - Opcion - strike <- 30
strike1<-30
pron1<-mean(pr.nn_[4],pr.nn_[5])
prima1<-(pron1-strike1)*((1+tasa)^(-1))

# A 3 meses - Forward
pron3<-pr.nn_[13]
pforward3<-(pron3)*((1+tasa)^(-3))

# A 6 meses - Opcion - strike <- 42
strike6<-46
pron6<-mean(pr.nn_[21],pr.nn_[22])
prima6<-(pron6-strike6)*((1+tasa)^(-6))

# A 12 meses - Forward
pron12<-pr.nn_[52]
pforward12<-(pron12)*((1+tasa)^(-12))


prima1_neg<-2.86
pforward3_neg<-37.14
prima6_neg<-3.99
pforward12_neg<-50.84

pyg1<-prima1_neg*(1+tasa)
pyg3<-pforward3_neg-38.34
pyg6<-((strike6+prima6_neg*((1+tasa)^6))-48.33)
pyg12<-pforward12_neg-53.72
```

La red fue comparada con una regresion lineal de los mismos datos teniendo los siguientes resultados para el error de la regresion lineal y el error de la red respectivamente: `r print(paste(MSE.lm,MSE.nn))`.

Despues de realizar los pronosticos, se propusieron los siguientes precios de negociacion, trayendo a valor presente la diferencia entre el strike y el pronostico en el caso de las opciones y trayendo a valor presente los pronosticos para el caso de los forwards: `r paste(c("Prima de 1 mes","Precio Forward 3 meses","Prima de 6 meses","Precio Forward de 12 meses"),c(prima1,pforward3,prima6,pforward12))`.

El anterior metodo buscaba seguir la siguiente logica: tomando el precio pronosticado como el precio que se espera para el spot en el futuro, quien quiera tener la opcion de comprar ese activo en el futuro deberia pagar lo correspondiente a la diferencia entre ese valor (pronostico) y el que quiera para la opcion (strike) traido a valor presente, de manera que inicialmente, ambas partes se encuentren en igualdad de condiciones.

Los precios pactados para la negociacion a 1,3,6 y 12 meses fueron: `r paste(c(prima1_neg,pforward3_neg,prima6_neg,pforward12_neg))`

Con esos precios pactados, las perdidas/ganancias fueron de: `r paste(c("PYG de 1 mes","PYG de 3 meses","PYG de 6 meses","PYG de 12 meses"),c(pyg1,pyg3,pyg6,pyg12))`

## Punto 3
```{r,include=FALSE}
tasa_anual<-0.0034

sd<-sd(diff(log(train$spot)))*sqrt(52)

spot<-test[1,4]

blackscholes<-function(strike,spot,sd,tasa,tiempo){
  d1<-(log(spot/strike)+(tasa+((sd^2)/2)*tiempo))/(sd*sqrt(tiempo))
  d2<- (log(spot/strike)+(tasa-((sd^2)/2)*tiempo))/(sd*sqrt(tiempo))
  n1 <- pnorm(d1,mean = 0,sd=1)
  n2 <- pnorm(d2,mean = 0,sd=1)
  call<-spot*exp(-tasa*tiempo)*n1-strike*exp(-tasa*tiempo)*n2
  return(call)
}

tiempo<-c(1/12,6/12)
strikes<-c(30,46)

resultadop <- rep(0,2)
for(i in 1:2){
   resultadop[i]<-blackscholes(strikes[i],spot,sd,tasa_anual,tiempo[i])
}
```

Se valoraron las dos opciones negociadas mediante el metodo de Black, Scholes y Merton, obteniendo los siguientes resultados: `r resultadop`.

Los resultados obtenidos por este metodo fueron bastante similares al obtenido por el metodo del pronostico para la primera opcion pero diferentes para el valor obtenido para la opcion a 6 meses.

## Punto 4

Para este punto se tomaron los datos del precio del petroleo WTI y Brent desde Enero de 2014 a Septiembre de 2017.

```{r exotico,include=FALSE}
wti<-dataexotico[,2]
brent<-dataexotico[,3]

retwti<-diff(log(wti))
retbrent<-diff(log(brent))

auto.arima(retwti) # AR(1) coeficiente -0.0771
auto.arima(retbrent) # AR(1) coeficiente -0.0782

a1_wti<--0.771
a1_brent<--0.0782
```

Se ajustaron los retornos de ambos subyacentes a un modelo ARIMA(p,i,q), obteniendo para cada uno, un modelo AR(1) con los coeficientes `r paste(c(a1_wti,a1_brent))` respectivamente.

Se modelaron los dos subyacentes mediante la siguiente funcion:

```{r}
ret<-NULL
gen_ret_ar1<-function(retorno,coef){
  for(i in 2:length(retorno+1)){
    ret[i]<-mean(retorno)*(1-coef)+coef*retorno[i-1]+rnorm(1,0,var(retorno))
  }
  return(ret)
}
```

A partir de los retornos ajustados al modelo AR(1), se generaron los precios a partir de esos retornos y partiendo de un precio inicial de la siguiente manera:

```{r,include=FALSE}
ret_wti_ar1<-gen_ret_ar1(retwti,a1_wti)[-is.na(gen_ret_ar1(retwti,a1_wti))]
ret_brent_ar1<-gen_ret_ar1(retbrent,a1_brent)[-is.na(gen_ret_ar1(retbrent,a1_brent))]
```

```{r}
gen_series_precio<-function(precio_inicial,retornos){
  ret<-1+retornos
  serie<-cumprod(c(precio_inicial,ret))
  return(serie)
}
```

Donde los precios modelados claramente incluian una componente aleatoria.

```{r,include=FALSE}
precio_wti_ar1<-gen_series_precio(98.7,ret_wti_ar1)
precio_brent_ar1<-gen_series_precio(110.92,ret_brent_ar1)
```

Se graficaron los precios reales vs. los precios de los retornos ajustados a los modelos AR(1), obteniendo los siguientes resultados:

```{r,echo=FALSE}
par(family="mono")
plot(brent,type="l",col="red",ylab="Precio",xlab="Tiempo",main="Modelo vs. Real")+
lines(precio_brent_ar1,col="blue")+
  text(300,40,labels="Spot Brent")+
  text(800,80,labels="Brent AR(1)")
```

```{r,echo=FALSE}
par(family="mono")
plot(wti,type="l",col="red",ylab="Precio",xlab="Tiempo",main="Modelo vs. Real")+
lines(precio_wti_ar1,col="blue")+
  text(300,30,labels="Spot WTI")+
  text(800,90,labels="WTI AR(1)")
```

```{r,include=FALSE}
inicial_exotico<-0.7*wti[length(wti)]+0.3*brent[length(brent)]
```

A continuacion se modelo un instrumento que dependiera de ambos subyacentes y con memoria de dos muestras hacia atras de ellos, que generara valores aleatorios de la siguiente manera:

```{r}
ret_exotico_aleat<-function(retornosa,coefa,retornosb,coefb){
  wti<-retornosa
  brent<-retornosb
  for (i in 3:length(brent)-1)
  ret[i]<-0.7*(0.6*wti[i-1]+0.4*wti[i-2])+0.3*(0.6*brent[i-1]+0.4*brent[i-2])
  return(ret[-is.na(ret)])
}
```

El modelo propuesto describe los retornos y constituye un derivado financiero cuyo valor depende del valor de los subyacentes, su valor depende en un 70% del valor del petroleo WTI y en un 30% del Brent y a su vez depende en un 60% del valor de una muestra hacia atras y en un 40% de dos muestras hacia atras.

Los precios del instrumento para los retornos calculados con el modelo son los siguientes:

```{r,include=FALSE}
ret_exotico_pasado<-ret_exotico_aleat(retwti,a1_wti,retbrent,a1_brent)[3:979]

exotico_pasado<-gen_series_precio(inicial_exotico,ret_exotico_pasado)
```

```{r,echo=FALSE}
par(family="mono")
plot(exotico_pasado,type="l",col="red",ylab="Precio",xlab="Tiempo",main="Modelo Exotico con subyacentes AR(1)")+
  text(500,35,labels="Exotico")
```

Para simular el comportamiento del instrumento se tomaron 252 valores aleatorios de los retornos observados del petroleo WTI y Brent, calculando asi los precios del instrumento en 25 simulaciones y obteniendo los siguientes resultados:

```{r,include=FALSE}
# Datos de un a??o antes de Sept. 2017
# Valores aleatorios de retornos de c/u

#simulacion_ret<-ret_exotico_aleat(sample(retwti,252),a1_wti,sample(retbrent,252),a1_brent)[3:250] # Evitar NAs manualmente

inicial_simulacion<-0.7*sample(wti,1)+0.3*sample(brent,1)

#precios_simulacion<-gen_series_precio(inicial_simulacion,simulacion_ret)

precios_simulacion<-gen_series_precio(inicial_simulacion,ret_exotico_aleat(sample(retwti,252),a1_wti,sample(retbrent,252),a1_brent)[3:250] )

ensayos<-25
simulacion<-replicate(ensayos,gen_series_precio(inicial_simulacion,ret_exotico_aleat(sample(retwti,252),a1_wti,sample(retbrent,252),a1_brent)[3:250] ))
```

```{r,echo=FALSE}
par(family="mono")
matplot(simulacion,type="l",ylab="Precio",xlab="Tiempo",main="Simulaciones")+
  text(70,29,labels="Simulaciones partiendo")+
  text(70,26,labels="de un precio comun")
```

El modelo propuesto es una opcion util para la inversion y cobertura en energia y especificamente de petroleo en paises donde el precio de referencia sea el del WTI (mayor ponderacion del WTI).

Las funciones utilizadas para modelar los subyacentes cuentan con una componente aleatoria cada una. Como puede verse a partir de los resultados de las simulaciones del instrumento, el efecto de esa componente aleatoria en su comportamiento es claro y lo hace lo suficientemente estocastico para evitar especulacion.


