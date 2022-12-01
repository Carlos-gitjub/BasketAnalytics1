library(ggplot2)
library(dplyr)
library(reshape2)

# lectura de datos
fgp_distance <- read.delim("fgp_distance.csv",sep=";")

# vista global de dependencia entre acierto(fgp=field goal percentage) y la  distancia del tiro
ggplot(fgp_distance) + geom_point(aes(shot_distance,fgp, color=player_name))

# Generación de conjuntos de test y entrenamiento.jugadores a considerar en el conjunto de entrenamiento.
tr.players= c("Avery Bradley", "DeAndre Jordan", "Isaiah Thomas", "James Harden",
              "JR Smith", "Kawhi Leonard", "Kevin Durant", "Kevin Love")
tr.data = fgp_distance %>% filter(player_name %in% tr.players)
test.data = fgp_distance %>% filter(!(player_name %in% tr.players))

# Funciones para ajustar modelos de Regresión Polínomica y para calcular las predicciones.
fit_polinomial_model <- function(train.data,order){
  lm(fgp~poly(shot_distance,order),data=train.data)
}
predict_fgp <- function(test.data,model){
  predict(model, newdata=test.data)
}

# Modelo polinómico de orden 3 y su Error Cuadrático Medio (MSE)
mod <- fit_polinomial_model(tr.data, order=3)
ytr= predict_fgp(tr.data,mod)
yte= predict_fgp(test.data,mod)
(mse.tr=mean((ytr-tr.data$fgp)^2))
(mse.test=mean((yte-test.data$fgp)^2))


# Modelos polinómico desde orden 1 a 10 y grafica de sus MSE
mse <- NULL
for(ord in 1:10){
  mod <- fit_polinomial_model(tr.data, order=ord)
  ytr<- predict_fgp(tr.data,mod)
  yte<- predict_fgp(test.data,mod)
  mse<-rbind(mse,data.frame(tipo="train", order=ord, mse=mean((ytr-tr.data$fgp)^2)))
  mse<-rbind(mse,data.frame(tipo="test", order=ord, mse=mean((yte-test.data$fgp)^2)))
}

ggplot(mse, aes(order,mse,color=tipo)) + geom_point() + geom_line(linetype=2) + 
  scale_x_continuous(breaks = 1:10)

# Grafica de la curva de ajuste para el polinomio con mejores resultados sobre
# el conjunto de test, con los puntos de fondo para el conjunto de entrenamiento
# y de test.
ord = 7
mod7<-fit_polinomial_model(tr.data, order = ord)
dist.data<-data.frame(shot_distance=seq(0,30,.25))
yfit<-predict_fgp(dist.data,mod7)

ggplot(tr.data) + geom_point(aes(shot_distance, fgp, color="train")) +
  geom_line(aes(shot_distance,yfit, color="fit"), data=dist.data)

ggplot(test.data) + geom_point(aes(shot_distance,fgp, color="test")) +
  geom_line(aes(shot_distance,yfit, color="fit"), data=dist.data)
# NOTA: El crecimiento de la curva ajustada a partir de una distancia de 27 pies parece 
# poco justificada y es debida a la peculiaridad de los datos en el conjunto de 
# entrenamiento.

# Curva de ajuste para orden 3 con las observaciones de fondo.
ord = 3
mod3<-fit_polinomial_model(tr.data, order = ord)
dist.data<-data.frame(shot_distance=seq(0,30,.25))
yfit<-predict_fgp(dist.data, mod3)

ggplot(tr.data) + geom_point(aes(shot_distance,fgp, color="train")) + 
  geom_line(aes(shot_distance,yfit, color="fit"),data=dist.data)

ggplot(test.data) + geom_point(aes(shot_distance,fgp, color="test")) +
  geom_line(aes(shot_distance,yfit, color="fit"),data=dist.data)


# Rentabilidad de tirar desde 15 pies o 25 pies (a partir de 24 es un triple,
# 22 en las esquinas)
(prob15<-predict_fgp(data.frame(shot_distance=15),mod3))
(prob25<-predict_fgp(data.frame(shot_distance=25),mod3))
# Podemos ver de 100 lanzamietos cuantos puntos se sacarían:
( (prob15*100)*2 )
( (prob25*100)*3 )
# también se podría hacer simplemente:
prob15*2
prob25*3
# Por tanto es mas rentable tirar desde 25 pies en vez de 15. (De 100 tiros se 
# obtendrían 109 y 82 respectivamente)