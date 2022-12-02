### Análisis de conjunto de datos jugadores NBA conjunto datos desde 2011/2012 hasta 2016/2017

library(ggplot2)
library(reshape2)
library(dplyr)
library(readr)

## Preparación de los datos
shot_data1 <- read_delim("shot_data_2011_2017_1.csv",delim=",")
shot_data2<- read_delim("shot_data_2011_2017_2.csv",delim=",")
shot_data<- rbind(shot_data1,shot_data2)

head(shot_data,1)


## Regresión Logística 
# Miramos que variables son estadísticamente significativas
train=round(nrow(shot_data)/2)
test=-train
shot_data <- shot_data %>% mutate(game_month=substr(game_date,5,6))
rl.mod=glm(shot_made_numeric ~ shot_distance + scoremargin  + home + simplified_area 
           + time_remaining + season + game_month + shot_type,
           data=shot_data, family = binomial)
(rl.summ<- summary(rl.mod))


## Predicciones
# Modelo con las variables seleccionadas
rl.mod<-glm(shot_made_numeric ~ shot_distance + scoremargin  + home + time_remaining + shot_type,
            data=shot_data, family = binomial)
summary(rl.mod)
# Predicción de ejemplo:
# Un jugador realiza un tiro en el último segundo del partido, desde 24 pies (de 3 puntos), su equipo 
# pierde de 2. El jugador juega en casa.
new_data <- data.frame(shot_distance=24,scoremargin=-2,home=TRUE, time_remaining=1/60,shot_type="3PT Field Goal")
# Si queremos predecir la probabilidad de encestar
predict(rl.mod,newdata = new_data, type = "response")
# Si queremos predecir el argumento de la exponencial, usaremos
predict(rl.mod,newdata = new_data, type = "link")
# predicción con jugador jugando fuera de casa:
new_data <- data.frame(shot_distance=24,scoremargin=-2,home=FALSE, time_remaining=1/60,shot_type="3PT Field Goal")
predict(rl.mod,newdata = new_data, type = "response")
# predicción tiro a 8 pies de distancia
new_data <- data.frame(shot_distance=8,scoremargin=-2, home=FALSE, time_remaining=1/60, shot_type="3PT Field Goal")
predict(rl.mod,newdata = new_data, type = "response")


## Interpretación del ajuste
# predicciones de acierto para todos los lanzamientos contenidos en el conjunto de datos
tmp <- shot_data
tmp$rl.prob <- predict(rl.mod, type="response")
# gráficas de las predicciones frente a las distintas variables numericas:
tmp1 <- melt(tmp,id=c("game_date","player_name","shot_made_numeric","rl.prob"),
             measure=c("shot_distance","scoremargin","time_remaining")) 
ggplot(tmp1,aes(value,rl.prob)) + #geom_point(aes(color=factor(shot_made_numeric)),size=0.5,alpha=0.2) + 
  geom_smooth() + facet_wrap(~variable, scales="free_x")
# variables categóricas:
tmp1 <- melt(tmp,id=c("game_date","player_name","shot_made_numeric","rl.prob"),measure=c("home","shot_type")) 
ggplot(tmp1,aes(value,rl.prob)) + geom_boxplot() + facet_wrap(~variable,scales="free_x")
# Si queremos diferenciar segun si el tiro realmete acertó o no
#ggplot(tmp1,aes(value,rl.prob)) + geom_boxplot(aes(color=factor(shot_made_numeric)))

# acierto en base a distancia de tiro, el tipo(de 2 o 3) y si juega en casa o no
#ggplot(tmp,aes(shot_distance,rl.prob)) + geom_point(aes(color=shot_made_flag),size=0.1,alpha=0.05) + geom_smooth() + facet_grid(shot_type ~ home)
ggplot(tmp,aes(shot_distance,rl.prob)) + geom_smooth(aes(color=shot_type)) + facet_wrap(~ home)

# acierto en base a la diferencia de puntos de los equipos
ggplot(tmp,aes(scoremargin,rl.prob)) + geom_smooth(aes(color=shot_type)) + facet_wrap(~ home)

# calculo del incremento de la probabilidad de acierto por el hecho de jugar en casa o fuera
# para cada distancia de tiro
preds<- expand.grid(shot_distance=1:20,home=c(FALSE,TRUE),scoremargin=-2,time_remaining=1/60,shot_type="2PT Field Goal") %>%
  summarise(shot_distance=.$shot_distance,scoremargin=.$scoremargin, home=.$home,
            time_remaining=.$time_remaining, shot_type=.$shot_type,
            pred=predict(rl.mod,newdata = ., type = "response")) %>% 
  arrange(shot_distance)
preds

# Estandarización de variables: shot_distance, scoremargin, time_remaining 
tmp <- shot_data
tmp <- tmp %>% mutate_at(vars(shot_distance, scoremargin, time_remaining), scale)
std.mod <- glm(shot_made_numeric ~ shot_distance + scoremargin  + home +  time_remaining + shot_type,
               data=tmp, family = binomial) 
summary(std.mod)

# Graficas
# ggplot(tmp) + geom_density(aes(rl.prob,color=factor(ntile(scoremargin,8))))
ggplot(tmp) + geom_density(aes(shot_distance,color=factor(ntile(scoremargin,8))))
# Density 2d -------------------------------------------------------------------
ggplot(tmp) + geom_point(aes(loc_x,loc_y),size=0.1,alpha=0.1) + geom_density2d(aes(loc_x,loc_y),bins=50)

## Modelos por jugador
ggplot(tmp) +
  stat_density_2d(aes(loc_x,loc_y,fill = ..level..), bins=50, geom = "polygon") + 
  geom_density2d(aes(loc_x,loc_y),color="white",bins=50,size=0.1)

new_data <- data.frame(shot_distance=24,scoremargin=-2,home=FALSE, time_remaining=1/60, shot_type="3PT Field Goal")
rl.mod.general  <- glm(shot_made_numeric ~ shot_distance + scoremargin + home + time_remaining + shot_type,
                       data=shot_data, family = binomial) 
# Ajuste para Curry
rl.mod.Curry<- glm(shot_made_numeric ~ shot_distance + scoremargin + home + time_remaining + shot_type,
                   data= shot_data %>% filter(player_name=="Stephen Curry"), family = binomial)
# Ajsute para Gasol
rl.mod.Gasol<- glm(shot_made_numeric ~ shot_distance + scoremargin + home + time_remaining + shot_type,
                   data= shot_data %>% filter(player_name=="Pau Gasol"), family = binomial)


(pred.general<- predict(rl.mod.general, newdata=new_data, type="response"))
(pred.Curry<- predict(rl.mod.Curry, newdata=new_data, type="response"))
(pred.Gasol<- predict(rl.mod.Gasol, newdata=new_data, type="response"))