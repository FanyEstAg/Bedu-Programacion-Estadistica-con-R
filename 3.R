getwd()
setwd("D:/Archivos/Cursos/BEDU/II.Programación y Estadistica con R/Postworks/files2") # Depende del usuario
dir()

#Importa los datos de soccer de las temporadas 2017/2018, 2018/2019 y 2019/2020 de la primera división de la liga española a R, los datos los puedes encontrar en el siguiente enlace: https://www.football-data.co.uk/spainm.php


df17_20 <- lapply(dir(),read.csv)

#Obten una mejor idea de las características de los data frames al usar las funciones: str, head, View y summary

str(df17_20); head(df17_20); View(df17_20); summary(df17_20)

#Con la función select del paquete dplyr selecciona únicamente las columnas Date, HomeTeam, AwayTeam, FTHG, FTAG y FTR; esto para cada uno de los data frames. (Hint: también puedes usar lapply).
library(dplyr)


#sel17_20 <- lapply(df17_20, select, Date:FTR)
#sel17_20 <- lapply(df17_20[2], select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR )

sel17_20 <- lapply(df17_20, select, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR )

#Asegúrate de que los elementos de las columnas correspondientes de los nuevos data frames sean del mismo tipo (Hint 1: usa as.Date y mutate para arreglar las fechas). 
mut17_18 <- lapply(sel17_20[1], mutate, Date = as.Date(Date, "%d/%m/%y"))
mut18_20 <- lapply(sel17_20[2:3], mutate, Date = as.Date(Date, "%d/%m/%Y"))
mut17_20 <- c(mut17_18, mut18_20)

head(mut17_20)
View(mut17_20)
#Con ayuda de la función rbind forma un único data frame que contenga las seis columnas mencionadas en el punto 3 (Hint 2: la función do.call podría ser utilizada).


data17_20 <- do.call(rbind, mut17_20)
head(data17_20)
tail(data17_20)
View(data17_20)

complete.cases(data17_20)
data17_20

#Con el último data frame obtenido en el postwork de la sesión 2, elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:
# La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x=0,1,2,)
Casa <- data.frame(Team_Casa <- data17_20$HomeTeam, Goles_Casa <- data17_20$FTHG)
tableFRCasa <- as.data.frame(table(Casa$Goles_Casa....data17_20.FTHG))
tableFRCasa
head(tableFRCasa)
tProbCasa<-transform(tableFRCasa,
                     FreqAcum=cumsum(tableFRCasa$Freq),
                     FrecRel=round(prop.table(tableFRCasa$Freq),3),
                     FrecRelAcum=round(cumsum(prop.table(tableFRCasa$Freq)),3))
tProbCasa
#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y=0,1,2,)
Visitante <- data.frame(Team_Away <- data17_20$AwayTeam, Goles_Visitante <- data17_20$FTAG)
tableFRVis <- as.data.frame(table(Visitante$Goles_Visitante....data17_20.FTAG))
tableFRVis
tProbVis<-transform(tableFRVis,
                     FreqAcum=cumsum(tableFRVis$Freq),
                     FrecRel=round(prop.table(tableFRVis$Freq),3),
                     FrecRelAcum=round(cumsum(prop.table(tableFRVis$Freq)),3))
tProbVis


#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x=0,1,2,, y=0,1,2,)
#####################PENDIENTE

#Realiza lo siguiente:
#  Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo de casa
library(ggplot2)
pCasa <- tProbCasa %>%
  ggplot(aes(x=Var1, y=FrecRel, fill=Freq))+
  geom_bar(stat="identity")+
  labs(x="no. de Gol",
       y="Probabilidad",
       fill="#Goles",
       title="Prob. marginales: # de goles de TeamHome",
     theme_light())
pCasa
#Un gráfico de barras para las probabilidades marginales estimadas del número de goles que anota el equipo visitante.
pVisitante <- tProbVis %>%
  ggplot(aes(x=Var1, y=FrecRel, fill=Freq))+
  geom_bar(stat="identity")+
  labs(x="no. de Gol",
       y="Probabilidad",
       fill="#Goles",
       title="Prob. marginales: # de goles de TeamAway",
       theme_light())
pVisitante
#Un HeatMap para las probabilidades conjuntas estimadas de los números de goles que anotan el equipo de casa y el equipo visitante en un partido.