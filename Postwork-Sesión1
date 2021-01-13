soccer <- read.csv("https://www.football-data.co.uk/mmz4281/2021/SP1.csv")
str(soccer)
names(soccer)
#Del data frame que resulta de importar los datos a R, extrae las columnas que contienen los números de goles anotados por los equipos que jugaron en casa (FTHG) y los goles anotados por los equipos que jugaron como visitante (FTAG)
golesCasa<-data.frame(TeamCasa<-soccer$HomeTeam,GolesCasa<-soccer$FTHG)
golesVisita<-data.frame(Visitantes<-soccer$AwayTeam,GolesVisit<-soccer$FTAG)


View(golesCasa)

#Consulta cómo funciona la función table en R al ejecutar en la consola ?table
?table

#Posteriormente elabora tablas de frecuencias relativas para estimar las siguientes probabilidades:

tFrecCasa<-as.data.frame(table(golesCasa$GolesCasa....soccer.FTHG))
tFrecVisit<-as.data.frame(table(golesVisita$GolesVisit....soccer.FTAG))


tFrecCasa<-transform(tFrecCasa,
          FreqAcum=cumsum(tFrecCasa$Freq),
          FrecRel=round(prop.table(tFrecCasa$Freq),3),
          FrecRelAcum=round(cumsum(prop.table(tFrecCasa$Freq)),3))

tFrecVisit<-transform(tFrecVisit,
                     FreqAcum=cumsum(tFrecVisit$Freq),
                     FrecRel=round(prop.table(tFrecVisit$Freq),3),
                     FrecRelAcum=round(cumsum(prop.table(tFrecVisit$Freq)),3))

tFrecCasa
tFrecVisit
#La probabilidad (marginal) de que el equipo que juega en casa anote x goles (x = 0, 1, 2, ...)

#La probabilidad (marginal) de que el equipo que juega como visitante anote y goles (y = 0, 1, 2, ...)
#La probabilidad (conjunta) de que el equipo que juega en casa anote x goles y el equipo que juega como visitante anote y goles (x = 0, 1, 2, ..., y = 0, 1, 2, ...)
