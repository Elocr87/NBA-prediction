library(rvest) #scrapping

#Primer partido temporada 2017/18=400974437,400975200
#Último partido 15/02/2018=400975612
initgame <-400975556
endgame <- 400975612
#game<-400975460

Stats = NULL

for(game in initgame:endgame){
  
  url <- paste("http://espndeportes.espn.com/basquetbol/nba/duelo?juegoId=", game, sep="")
  
  html <- read_html(url)#leemos la página
  score<-html_nodes(html,"#teamstats-wrap td") #seleccionamos los nudos que queremos leer
  score<-html_text(score) #Lo leemos    
  score <- matrix(score,ncol=3,byrow = TRUE)
  
  d<-gsub("(\t|\n)*","",score) #quitamos /t y /n
  d<-t(data.frame(d)) #lo convertimos en data frame y lo trasponemos
  colnames(d) <- d[1,] 
  d = d[-1,]
  
  #Sacar Puntiación
  puntos<-html_nodes(html,".score")
  puntos = cbind( html_text(puntos),d )
  
  #Sacar Equipo para local y visitante
  Equipo<-html_nodes(html,".short-name")
  Stats0 = cbind( html_text(Equipo),puntos )
  
  #Ponemos nombre para local o vicitante
  Local<-c("Vis","L")
  Stats0 = cbind( Local,Stats0 )
  
  #Ponemos número de partido
  Stats1=cbind(game,Stats0)
  Stats = rbind(Stats, Stats1)}

Stats=data.frame(Stats) #Convertimos en data frame

#Renombramos los campos
names(Stats)[1]<-"Partido"
names(Stats)[3]<-"Equipo"
names(Stats)[4]<-"Puntos"
names(Stats)[6]<-"PorcT2"
names(Stats)[8]<-"PorcT3"
names(Stats)[10]<-"PorcTL"
names(Stats)[16]<-"Tapones"
names(Stats)[17]<-"Perdidas"
names(Stats)[18]<-"Ptsprdida"
names(Stats)[20]<-"Ptszona"
names(Stats)[21]<-"Faltas"

#Guardamos en csv
write.csv(Stats, paste("stats2.csv", sep=";"), row.names=FALSE)
