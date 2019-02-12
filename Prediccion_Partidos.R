library(plyr) #para contar
library(adabag) #adaboost
library(randomForest) 
library(party) #ctree
library(rpart) #rpart
library(rpart.plot) 
library(e1071) #svm
library(aod) 
library(nnet) #neural network
library(ggplot2)
library(neuralnet) #neural network

#establecemos el directorio de trabajo

setwd("C:/Users/XXX/Desktop")

#Cargamos los dataset

partidos<-read.csv("stats.csv", header=T,sep=";")
partidosNN<-read.csv("stats NN.csv", header=T,sep=";")

#estructura dataset

str(partidos)
str(partidosNN)

#dividimos aleatoraimiento en 2 data frame para entrenar y probar 
ind = sample(1:nrow(partidos),nrow(partidos)/2, replace = FALSE) 
trainData<-partidos[ind,]
testData<-partidos[-ind,]

ind = sample(1:nrow(partidosNN),nrow(partidosNN)/2, replace = FALSE) 
trainDataNN<-partidosNN[ind,]
testDataNN<-partidosNN[-ind,]

#Creamos 2 variables 
train=trainData
test=train

trainNN=trainDataNN
testNN=trainNN

#############ÁRBOL DE DECISIÓN#####################

#############CTREE 1#################
#Solo con estadística a favor
Arboldecision=ctree(Ganador~Puntos+Local+Ptsencontra+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                      Ptscontrataque+Ptszona+Ptsperdida,train)

plot(Arboldecision)

testtree<-predict(Arboldecision,test)
table(testtree,test$Ganador)

Acierto=round(sum(testtree==test$Ganador)/length(test$Ganador)*100,2)
Acierto

Modelos=cbind("ctree",Acierto)
Modelos=data.frame(Modelos)
names(Modelos)[1]="Modelos"

#############CTREE 2#################
#Añadimos Conferencias y Niveles de equipo
Arboldecision=ctree(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                    +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                      Ptscontrataque+Ptszona+Ptsperdida,train)

plot(Arboldecision)

testtree<-predict(Arboldecision,test)
table(testtree,test$Ganador)

Acierto2=round(sum(testtree==test$Ganador)/length(test$Ganador)*100,2)

Modelos=cbind(Modelos,Acierto2)

#############CTREE 3#################
#Añadimos Estadística del equipo contrario
Arboldecision=ctree(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                    +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                      Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR
                    +RobosR+TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR
                    +FaltasR+TecnicasR+IntencionalesR,train)
plot(Arboldecision)

testtree<-predict(Arboldecision,test)
table(testtree,test$Ganador)

Acierto3=round(sum(testtree==test$Ganador)/length(test$Ganador)*100,2)

Modelos=cbind(Modelos,Acierto3)
Modelos

#############rpart 1#################
#Solo con estadística a favor

ArbolRpart<-rpart(Ganador~Puntos+Ptsencontra+Local+T2A+T2I+T3A+T3I
                  +TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                    Ptscontrataque+Ptszona+Ptsperdida,train)

#visualizamos el árbol

rpart.plot(ArbolRpart,extra=4)

#visualizamos el donde comienza el error

plotcp(ArbolRpart)

#podamos
pArbolRpart<-prune(ArbolRpart,cp=ArbolRpart$cptable[which.min(ArbolRpart$cptable[,"xerror"]),"CP"])
rpart.plot(pArbolRpart,extra=6)

rpart.plot(pArbolRpart,extra=6)

#probamos el árbol con el test
testPredRpart<-predict(pArbolRpart,test,typle="class")
head(testPredRpart)

Ganador=cbind(testPredRpart,test)
Ganador=data.frame(Ganador$V,Ganador$Ganador)
names(Ganador)[1]='Prob'
names(Ganador)[2]='Resultado'

Totalpartidos=count(Ganador[Ganador$Prob>0.5,])
Totalpartidos
Resultado=count(Totalpartidos[Totalpartidos$Resultado=="V",])
Resultado=sum(as.numeric(Resultado$freq))
Numpartidos=sum(as.numeric(Totalpartidos$freq))

Acierto=round(Resultado/Numpartidos*100,2)

Rpart1=cbind("rpart",Acierto)
Rpart=data.frame(Rpart1)
names(Rpart)[1]="Modelos"


#############rpart 2#################
#Añadimos Conferencias y Niveles de equipo

ArbolRpart<-rpart(Ganador~Local+Conferencia+Nivel+ConfRival+NivelRival+T2A+T2I+T3A+T3I
                  +TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                    Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR
                  +RebDR+AsistR+RobosR+TaponesR+PerdidasR
                  +PtsperdidaR+PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR,train)

#visualizamos el árbol

rpart.plot(ArbolRpart,extra=4)

#visualizamos el donde comienza el error

plotcp(ArbolRpart)

#podamos
pArbolRpart<-prune(ArbolRpart,cp=ArbolRpart$cptable[which.min(ArbolRpart$cptable[,"xerror"]),"CP"])

rpart.plot(pArbolRpart,extra=6)

#probamos el árbol con el test
testPredRpart<-predict(pArbolRpart,test,typle="class")
head(testPredRpart)


Ganador=cbind(testPredRpart,test)
Ganador=data.frame(Ganador$V,Ganador$Ganador)
names(Ganador)[1]='Prob'
names(Ganador)[2]='Resultado'

Totalpartidos=count(Ganador[Ganador$Prob>0.5,])
Totalpartidos
Resultado=count(Totalpartidos[Totalpartidos$Resultado=="V",])
Resultado=sum(as.numeric(Resultado$freq))
Numpartidos=sum(as.numeric(Totalpartidos$freq))

Acierto2=round(Resultado/Numpartidos*100,2)

Rpart=cbind(Rpart,Acierto2)

#############rpart 3#################
#Añadimos Estadística del equipo contrario

ArbolRpart<-rpart(Ganador~Local+Conferencia+Nivel+ConfRival+NivelRival+T2A+T2I+T3A+T3I
                  +TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                    Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR
                  +RebDR+AsistR+RobosR+TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR
                  +FaltasR+TecnicasR+IntencionalesR,train)

#visualizamos el árbol

rpart.plot(ArbolRpart,extra=4)

#visualizamos el donde comienza el error

plotcp(ArbolRpart)

#podamos
pArbolRpart<-prune(ArbolRpart,cp=ArbolRpart$cptable[which.min(ArbolRpart$cptable[,"xerror"]),"CP"])

rpart.plot(pArbolRpart,extra=6)

#probamos el árbol con el test
testPredRpart<-predict(pArbolRpart,test,typle="class")
head(testPredRpart)

Ganador=cbind(testPredRpart,test)
Ganador=data.frame(Ganador$V,Ganador$Ganador)
names(Ganador)[1]='Prob'
names(Ganador)[2]='Resultado'

Totalpartidos=count(Ganador[Ganador$Prob>0.5,])
Totalpartidos
Resultado=count(Totalpartidos[Totalpartidos$Resultado=="V",])
Resultado=sum(as.numeric(Resultado$freq))
Numpartidos=sum(as.numeric(Totalpartidos$freq))

Acierto3=round(Resultado/Numpartidos*100,2)

Rpart=cbind(Rpart,Acierto3)
Modelos=rbind(Modelos,Rpart)

##################RANDOM FOREST#################

###############random Forest 1#############
#Solo con estadística a favor

Modelo<-randomForest(Ganador~Puntos+Ptsencontra+Local+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD
                     +Asist+Robos+Tapones+Perdidas+Faltas+Ptscontrataque
                     +Ptszona+Ptsperdida,train, # datos para entrenar 
                     ntree=50,           # cantidad de arboles   
                     mtry=20,             # cantidad de variables
                     replace=T)          # muestras con reemplazo

# Prediccion
Prediccion <- predict (Modelo , test,typle="class")

# Matriz de Confusion
MC<-table(test$Ganador,Prediccion)     
Acierto=round(sum(Prediccion==test$Ganador)/length(test$Ganador)*100,2)
Acierto

randomF=cbind("randomForest",Acierto)
randomF=data.frame(randomF)
names(randomF)[1]="Modelos"

###############random Forest 2#############
#Añadimos Conferencias y Niveles de equipo

Modelo<-randomForest(Ganador~Puntos+Ptsencontra+Local+Conferencia+Nivel+ConfRival+NivelRival
                     +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                       Ptscontrataque+Ptszona+Ptsperdida,train, # datos para entrenar 
                     ntree=75,           # cantidad de arboles   
                     mtry=24,             # cantidad de variables
                     replace=T)          # muestras con reemplazo

# Prediccion
Prediccion <- predict (Modelo , test,typle="class"); 

# Matriz de Confusion
MC<-table(test$Ganador,Prediccion)     

Acierto2=round(sum(Prediccion==test$Ganador)/length(test$Ganador)*100,2)

randomF=cbind(randomF,Acierto2)
randomF

###############random Forest 3#############
#Añadimos Estadística del equipo contrario

Modelo<-randomForest(Ganador~Puntos+Ptsencontra+Local+Conferencia+Nivel+ConfRival+NivelRival
                     +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                       Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR
                     +RebOR+RebDR+AsistR+RobosR+TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR
                     +PtszonaR+FaltasR+TecnicasR+IntencionalesR,train, # datos para entrenar 
                     ntree=72,           # cantidad de arboles   
                     mtry=43,             # cantidad de variables
                     replace=T)          # muestras con reemplazo

# Prediccion
Prediccion <- predict (Modelo , test,typle="class"); 

# Matriz de Confusion
MC<-table(test$Ganador,Prediccion)     
MC
Acierto3=round(sum(Prediccion==test$Ganador)/length(test$Ganador)*100,2)

randomF=cbind(randomF,Acierto3)
Modelos=rbind(Modelos,randomF) #Juntamos con modelos anteriores

#############adaboost#############
########adaboost1#############
#Solo con estadística a favor

model <- boosting(Ganador~Puntos+Local+Ptsencontra+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist
                  +Robos+Tapones+Perdidas+Faltas+
                    Ptscontrataque+Ptszona+Ptsperdida,train)

# Importancia de cada variable
round(model$importance,2)

# Prediccion
results <- predict(object = model, test, type = "class")

results$confusion

# Correctamente clasificados
Acierto=round(100 * sum(diag(results$confusion)) / sum(results$confusion),2)

Aboost=cbind("Adaboost",Acierto)
Aboost=data.frame(Aboost)
names(Aboost)[1]="Modelos"

########adaboost2#############
#Añadimos Conferencias y Niveles de equipo

model <- boosting(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                  +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                    Ptscontrataque+Ptszona+Ptsperdida,train)

# Importancia de cada variable
round(model$importance,2)

# predecimos
results <- predict(object = model, test, type = "class")

results$confusion

# Correctamente clasificados
Acierto2=round(100 * sum(diag(results$confusion)) / sum(results$confusion),2)

Aboost=cbind(Aboost,Acierto2)

########adaboost3#############
#Añadimos Estadística del equipo contrario
model <- boosting(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                  +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                    Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR+RobosR
                  +TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR,train)

# Importancia de cada variable
round(model$importance,2)

# Prediccion
results <- predict(object = model, test, type = "class")

results$confusion

# Correctamente clasificados
Acierto3=round(100 * sum(diag(results$confusion)) / sum(results$confusion),2)

Aboost=cbind(Aboost,Acierto3)
Modelos=rbind(Modelos,Aboost) #Juntamos con modelos anteriores

#############LOGIT REGRESSION#############
#############LOGIT REGRESSION 1#########

#Solo con estadística a favor

mylogit <- glm(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival+T2A+T2I+T3A
               +T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+Ptscontrataque+Ptszona+Ptsperdida+
                 T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR+RobosR+TaponesR+PerdidasR+PtsperdidaR
               +PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR,train, family = "binomial")
summary(mylogit)

round(exp(coefficients(mylogit)),2)

log.odds <- predict(mylogit, test)
head(log.odds)
b=data.frame(exp(log.odds)/(1+exp(log.odds)))
names(b)="Prob"

Ganador=cbind(b,test)
Ganador=data.frame(Ganador$Prob,Ganador$Ganador)
names(Ganador)[1]='Prob'
names(Ganador)[2]='Resultado'
Totalpartidos=count(Ganador[Ganador$Prob>=0.5,])
Totalpartidos
Resultado=count(Totalpartidos[Totalpartidos$Resultado=="V",])
Resultado=sum(as.numeric(Resultado$freq))
Numpartidos=sum(as.numeric(Totalpartidos$freq))

Acierto=round(Resultado/Numpartidos*100,2)

Logit=cbind("Logit",Acierto)
Logit=data.frame(Logit)
names(Logit)[1]="Modelos"

#############LOGIT REGRESSION 2#########

#Añadimos Conferencias y Niveles de equipo

mylogit <- glm(Ganador~Ptszona+Conferencia+Nivel+ConfRival+NivelRival+T2IR+TLIR
               +RobosR+PtszonaR-1, train, family = "binomial")
summary(mylogit)

log.odds <- predict(mylogit, test)
head(log.odds)
b=data.frame(exp(log.odds)/(1+exp(log.odds)))
names(b)="Prob"

Ganador=cbind(b,test)
Ganador=data.frame(Ganador$Prob,Ganador$Ganador)
names(Ganador)[1]='Prob'
names(Ganador)[2]='Resultado'
Totalpartidos=count(Ganador[Ganador$Prob>=0.5,])
Totalpartidos
Resultado=count(Totalpartidos[Totalpartidos$Resultado=="V",])
Resultado=sum(as.numeric(Resultado$freq))
Numpartidos=sum(as.numeric(Totalpartidos$freq))

Acierto2=round(Resultado/Numpartidos*100,2)

Logit=cbind(Logit,Acierto2)

#############LOGIT REGRESSION 3#########
#Añadimos Estadística del equipo contrario

mylogit <- glm(Ganador~Ptszona+Conferencia+Nivel+ConfRival+NivelRival,train, family = "binomial")
summary(mylogit)

log.odds <- predict(mylogit, test)
head(log.odds)
b=data.frame(exp(log.odds)/(1+exp(log.odds)))
names(b)="Prob"

Ganador=cbind(b,test)
Ganador=data.frame(Ganador$Prob,Ganador$Ganador)
names(Ganador)[1]='Prob'
names(Ganador)[2]='Resultado'
Totalpartidos=count(Ganador[Ganador$Prob>=0.5,])
Totalpartidos
Resultado=count(Totalpartidos[Totalpartidos$Resultado=="V",])
Resultado=sum(as.numeric(Resultado$freq))
Numpartidos=sum(as.numeric(Totalpartidos$freq))

Acierto3=round(Resultado/Numpartidos*100,2)

Logit=cbind(Logit,Acierto3)
Modelos=rbind(Modelos,Logit) #Juntamos con modelos anteriores

#########################SVM#####################
#####################SVM1################
#Solo con estadística a favor

model <- svm(Ganador~Puntos+Local+Ptsencontra+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
               Ptscontrataque+Ptszona+Ptsperdida,train)

predictedY <- predict(model, test)

Acierto=as.numeric(round(sum(predictedY==test$Ganador)/length(test$Ganador)*100,2))

svm=cbind("SVM",Acierto)
svm=data.frame(svm)
names(svm)[1]="Modelos"

#####################SVM2################
#Añadimos Conferencias y Niveles de equipo
model <- svm(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
             +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
               Ptscontrataque+Ptszona+Ptsperdida,train)

predictedY <- predict(model, test)

Acierto2=round(sum(predictedY==test$Ganador)/length(test$Ganador)*100,2)

svm=cbind(svm,Acierto2)

#####################SVM3################
#Añadimos Estadística del equipo contrario
model <- svm(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
             +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
               Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR+RobosR
             +TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR,train)

predictedY <- predict(model, test)

Acierto3=round(sum(predictedY==test$Ganador)/length(test$Ganador)*100,2)

svm=cbind(svm,Acierto3)
Modelos=rbind(Modelos,svm) #Juntamos con modelos anteriores

#############NEURAL NETWORK#############
#############NEURAL NETWORK 1#########
#Solo con estadística a favor

NeuralN = nnet(Ganador~Puntos+Ptsencontra+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                 Ptscontrataque+Ptszona+Ptsperdida,trainData, skip=TRUE,size=25)

Prediccion=predict(NeuralN,test,type='class')

MC<-table(test$Ganador,Prediccion)     
MC
Acierto=round(sum(Prediccion==test$Ganador)/length(test$Ganador)*100,2)

NN=cbind("NN",Acierto)
NN=data.frame(NN)
names(NN)[1]="Modelos"


#############NEURAL NETWORK 2#########
#Añadimos Conferencias y Niveles de equipo

NeuralN = nnet(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
               +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                 Ptscontrataque+Ptszona+Ptsperdida, train, skip=TRUE,size=30)

Prediccion=predict(NeuralN,test,type='class')

MC<-table(test$Ganador,Prediccion)     
Acierto2=round(sum(Prediccion==test$Ganador)/length(test$Ganador)*100,2)

NN=cbind(NN,Acierto2)

#############NEURAL NETWORK 3#########
#Añadimos Estadística del equipo contrario

NeuralN = nnet(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
               +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                 Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR+RobosR
               +TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR,train, skip=TRUE,size=20)

Prediccion=predict(NeuralN,test,type='class')

MC<-table(test$Ganador,Prediccion)     
Acierto3=round(sum(Prediccion==test$Ganador)/length(test$Ganador)*100,2)
NN=cbind(NN,Acierto3)
Modelos=rbind(Modelos,NN)
Modelos

#############NEURAL NET############# 

#############NEURAL NET 1-algoritmo="rprop+"#########
#Solo con estadística a favor

Nnet<-neuralnet(Ganador~Puntos+Local+Ptsencontra+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                  Ptscontrataque+Ptszona+Ptsperdida, trainNN,hidden=c(2),err.fct="sse", act.fct = "logistic",linear.output=F)

Nnet1=plot(Nnet)


head(Nnet$net.result[[1]]) #porcentaje victoria de cada partido

nn1=ifelse(Nnet$net.result[[1]]>0.5,1,0)#ponemos 1 en caso de mayor de 0.5 y 0 lo contrario

MC<-table(testNN$Ganador,nn1) #???matriz de confusión
MC

Acierto=round((1-mean(testNN$Ganador!=nn1))*100,2) #cuanto acierta nuestro modelo
Acierto

NeuralNet=cbind("NeuralNet rprop+",Acierto)
NeuralNet=data.frame(NeuralNet)
names(NeuralNet)[1]="Modelos"

#############NEURAL NET 2-algoritmo="rprop+"#########
#Añadimos Conferencias y Nivels de equipo

Nnet<-neuralnet(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                  Ptscontrataque+Ptszona+Ptsperdida, trainNN,hidden=c(4),err.fct="sse", act.fct = "logistic",linear.output=F)
Nnet2=plot(Nnet)

head(Nnet$net.result[[1]]) #porcentaje victoria de cada partido

nn1=ifelse(Nnet$net.result[[1]]>0.5,1,0)#ponemos 1 en caso de mayor de 0.5 y 0 lo contrario

MC<-table(testNN$Ganador,nn1) #matriz de confusión
MC

Acierto2=round((1-mean(testNN$Ganador!=nn1))*100,2) #cuanto acierta nuestro modelo
NeuralNet=cbind(NeuralNet,Acierto2)

#############NEURAL NET 3-algoritmo="rprop+"#########
#Añadimos Estadística del equipo contrario

Nnet<-neuralnet(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                  Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR+RobosR
                +TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR, 
                trainNN,hidden=c(4,3),err.fct="sse", act.fct = "logistic",linear.output=F)

Nnet3=plot(Nnet)

head(Nnet$net.result[[1]]) #porcentaje victoria de cada partido

nn1=ifelse(Nnet$net.result[[1]]>0.5,1,0)#ponemos 1 en caso de mayor de 0.5 y 0 lo contrario

MC<-table(testNN$Ganador,nn1) #matriz de confusión
MC

Acierto3=round((1-mean(testNN$Ganador!=nn1))*100,2) #cuanto acierta nuestro modelo
NeuralNet=cbind(NeuralNet,Acierto3)
Modelos=rbind(Modelos,NeuralNet)

#############NEURAL NET 1-algoritmo="sag"#########
#Solo con estadística a favor

Nnet<-neuralnet(Ganador~Puntos+Local+Ptsencontra+T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                  Ptscontrataque+Ptszona+Ptsperdida, trainNN,hidden=c(3),algorithm = "sag", linear.output=F)

Nnetsag1=plot(Nnet)

head(Nnet$net.result[[1]]) #porcentaje victoria de cada partido

nn1=ifelse(Nnet$net.result[[1]]>0.5,1,0)#ponemos 1 en caso de mayor de 0.5 y 0 lo contrario

MC<-table(testNN$Ganador,nn1) #matriz de confusión
MC

Acierto=round((1-mean(testNN$Ganador!=nn1))*100,2) #cuanto acierta nuestro modelo
Acierto

NeuralNet=cbind("NeuralNetsag",Acierto)
NeuralNet=data.frame(NeuralNet)
names(NeuralNet)[1]="Modelos"

#############NEURAL NET 2-algoritmo="sag"#########
#Añadimos Conferencias y Niveles de equipo

Nnet<-neuralnet(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                  Ptscontrataque+Ptszona+Ptsperdida, trainNN,hidden=c(4), algorithm = "sag", linear.output=F)

Nnetsag2=plot(Nnet)


head(Nnet$net.result[[1]]) #porcentaje victoria de cada partido

nn1=ifelse(Nnet$net.result[[1]]>0.5,1,0)#ponemos 1 en caso de mayor de 0.5 y 0 lo contrario

MC<-table(testNN$Ganador,nn1) #matriz de confusión
MC

Acierto2=round((1-mean(testNN$Ganador!=nn1))*100,2) #cuanto acierta nuestro modelo
NeuralNet=cbind(NeuralNet,Acierto2)

#############NEURAL NET 3-algoritmo="sag"#########
#Añadimos Estadística del equipo contrario

Nnet<-neuralnet(Ganador~Puntos+Local+Ptsencontra+Conferencia+Nivel+ConfRival+NivelRival
                +T2A+T2I+T3A+T3I+TLA+TLI+Reb+RebO+RebD+Asist+Robos+Tapones+Perdidas+Faltas+
                  Ptscontrataque+Ptszona+Ptsperdida+T2AR+T2IR+T3AR+T3IR+TLAR+TLIR+RebR+RebOR+RebDR+AsistR+RobosR
                +TaponesR+PerdidasR+PtsperdidaR+PtscontrataqueR+PtszonaR+FaltasR+TecnicasR+IntencionalesR, 
                trainNN,hidden=c(4,3),algorithm = "sag",linear.output=F)

Nnetsag3=plot(Nnet)

head(Nnet$net.result[[1]]) #porcentaje victoria de cada partido

nn1=ifelse(Nnet$net.result[[1]]>0.5,1,0)#ponemos 1 en caso de mayor de 0.5 y 0 lo contrario

MC<-table(testNN$Ganador,nn1) #matriz de confusión
MC

Acierto3=round((1-mean(testNN$Ganador!=nn1))*100,2) #cuanto acierta nuestro modelo
NeuralNet=cbind(NeuralNet,Acierto3)
NeuralNet
Modelos=rbind(Modelos,NeuralNet) #Juntamos con modelos anteriores
Modelos

write.csv(Modelos, paste("Modelos train.csv", sep=";"), row.names=FALSE)
