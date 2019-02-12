# Predicción Resultados NBA 17-18

Basado en la temporada 2017-18 de la NBA para predecir el ganador del partido. Este proyecto está dividido en 2 scripts:

1. ESPN_NBA_stats - Se obtienen las estadísticas de los partidos.
2. Prediccion_Partidos* - Se ejecutan diferentes algoritmos predictivos (Random Forest, Decision Tree, Adabost, Logit, SVM, Neural Network) con el fin de determinar que modelo es más certero para la predicción del ganador del partido. 

*Previamente a la carga del csv en R, se han hecho unas modificaciones en Excel:
    -Añadido columnas estadísticas rival.
    -Añadido nivel de equipos y conferencias: 
          -Los equipos quedan dividios en 3 niveles (0,1,2), siendo el 2 el nivel más alto.
          -Los niveles están basados en el número de victorias conseguidas por cada uno de los 30 equipos.
    
*Se utilizan 2 csv. El archivo Prediccion_Partidos_NN se utilizará solamente para las redes neuronales ya que el ganador y perdedor está cambiado a 1 y 0.



