# Predicción Resultados NBA 17-18

Basado en la temporada 2017-18 de la NBA para predecir ganador del partido. Este proyecto está dividido en 2 scripts:

1. ESPN_NBA_stats - Se obtienen las estadísticas de los partidos.
2. Prediccion_Partidos* - Se ejecutan diferentes algoritmos predictivos (Random Forest, Decision Tree, Adabost, Logit, SVM, Neural Network) con el fin de determinar que modelo es más certero para la predicción del ganador del partido.

*Previamente a la carga del csv en R, se han hecho unas modificaciones en Excel.
*El csv "Prediccion_Partidos_NN" se utiliza para la red neuronal ya que transformamos Ganador y perdedor en 1s y 0s
