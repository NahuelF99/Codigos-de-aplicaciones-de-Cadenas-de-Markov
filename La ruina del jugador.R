#Parámetros del ejemplo del BlackJack
Capital_del_jugador <- 95 #Capital inicial del jugador
Capital_del_oponente <- 205 #Capital inicial de la casa para el problema
Prob_ganar <- 0.5 #Probabilidad de ganar una mano
max_it <- 1000 #Máximo de iteraciones para no entrar en bucle

#Inicialización de las variables 
iteracion <- 1
Capital_total_del_jugador <- c(Capital_del_jugador)
Capital_total_del_oponente <- c(Capital_del_oponente)

#Bucle principal de la simulación 
while (Capital_del_jugador>0 && Capital_del_oponente>0 && iteracion<=max_it) {
  #Simular una apuesta
  if (runif(1)<Prob_ganar) {
    #El jugador gana
    Capital_del_jugador <- Capital_del_jugador + 1
    Capital_del_oponente <- Capital_del_oponente - 1
  } else {
    #El jugador pierde
    Capital_del_jugador <- Capital_del_jugador - 1
    Capital_del_oponente <- Capital_del_oponente + 1
  }
  
  #Guardo el capital ganado y perdido
  Capital_total_del_jugador <- c(Capital_total_del_jugador, Capital_del_jugador)
  Capital_total_del_oponente <- c(Capital_total_del_oponente, Capital_del_oponente)
  
  iteracion <- iteracion + 1
}

#Resultados de la simulación
if (Capital_del_jugador==0) {
  cat("El jugador está en la ruina.\n")
} else if (Capital_del_oponente==0) {
  cat("El jugador cumplió con su objetivo.\n")
} else {
  cat("La simulación alcanzó el límite de iteraciones.\n")
}

#Gráfico del historial de capitales
plot(1:length(Capital_total_del_jugador), Capital_total_del_jugador, type="p", col="blue", pch=16, xlab="Iteración", ylab="Capital", main="Simulación de la ruina del jugador", ylim=c(0,max(c(Capital_total_del_jugador, Capital_total_del_oponente))))
points(1:length(Capital_total_del_oponente), Capital_total_del_oponente, col="red", pch=16)
legend("topright",legend=c("Jugador","Oponente"), col=c("blue","red"), lty=1)



