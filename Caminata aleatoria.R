#Caminata aleatoria (el programa corta cuando llega a uno de los estados absorbentes)
Caminata_aleatoria <- function(N, p) {
  
  # Estado inicial
  posicion <- 0 #Se puede modificar este valor, es en donde arranca la caminata
  trayectoria <- c(posicion)
  
  # Simulación hasta absorción
  while (posicion != N & posicion != -N) {
    
    # Paso de la caminata
    paso <- sample(c(-1, 1), size = 1, prob = c(1 - p, p))
    
    # Nueva posición
    posicion <- posicion + paso
    
    # Guardar
    trayectoria <- c(trayectoria, posicion)
  }
  
  # Gráfico
  plot(trayectoria, type = "l", lwd = 2,
       xlab = "Tiempo",
       ylab = "Posición",
       main = "Caminata aleatoria")
  
  abline(h = N, col = "red", lty = 2)
  abline(h = -N, col = "red", lty = 2)
  
  return(trayectoria)
}

#Cambiar los valores de la función (lo que está entre paréntesis para que se modifique el gráfico)
#El gráfico muestra la trayectoria de la caminata
tray <- Caminata_aleatoria(5,0.5)

