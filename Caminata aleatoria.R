Caminata_aleatoria <- function(N, p) {
  
  # Estado inicial
  posicion <- 0
  trayectoria <- c(posicion)
  
  # Simulación hasta absorción
  while (posicion != N & posicion != -N) {
    
    paso <- sample(c(-1, 1), size = 1, prob = c(1 - p, p))
    posicion <- posicion + paso
    trayectoria <- c(trayectoria, posicion)
  }
  
  # Gráfico SOLO con puntos, sin nada adicional
  plot(trayectoria,
       type = "p",     # solo puntos
       pch = 16,        # punto sólido
       cex = 1.2,
       col = "blue",
       xlab = "Tiempo",
       ylab = "Posición",
       main = "Caminata aleatoria")
  
  return(trayectoria)
}

# Ejecutar
tray <- Caminata_aleatoria(5, 0.5)

