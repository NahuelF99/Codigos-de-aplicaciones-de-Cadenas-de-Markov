Ehrenfest <- function(N, steps, x0) {
  # N     = total de partículas
  # steps = número de transiciones
  # x0    = partículas en la urna 1 al inicio
  
  x <- numeric(steps + 1)
  x[1] <- x0
  
  # Dinámica Ehrenfest
  for (t in 1:steps) {
    if (runif(1) < x[t] / N) {
      x[t + 1] <- x[t] - 1
    } else {
      x[t + 1] <- x[t] + 1
    }
  }
  
  # Colores fijos (sin degradé)
  cold_color <- "blue"   # urna 1
  warm_color <- "red"    # urna 2
  
  # Gráfico vacío
  plot(0:steps, x,
       type = "n",
       ylim = c(0, N),
       xlab = "Transición",
       ylab = "Cantidad de partículas",
       main = "Modelo de Ehrenfest")
  
  # Dibujar los PUNTOS
  for (t in 0:steps) {
    urna1 <- x[t + 1]
    urna2 <- N - x[t + 1]
    
    points(t, urna1, col = cold_color, pch = 16, cex = 1.2)  # azul
    points(t, urna2, col = warm_color, pch = 16, cex = 1.2)  # rojo
  }
  
  return(x)
}

# Ejecutar
Ehrenfest(10, 25, 8)
