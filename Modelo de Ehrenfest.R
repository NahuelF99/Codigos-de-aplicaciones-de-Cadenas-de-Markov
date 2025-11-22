#Modelo de Ehrenfest
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
  
  # Colores frío y cálido
  cold_color <- "blue"
  warm_color <- "red"
  
  # Gráfico base
  plot(0:steps, x,
       type = "n",
       ylim = c(0, N),
       xlab = "Transición",
       ylab = "Cantidad de partículas",
       main = "Modelo de Ehrenfest")
  
  # Dibujar SOLO 2 puntos por tiempo
  for (t in 0:steps) {
    urna1 <- x[t + 1]
    urna2 <- N - x[t + 1]
    
    points(t, urna1, col = cold_color, pch = 16, cex = 1.2)  # punto frío
    points(t, urna2, col = warm_color, pch = 16, cex = 1.2) # punto cálido
  }
  
  return(x)
}

#Cambiar los valores de la función (lo que está entre paréntesis para que se modifique el gráfico)
#El gráfico muestra la cantidad de partículas en cada urna hasta llegar al equilibrio
Ehrenfest(2001,600,1000)