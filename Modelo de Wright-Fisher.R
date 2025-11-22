#Modelo de Wright-Fisher (usando una distribución binomial)
Wright_Fisher <- function(N, p0, g) {
#N <- Cantidad total de alelos
#p0 <- Frecuencia inicial del alelo A
#g <- Cantidad de generaciones a simular
  
  X <- numeric(g + 1)   
  X[1] <- rbinom(1, N, p0) #Cantidad de alelos A
  
  for (t in 1:g) {
    p <- X[t] / N                 # Frecuencia del alelo A en generación t
    X[t + 1] <- rbinom(1, N, p)   # Transición Wright-Fisher (binomial)
  }
  
  return(X)
}

# Gráfico con cantidades de individuos por alelo

plot_wf_counts <- function(X, N) {
  g <- 0:(length(X) - 1)
  
  count_A <- X
  count_a <- N - X
  
  plot(
    g, count_A,
    type = "l", lwd = 2, col = "blue",
    ylim = c(0, N),
    xlab = "Generación",
    ylab = "Cantidad de individuos",
    main = "Modelo Wright–Fisher (2 alelos) — Cantidades"
  )
  
  points(g, count_A, col = "blue", pch = 16)
  
  lines(g, count_a, col = "red", lwd = 2)
  points(g, count_a, col = "red", pch = 16)
  
  legend(
    "topright",
    legend = c("Alelo A", "Alelo a"),
    col = c("blue", "red"),
    lwd = 2, pch = 16, bty = "n"
  )
}

#Cambiar los valores de N, p0 y g para modificar el gráfico
#El gráfico muestra la cantidad de individuos con tales alelos 
N <- 300
p0 <- 0.5
g <- 1000
traj <- Wright_Fisher(N, p0, g)
plot_wf_counts(traj, N)


