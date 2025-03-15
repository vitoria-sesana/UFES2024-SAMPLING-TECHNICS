# Matéria: Amostragem 
# Graduanda: Vitória Sesana
# Ministrante da Matéria: Agatha Rodrigues

# bibliotecas -------------------------------------------------------------

library(dplyr)
library(plotly)

# S3 ----------------------------------------------------------------------

# valores do tamanho população e da amostra

valores_N <- seq(100, 1000, by = 100)  
valores_n <- seq(5, 50, by = 5)     


# com reposição -----------------------------------------------------------

# combinação entre tamanho das amostras e da população

base_AASc <- expand.grid(N = valores_N, n = valores_n)

# calculando probabilidade de inclusão
base_AASc$prob <- 1 - (1  - (1 / base_AASc$N))^base_AASc$n  


# gráfico

plotly::plot_ly(
  data = base_AASc,
  x = ~N,           
  y = ~n,           
  z = ~prob,        
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, color = ~prob, 
                colorscale = "Viridis", showscale = TRUE)
) %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "N"),
      yaxis = list(title = "n"),
      zaxis = list(title = "Prob")))

# sem reposição -----------------------------------------------------------

# combinação entre tamanho das amostras e da população

base_AASs <- expand.grid(N = valores_N, n = valores_n)

# calculando probabilidade de inclusão
base_AASs$prob <- base_AASs$n / base_AASs$N  


# gráfico

plotly::plot_ly(
  data = base_AASs,
  x = ~N,           
  y = ~n,           
  z = ~prob,        
  type = "scatter3d",
  mode = "markers",
  marker = list(size = 3, color = ~prob, 
                colorscale = "Viridis", showscale = TRUE)
) %>%
  layout(
    title = "",
    scene = list(
      xaxis = list(title = "N"),
      yaxis = list(title = "n"),
      zaxis = list(title = "Prob")))

