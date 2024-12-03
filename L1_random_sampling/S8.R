# Matéria: Amostragem 
# Graduanda: Vitória Sesana
# Ministrante da Matéria: Agatha Rodrigues


# bibliotecas -------------------------------------------------------------

library(dplyr)
library(rvest)
library(stringr)

# web scrappling ----------------------------------------------------------

# inspect element > console > "document.querySelectorAll("table")"

url <- paste0("https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_popula%C3%A7%C3%A3o_(2022)")
webpage <- read_html(url)


j <- webpage %>%
  html_table() 


# manipulation -------------------------------------------------------------

base <- j[[1]] %>% 
  janitor::clean_names() %>% 
  filter(!(municipio == "Brasil")) %>% 
  mutate(codigo_ibge = as.numeric(codigo_ibge),
       populacao = as.numeric(str_remove_all(populacao, "\\s")))

# analysis ----------------------------------------------------------------

mean_pop <- mean(base$populacao); mean_pop
var_pop <- var(base$populacao); var_pop

boxplot(tail(base$populacao), 100)


# amostras ----------------------------------------------------------------

N = nrow(base)
n_tamanhos = c(10, 50, 100, 200, 400, 800, 1000, 2000, 4000)

# amostras sem reposição --------------------------------------------------
set.seed(0304)
medias_amostrais_SEM <- sapply(n_tamanhos, function(n) {
  amostras <- sample(base$populacao, size = n, replace = FALSE)
  mean(amostras)
})

cbind(n_tamanhos, medias_amostrais_SEM) %>% 
  xtable::xtable(caption = "Médias amostrais de amostras aleatórias sem reposição.") %>% 
  print(include.rownames = FALSE)


# amostras com reposição --------------------------------------------------

set.seed(0304)
medias_amostrais_COM <- sapply(n_tamanhos, function(n) {
  amostras <- sample(base$populacao, size = n, replace = TRUE)
  mean(amostras)
})

cbind(n_tamanhos, medias_amostrais_COM) %>% 
  xtable::xtable(caption = "Médias amostrais de amostras aleatórias sem reposição.") %>% 
  print(include.rownames = FALSE)
