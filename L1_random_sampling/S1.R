# Matéria: Amostragem 
# Graduanda: Vitória Sesana
# Ministrante da Matéria: Agatha Rodrigues

# bibliotecas -------------------------------------------------------------

library(dplyr)
library(ggplot2)

# Informações -------------------------------------------------------------

populacao = c(1,2,4,4,7,7,7,8)

N = 8
n = 4

# AASc --------------------------------------------------------------------

# quantidade de combinações para AASc
qntd_AASc <- N^n 

# possíveis amostras para AASc
possiveis_amostras_AASc <- expand.grid(rep(list(populacao), n))

# média e proporção das amostras 
medias_amostras_AASc <- apply(possiveis_amostras_AASc, 1, mean)

prop_amostras_AASc <- 
  apply(possiveis_amostras_AASc, 
        1, 
        function(x) {ifelse(x > 4, 1, 0)}) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(prop = (Var1 + Var2 +Var3 +Var4)/4)

base_AASc <- cbind(Amostra = paste("Amostra", 1:qntd_AASc),
                    possiveis_amostras_AASc,
                    medias_amostras_AASc, 
                    prop_amostras_AASc$prop)

colnames(base_AASc) <- 
  c("Amostra ASc",
    paste("Amostra", 1:n),
    "Média", 
    "Proproção")

base_AASc %>% 
  head() %>% 
  xtable::xtable()


# Histograma básico

ggplot(base_AASc, aes(x = Média)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "",
       x = "Média",
       y = "Frequência/Densidade") +
  theme_minimal()


ggplot(base_AASc, aes(x = Proproção)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.6) +
  labs(title = "",
       x = "Proporção",
       y = "Frequência") +
  theme_minimal()

# AASs --------------------------------------------------------------------

# quantidade de combinações para AASs
qntd_AASs <- factorial(N) / (factorial(n) * factorial(N - n))

# possíveis amostras para AASs
possiveis_amostras_AASs <- 
  combn(populacao, 4) %>% 
  t() %>% 
  as.data.frame()

# média e proporção das amostras 
medias_amostras_AASs <- 
  apply(possiveis_amostras_AASs, 1, mean)

prop_amostras_AASs <- 
  apply(possiveis_amostras_AASs, 
        1,
        function(x) {ifelse(x > 4, 1, 0)}) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(prop = (V1 + V2 +V3 +V4)/4)


# base final
base_AASs <- cbind(Amostra = paste("Amostra", 1:qntd_AASs),
              possiveis_amostras_AASs,
              medias_amostras_AASs, 
              prop_amostras_AASs$prop)

colnames(base_AASs) <- 
  c("Amostra ASs",
    paste("Amostra", 1:n),
    "Média", 
    "Proproção")

base_AASs %>% 
  head() %>% 
  xtable::xtable()

# Histogramas

ggplot(base_AASs, aes(x = Média)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.6) +
  geom_density(color = "red", size = 1) +
  labs(title = "",
       x = "Média",
       y = "Frequência/Densidade") +
  theme_minimal()


ggplot(base_AASs, aes(x = Proproção)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "lightblue", color = "black", alpha = 0.6) +
  labs(title = "",
       x = "Proporção",
       y = "Frequência") +
  theme_minimal()
