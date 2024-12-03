# Matéria: Amostragem 
# Graduanda: Vitória Sesana
# Ministrante da Matéria: Agatha Rodrigues



# bibliotecas -------------------------------------------------------------

library(dplyr)

# S1 ----------------------------------------------------------------------

populacao = c(1,2,4,4,7,7,7,8)

N = 8
n = 4

## a

# Consegue-se calcular a quantidade de todas as possíveis amostras com base nas combinações
qntd_combinacoes = factorial(N) / (factorial(n) * factorial(N-n))

possiveis_amostras <- combn(populacao, 4) %>% 
  t() %>% 
  as.data.frame()

print(possiveis_amostras)

# considera-se amostragem com reposição

# b

medias_amostras <- apply(possiveis_amostras, 1, mean)

prop_amostras <- apply(possiveis_amostras, 1, function(x) {ifelse(x > 4, 1, 0)}) %>% 
  t() %>% 
  as.data.frame() %>% 
  mutate(prop = (V1 + V2 +V3 +V4)/4)

medias_amostras
prop_amostras

base <- cbind(possiveis_amostras,medias_amostras, prop_amostras$prop)
base





