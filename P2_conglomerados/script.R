
# pacotes -------------------

library(dplyr)


# base ----------------------

conglomerado <- 1:12
B_alpha <- c(6,2,11,7,8,14,6,2,2,5,12,6)
mu_alpha <- c(24.32, 27.06, 27.6, 28.01, 
              27.56, 29.07, 32.03, 28.41,
              28.91, 25.55, 28.58, 27.27)
sigma2_alpha <- c(5.07, 5.53, 6.24, 6.59,
                  6.21, 6.12, 5.97, 6.01, 
                  5.74, 6.78, 5.87, 5.38)

base <- 
  cbind(
    conglomerado, 
    B_alpha,
    mu_alpha,
    sigma2_alpha
    ) %>% 
  as.data.frame() %>% 
  mutate(
    conglomerado = as.integer(conglomerado)
  ) 

base %>% 
  select(-conglomerado) %>% 
  xtable::xtable()


# amostragem por conglomerados --------

N <- sum(base$B_alpha)
A <- 12
a <- 4
B_barra <- N / A
base <- base %>% 
  mutate(
    tau_alpha = B_alpha*mu_alpha
  )

tau <- sum(base$tau_alpha)
media <- tau / N

set.seed(0304)

selecao_amostras_AC <- 
  sample(
    x = base$conglomerado,
    size = a,
    replace = TRUE
    ) 

amostras_AC <- data.frame( 
  conglomerado = selecao_amostras_AC
  ) %>% 
  left_join( base, by = c("conglomerado"))

tau_barra <- sum(amostras_AC$tau_alpha) / a

mu_AC <- tau_barra / B_barra

sigma2_ec <- (1/A) * sum(( (base$B_alpha/B_barra) * base$mu_alpha - media)^2)

var_AC <- sigma2_ec / 2

est_var_AC <- (1/(a*(a-1))) * sum(( (base$B_alpha/B_barra) * base$mu_alpha - mu_AC)^2)

# amostragem aleatoria simples --------

n <- 27
set.seed(0304)
sample(
  x = filial,
  size = n,
  replace = TRUE
  )

# saidas ------------------------------

mu_AC
var_AC
est_var_AC
