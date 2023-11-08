library(data.table)
library(tidyverse)
library(knitr)
library(CausalQueries)

imbituba <- fread("Dados/Servidores_efetivos_imbituba.csv", skip = 14)
glimpse(imbituba)

imbituba %>%
  summarise(num_engenheiro = sum(grepl("[Ee]ngenheiro [Cc]ivil", V3)))
# 21


imbituba_eng <- imbituba %>%
  filter(grepl("[Ee]ngenheiro [Cc]ivil", V3))


lajeado <- fread("Dados/relacaoservidores-1697891422613.csv", skip = 14, encoding = "Latin-1")
glimpse(lajeado)

lajeado %>%
  mutate(V2 = tolower(V2)) %>%
  summarise(num_engenheiro = sum(grepl("engenheiro civil", 2)))
View(lajeado)

# pelotas

pelotas <- fread("Dados/Pelotas_servidores2018.csv", encoding = "Latin-1")
glimpse(pelotas)

pelotas_cargo <- pelotas %>%
  filter(MES == 9) %>%
  filter(grepl("Engenheiro Civil", CARGO)) %>%
  group_by(CARGO) %>%
  summarise(total = n_distinct(MATRICULA), tot = n_distinct(ID_CONTRATO_RH))

pelotas %>%
  filter(MES == 9) %>%
  filter(grepl("Engenheiro Civil", CARGO)) %>%
  group_by(CARGO, LOTACAO) %>%
  summarise(total = n_distinct(MATRICULA), tot = n_distinct(ID_CONTRATO_RH))

pelotas_cargo1 <- pelotas %>%
  filter(MES == 9) %>%
  filter(grepl("Agente Fiscal", CARGO))


model1 <- make_model("X->Y") %>% set_restrictions(decreasing("X", "Y"))
get_parameter_matrix(model1)

model1 %>%
  query_model("Y[X=1] - Y[X=0]", n_draws = 10000) %>%
  kable

## Taubae causal model
options(mc.cores = parallel::detectCores())
# modelo nulo
model_taubate <- make_model("M -> C") %>% set_restrictions(non_decreasing("M", "C"), keep=T)
# model_taubate1 <- make_model("M -> C; M <-> C") %>% 
#   set_restrictions(labels = list(M = "M0"), keep = TRUE) # preciso rule out C.01_M.0 e C.01_M.1# i.e., não tem adverso.

model_taubate1 <- make_model("O -> M -> C; M <-> C") %>%
  set_restrictions(non_decreasing("M", "C"), keep=T) # rule out C.10_M.0 e C.10_M.1# i.e., não tem adverso.



plot(model_taubate1)

draw_causal_type(model_taubate)
get_causal_types(model_taubate)

draw_causal_type(model_taubate1)
get_causal_types(model_taubate1)
get_nodal_types(model_taubate1)
get_parameters(model_taubate1)
# model_taubate <- model_taubate %>% set_priors(c(1,1,81.5,18.5/3, 18.5/3,18.5/3))

model_taubate %>% get_priors
model_taubate$parameters_df 

model_taubate1$parameters_df 

model_taubate %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", Share_positive = "C[M=1] > C[M=0]"),
              given = c(TRUE,  "C==1 & M==1"),
              using = c("parameters", "priors"),
              n_draws = 10000) %>%
  kable

model_taubate1 %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", Share_positive = "C[M=1] > C[M=0]"),
              given = c(TRUE,  "C==1 & M==1"),
              using = c("priors", "priors"),
              n_draws = 10000) %>%
  kable

updated1 %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", Share_positive = "C[M=1] > C[M=0]"),
              given = c(TRUE,  "C==1 & M==1"),
              using = c("posteriors"),
              expand_grid = TRUE,
              n_draws = 10000) %>%
  kable

model_taubate %>%
  query_model("C[M=1] - C[M=0]", n_draws = 10000, case_level = T) %>%
  kable

data_taubate  <- data.frame(O = rep(1, 8), M = c(0,0,0,0, 1, 1, 1, 1), C = c(0, 0, 0, 0, 1, 1, 1, 1))
compact_data <-  collapse_data(data_taubate, model_taubate) 

compact_data1 <-  collapse_data(data_taubate, model_taubate1)

updated <- update_model(model_taubate, compact_data, iter  = 10000)
updated1 <- update_model(model_taubate1, compact_data1, iter  = 10000)

updated1$causal_types %>% data.frame() %>% glimpse()
updated$causal_types %>% data.frame() %>% glimpse()

updated1$posterior_distribution %>% 
  data.frame() %>%
  glimpse()

mydf1 <- updated1$posterior_distribution %>% 
  data.frame() 

mydf1 %>%
  mutate(ate = C.01_M.01*M.01 + C.01_M.10*M.10) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate),
            q.025 = quantile(ate, .025),
            q.975 = quantile(ate, .975))

mydf1 %>%
  mutate(ate = C.11_M.1*M.1 + C.11_M.0*M.0) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate),
            q.025 = quantile(ate, .025),
            q.975 = quantile(ate, .975))

mydf1 %>%
  mutate(post_ratio = (C.01_M.1*M.1 + C.01_M.0*M.0)/(C.11_M.1*M.1 + C.11_M.0*M.0)) %>%
  summarise(mean_ate = mean(post_ratio),
            sd_ate = sd(post_ratio),
            q.025 = quantile(post_ratio, .025),
            q.975 = quantile(post_ratio, .975))

mydf1 %>%
  mutate(estimand1 = C.01_M.0*M.0 + C.01_M.1*M.1 - C.11_M.0*M.0 - C.11_M.1*M.1 ) %>%
  summarise(media = mean(estimand1),
            num_pos = sum(estimand1>0)/n())

print(updated1$stan_objects)

updated$stan_objects$type_distribution %>% data.frame() %>% glimpse() %>% summary()

updated1$stan_objects$type_distribution %>% data.frame() %>% glimpse() %>% summary()

updated1$posterior_distribution %>% 
  data.frame() %>%
  ggplot(aes(M.1, C.01_M.1 - C.00_M.1)) + 
  geom_point()

updated1$posterior_distribution %>% 
  data.frame() %>%
  ggplot(aes(C.01_M.1 - C.00_M.1)) + geom_histogram()



mydf1 %>%
  mutate(ate = C.11_M.1*M.1 + C.11_M.0*M.0) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate),
            q.025 = quantile(ate, .025),
            q.975 = quantile(ate, .975))

mydf1 %>%
  mutate(ate = C.00_M.1*M.1 + C.00_M.0*M.0) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate),
            q.025 = quantile(ate, .025),
            q.975 = quantile(ate, .975))


mydf1 %>%
  mutate(ate = C.11_M.1*M.1) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate),
            q.025 = quantile(ate, .025),
            q.975 = quantile(ate, .975))

mydf <- updated$posterior_distribution %>% 
  data.frame() 

mydf %>%
  mutate(ate = C.01) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate),
            q.025 = quantile(ate, .025),
            q.975 = quantile(ate, .975))
updated1 %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", Share_positive = "C[M=1] > C[M=0]"),
              given = c(TRUE,  "C==1 & M==1"),
              using = c("posteriors"),
              expand_grid = TRUE,
              n_draws = 10000) %>%
  kable


updated1 %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", Share_positive = "C[M=1] > C[M=0]"),
              given = c(TRUE,  "C==1 & M==1"),
              using = c("posteriors"),
              expand_grid = TRUE,
              n_draws = 10000) %>%
  kable


mydf %>%
  mutate(ate = C.01_M.1) %>%
  summarise(mean_ate = mean(ate),
            sd_ate = sd(ate))

mydf %>%
  ggplot(aes(C.01_M.0 - C.00_M.0)) + geom_histogram()

model_taubate1 %>% 
  query_distribution(list(increasing = increasing("M", "C")), 
                     using = "priors") %>%
  ggplot(aes(increasing)) + geom_histogram()  + 
  xlab("Prior on C increasing in M")  

Q <- "C[M=1] - C[M=0]"

updated1 %>% 
  query_distribution(list(increasing = increasing("M", "C"), Q), 
                     using = "posteriors") %>%
  ggplot(aes(increasing)) + geom_histogram()  + 
  xlab("Posterior on ATE")  

updated1 %>% 
  query_distribution(list(increasing = increasing("M", "C"), ATE = Q), 
                     using = "posteriors") %>%
  ggplot(aes(ATE)) + geom_histogram()  + 
  xlab("Posterior on C increasing in M") 

updated1 %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", 
                           Share_positive = "C[M=1] > C[M=0]"),
              given = c(TRUE,"C==1 & M==1"),
              using = c("priors", "posteriors"),
                        expand_grid = TRUE)

model_taubate1 %>% get_query_types("C[M=1]>C[M=0]")
model_taubate1 %>% get_query_types("C[M=0]>=C[M=1]")

get_parameter_matrix(updated)

updated1 %>%
  query_model(query = list(ATE = "C[M=1] - C[M=0]", 
                           Share_positive = "C[M=1] > C[M=0]",
                           Causal_atrib = "C[M=1]> C[M=0]"),
              given = c(TRUE,  "C==1 & M==1", "M==1"),
              using = c("posteriors", "posteriors", "posteriors" ))

updated1$parameters_df %>% kable
updated1$parameters_df %>% kable

# modelo causal
model_taubate <- make_model("CSO -> D")

model_taubate <- model_taubate %>% set_priors(c(1,1,81.5,18.5/3, 18.5/3,18.5/3))

model_taubate %>% get_priors
model_taubate$parameters_df 

model_taubate %>%
  query_model("D[CSO=1] - D[CSO=0]", n_draws = 10000) %>%
  kable

data_taubate  <- data.frame(CSO = c(0,0,0,0, 1, 1, 1, 1), D = c(0, 0, 0, 0, 1, 1, 1, 1))

compact_data <-  collapse_data(data_taubate, model_taubate) 

updated <- update_model(model_taubate, compact_data, iter  = 10000)

query_model(
  updated,
  query = list("D | CSO=1" = "CSO==1", 
               ATE = "D[CSO=1] - D[CSO=0]"),
  using = "priors")


results <- query_model(
  updated,
  query = list("D | CSO=1" = "CSO==1", 
               ATE = "D[CSO=1] - D[CSO=0]"),
  using = "posteriors")

results


# wit confounding

model_taubate_confound <- make_model("CSO -> D; CSO <-> D")

# model_taubate <- model_taubate %>% set_priors(c(1,1,81.5,18.5/3, 18.5/3,18.5/3))

model_taubate_confound %>% get_priors
model_taubate_confound$parameters_df 

model_taubate_confound %>%
  query_model("D[CSO=1] - D[CSO=0]", n_draws = 10000) %>%
  kable

data_taubate  <- data.frame(CSO = c(0,0,0,0, 1, 1, 1, 1), D = c(0, 0, 0, 0, 1, 1, 1, 1))

compact_data <-  collapse_data(data_taubate, model_taubate_confound) 

updated <- update_model(model_taubate_confound, compact_data, iter  = 10000)

query_model(
  updated,
  query = list("D | CSO=1" = "CSO==1", 
               ATE = "D[CSO=1] - D[CSO=0]"),
  using = "priors")


results_confound <- query_model(
  updated,
  query = list(ATE = "D[CSO=1] - D[CSO=0]"),
  using = "posteriors")

results_confound %>%
  kable()

p  = get_type_prob(model_taubate_confound)

## Causal queries ; T <-> I

model <- make_model("T -> I <- SUP; T -> R <- I") %>%
  set_restrictions("(R[I=0]==0)") 
                   # I is not decreasing in T
 # I is not decreasing in SUP non_decreasing("T", "I"), c(non_decreasing("T","R"),



model %>%  plot(model)

model %>%
  query_model("R[T=1, SUP = 0] - R[T=0, SUP = 0]", n_draws = 10000) %>%
  kable

model %>%
  query_model("R[T=1] - R[T=0]", n_draws = 10000) %>%
  kable

# First row -> Taubaté
# second Foz do Iguaçu
# Palhoça
# Goiorê
data  <- data.frame(city = c("Taubaté-SP", "Foz do Iguaçu - PR", "Palhoça-SC",
                             "Goioerê - PR", "Cascavel - PR", "Paranaguá - PR",
                             "São José dos Campos - SP"),
                    T = c(1, 1, 1, 1, 1, 1, 1), R = c(1, 1, 1, 1, 0, 0, 0), 
                    SUP = c( 0, 0 , 0, 0, 1, 1, 0), I = c(NA, NA, NA, NA, NA, NA, NA))

data1 <- data %>%
  select(-city)
compact_data <-  collapse_data(data1, model) 

CausalQueries:::prep_stan_data(model, compact_data)

updated <- update_model(model, data, iter  = 10000)

results <- query_model(
  updated,
  query = list("F | T=1" = "T==1", 
               ATE = "F[T=1] - F[T=0]", 
               PC  = "F[T=1] > F[T=0]"),
  given = c("T==1", TRUE, "T==1 & F==1"),
  using = "posteriors")

results

model$nodal_types
model$causal_types
model$parameters_df %>%
  kable()
