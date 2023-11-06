library(data.table)
library(here)
library(janitor)
library(lubridate)
library(readxl)
library(tidyverse)
library(gt)
library(ipeadatar)
library(munifacil)
library(ribge)

list.files(here("Dados"))
simec <- fread(here("Dados", "simec 25-10-23 - simec.csv"), encoding = "UTF-8")
glimpse(simec)

# foz <- simec %>%
#   clean_names() %>%
#   dplyr::filter(grepl("Foz", municipio)) %>%
#   mutate(data_da_ultima_vistoria_do_estado_ou_municipio  = ymd(data_da_ultima_vistoria_do_estado_ou_municipio))


ot <- simec %>%
  clean_names() %>%
  dplyr::filter(!grepl("quadra", tolower(tipo_do_projeto)))
                
foz <- ot %>%
  dplyr::filter(grepl("Foz", municipio)) %>%
  mutate(data_da_ultima_vistoria_do_estado_ou_municipio  = ymd(data_da_ultima_vistoria_do_estado_ou_municipio))

lista_cidades_ot <- c("Araucária", "Caçador", "Campo Mourão", "Cascavel", "Chapecó",
                      "Foz do Iguaçu", "Goioerê", "Gravataí", "Guarapuava", "Imbituba", "Lajeado",
                      "Limeira", "Palhoça", "Paranaguá", "Pelotas", "Ponta Grossa", "Santa Maria", 
                      "São Francisco do Sul", "São José dos Campos", "Taubaté", "Uberlândia")

df_cidades_ot <- data.frame(municipio = lista_cidades_ot, indicator_ot = 1)

ot_cidades <- ot %>%
  left_join(df_cidades_ot, by = "municipio")
# tcu

load(here("Dados","tcu_creches.RData"))
load(here("Dados", "sit_obras_final.RData"))

tdp <- fread(here("Dados", "sit_obras_final.csv"), encoding = "Latin-1")
glimpse(tdp)

tdp1 <- tdp %>%
  dplyr::select(id, nome, municipio, uf, responsabilidade, logradouro,percentual_de_execucao, cancelada,
                concluida, execucao, atrasada, status, situacao_segundo_tbrasil, status_segundo_simec,
                data_final_prevista_e_estimada) %>%
  filter(status != "concluida")

paper_ot <- ot_cidades %>%
  inner_join(tdp1, by="id")

paper_ot %>%
  group_by( indicator_ot, situacao_da_vistoria) %>%
  summarise(contagem = n()) %>%
  mutate(total = sum(contagem), 
         indicator_ot = ifelse(is.na(indicator_ot), 0, indicator_ot),
         perc = round(contagem/total, 2))

# dados do ipea
series_ipeadata <- ipeadatar::available_series()

series_ipeadata_social <- series_ipeadata %>%
  dplyr::filter(as.character(theme) == "Social") %>%
  dplyr::filter(grepl("IBGE", as.character(source)))

series_ipeadata_econ <- series_ipeadata %>%
  dplyr::filter(as.character(theme) != "Social") %>%
  dplyr::filter(grepl("IBGE", as.character(source)))

# ADH_IDHM
# ADH_PEA18M
# ADH_PIND
# ADH_PIND_NEG
# ADH_PMPOB
# ADH_PREN10RICOS
# ADH_RDPC
# ADH_RDPC_MUL
# ADH_RDPC_NEG
# ADH_SOBRE40
# ADH_T_AGUA
# ADH_T_AGUA_ESGOTO
# ADH_T_ANALF15M
# ADH_T_FLFUND
# ADH_T_FREQ15A17
# ADH_T_MED25M
#ADH12_POP25M
# ADH12_POP65M
# PAN_QIIGG
# PAN_PIBPMG
# SCN10_CFGGN10
# SCN10_CFG10

aux_muni <- geobr::lookup_muni(code_muni = "all") %>%
  dplyr::select(-c(name_state, name_intermediate))

ipea_idhm <- ipeadatar::ipeadata("ADH_IDHM") %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_pea18 <- ipeadatar::ipeadata("ADH_PEA18M")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_ext_pobres <- ipeadatar::ipeadata("ADH_PIND")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_ext_pobres_neg <- ipeadatar::ipeadata("ADH_PIND_NEG")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_rpc <- ipeadatar::ipeadata("ADH_RDPC")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_rpc_mulheres <- ipeadatar::ipeadata("ADH_RDPC_MUL")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_rpc_negros <- ipeadatar::ipeadata("ADH_RDPC_NEG")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_ext_prop_pobres <- ipeadatar::ipeadata("ADH_PMPOB")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_10p_ricos_desigualdade <- ipeadatar::ipeadata("ADH_PREN10RICOS")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_freq_escola_15_17 <- ipeadatar::ipeadata("ADH_T_FREQ15A17")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

ipea_analfabetismo <- ipeadatar::ipeadata("ADH_T_ANALF15M")  %>%
  dplyr::filter(date > "2001-01-1" & as.character(uname) == "Municipality")

pop2010 <- ribge::habitantes2010 %>%
  mutate(tcode = as.integer(paste0(codigo_uf, codigo_munic)))


ipea_final <- ipea_idhm %>%
  inner_join(aux_muni, by = join_by(tcode == code_muni)) %>%
  inner_join(ipea_pea18, by = "tcode") %>%
  inner_join(ipea_ext_pobres, by = "tcode") %>%
  inner_join(ipea_ext_pobres_neg, by = "tcode") %>%
  inner_join(ipea_rpc, by = "tcode") %>%
  inner_join(ipea_rpc_mulheres, by = "tcode") %>%
  inner_join(ipea_rpc_negros, by = "tcode") %>%
  inner_join(ipea_ext_prop_pobres, by = "tcode") %>%
  inner_join(ipea_10p_ricos_desigualdade, by = "tcode") %>%
  inner_join(ipea_freq_escola_15_17, by = "tcode") %>%
  inner_join(ipea_analfabetismo, by = "tcode") %>%
  inner_join(pop2010, by = "tcode")

m.out <- matchit(treat ~ age + educ + race + married + 
                   nodegree + re74 + re75, data = lalonde,
                 replace = TRUE)

dplyr::filter(
  series_ipeadata,
  stringr::str_detect(source, stringr::regex("caged", ignore_case = TRUE))
)

## 
load(here("Dados", "obras_inicio_projeto.Rdata"))
glimpse(obras_inicio_projeto)

load(here("Dados", "obras_fim_seg_fase.Rdata"))

obras_fim_seg_fase <- obras_fim_seg_fase %>%
  mutate(concluida = as.numeric(grepl("Concluída", situacao))) %>%
  left_join(df_cidades_ot, by="municipio")

glimpse(obras_fim_seg_fase)

obras_inicio_projeto_ot <- obras_inicio_projeto %>%
  left_join(df_cidades_ot, by="municipio")
glimpse(obras_inicio_projeto_ot)

unique(obras_inicio_projeto_ot$situacao)
obras_inicio_projeto_ot <- obras_inicio_projeto_ot %>%
  mutate(concluida = grepl("Concluída", situacao))

antes_depois_conclusao <- obras_inicio_projeto_ot %>%
  group_by(indicator_ot, concluida) %>%
  summarise(contagem = n()) %>%
  mutate(indicator_ot = ifelse(is.na(indicator_ot), 0, indicator_ot),
         total = sum(contagem),
         perc = round(contagem/total, 2))

antes_depois_conclusao %>%
  dplyr::filter(concluida == TRUE) %>%
  dplyr::select(-c(concluida, contagem, total)) %>%
  ungroup() %>%
  gt()%>%
  tab_header(
    title = "Percentage of finished fonstruction works before the project",
    subtitle = "Situation in August 2017") %>%
  cols_label(
    indicator_ot = "Obra Transparente Indicator",
    perc = "Percentage of finished constructions"
  )

lista_obras_ot <- read_excel("Dados/181114 - Situação obras_atual.xlsx", 
                             sheet = "SIMEC-Mai17", skip = 1) %>%
  clean_names() %>%
  dplyr::select(id, nome, situacao, situacao2, levant_os_jun_17, municipio, uf)
glimpse(lista_obras_ot)
