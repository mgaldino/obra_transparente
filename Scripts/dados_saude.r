
library(tidyverse)
library(here)
library(readxl)
library(janitor)

df1 <- read_xlsx(here("Dados", "relatorio-geral SISMOB.xlsx"),
skip = 1) %>% 
    clean_names()
glimpse(df1)
View(df1)

df2 <- readxl::read_xlsx(here("Dados", "Obras levantamento.xlsx")) %>% 
    clean_names()

glimpse(df2)
