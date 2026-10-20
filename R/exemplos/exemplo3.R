library(dplyr)
library(ggplot2)

options(scipen = 999)

url <- "https://raw.githubusercontent.com/Davidcms23/minicurso_jurimetria/refs/heads/main/dados/eleicao.csv"
base <- read.csv(url)

base <- read.csv("eleicao.csv")

eleitoral <- base |> filter(justica == "Eleitoral")
dim(eleitoral)                 
n_distinct(eleitoral$sigla)    

eleitoral <- eleitoral |>
  mutate(ciclo = ifelse(ano %in% c(2012, 2016, 2020, 2024), "1. Eleição Municipal",
                        ifelse(ano %in% c(2014, 2018, 2022),       "2. Eleição Geral",
                               "3. Ano Não Eleitoral")))

por_ano <- eleitoral |>
  group_by(ano, ciclo) |>
  summarise(total_cn_mil = sum(cn, na.rm = TRUE) / 1000,
            mediana_iad  = median(iad, na.rm = TRUE),
            .groups = "drop")
por_ano

ggplot(por_ano, aes(x = factor(ano), y = total_cn_mil, fill = ciclo)) +
  geom_col() +
  labs(title = "Casos novos na Justiça Eleitoral por ciclo (2011-2025)",
       x = "Ano", y = "Total de casos novos (em milhares)", fill = "Ciclo Eleitoral") +
  theme_minimal()

ggplot(eleitoral, aes(x = ciclo, y = iad, fill = ciclo)) +
  geom_boxplot(show.legend = FALSE) +
  labs(title = "Índice de Atendimento à Demanda (IAD) por ciclo eleitoral",
       x = "Tipo de ano", y = "IAD (tbaix / cn)") +
  theme_minimal()

el24 <- eleitoral |>
  filter(ano == 2024) |>
  mutate(el_milhoes   = el / 1e6,
         el_por_urna  = el / urnas,
         proc_100k_el = (cn / el) * 100000)

el24 |>
  arrange(desc(proc_100k_el)) |>
  select(sigla, porte, cn, el, proc_100k_el) |>
  head(5)

ggplot(el24, aes(x = urnas, y = el_milhoes, color = porte)) +
  geom_point(size = 3) +
  labs(title = "Urnas eletrônicas vs. Eleitorado nos estados (2024)",
       x = "Número de urnas eletrônicas",
       y = "Total de eleitores (em milhões)",
       color = "Porte do TRE") +
  theme_minimal()
