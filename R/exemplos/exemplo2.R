library(dplyr)

url <- "https://raw.githubusercontent.com/Davidcms23/minicurso_jurimetria/refs/heads/main/dados/violencia.csv"
vd <- read.csv(url)

vd <- read.csv("violencia.csv")

dim(vd)                        
head(vd)                       
n_distinct(vd$sigla)           

casos_ano <- vd |>
  group_by(ano) |>
  summarise(total_vd    = sum(cncvd, na.rm = TRUE),
            total_varas = sum(vexvd, na.rm = TRUE))
casos_ano

plot(casos_ano$ano, casos_ano$total_vd, type = "b",
     main = "Casos novos de Violência Doméstica no Brasil (2016-2025)",
     xlab = "Ano", ylab = "Total de casos novos (cncvd)")

vd25 <- vd |> filter(ano == 2025)

summary(vd25$cncvd)
mean(vd25$cncvd)               
median(vd25$cncvd)             

vd25 |>
  arrange(desc(cncvd)) |>
  select(sigla, porte, cncvd, vexvd) |>
  head(5)

vd <- vd |>
  mutate(taxa_vd = cncvd / h2m,
         iad_vd  = tbaixcvd / cncvd)

vd25 <- vd |> filter(ano == 2025)

vd25 |>
  arrange(desc(taxa_vd)) |>
  select(sigla, porte, cncvd, h2m, taxa_vd) |>
  head(5)

vd25 |>
  arrange(taxa_vd) |>
  select(sigla, porte, cncvd, h2m, taxa_vd) |>
  head(5)

boxplot(taxa_vd ~ ano, data = vd, las = 2,
        main = "Taxa de Violência Doméstica por 100 mil mulheres (2016-2025)",
        xlab = "Ano", ylab = "Casos novos por 100 mil mulheres")

plot(vd25$cncvd, vd25$vexvd,
     main = "Casos novos vs. Varas exclusivas de Violência Doméstica (2025)",
     xlab = "Casos novos em 2025 (cncvd)",
     ylab = "Varas exclusivas (vexvd)",
     pch = 19, col = "steelblue")

sum(vd25$iad_vd < 1, na.rm = TRUE)
hist(vd25$iad_vd,
     main = "IAD de Violência Doméstica - Justiça Estadual, 2025",
     xlab = "IAD (tbaixcvd / cncvd)", ylab = "Número de tribunais",
     col = "grey80")