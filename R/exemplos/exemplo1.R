library(dplyr)

url <- "https://raw.githubusercontent.com/Davidcms23/minicurso_jurimetria/refs/heads/main/dados/justica.csv"
base <- read.csv(url)

base <- read.csv("justica.csv")

dim(base)           
head(base)          
str(base)            

estadual <- base |> filter(justica == "Estadual")
dim(estadual)
n_distinct(estadual$sigla)  

est25 <- estadual |> filter(ano == 2025)

summary(est25$tc)             
mean(est25$tc)                
median(est25$tc)              

est25 |>
  arrange(desc(tc)) |>
  select(sigla, cn, cp, tbaix, tc) |>
  head(5)

est25 |> arrange(tc) |>
  select(sigla, cn, cp, tbaix, tc) |>
  head(5)

hist(est25$tc,
     main = "Taxa de congestionamento - Justiça Estadual, 2025",
     xlab = "Taxa de congestionamento", ylab = "Número de tribunais",
     col = "grey80")

boxplot(tc ~ ano, data = estadual, las = 2,
        main = "Taxa de congestionamento - Justiça Estadual",
        xlab = "Ano", ylab = "Taxa de congestionamento")

por_ano <- estadual |>
  group_by(ano) |>
  summarise(mediana = median(tc, na.rm = TRUE),
            media   = mean(tc,   na.rm = TRUE),
            n       = n())
por_ano

plot(por_ano$ano, por_ano$mediana, type = "b",
     main = "Mediana da taxa de congestionamento - Justiça Estadual",
     xlab = "Ano", ylab = "Mediana de tc")

estadual |>
  group_by(ano) |>
  summarise(abaixo_de_1 = sum(iad < 1, na.rm = TRUE),
            total       = n())

mean(estadual$iad,   na.rm = TRUE)
median(estadual$iad, na.rm = TRUE)
hist(estadual$iad, main = "IAD - Justiça Estadual", xlab = "IAD", col = "grey80")
