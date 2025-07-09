
##### CODA.BR 2022 - BOOTCAMP R #####

##Exercicios Tidyverse

# Primeiro, carregamos o pacote
library(tidyverse)

# Depois, carregamos a base original
alesp <- read.csv2("data/alesp.csv",
                   fileEncoding = "Windows-1252") #Com o ajuste de Encoding!


# Pergunta 1: Quais mulheres foram eleitas em 2022?
# Pergunta 2: Qual foi a média de votos em 2022 por raça?
# Pergunta 3: Qual é o ranking de votos totais por partido em 2022?
# Pergunta 4: Qual deputado reeleito ganhou mais votos de 2018 para 2022?

#### GABARITO

# Pergunta 1: Quais mulheres foram eleitas em 2022?
sexo_feminino <- alesp |>
  filter(genero == "FEMININO")

View(sexo_feminino)

sexo_feminino

# Pergunta 2: Qual foi a média de votos em 2022 por raça?
media_raca <- alesp |>
  group_by(cor_raca) |>
  summarise(media_votos = mean(votos_2022))

View(media_raca)

media_raca

# Pergunta 3: Qual é o ranking de votos totais por partido em 2022?
votos_partidos <- alesp |>
  group_by(sigla_partido) |>
  summarise(total_votos = sum(votos_2022)) |>
  arrange(desc(total_votos))

View(votos_partidos)


# Pergunta 4: Qual deputado reeleito ganhou mais votos de 2018 para 2022?

deputados <- alesp |>
  filter(!is.na(votos_2018)) |>
  group_by(nome_urna_candidato) |>
  summarise(total_votos_2018 = sum(votos_2018),
            total_votos_2022 = sum(votos_2022)) |>
  mutate(variacao_votos = total_votos_2022 - total_votos_2018,
         pct_variacao_votos = ( variacao_votos * 100 ) / total_votos_2018) |>
  arrange(desc(pct_variacao_votos))

View(deputados)

## SALVAR AS TABELAS

write.csv2(sexo_feminino, "data/sexo_feminino.csv", row.names = F)
write.csv2(media_raca, "data/media_raca.csv", row.names = F)
write.csv2(votos_partidos, "data/votos_partidos.csv", row.names = F)
write.csv2(deputados, "data/deputados.csv", row.names = F)
