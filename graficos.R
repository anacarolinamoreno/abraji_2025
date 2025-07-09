

## APRENDENDO A FAZER GRÁFICOS COM R (GGPLOT2)

#Referência: https://livro.curso-r.com/8-1-o-pacote-ggplot2.html

#Carregar pacotes
library(ggplot2)

##APRENDENDO A GRAMÁTICA DO GGPLOT:
#A lógica desse pacote é inserir camadas a cada linha.
#Primeiro, o tipo de gráfico e quais colunas da tabela vão ser usadas.
#Depois, inserir cores, tamanhos, legendas, título, fonte etc.

##PRINCIPAIS CARACTERÍSTICAS DO GGPLOT:
#Para adicionarmos mais camadas, não usamos o PIPE, e sim o sinal de +
#Uma palavra-chave do ggplot é "aes" (aesthetic, a estética do gráfico)
#Podemos criar um novo objeto para o plot...
#...ou já incluir o código dentro do "parágrafo".

##EXEMPLO: GRÁFICO DE BARRAS.
#Vamos usar a tabela MEDIA_RACA:

#Passo 1 - abrir a tabela:
media_raca <- read.csv2("data/media_raca.csv")

#Passo 2 - definir as colunas que serão usadas:
View(media_raca)
#No eixo X teremos a raça, no eixo Y a média.

#Passo 3 - fazer os ajustes:
#Arredondar os valores
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0))

#Ordenar do maior valor para o menor (mas para o gráfico)
#OBS: Nesse caso, não é com um simples arrange.
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos, decreasing = T))

#Criar um gráfico de barras (esse caso, vertical, em colunas)
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos, decreasing = T)) |>
  ggplot() +
  geom_col(aes(x = cor_raca, y = media_votos))

#Agora vamos criar o mesmo gráfico, mas com barras na horizontal
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos, decreasing = T)) |>
  ggplot() +
  geom_col(aes(x = media_votos, y = cor_raca))

#Nesse caso, o "decreasing = T" não serve pra gente.
#Vamos refazer o código, mas deixando na ordem "padrão":
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos)) |>
  ggplot() +
  geom_col(aes(x = media_votos, y = cor_raca))

#Hora de escolher um tema mais bonitinho:
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos)) |>
  ggplot() +
  geom_col(aes(x = media_votos, y = cor_raca)) +
  theme_light()

#Agora, vamos adicionar legenda, título e fonte:
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos)) |>
  ggplot() +
  geom_col(aes(x = media_votos, y = cor_raca)) +
  theme_light() +
  labs(title="MÉDIA DE VOTOS POR COR/RAÇA",
      subtitle="Eleições 2022 para a Alesp",
      caption="Fonte: TRE-SP",
      x="Média de votos",
      y="Cor ou raça")

#Podemos também incluir o valor de cada barra:
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos)) |>
  ggplot() +
  geom_col(aes(x = media_votos, y = cor_raca)) +
  theme_light() +
  labs(title="MÉDIA DE VOTOS POR COR/RAÇA",
       subtitle="Eleições 2022 para a Alesp",
       caption="Fonte: TRE-SP",
       x="Média de votos",
       y="Cor ou raça") +
  geom_label(aes(x = media_votos, y = cor_raca, label = media_votos))

#Por último, vamos dar uma cor mais bonita ao gráfico:
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos)) |>
  ggplot() +
  geom_col(aes(x = media_votos,
               y = cor_raca,
               fill = cor_raca)) +
  theme_light() +
  labs(title="MÉDIA DE VOTOS POR COR/RAÇA",
       subtitle="Eleições 2022 para a Alesp",
       caption="Fonte: TRE-SP",
       x="Média de votos",
       y="Cor ou raça") +
  geom_label(aes(x = media_votos, y = cor_raca, label = media_votos))

#Mas, não queremos a legenda, porque não é necessário nesse caso:
plot_media_raca <- media_raca |>
  mutate(media_votos = round(media_votos,0),
         cor_raca = reorder(cor_raca, media_votos)) |>
  ggplot() +
  geom_col(aes(x = media_votos,
               y = cor_raca,
               fill = cor_raca),
           show.legend = F) +
  theme_light() +
  labs(title="MÉDIA DE VOTOS POR COR/RAÇA",
       subtitle="Eleições 2022 para a Alesp",
       caption="Fonte: TRE-SP",
       x="Média de votos",
       y="Cor ou raça") +
  geom_label(aes(x = media_votos, y = cor_raca, label = media_votos))

#Finalizado o gráfico, vamos salvar a imagem em um arquivo PNG.

##SALVAR IMAGENS
#Para isso, vamos usar o pacote COWPLOT.

install.packages("cowplot")

#Vamos carregar o pacote de um outro jeito:
cowplot::save_plot("grafico_media_raca.png", plot_media_raca)

#Pronto! =)







########PRÁTICA DA VIDA REAL########
plot_eleitos_raca <- media_raca %>% 
  mutate(cor_raca = reorder(cor_raca, total_candidatos)) %>% 
  ggplot() +
  geom_col(aes(x = total_candidatos,
               y = cor_raca,
               fill = cor_raca),
           show.legend = F) +
  theme_light() +
  labs(title="TOTAL DE ELEITOS POR COR/RAÇA",
       subtitle="Eleições 2022 para a Alesp",
       caption="Fonte: TRE-SP",
       x="Média de votos",
       y="Cor ou raça") +
  geom_label(aes(x = total_candidatos, y = cor_raca, label = total_candidatos))




               
plot_eleitos_raca
               
cowplot::save_plot("grafico_eleitos_raca.png", plot_eleitos_raca)


