# Script para analisar dados do Censo IBGE mostrado na oficina do Rafael Pereira
# Fonte: https://ipeagit.github.io/censobr_oficina_abraji_2025/

# Abaixo temos análises pro três aspectos:
# 1- POPULAÇÃO
# 2- DOMICÍLIO
# 3- SETOR CENSITÁRIO


# Criar um objeto chamado pkgs ("packages") que contenha uma lista de STRINGS com nomes de pacotes
pkgs <- c(
  'censobr', # Pacote que puxa dados do Censo IBGE (por enquanto atualizado até 2010)
  'geobr', # Pacote que ajuda a criar mapas do
  'arrow', # Pacote útil pra trabalhar com grandes bases de dados que usam muita memória
  'dplyr', # Pacote do Tidyverse com funções pra limpar e manipular bases
  'ggplot2', # Pacote do Tidyverse com funções pra criar visualizações
  'quantreg', # Pacote com funções que fazem cálculos estatísticos de "regressão quantílica"
  'sf' # Pacote Simple Features, para trabalhar com mapas
)

# Mandar o RStudio salvar os pacotes nomeados dentro do objeto pkgs
install.packages(pkgs)

### Carregar bibliotecas
library(censobr)
library(arrow)
library(dplyr)
library(ggplot2)


##### A- POPULAÇÃO #####

#### ANÁLISE 1 - PIRÂMIDE ETÁRIA

## Função de análise da população: read_population()
## Cria objeto com dados puxados do Censo IBGE 2010 sobre a população
pop <- read_population(
  year = 2010,
  columns = c('V0010', 'V0601', 'V6036'),
  add_labels = 'pt',
  showProgress = TRUE # Se estiver no "true", mostra uma barra de progresso da tarefa
)

# Checa qual é a classe do objeto
class(pop)

# Mostra só as primeiras linhas da tabela (cabeçalho) no Console
head(pop) |>
  collect()

# Altera o objeto pop com uma nova coluna de "faixa etária" (age_group)

pop <- pop |>
  mutate(
    age_group = dplyr::case_when(
      V6036 <= 04              ~ "00-05",
      V6036 >= 05 & V6036 < 10 ~ "05-10",
      V6036 >= 10 & V6036 < 15 ~ "10-15",
      V6036 >= 15 & V6036 < 20 ~ "15-20",
      V6036 >= 20 & V6036 < 25 ~ "20-25",
      V6036 >= 25 & V6036 < 30 ~ "25-30",
      V6036 >= 30 & V6036 < 35 ~ "30-35",
      V6036 >= 35 & V6036 < 40 ~ "35-40",
      V6036 >= 40 & V6036 < 45 ~ "40-45",
      V6036 >= 45 & V6036 < 50 ~ "45-50",
      V6036 >= 50 & V6036 < 55 ~ "50-55",
      V6036 >= 55 & V6036 < 60 ~ "55-60",
      V6036 >= 60 & V6036 < 65 ~ "60-65",
      V6036 >= 65 & V6036 < 70 ~ "65-70",
      V6036 >= 70              ~ "70+"
    ))

head(pop) |>
  collect()

# Cria objeto chamado "piramide_df" que cacula tabela de contagem de pessoas por idade
piramide_df <- pop |>
  group_by(V0601, age_group) |>
  summarise(pop_count = sum(V0010)) |>
  collect()

head(piramide_df)

# remove grupo com idade missing `NA`
piramide_df <- filter(piramide_df, !is.na(age_group))

# transforma a contagem de mulheres para valores negativos
piramide_df <- piramide_df |>
  mutate(pop_count = if_else(V0601 == "Masculino", pop_count, -pop_count))

# Gera o gráfico da pirâmide etária
ggplot(data = piramide_df,
       aes(x = pop_count / 1000,
           y = age_group,
           fill = V0601)) +
  geom_col() +
  scale_fill_discrete(name="", type=c("#ffcb69","#437297")) +
  scale_x_continuous(labels = function(x){scales::comma(abs(x))},
                     breaks = c(-8000, -4000,0,4000, 8000),
                     name = "População (em milhares)") +
  theme_classic() +
  theme(
    legend.position = "top",
    axis.title.y=element_blank(),
    panel.grid.major.x = element_line(color = "grey90")
  )

##### B- DOMICÍLIO #####


## Função de análise por domicílio: read_households()

dom <- read_households(
  year = 2010,
  showProgress = TRUE
)

#### ANÁLISE 2 - ESGOTO POR DOMICÍLIO (agrupar domicílios por região brasileira e município)

esg <- dom |>
  compute() |>
  group_by(name_region, code_muni) |>
  summarize(rede = sum(V0010[which(V0207=='1')]),
            total = sum(V0010)) |>
  mutate(cobertura = rede / total) |>
  collect()

head(esg)

# Gerar um boxplot (gráfico de dispersão) com os dados por região

ggplot(esg) +
  geom_boxplot(aes(x=reorder(name_region, -cobertura), y=cobertura,
                   weight  = rede, color=name_region),
               show.legend = F, outlier.alpha = 0.1) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Região", y="Quantidade de domicílios\nconectados à rede de esgoto") +
  theme_classic()

# OBS: Um boxplot é difícil e ler e comparar. Um mapa é BEM MELHOR pra isso.

## Integração entre {censobr}, onde [uxamos os dados,] e {geobr}, que vamos usar pra fazer mapa

# carrega pacote
library(geobr)

# Lê os dados de 2010 por região
regioes_df <- read_region(year = 2010,
                          showProgress = FALSE)

# Lê os dados de 2010 por município
muni_sf <- read_municipality(year = 2010,
                             showProgress = FALSE)


# Juntar os dados de município e dos dados do esgosto
esg_sf <- dplyr::left_join(muni_sf, esg, by = 'code_muni')

# Gerar mapa
ggplot() +
  geom_sf(data = esg_sf, aes(fill = cobertura), color=NA) + # colocar dados sobre esgoto por município
  geom_sf(data = regioes_df, color = 'gray20', fill=NA) + # colocar camada com a delimitação das regiões
  labs(title = "Quantidade de domicílios conectados à rede de esgoto") + # adicina "label"/rótulos, nesse caso, o "title" (título)
  scale_fill_distiller(palette = "Greens", direction = 1, # define a escala de cores na paleta verde
                       name='Proporção de domicílios', # define o nome da legenda
                       labels = scales::percent) +
  theme_void() + # tema escolhido pra essa visualização
  theme(legend.position = 'bottom') # posição da legenda

head(muni_sf)


#### ANÁLISE 3 - VALOR DO ALUGUEL NA GRANDE SÃO PAULO

# Pegar a camada do mapa do geobr para as cidades na Grande SP
metro_sp <- geobr::read_metro_area(year = 2010,
                                   showProgress = FALSE) |>
  filter(name_metro == "RM São Paulo")


# Faz a ponderação do peso dos dados e filtrar só
wt_areas <- geobr::read_weighting_area(code_weighting = "SP",
                                       year = 2010,
                                       simplified = FALSE,
                                       showProgress = FALSE)


wt_areas <- filter(wt_areas, code_muni %in% metro_sp$code_muni)

head(wt_areas)

rent <- dom |>
  filter(code_muni %in% metro_sp$code_muni) |>
  compute() |>
  group_by(code_weighting) |>
  summarize(avgrent=weighted.mean(x=V2011, w=V0010, na.rm=TRUE)) |>
  collect()

head(rent)

# Criar a camada de geolocalização das áreas da região
rent_sf <- left_join(wt_areas, rent, by = 'code_weighting')

# Gerar o mapa
ggplot() +
  geom_sf(data = rent_sf, aes(fill = avgrent), color=NA) +
  geom_sf(data = metro_sp, color='gray', fill=NA) +
  labs(title = "Valor médio do aluguel por área de ponderação",
       subtitle = "Região Metropolitana de São Paulo, 2010") +
  scale_fill_distiller(palette = "Purples", direction = 1,
                       name='Valores\nem R$',
                       labels = scales::number_format(big.mark = ".")) +
  theme_void()

##### C- SETOR CENSITÁRIO #####

# Acessando os dicionários de variáveis da base

dom <- censobr::read_tracts(
  year = 2022,
  dataset = 'Domicilio',
  showProgress = FALSE
)

names(dom)[c(30:33,119:121, 526:528)]

# Abrindo o dicionário de variáveis por setor censitário de 2022

censobr::data_dictionary(
  year = 2022,
  dataset = 'tracts'
)

# Carregar pacotes pra análises de exemplos
library(censobr)
library(geobr)
library(arrow)
library(dplyr)
library(ggplot2)


#### ANÁLISE 4 - DISTRIBUIÇÃO ESPACIAL DE RENDA

# baixa os dados
tract_basico <- read_tracts(
  year = 2010,
  dataset = "Basico",
  showProgress = FALSE
)

tract_income <- read_tracts(
  year = 2010,
  dataset = "DomicilioRenda",
  showProgress = FALSE
)

# selecionar colunas
tract_basico <- tract_basico |> select('code_tract','V002')
tract_income <- tract_income |> select('code_tract','V003')

# unir as tabelass
tracts_df10 <- left_join(tract_basico, tract_income)

# calcular a renda per capita
tracts_df10 <- tracts_df10 |>
  mutate(income_pc = V003 / V002) |>
  collect()

head(tracts_df10)


## Juntar com geobr:

# busca qual o código do municipio de Belo Horizonte
bh_info <- geobr::lookup_muni(name_muni = 'Belo Horizonte')
#> code_muni: 3106200

# baixa municipio de BH
muni_bh <- geobr::read_municipality(
  code_muni = 'MG',
  year = 2010,
  showProgress = FALSE) |>
  filter(name_muni == "Belo Horizonte")

# baixa todos setores de Minas Gerais
tracts_2010 <- geobr::read_census_tract(
  code_tract = "MG",
  year = 2010,
  simplified = FALSE,
  showProgress = FALSE)

# filtra setores de BH
tracts_2010 <- filter(tracts_2010, name_muni == 'Belo Horizonte')

# mapa de setores censitarios
ggplot() +
  geom_sf(data=tracts_2010, fill = 'gray90', color='gray60') +
  theme_void()

# Juntar mapa de BH com base com dados sobre renda per capita por domicílio
# A chave desse join é o código do setor censitário
bh_tracts <- left_join(tracts_2010, tracts_df10, by = 'code_tract')

# Gerar mapa com esses dados de distribuição de renda do IBGE
ggplot() +
  geom_sf(data = bh_tracts, aes(fill = ifelse(income_pc<10000,income_pc,10000)),
          color=NA) +
  geom_sf(data = muni_bh, color='gray10', fill=NA) +
  labs(title = 'Renda per capita dos setores censitários',
       subtitle= 'Belo Horizonte, 2010') +
  scale_fill_viridis_c(name = "Reda per\ncapita (R$)",
                       na.value="white",
                       option = 'cividis',
                       breaks = c(0,  1e3, 4e3, 8e3, 1e4) ,
                       labels  = c('0',  '1.000', '4.000', '8.000', '> 10.000')
  ) +
  theme_void()

#### ANÁLISE 6 - DENSIDADE POPULACIONAL

# download dados preliminares dos setores de 2022
tracts_df <- censobr::read_tracts(
  year = 2022,
  dataset = "Basico",
  showProgress = FALSE) |>
  filter(name_muni == 'Belo Horizonte') |>
  collect()

# baixa todos setores de Minas Gerais
tracts_sf <- geobr::read_census_tract(
  code_tract = "MG",
  year = 2022,
  simplified = FALSE,
  showProgress = FALSE
)

# filtra setores de BH
tracts_sf <- filter(tracts_sf, name_muni == 'Belo Horizonte')

# merge tables
tracts_sf$code_tract <- as.character(tracts_sf$code_tract)
bh_tracts22 <- left_join(tracts_sf, tracts_df, by = 'code_tract')

# calcula a área dos setores
bh_tracts22 <- bh_tracts22 |>
  mutate(tract_aream2 = sf::st_area(bh_tracts22),
         tract_areakm2 = units::set_units(tract_aream2, km2))

# calcula densidade demografica
bh_tracts22 <- bh_tracts22 |>
  mutate(pop_km2 = as.numeric(V0001/ tract_areakm2))

# map
ggplot() +
  geom_sf(data = bh_tracts22, color=NA,
          aes(fill = ifelse(pop_km2<20000,pop_km2,20000))) +
  geom_sf(data = muni_bh, color='gray10', fill=NA) +
  labs(title = 'Densidade populacional dos setores censitários',
       subtitle= 'Belo Horizonte, 2022') +
  scale_fill_distiller(palette = "Reds", direction = 1,
                       name='População por'~Km^2,
                       breaks = c(0,  5e3, 10e3, 15e3, 2e4) ,
                       labels  = c('0',  '5.000', '10.000', '15.000', '> 20.000')) +
  theme_void()
