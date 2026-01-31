# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios II — Tipos de Variáveis e Estatísticas Descritivas

# Nome: DEBORA RODRIGUES PEREIRA DIAS
# Matrícula/RA: 21202510146
# Turma: PPU00920253
# Data: 21/01/2026
# Descrição: Respostas da Lista #02
# ============================================================

# 1) Preparação do ambiente
# Instalação (só precisa rodar uma vez)
# install.packages(c("tidyverse", "readr", "ggplot2"))

# Carregamento (sempre que abrir o R)
library(tidyverse)
library(readr)
library(ggplot2)
library(dplyr) # para usar count()

options(scipen = 999) # evita notação científica

# Lê o arquivo educ_saude.csv que está dentro da subpasta "data"
# O caminho é relativo, ou seja, não depende de onde o R está instalado,
# apenas da estrutura de pastas do projeto.
# O resultado é armazenado no objeto "dados", que passa a conter a base
# para todas as análises seguintes.
dados <- read_csv("data/educ_saude.csv")


# ============================================================
# 2) Base de dados 
# Tarefa 1. Importação e inspeção
# ============================================================

library(readr)

# Importa a base
dados <- read_csv("data/educ_saude.csv")

# Estrutura geral: nº de linhas/colunas e tipo de cada variável
glimpse(dados)

# Estatísticas descritivas básicas
summary(dados)

# Número de linhas e colunas
nrow(dados)  # total de linhas (observações)
ncol(dados)  # total de colunas (variáveis)

# Classes das variáveis
str(dados)

# Verificação de valores ausentes (NA)
sum(is.na(dados))       # total de NA na base
colSums(is.na(dados))   # total de NA por coluna


## Resposta Tarefa 1:
# A base possui 10.000 linhas e 13 variáveis.
# Oito variáveis são qualitativas (texto/chr) e cinco são quantitativas (números/dbl).
# Não foram identificados valores ausentes (NA) nesta inspeção inicial.

# ============================================================
# # 3) Classificação de variáveis 
# Tarefa 2. Classifique e ajuste tipos
# ============================================================


dados <- dados |>
  mutate(
    sexo         = factor(sexo),                # qualitativa nominal (categorias F/M sem ordem)
    rede_escolar = factor(rede_escolar),        # qualitativa nominal (pública/privada sem hierarquia)
    plano_saude  = factor(plano_saude),         # qualitativa nominal (SUS/privado/ambos/nenhum)
    diagnostico  = factor(diagnostico),         # qualitativa nominal ("sem", "HAS", "DM", "outros")
    escolaridade = factor(escolaridade,
                          levels = c("Fundamental","Médio","Superior"),
                          ordered = TRUE)       # qualitativa ordinal (há hierarquia entre níveis)
  )

# Tabela de classificação teórica
dfl <- data.frame(
  variavel=c("id","sexo","escolaridade","anos_estudo","rede_escolar","municipio","UF",
             "idade","faltas_esc","tempo_estudo_h","pressao_sistolica","diagnostico","plano_saude"),
  tipoteorico=c("nominal","nominal","ordinal","discreta","nominal","nominal","nominal",
                "contínua","discreta","contínua","contínua","nominal","nominal"),
  stringsAsFactors=FALSE
)

dfl

# Conferência da estrutura
str(dados)
glimpse(dados)

## Comentários:
# - id: nominal, apenas identificador sem ordem.
# - sexo: nominal, categorias F/M sem hierarquia.
# - escolaridade: ordinal, pois há hierarquia (Fundamental < Médio < Superior).
# - anos_estudo: discreta, número inteiro de anos concluídos.
# - rede_escolar: nominal, categorias sem ordem (pública/privada).
# - municipio: nominal, nomes de cidades sem hierarquia.
# - UF: nominal, siglas de estados sem hierarquia.
# - idade: contínua, medida em anos, pode assumir muitos valores.
# - faltas_esc: discreta, número inteiro de faltas.
# - tempo_estudo_h: contínua, horas de estudo por semana (valores fracionados).
# - pressao_sistolica: contínua, medida em mmHg.
# - diagnostico: nominal, categorias sem ordem ("sem", "HAS", "DM", "outros").
# - plano_saude: nominal, categorias sem hierarquia (SUS, privado, ambos, nenhum).

## Resposta Tarefa 2:
# As variáveis foram classificadas de acordo com sua natureza teórica:
# - Nominais: id, sexo, rede_escolar, municipio, UF, diagnostico, plano_saude.
# - Ordinal: escolaridade (há hierarquia entre Fundamental < Médio < Superior).
# - Discretas: anos_estudo, faltas_esc (valores inteiros, contagens).
# - Contínuas: idade, tempo_estudo_h, pressao_sistolica (medidas que podem assumir muitos valores).
# Após o ajuste com mutate(), as variáveis qualitativas foram convertidas em fatores
# e escolaridade foi definida como ordinal. A estrutura final está coerente com a teoria.
#Essa coerência entre tipo teórico e classe no R é essencial para aplicar estatísticas e gráficos corretamente, 
#evitando interpretações equivocadas.

# ============================================================
# 4) Variáveis qualitativas — frequências e gráficos
# Tarefa 3A. Frequências absolutas e relativas
# ============================================================

# Tarefa 3A. Frequências absolutas e relativas
tab_plano <- table(dados$plano_saude)
prop_plano <- prop.table(tab_plano)
cbind(FA = tab_plano, FR = round(100 * prop_plano, 1))

## Resposta Tarefa 3A:
# A moda é a categoria "SUS", com 5024 pessoas.
# A proporção dominante é 50,2%, indicando que metade dos entrevistados depende do SUS.
# As demais categorias aparecem em menor proporção: privado (24,7%), nenhum (18,7%) e ambos (6,3%).
#Esses resultados mostram a predominância do SUS como principal forma de acesso à saúde,
#enquanto planos privados e ausência de cobertura aparecem em menor proporção.

# Tarefa 3B. Gráfico de barras (qualitativa nominal)
dados |>
  count(plano_saude) |>
  ggplot(aes(x = plano_saude, y = n)) +
  geom_col() +
  labs(x = "Plano de saúde", y = "Frequência",
       title = "Distribuição de plano de saúde")


## Resposta Tarefa 3B:
# O gráfico mostra a distribuição dos planos de saúde.
# A categoria "SUS" aparece como a mais frequente, confirmando a moda e a proporção dominante
# já observadas na Tarefa 3A (50,2%). As categorias "privado", "nenhum" e "ambos" aparecem
# em proporções menores, o que reforça a predominância do SUS na amostra.
#O gráfico facilita a comparação visual entre categorias, 
#destacando a predominância do SUS em relação às demais formas de cobertura.

# Tarefa 3C. Barras ordenadas (qualitativa ordinal)
dados |>
  count(escolaridade) |>
  ggplot(aes(x = escolaridade, y = n)) +
  geom_col() +
  labs(x = "Escolaridade (ordem substantiva)", y = "Frequência")

## Resposta Tarefa 3C:
# O gráfico mostra a distribuição dos níveis de escolaridade.
# A ordem das categorias é fundamental porque "escolaridade" é uma variável ordinal.
# Com a ordem correta (Fundamental < Médio < Superior), conseguimos interpretar a progressão
# dos níveis educacionais e comparar a distribuição de forma substantiva.
# Se as barras fossem mostradas em ordem aleatória, perderíamos essa hierarquia
# e a análise ficaria prejudicada.
#Assim, respeitar a ordem das categorias garante que o gráfico represente corretamente a progressão educacional 
#e evita interpretações equivocadas.


# ============================================================
#5 Variáveis quantitativas — medidas e distribuição
# Escolha duas variáveis: idade e pressao_sistolica.
# ============================================================

#Tarefa 4A. Tendência central e dispersão

sumario_idade <- dados |>
  summarise(
    n = sum(!is.na(idade)),
    media = mean(idade, na.rm = TRUE),
    mediana= median(idade, na.rm = TRUE),
    min = min(idade, na.rm = TRUE),
    max = max(idade, na.rm = TRUE),
    dp  = sd(idade, na.rm = TRUE)
  )
sumario_idade

#resumo para pressão sistólica
# Repita para pressao_sistolica. Compare média vs. mediana. Há assimetria?

## Resposta Tarefa 4A:
# Idade: média (40,4) ≈ mediana (40), indicando simetria. 
# Intervalo de 18 a 80 anos, com dispersão moderada (dp = 12,5).
# Pressão sistólica: média (122) = mediana (122), também simétrica.
# Valores entre 85 e 175 mmHg, com dispersão moderada (dp = 14,1).
# Esses resultados indicam distribuições aproximadamente simétricas, 
#sem grandes desvios entre média e mediana, 
#o que sugere ausência de forte assimetria nos dados.

#Tarefa 4B. Histograma e boxplot

# Idade
ggplot(dados, aes(x = idade)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograma de Idade")

ggplot(dados, aes(y = idade)) +
  geom_boxplot() +
  labs(title = "Boxplot de Idade")

# Pressão Sistólica
ggplot(dados, aes(x = pressao_sistolica)) +
  geom_histogram(bins = 20) +
  labs(title = "Histograma de Pressão Sistólica")

ggplot(dados, aes(y = pressao_sistolica)) +
  geom_boxplot() +
  labs(title = "Boxplot de Pressão Sistólica")


## Resposta Tarefa 4B:
# Histograma e boxplot de Idade: distribuição simétrica, caudas curtas, poucos outliers.
# Histograma e boxplot de Pressão Sistólica: distribuição simétrica, caudas moderadas, alguns valores extremos.
#“Esses gráficos complementam as medidas descritivas, permitindo visualizar a forma da distribuição 
#e identificar valores extremos de maneira intuitiva.

# ============================================================
# 6 Tabelas cruzadas e resumos por grupo
# ============================================================

# Tarefa 5A - Qual diagnóstico é mais prevalente dentro de cada tipo de plano?

tab_cross <- table(dados$diagnostico, dados$plano_saude)
tab_cross

# Percentuais por coluna (plano de saúde)
round(100 * prop.table(tab_cross, margin = 2), 1)

## Resposta Tarefa 5A:
# Em todos os planos de saúde, a maioria dos indivíduos está na categoria "sem",
# que significa ausência de diagnóstico registrado (≈ 80% em cada plano).
# Entre os diagnósticos presentes, HAS é o mais prevalente em todos os planos (≈ 12%),
# seguido por "outros" e DM, com proporções menores.
# Observa-se pequena variação: HAS é ligeiramente mais frequente no SUS (12,5%).
#Esse cruzamento mostra que a ausência de diagnóstico é predominante em todos os planos,
#mas a hipertensão (HAS) aparece como o principal problema de saúde identificado, 
#especialmente entre usuários do SUS.


# Tarefa 5B 1 - Resumo de idade por sexo
dados |>
  group_by(sexo) |>
  summarise(
    n = n(),
    media_idade = mean(idade, na.rm = TRUE),
    dp_idade = sd(idade, na.rm = TRUE),
    mediana_idade = median(idade, na.rm = TRUE)
  )

## Resposta Tarefa 5B 1 (Idade por sexo):
# As médias e medianas de idade são praticamente iguais entre homens (40,5) e mulheres (40,3),
# indicando distribuições muito semelhantes. O desvio padrão também é próximo (12,4 vs 12,5),
# mostrando que a variabilidade da idade é equivalente nos dois grupos.

# Tarefa 5B 2 - Resumo de idade por escolaridade
dados |>
  group_by(escolaridade) |>
  summarise(
    n = n(),
    media_idade = mean(idade, na.rm = TRUE),
    dp_idade = sd(idade, na.rm = TRUE),
    mediana_idade = median(idade, na.rm = TRUE)
  )
## Resposta Tarefa 5B 2 (Idade por escolaridade):
# As médias e medianas de idade são praticamente iguais entre os três níveis de escolaridade (≈ 40 anos),
# indicando distribuições muito semelhantes. Os desvios padrão também são próximos (≈ 12,5),
# mostrando que a variabilidade da idade não difere de forma relevante entre os grupos.

## Conclusão Tarefa 5B:
# Tanto por sexo quanto por escolaridade, as idades médias e medianas são praticamente iguais,
# e os desvios padrão também são muito próximos. Isso indica que, neste conjunto de dados,
# não há diferenças relevantes na distribuição da idade entre os grupos analisados.
#Esses resultados sugerem que, na amostra, idade não varia de forma significativa entre grupos de sexo ou escolaridade, 
#o que pode indicar homogeneidade etária na população estudada.

# ============================================================
# 7 Valores ausentes e outliers
# ============================================================

# Tarefa 6A - Valores ausentes
colSums(is.na(dados))

# Exemplo: calcular média de idade com e sem NA
mean(dados$idade)              # dá NA se houver valores ausentes
mean(dados$idade, na.rm = TRUE) # ignora os NAs

## Resposta Tarefa 6A:
# A função colSums(is.na(dados)) mostrou que não há valores ausentes em nenhuma variável.
# Quando existem NAs, funções como mean() retornam NA se não especificarmos na.rm=TRUE.
# O argumento na.rm=TRUE serve para ignorar os valores ausentes e calcular a estatística
# apenas com os dados válidos. No nosso caso, como não há NAs, os resultados são iguais.
#Esse procedimento é essencial em bases reais, 
#pois garante que estatísticas não sejam invalidadas pela presença de valores ausentes.

# Tarefa 6B - Outliers em tempo de estudo
Q <- quantile(dados$tempo_estudo_h, probs = c(.25, .75), na.rm = TRUE)
IQRv <- IQR(dados$tempo_estudo_h, na.rm = TRUE)

lim_inf <- Q[1] - 1.5 * IQRv
lim_sup <- Q[2] + 1.5 * IQRv

subset_out <- dados |>
  filter(tempo_estudo_h < lim_inf | tempo_estudo_h > lim_sup)

nrow(subset_out)   # quantos outliers
head(subset_out)   # primeiros casos de outliers

## Resposta Tarefa 6B:
# Foram identificados 348 outliers em tempo_estudo_h pela regra do IQR.
# Esses valores estão acima do limite superior calculado e representam tempos de estudo
# bem maiores que a maioria dos casos.
# Outliers podem ter diferentes significados:
# - Erros de digitação ou coleta (ex.: valores exagerados ou improváveis).
# - Casos raros (indivíduos que realmente estudaram muito mais que os demais).
# - Informações válidas que refletem diversidade real da amostra.
# A decisão sobre como tratá-los depende do contexto da análise:
# em alguns estudos podem ser removidos, em outros podem trazer insights importantes
# sobre comportamentos extremos.
#Portanto, a análise de outliers deve sempre considerar o contexto substantivo,
#evitando exclusões automáticas que possam eliminar informações relevantes.

# ============================================================
# 8 Exercícios aplicados (educação e saúde)
# ============================================================

# Frequência absoluta (FA) e relativa (FR) da rede escolar
tab_rede <- table(dados$rede_escolar)
tab_rede

prop_rede <- prop.table(tab_rede) * 100
round(prop_rede, 1)

## Resposta Educação (a) ) Distribuição de rede_escolar (FA/FR).:
# A frequência absoluta mostra que 2796 alunos estão na rede privada e 7204 na rede pública.
# A frequência relativa indica que 28% pertencem à rede privada e 72% à rede pública.
# Portanto, a maioria dos alunos da amostra está na rede pública.

# Boxplots de tempo de estudo por escolaridade
ggplot(dados, aes(x = escolaridade, y = tempo_estudo_h)) +
  geom_boxplot() +
  labs(title = "Tempo de estudo por escolaridade", 
       x = "Escolaridade", 
       y = "Tempo de estudo (h)")

## Resposta Educação (b/c) 
##b) Compare tempo_estudo_h por escolaridade com boxplots.
##c) Interprete: há padrão monotônico com a ordem da escolaridade?:
# Os boxplots mostram que a mediana do tempo de estudo é semelhante entre os três níveis de escolaridade,
# com pequenas variações. As distribuições apresentam dispersão parecida e alguns outliers em todos os grupos.
# Não se observa um padrão monotônico claro (isto é, não há aumento consistente do tempo de estudo
# do Fundamental para o Médio e para o Superior).
#Esses resultados mostram que a rede pública concentra a maioria dos alunos 
#e que o tempo de estudo não varia de forma consistente com a escolaridade.

#Saúde
# Histograma de pressão sistólica
ggplot(dados, aes(x = pressao_sistolica)) +
  geom_histogram(bins = 20, fill = "skyblue", color = "black") +
  labs(title = "Histograma de Pressão Sistólica", 
       x = "Pressão Sistólica (mmHg)", 
       y = "Frequência")

# Estatísticas resumo
mean(dados$pressao_sistolica, na.rm = TRUE)
median(dados$pressao_sistolica, na.rm = TRUE)
sd(dados$pressao_sistolica, na.rm = TRUE)

## Resposta (a) Histograma de pressao_sistolica e reporte média/mediana/DP.:
# O histograma da pressão sistólica mostra uma distribuição aproximadamente simétrica.
# A média foi de 122,2 mmHg, a mediana 122 mmHg e o desvio padrão 14,0 mmHg.
# Isso indica que os valores estão concentrados em torno de 120 mmHg, sem grandes desvios,
# e que a média e a mediana praticamente coincidem, reforçando a simetria da distribuição.
#A pressão sistólica apresenta distribuição simétrica em torno de 120 mmHg, 
#e a hipertensão (HAS) é o diagnóstico mais prevalente em todos os planos de saúde

# Cruzamento diagnostico × plano_saude (% por coluna)
tab_cross <- table(dados$diagnostico, dados$plano_saude)
round(100 * prop.table(tab_cross, margin = 2), 1)

## Resposta (b) Cruzamento diagnostico × plano_saude (% por coluna).:
# Em todos os planos de saúde, a maioria dos indivíduos está na categoria "sem diagnóstico"
# (≈ 80%). O diagnóstico de HAS aparece em segundo lugar, com cerca de 12% em todos os planos.
# Os demais diagnósticos (DM e outros) têm proporções menores, variando entre 0,7% e 6,6%.

## Resposta (c)  Interprete: qual diagnóstico é mais prevalente em cada plano?:
# O diagnóstico mais prevalente em cada plano é HAS, seguido por "outros" e DM.
# Apesar de pequenas variações entre os planos (HAS ligeiramente mais frequente no SUS),
# o padrão geral se mantém: HAS é o diagnóstico predominante entre os casos registrados.

# ============================================================
# 9 Desafio 
#Crie uma tabela-síntese por escolaridade com:
#  • n, média e mediana de idade, DP, mínimo e máximo;
#• FA/FR de plano_saude dentro de cada escolaridade.
# ============================================================

# Tabela-síntese por escolaridade
dados %>%
  group_by(escolaridade) %>%
  summarise(
    n = n(),
    media_idade = mean(idade, na.rm = TRUE),
    mediana_idade = median(idade, na.rm = TRUE),
    dp_idade = sd(idade, na.rm = TRUE),
    min_idade = min(idade, na.rm = TRUE),
    max_idade = max(idade, na.rm = TRUE)
  )

# Frequência absoluta e relativa de plano_saude por escolaridade
dados %>%
  group_by(escolaridade, plano_saude) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(prop = round(100 * n / sum(n), 1))

#1. Barras empilhadas (proporções)

dados %>%
  count(escolaridade, plano_saude) %>%
  group_by(escolaridade) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = escolaridade, y = prop, fill = plano_saude)) +
  geom_col(position = "fill") +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribuição de plano de saúde por escolaridade",
       x = "Escolaridade", y = "Proporção")

# 2. Boxplot de tempo de estudo por escolaridade
ggplot(dados, aes(x = escolaridade, y = tempo_estudo_h)) +
  geom_boxplot() +
  labs(title = "Tempo de estudo por escolaridade",
       x = "Escolaridade", y = "Tempo de estudo (h)")

## Resposta Desafio 9:
# A tabela-síntese mostra que a idade média e mediana são próximas (~40 anos) em todos os níveis de escolaridade,
# com desvio padrão em torno de 12 anos e valores mínimos e máximos semelhantes.
# A distribuição de plano de saúde dentro de cada escolaridade revela que a maioria dos indivíduos está vinculada ao SUS,
# seguido por plano privado, nenhum e ambos, com pequenas variações entre os grupos.
# O gráfico de barras empilhadas confirma essas proporções de forma visual.
# O boxplot de tempo de estudo por escolaridade mostra distribuições semelhantes entre os grupos,
# sem padrão monotônico claro, mas com presença de outliers em todos os níveis.
#Assim, o desafio integra estatísticas descritivas, frequências e gráficos, 
#oferecendo uma visão abrangente da relação entre escolaridade, idade, tempo de estudo e acesso à saúde na amostra.

# Nota final sobre NAs e outliers:
# - Não foram identificados valores ausentes (NA) na base, conforme verificação com colSums().
# - Foram identificados 348 outliers em tempo_estudo_h pela regra do IQR.
#   Esses valores foram comentados e interpretados como possíveis casos extremos ou erros de coleta.

## Conclusão Geral
# A Lista 2 permitiu aplicar conceitos de tipos de variáveis e estatísticas descritivas
# em um conjunto de dados de educação e saúde. Foram realizadas inspeções iniciais,
# classificações teóricas, cálculos de frequências, medidas de tendência central e dispersão,
# além de representações gráficas e tabelas cruzadas. A análise mostrou predominância da rede
# pública na educação, do SUS na saúde e da hipertensão como diagnóstico mais prevalente.
# Também foi possível discutir a importância do tratamento de valores ausentes e outliers.
# Assim, o exercício consolidou a prática de análise exploratória de dados, conectando teoria
# estatística com aplicações em políticas públicas.

# Checklist atendido:
# - Script funcional com comentários ✔
# - Tabelas de frequência (rede_escolar, plano_saude) ✔
# - Estatísticas descritivas (idade, pressao_sistolica) ✔
# - Histogramas e boxplots (pressao_sistolica, tempo_estudo_h) ✔
# - Tabelas cruzadas (diagnostico × plano_saude) ✔
# - Resumo por grupo (idade por escolaridade) ✔
# - Tratamento de NA e comentário sobre outliers ✔
# - Interpretações claras em cada item ✔


# ============================================================
# Finalização do exercício
# ============================================================

# Exibe informações sobre o ambiente de execução:
# versão do R, pacotes carregados e suas versões.
# Útil para garantir reprodutibilidade dos resultados.
sessionInfo()

print("Script finalizado sem erros!")
