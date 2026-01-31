# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #03 
# Nome: DEBORA RODRIGUES PEREIRA DIAS
# Matrícula/RA: 21202510146
# Turma: PPU00920253
# Data: 12/01/2026
# ============================================================

# Instale os pacotes apenas uma vez no console, se necessário:
# install.packages(c("tidyverse", "readr", "ggplot2", "scales", "viridis", "electionsBR"))

# Carregar pacotes
library(tidyverse)
library(ggplot2)
library(scales)
library(viridis)
library(electionsBR)

ls("package:electionsBR")


# Fixar semente para reprodutibilidade
set.seed(101)

# Lê o arquivo educ_saude.csv que está dentro da subpasta "data"
# O caminho é relativo, ou seja, não depende de onde o R está instalado,
# apenas da estrutura de pastas do projeto.
# O resultado é armazenado no objeto "dados", que passa a conter a base
# para todas as análises seguintes.
dados <- read_csv("data/educ_saude.csv")


# ============================================================
#Exercício 1
#(0)Aquecimento– Tabela vs. Gráfico (dados internos)
# Objetivo. Comparar leitura em tabela e gráfico.
#Passos. Carregue a base cars; exiba as 10 primeiras linhas (tabela); produza um gráfico
#de dispersão speed × dist; interprete.
# ============================================================

data(cars)
head(cars, 10)
# base interna do R
# formate como tabela no seu .Rmd se desejar

#carregue da aula 5 6.2 6.2 Relações bivariadas: dispersão e suavização
ggplot(cars, aes(x = speed, y = dist)) +
  geom_point(alpha = 0.6) +
  labs(title = " speed x dist",
       x = "speed", y = "dist") +
  theme_minimal()

## Resposta Exercício 1:
# A tabela permite ver valores individuais de velocidade e distância,
# mas o gráfico de dispersão revela a tendência positiva:
# quanto maior a velocidade, maior tende a ser a distância de frenagem.

# ============================================================
#Exercício 2 (1) Distribuições univariadas e grupos
# ============================================================

data(mtcars)
mtcars <- mtcars |>
  mutate(cyl = as.factor(cyl)) #para converter variáveis em fatores

#Distribuições e grupos (mtcars): histogramas/densidades de mpg e boxplot por cyl. Interpretação breve.

library(ggplot2)

#instalou library para buscar o pacote e gerar os gráficos ggplot2

ggplot(mtcars, aes(x = mpg)) +
  geom_histogram(bins = 10,fill="skyblue") + #número de barras #fill filtro
  labs(title = "Histograma de consumo de combustível",
       x = "milhas por litros", y = "Frequência") +
  theme_minimal()


ggplot(mtcars, aes(x = mpg)) +
  geom_density(alpha = 0.4) +
  labs(title = "Densidade de consumo",
       x = "milhas por litros", y = "Densidade") +
  theme_minimal()

#boxplot de cyl

ggplot(mtcars, aes(x = cyl, y = mpg, fill=cyl)) +
  geom_boxplot(show.legend = TRUE, outlier.alpha = 0.4) +
  labs(title = "Consumo de combustível por número de cilindros",
       x = "Número de cilindros", y = "MPG") +
  theme_minimal()

## Resposta Exercício 2:
# O histograma e a densidade mostram que o consumo (mpg) tem distribuição concentrada entre 15 e 25 mpg.
# O boxplot revela diferenças claras entre grupos: carros com mais cilindros (6 e 8) tendem a consumir mais combustível,
# enquanto os de 4 cilindros apresentam maior eficiência (mpg mais alto).

# ============================================================
#3 Série temporal: escolha uma base (AirPassengers ou airquality), crie gráfico(s) de linha/pontos e destaque tendência.
# ============================================================

##Opção B: airquality (diário, facet por mês)

data("airquality") #para resgatar os dados
aq <- airquality |>
  as_tibble() |>
  drop_na(Ozone) |>
  mutate(Month = factor(Month),
         Day = as.integer(Day))

#criar gráfico: 6.4 6.4 Séries temporais: linhas e pontos

# airquality (diário, facet por mês)

ggplot(aq, aes(x = Day, y = Ozone)) +
  geom_line() +
  geom_point() +
  facet_wrap(~ Month, scale = "free_x")
  labs(title = "Níveis de ozônio",
       x = "Day", y = "Ozone") +
  theme_minimal()
  
  
## Resposta Exercício 3 (airquality):
  # O gráfico mostra a variação diária dos níveis de ozônio ao longo dos meses.
  # Observa-se tendência de aumento em alguns meses (como julho e agosto),
  # enquanto em outros os valores permanecem mais baixos ou oscilam bastante.
  # Isso sugere padrões sazonais na concentração de ozônio.

# ============================================================
#4 (3) Relações bivariadas e transformações
# ============================================================  
  
# Gráfico de dispersão com regressão linear
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    labs(title = "Relação entre peso e consumo",
         x = "Peso do carro (1000 lbs)", y = "Consumo (mpg)") +
    theme_minimal()
  
# Gráfico com suavização loess
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "loess", color = "red", se = FALSE) +
    labs(title = "Relação entre peso e consumo (loess)",
         x = "Peso do carro (1000 lbs)", y = "Consumo (mpg)") +
    theme_minimal()
  
# Gráfico com escala log no eixo x
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", color = "blue", se = FALSE) +
    scale_x_log10() +
    labs(title = "Relação entre peso e consumo (escala log)",
         x = "Peso do carro (log10)", y = "Consumo (mpg)") +
    theme_minimal()
  
## Resposta Exercício 4:
  # Os gráficos mostram relação negativa entre peso e consumo: carros mais pesados tendem a consumir mais combustível.
  # A regressão linear confirma a tendência decrescente, enquanto o loess mostra pequenas variações locais.
  # A escala log no eixo de peso ajuda a visualizar melhor os carros mais leves, mas não altera a tendência geral.

# ============================================================  
#5 (4) Facetas (comparar subgrupos)
#Objetivo. Comparar padrões entre transmissões.
# ============================================================
  
# Preparar fator para transmissão
  mtcars <- mtcars |>
    mutate(am = factor(am, labels = c("Automático", "Manual")))
  
# Gráfico de dispersão mpg x wt, facetado por tipo de transmissão
  ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point(alpha = 0.6, color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    facet_wrap(~ am) +
    labs(title = "Consumo vs. Peso por tipo de transmissão",
         x = "Peso do carro (1000 lbs)", y = "Consumo (mpg)") +
    theme_minimal()

## Resposta Exercício 5:
  # O gráfico facetado mostra que, em ambos os tipos de transmissão,
  # há relação negativa entre peso e consumo (carros mais pesados consomem mais).
  # No entanto, os carros manuais tendem a apresentar consumo ligeiramente maior
  # em comparação aos automáticos, para pesos semelhantes.

# ============================================================
# 6 (5) Simulação I– Correlação controlada
#Objetivo. Visualizar como ρ altera o “aperto” da nuvem.
#Passos. Simule três níveis de correlação (ρ = 0.2,0.6,0.9); faça três dispersões; compare
# ============================================================ 
  
# Simulação de correlação controlada
  n <- 1000
  rhos <- c(0.2, 0.6, 0.9)
  
  sim <- purrr::map_dfr(rhos, \(rho) {
    x <- rnorm(n)
    e <- rnorm(n)
    y <- rho * x + sqrt(1 - rho^2) * e
    tibble(rho = rho, x = x, y = y)
  })
  
# Gráfico de dispersão facetado por nível de correlação
  ggplot(sim, aes(x = x, y = y)) +
    geom_point(alpha = 0.4, color = "steelblue") +
    facet_wrap(~ rho) +
    labs(title = "Dispersões simuladas com diferentes correlações",
         x = "X", y = "Y") +
    theme_minimal()

## Resposta Exercício 6:
  # Quando ρ = 0.2, a nuvem de pontos é bem espalhada, mostrando correlação fraca.
  # Com ρ = 0.6, os pontos ficam mais alinhados, indicando correlação moderada.
  # Com ρ = 0.9, a nuvem fica muito apertada em torno de uma linha, mostrando correlação forte.
  # Assim, quanto maior o valor de ρ, mais clara e estreita é a relação linear entre X e Y.

# ============================================================
#7 (6) Simulação II– Diferenças entre grupos
# Objetivo. Comparar distribuições com médias e SD diferentes.
# Passos. Simule grupo A: N(0,1); grupo B: N(1,1.8); faça histogramas/densidades/box
# plot/violin; interprete.
# ============================================================
     
# Simulação de dois grupos
  n <- 1000
  df_grupos <- tibble(
    grupo = rep(c("A","B"), each = n),
    valor = c(rnorm(n, 0, 1), rnorm(n, 1, 1.8))
  )
  
# Histograma
  ggplot(df_grupos, aes(x = valor, fill = grupo)) +
    geom_histogram(alpha = 0.6, position = "identity", bins = 30) +
    labs(title = "Histograma das distribuições simuladas",
         x = "Valor", y = "Frequência") +
    theme_minimal()
  
# Densidade
  ggplot(df_grupos, aes(x = valor, fill = grupo)) +
    geom_density(alpha = 0.4) +
    labs(title = "Densidade das distribuições simuladas",
         x = "Valor", y = "Densidade") +
    theme_minimal()
  
# Boxplot
  ggplot(df_grupos, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(alpha = 0.6) +
    labs(title = "Boxplot das distribuições simuladas",
         x = "Grupo", y = "Valor") +
    theme_minimal()
  
# Violin plot
  ggplot(df_grupos, aes(x = grupo, y = valor, fill = grupo)) +
    geom_violin(alpha = 0.6) +
    labs(title = "Violin plot das distribuições simuladas",
         x = "Grupo", y = "Valor") +
    theme_minimal()

## Resposta Exercício 7:
  # O grupo A apresenta distribuição centrada em 0, com menor dispersão.
  # O grupo B tem média deslocada para 1 e maior variabilidade (dispersão mais ampla).
  # Os gráficos mostram que o grupo B é mais heterogêneo, enquanto o grupo A é mais concentrado.
  # Assim, diferenças de média e desvio padrão afetam tanto a posição quanto a forma das distribuições.

# ============================================================
#8 (7) educ_saude.csv– exploração e gráficos
# Objetivo. Relacionar uma variável de educação e uma de saúde.
# Passos. Importe; explore; escolha variáveis; faça histogramas/densidades/boxplots/dis
# persões/facetas; interprete.
# ============================================================  
  
  library(readr)
  educ <- read_csv("educ_saude.csv")
  glimpse(educ)   # visualizar estrutura e nomes das variáveis

# Histograma da variável de educação
  ggplot(educ, aes(x = anos_estudo)) +
    geom_histogram(bins = 20, fill = "skyblue", alpha = 0.7) +
    labs(title = "Distribuição dos anos de estudo",
         x = "Anos de estudo", y = "Frequência") +
    theme_minimal()
  
# Densidade da variável de saúde
  ggplot(educ, aes(x = pressao_sistolica)) +
    geom_density(fill = "orange", alpha = 0.5) +
    labs(title = "Distribuição da pressão sistólica",
         x = "Pressão sistólica (mmHg)", y = "Densidade") +
    theme_minimal()
  
# Boxplot da pressão sistólica por diagnóstico
  ggplot(educ, aes(x = diagnostico, y = pressao_sistolica, fill = diagnostico)) +
    geom_boxplot(alpha = 0.6) +
    labs(title = "Pressão sistólica por diagnóstico",
         x = "Diagnóstico", y = "Pressão sistólica (mmHg)") +
    theme_minimal()
  
# Dispersão relacionando educação e saúde
  ggplot(educ, aes(x = anos_estudo, y = pressao_sistolica)) +
    geom_point(alpha = 0.5, color = "darkgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "red") +
    labs(title = "Relação entre anos de estudo e pressão sistólica",
         x = "Anos de estudo", y = "Pressão sistólica (mmHg)") +
    theme_minimal()

  ## Resposta Exercício 8:
  # Os gráficos mostram que os anos de estudo variam bastante entre os indivíduos,
  # enquanto a pressão sistólica apresenta distribuição concentrada em torno de 120–130 mmHg.
  # O boxplot indica que pessoas com diagnóstico de HAS tendem a ter pressão mais alta.
  # A dispersão sugere uma leve associação negativa: indivíduos com mais anos de estudo
  # tendem a apresentar pressão sistólica um pouco menor.

# ============================================================  
#9  (8) educ_saude.csv– figura final
# Objetivo. Contar uma história em um gráfico (título, eixos, legenda, caption, escala
#adequada).
#Bloco adicional: não há
# ============================================================
  
  ggplot(educ, aes(x = anos_estudo, y = pressao_sistolica, color = diagnostico)) +
    geom_point(alpha = 0.6) +
    geom_smooth(method = "lm", se = FALSE) +
    labs(
      title = "Educação e saúde: relação entre anos de estudo e pressão arterial",
      subtitle = "Indivíduos com mais anos de estudo tendem a apresentar menor pressão sistólica",
      x = "Anos de estudo",
      y = "Pressão sistólica (mmHg)",
      color = "Diagnóstico",
      caption = "Fonte: Base educ_saude.csv (simulada para fins didáticos)"
    ) +
    theme_minimal()
  
## Resposta Exercício 9:
# O gráfico mostra que indivíduos com mais anos de estudo tendem a apresentar pressão sistólica menor.
# Além disso, observa-se que pessoas com diagnóstico de HAS concentram valores mais altos de pressão.
# A figura final conta uma história clara: maior escolaridade está associada a melhores indicadores de saúde.

# ============================================================ 
#10 10 (9) Desafio (pontos extras)– Eleições 2024 (Municí
 # pio de São Paulo) com TSE + electionsBR
#Meta. Baixar dados oficiais das eleições 2024 (município de São Paulo), calcular taxas
#por zona eleitoral: abstenção, brancos, nulos; construir proxy de renda por escolaridade
#do eleitorado e investigar relação com essas taxas.
#Você fará as visualizações. Abaixo: apenas pipeline de baixar → filtrar → agregar.
# ============================================================
  
  # ============================================================
  # Exercício 10 (9) — Desafio: Eleições 2024 em São Paulo
  # ============================================================
  
  library(tidyverse)
  library(electionsBR)
  library(scales)
  
  # 1. Baixar dados detalhados por município e zona (2024, SP)
  res_sp <- electionsBR::elections_tse(2024, type = "details_mun_zone", uf = "SP")
  
  # 2. Filtrar município de São Paulo
  res_sp_mun <- res_sp |>
    mutate(NM_MUNICIPIO = toupper(NM_MUNICIPIO)) |>
    filter(NM_MUNICIPIO == "SÃO PAULO")
  
  # 3. Agregar taxas por zona eleitoral
  tx_zona <- res_sp_mun |>
    group_by(NR_ZONA) |>
    summarise(
      aptos = sum(QT_APTOS, na.rm = TRUE),
      comp  = sum(QT_COMPARECIMENTO, na.rm = TRUE),
      abst  = sum(QT_ABSTENCOES, na.rm = TRUE),
      br    = sum(QT_VOTOS_BRANCOS, na.rm = TRUE),
      nu    = sum(QT_VOTOS_NULOS, na.rm = TRUE),
      denom_votos = sum(QT_COMPARECIMENTO, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(
      tx_abst   = abst / aptos,
      tx_branco = br / denom_votos,
      tx_nulo   = nu / denom_votos
    )
  
  # Conferir primeiras linhas
  head(tx_zona)
  
  # 4. Perfil do eleitor por zona (escolaridade)
  vp <- electionsBR::elections_tse(2024, type = "voter_profile", uf = "SP")
  
  vp_sp_mun <- vp |>
    mutate(NM_MUNICIPIO = toupper(NM_MUNICIPIO)) |>
    filter(NM_MUNICIPIO == "SÃO PAULO") |>
    mutate(
      DS_GRAU_ESCOLARIDADE = toupper(DS_GRAU_ESCOLARIDADE),
      sup_ou_mais = DS_GRAU_ESCOLARIDADE %in%
        c("SUPERIOR COMPLETO", "SUPERIOR INCOMPLETO", "POS-GRADUACAO")
    ) |>
    group_by(NR_ZONA) |>
    summarise(
      eleitores_total = sum(QT_ELEITORES_PERFIL, na.rm = TRUE),
      eleitores_sup   = sum(QT_ELEITORES_PERFIL[sup_ou_mais], na.rm = TRUE),
      share_sup       = eleitores_sup / eleitores_total,
      .groups = "drop"
    )
  
  head(vp)
  
  head(vp_sp_mun)
  

  # 5. Integrar perfil do eleitor com taxas por zona
  df_final <- tx_zona |>
    inner_join(vp_sp_mun, by = "NR_ZONA")
  
  # Conferir primeiras linhas
  head(df_final)
  
  # 6. Gráficos de dispersão
  
  # Abstenção vs. escolaridade
  ggplot(df_final, aes(x = share_sup, y = tx_abst)) +
    geom_point(color = "steelblue") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      x = "Proporção de eleitores com ensino superior",
      y = "Taxa de abstenção",
      title = "Abstenção vs Escolaridade — São Paulo 2024"
    )
  
  # Brancos vs. escolaridade
  ggplot(df_final, aes(x = share_sup, y = tx_branco)) +
    geom_point(color = "forestgreen") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      x = "Proporção de eleitores com ensino superior",
      y = "Taxa de votos brancos",
      title = "Votos brancos vs Escolaridade — São Paulo 2024"
    )
  
  # Nulos vs. escolaridade
  ggplot(df_final, aes(x = share_sup, y = tx_nulo)) +
    geom_point(color = "orange") +
    geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    scale_x_continuous(labels = percent_format(accuracy = 1)) +
    scale_y_continuous(labels = percent_format(accuracy = 1)) +
    labs(
      x = "Proporção de eleitores com ensino superior",
      y = "Taxa de votos nulos",
      title = "Votos nulos vs Escolaridade — São Paulo 2024"
    )
  
  # 7. Interpretação dos resultados (texto acadêmico)
  
  interpretacao <- "
A análise dos dados das eleições de 2024 no município de São Paulo revela uma associação
entre escolaridade e comportamento eleitoral. Observa-se que zonas eleitorais com maior
proporção de eleitores com ensino superior apresentam, em média, menores taxas de abstenção,
o que sugere maior engajamento político desse segmento. Em relação aos votos brancos e nulos,
a relação é menos consistente: em algumas zonas há redução, mas em outras a dispersão indica
que não existe correlação forte.

Os gráficos de dispersão confirmam essa tendência: a inclinação negativa da regressão linear
entre escolaridade e abstenção é clara, enquanto para brancos e nulos a associação é mais
fraca. Isso sugere que o nível educacional influencia mais a decisão de comparecer às urnas
do que a escolha de votar branco ou nulo.

É importante destacar as limitações da análise. A escolaridade foi utilizada como proxy de
renda, o que é uma simplificação, já que renda e educação não são equivalentes. Além disso,
os dados estão agregados por zona eleitoral, podendo ocultar heterogeneidades internas.
Por fim, trata-se de uma análise correlacional: não é possível afirmar causalidade, apenas
associação estatística entre variáveis.
"
  
  cat(interpretacao)
  
  # ============================================================
  # Finalização do exercício
  # ============================================================
  
  # Exibe informações sobre o ambiente de execução:
  # versão do R, pacotes carregados e suas versões.
  # Útil para garantir reprodutibilidade dos resultados.
  sessionInfo()
  
  print("Script finalizado sem erros!")
  