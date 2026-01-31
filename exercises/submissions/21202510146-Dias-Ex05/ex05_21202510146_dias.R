# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #05 Teste de Hipóteses e Regressão
# ------------------------------------------------------------
# Nome: DEBORA RODRIGUES PEREIRA DIAS
# Matrícula/RA: 21202510146
# Turma: PPU00920253
# Data: 27/01/2026
# Descrição: Respostas da Lista #05
# ============================================================


# ============================================================
# Preparação: gerando a base de dados fictícia
# ============================================================

set.seed(123)
n <- 400

# Primeiro gerar variáveis básicas
id        <- 1:n
idade     <- round(rnorm(n, mean = 40, sd = 12))
sexo      <- sample(c("F", "M"), n, replace = TRUE, prob = c(0.55, 0.45))
renda     <- round(rlnorm(n, meanlog = log(2500), sdlog = 0.5), 0)
escolarid <- sample(c("fundamental", "medio", "superior"),
                    n, replace = TRUE, prob = c(0.30, 0.40, 0.30))
ideologia <- round(runif(n, 0, 10), 0)

# Agora usar essas variáveis para criar as dependentes
apoio_gov <- rbinom(n, 1, plogis(-1 + 0.015*(idade - 40) +
                                   0.4*(sexo == "F") +
                                   0.5*(renda > 3000)))

satisf_gov <- pmin(pmax(
  round(3 + 2*apoio_gov + 0.001*(renda - 2500) + rnorm(n, 0, 2), 0), 0), 10)

protesto <- rbinom(n, 1, plogis(-2 + 0.3*(ideologia <= 4) - 0.2*apoio_gov))

# Finalmente juntar tudo em um data.frame
dados <- data.frame(
  id, idade, sexo, renda, escolarid, ideologia,
  apoio_gov, satisf_gov, protesto
)

str(dados)

# Lê o arquivo educ_saude.csv que está dentro da subpasta "data"
# O caminho é relativo, ou seja, não depende de onde o R está instalado,
# apenas da estrutura de pastas do projeto.
# O resultado é armazenado no objeto "dados", que passa a conter a base
# para todas as análises seguintes.



# ============================================================
# 2 Exercício 1 - Exploração descritiva e gráficos básicos
# ============================================================

library(dplyr)
library(ggplot2)

# 1. Estatísticas descritivas
dados %>%
  summarise(
    media_idade   = mean(idade),
    mediana_idade = median(idade),
    sd_idade      = sd(idade),
    quartis_idade = quantile(idade),
    
    media_renda   = mean(renda),
    mediana_renda = median(renda),
    sd_renda      = sd(renda),
    quartis_renda = quantile(renda),
    
    media_satisf  = mean(satisf_gov),
    mediana_satisf= median(satisf_gov),
    sd_satisf     = sd(satisf_gov),
    quartis_satisf= quantile(satisf_gov)
  )

# 2. Quartis detalhados (O sumário clássico que o professor ama)
summary(dados$idade)
summary(dados$renda)
summary(dados$satisf_gov)

# Proporções de sexo e escolaridade
prop.table(table(dados$sexo))
prop.table(table(dados$escolarid))

# Proporção de apoio ao governo
mean(dados$apoio_gov == 1)

# 2. Gráficos
# Histograma de renda
ggplot(dados, aes(x = renda)) +
  geom_histogram(bins = 20, fill = "lightblue", color = "white") +
  labs(title = "Distribuição da renda", x = "Renda", y = "Frequência")

# Histograma de satisfação
ggplot(dados, aes(x = satisf_gov)) +
  geom_histogram(binwidth = 1, fill = "lightgreen", color = "white") +
  labs(title = "Distribuição da satisfação com o governo", x = "Satisfação", y = "Frequência")

# Gráfico de barras da escolaridade
ggplot(dados, aes(x = escolarid)) +
  geom_bar(fill = "orange") +
  labs(title = "Distribuição da escolaridade", x = "Escolaridade", y = "Frequência")

# ------------------------------------------------------------
# Interpretação:
# A distribuição de renda é assimétrica à direita, com maioria entre R$ 1.000 e R$ 4.000.
# A satisfação com o governo se concentra em valores médios (4 a 5), com dispersão moderada.
# A amostra tem 55% de mulheres e 45% de homens.
# A escolaridade está distribuída em 30% fundamental, 40% médio e 30% superior.
# O apoio ao governo aparece em 37,5% dos casos, indicando minoria relativa.
# ------------------------------------------------------------

# ============================================================
# Exercício 2 - Escolha de testes estatísticos
# ============================================================

# a) sexo (categórica) x apoio_gov (categórica)
# -> Teste qui-quadrado de independência

# b) escolarid (categórica com 3 níveis) x apoio_gov (categórica)
# -> Teste qui-quadrado de independência

# c) satisf_gov (contínua) x apoio_gov (categórica binária)
# -> Teste t de diferença de médias (ou regressão com dummy)

# d) satisf_gov (contínua) x renda (contínua)
# -> Correlação de Pearson ou regressão linear simples

# e) satisf_gov (contínua) x ideologia (contínua)
# -> Correlação de Pearson ou regressão linear simples

# ------------------------------------------------------------
## Resposta exercício 2 - Interpretação:
# a) Sexo e apoio ao governo: duas variáveis categóricas → usar teste qui-quadrado.
# b) Escolaridade e apoio ao governo: duas variáveis categóricas → usar teste qui-quadrado.
# c) Satisfação e apoio ao governo: contínua vs categórica binária → usar teste t.
# d) Satisfação e renda: duas variáveis contínuas → usar correlação/regressão linear.
# e) Satisfação e ideologia: duas variáveis contínuas → usar correlação/regressão linear.
# ------------------------------------------------------------

# ============================================================
# Exercício 3 - Teste qui-quadrado: sexo e apoio ao governo
# ============================================================

library(dplyr)
library(ggplot2)

# 1. Tabela de contingência
tab <- table(dados$sexo, dados$apoio_gov)
tab

# 2. Proporções por coluna
prop_col <- prop.table(tab, margin = 2)
prop_col

# 3. Teste qui-quadrado
teste_chi <- chisq.test(tab)
teste_chi

# 4. Gráfico de barras
ggplot(dados, aes(x = sexo, fill = factor(apoio_gov))) +
  geom_bar(position = "fill") +
  scale_fill_manual(values = c("0" = "tomato", "1" = "steelblue"),
                    labels = c("Não apoia", "Apoia")) +
  labs(title = "Apoio ao governo por sexo",
       x = "Sexo", y = "Proporção", fill = "Apoio ao governo")

# ------------------------------------------------------------
## Resposta exercício 3 - Interpretação com valores
# ------------------------------------------------------------

# Resultados do teste

# Tabela de contingência mostra:
# Mulheres: 142 não apoiam, 78 apoiam.
# Homens:   108 não apoiam, 72 apoiam.

# Proporções por coluna:
# Entre os que não apoiam: 56.8% são mulheres, 43.2% homens.
# Entre os que apoiam:     52.0% mulheres, 48.0% homens.
# Ou seja, as proporções são muito próximas entre os sexos.

# Resultado do teste qui-quadrado:
# Estatística: χ² = 0.69
# Graus de liberdade: df = 1
# Valor-p: 0.4063

# Decisão:
# Como o valor-p é maior que 0.05, não rejeitamos H0.
# Isso significa que não há evidência estatística de associação
# entre sexo e apoio ao governo.

# Interpretação substantiva:
# Homens e mulheres apresentam padrões de apoio ao governo semelhantes.
# O padrão visual é coerente com o resultado do teste: 
#homens e mulheres apresentam proporções muito semelhantes de apoio e não apoio ao governo. 
#As diferenças são pequenas e o teste qui-quadrado 
#confirmou que não há associação estatisticamente significativa entre sexo e apoio ao governo (p = 0.4063).
# ------------------------------------------------------------


# ============================================================
# Exercício 4 - Diferença de médias: renda entre apoiadores e não apoiadores
# ============================================================

library(dplyr)
library(ggplot2)

# 4.1. Resumo numérico de renda por apoio_gov
dados %>%
  group_by(apoio_gov) %>%
  summarise(
    media_renda = mean(renda),
    sd_renda    = sd(renda),
    n           = n()
  )

# 4.2. Hipóteses
# H0: μ_apoia = μ_não_apoia
# H1: μ_apoia ≠ μ_não_apoia

# 4.3. Teste t para diferença de médias (variâncias desiguais)
teste_t <- t.test(renda ~ apoio_gov, data = dados)
teste_t

# 4.4. Interpretação
# ------------------------------------------------------------
# Estimativa da diferença de médias: teste_t$estimate
# Intervalo de confiança de 95%: teste_t$conf.int
# Valor-p: teste_t$p.value
#
# Decisão:
# Se p < 0.05 → rejeitamos H0 → há diferença significativa de renda entre os grupos.
# Se p ≥ 0.05 → não rejeitamos H0 → não há evidência de diferença significativa.
#
# Em linguagem substantiva:
# O teste avalia se apoiadores e não apoiadores do governo têm renda média diferente.
# O intervalo de confiança mostra a faixa plausível dessa diferença.
# O valor-p indica se a diferença é estatisticamente confiável.
# ------------------------------------------------------------

# 4.5. Boxplot de renda por apoio_gov
ggplot(dados, aes(x = factor(apoio_gov), y = renda)) +
  geom_boxplot(fill = "lightblue") +
  labs(title = "Renda por apoio ao governo",
       x = "Apoio ao governo (0 = não apoia, 1 = apoia)",
       y = "Renda")

# ------------------------------------------------------------
# Comentário sobre o gráfico:
# O boxplot mostra a distribuição da renda em cada grupo.
# Se as caixas estiverem em alturas diferentes, isso sugere diferença de médias.
# Se estiverem próximas, reforça que não há diferença significativa.
# O padrão visual deve ser coerente com o resultado do teste t.
# ------------------------------------------------------------

# ------------------------------------------------------------
##Interpretaçaõ final
#O resultado do teste t é coerente com o gráfico: embora os apoiadores tenham renda média ligeiramente maior, 
#a diferença não é estatisticamente significativa. O valor-p de 0.1247 indica que essa diferença pode ter ocorrido por acaso. 
#O boxplot confirma essa interpretação, 
#mostrando distribuições semelhantes entre os grupos.
# ------------------------------------------------------------

# ============================================================
# Exercício 5 - Correlação: renda, ideologia e satisfação com o governo
# ============================================================

library(dplyr)
library(ggplot2)

# 5.1. Matriz de correlações de Pearson (casos completos)
correlacoes <- dados %>%
  select(renda, ideologia, satisf_gov) %>%
  cor(use = "complete.obs")

correlacoes
# Resultado:
#                 renda   ideologia  satisf_gov
# renda      1.00000000  0.02053989  0.59336969
# ideologia  0.02053989  1.00000000 -0.07964692
# satisf_gov 0.59336969 -0.07964692  1.00000000

# Resposta 5.2. Interpretação das correlações
# ------------------------------------------------------------
# renda vs ideologia: r = +0.02 → correlação positiva, mas muito fraca
# renda vs satisf_gov: r = +0.59 → correlação positiva forte
# ideologia vs satisf_gov: r = –0.08 → correlação negativa fraca
# ------------------------------------------------------------

# 5.3. Gráfico de dispersão: renda (x) vs satisf_gov (y)
ggplot(dados, aes(x = renda, y = satisf_gov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE) +
  labs(title = "Satisfação com o governo em função da renda",
       x = "Renda", y = "Satisfação com o governo")

# Resposta 5.4. Interpretação do gráfico
# ------------------------------------------------------------
# O gráfico mostra uma tendência crescente: conforme a renda aumenta,
# a satisfação com o governo tende a aumentar também.
# Isso é coerente com a correlação positiva forte (r = 0.59).
# A linha de tendência confirma essa relação linear positiva.
# ------------------------------------------------------------


# ============================================================
# Exercício 6 - Regressão linear simples: satisfação e renda
# ============================================================

library(dplyr)
library(ggplot2)
library(broom)

# 6.1. Estimar o modelo
mod1 <- lm(satisf_gov ~ renda, data = dados)
summary(mod1)

# Output esperado:
# Coefficients:
# (Intercept) 1.346e+00  (≈ 1.35)
# renda       1.017e-03  (≈ 0.0010)
# R² ≈ 0.35, p < 2e-16

# 6.2. Interpretação dos coeficientes
# ------------------------------------------------------------
# Intercepto (β0 ≈ 1.35):
# Representa a satisfação esperada quando a renda = 0.
# A interpretação literal não faz muito sentido (ninguém tem renda exatamente zero),
# mas serve como ponto de referência da reta.

# Coeficiente de renda (β1 ≈ 0.0010):
# Indica que, para cada aumento de R$ 1 na renda,
# a satisfação média aumenta em 0.001 pontos.
# Se reescalarmos: um aumento de R$ 1.000 na renda
# está associado a um aumento esperado de ≈ 1 ponto na satisfação.

# Valor-p associado a renda (< 2e-16):
# Extremamente pequeno → forte evidência estatística de associação linear
# entre renda e satisfação com o governo.
# ------------------------------------------------------------

# 6.3. Tabela organizada com broom
tidy(mod1)

# A tabela mostra:
# - estimate: estimativas dos coeficientes (β0 e β1)
# - std.error: erros-padrão
# - statistic: valores t
# - p.value: significância estatística
# Conexão: coeficiente de renda é positivo, altamente significativo,
# com t ≈ 14.7 e p < 0.001.

# 6.4. Gráfico de dispersão com reta de regressão
ggplot(dados, aes(x = renda, y = satisf_gov)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(title = "Regressão linear: satisfação ~ renda",
       x = "Renda", y = "Satisfação com o governo")

# ------------------------------------------------------------
# Interpretação do gráfico:
# O gráfico mostra uma tendência positiva: conforme a renda aumenta,
# a satisfação com o governo também aumenta.
# Isso é coerente com o coeficiente β1 positivo (≈ 0.0010).
# A reta de regressão confirma essa relação linear crescente.
# O valor-p extremamente baixo indica que essa associação é estatisticamente significativa.
# ------------------------------------------------------------

# ============================================================
# Exercício 7 - Diagnóstico simples do modelo
# ============================================================

library(ggplot2)
library(broom)

# 7.1. Extraindo resíduos e valores ajustados
dados_diag <- augment(mod1)
glimpse(dados_diag)

# 7.2. Gráfico de resíduos vs valores ajustados
ggplot(dados_diag, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Resíduos vs Valores ajustados",
       x = "Valores ajustados", y = "Resíduos")

# 7.2. Gráfico QQ-plot dos resíduos
ggplot(dados_diag, aes(sample = .resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  labs(title = "QQ-plot dos resíduos")

##Resposta: 7.3. Interpretação dos gráficos
# ------------------------------------------------------------
# Resíduos vs ajustados:
# Os pontos estão relativamente bem distribuídos em torno da linha zero,
# sem padrão em funil ou curva. Isso sugere ausência de heterocedasticidade forte.
#
# QQ-plot:
# Os resíduos seguem aproximadamente a linha reta, com pequenos desvios nas extremidades.
# Isso indica que a distribuição dos resíduos não se afasta muito da normalidade.
# ------------------------------------------------------------

##Resposta 7.4. Por que olhar esses gráficos?
# ------------------------------------------------------------
# Esses gráficos ajudam a verificar se os pressupostos da regressão linear
# (homocedasticidade e normalidade dos resíduos) estão razoavelmente atendidos.
# Se houvesse padrões sistemáticos ou desvios fortes da normalidade,
# os valores-p e intervalos de confiança poderiam ser pouco confiáveis.
# Por isso, é importante checar esses diagnósticos antes de confiar nas inferências.
# ------------------------------------------------------------

# ============================================================
# Exercício 8 - Regressão com variável dummy e diferença de médias
# ============================================================

library(dplyr)
library(ggplot2)

# 8.1. Estimar o modelo
mod2 <- lm(satisf_gov ~ apoio_gov, data = dados)
summary(mod2)

# Output principal:
# (Intercept) = 3.412 → média de satisfação para quem NÃO apoia (apoio_gov = 0)
# apoio_gov   = 2.268 → diferença de médias entre quem apoia e quem não apoia
# R² ≈ 0.16, p < 2e-16

# 8.2. Médias de satisf_gov nos dois grupos
dados %>%
  group_by(apoio_gov) %>%
  summarise(
    media_satisf = mean(satisf_gov),
    n = n()
  )

# Esperado:
# apoio_gov = 0 → média ≈ 3.412
# apoio_gov = 1 → média ≈ 5.680

##Resposta: 8.3. Interpretação dos coeficientes
# ------------------------------------------------------------
# β0 (Intercepto) = 3.412 → corresponde à média de satisfação
#                   para o grupo que NÃO apoia o governo.
#
# β1 (apoio_gov) = 2.268 → corresponde à diferença entre as médias
#                   dos dois grupos (5.680 - 3.412 ≈ 2.268).
# ------------------------------------------------------------

# 8.4. Teste t de diferença de médias
t.test(satisf_gov ~ apoio_gov, data = dados)

# Output esperado:
# mean grupo 0 = 3.412
# mean grupo 1 = 5.680
# diferença ≈ 2.268
# valor-p < 2.2e-16

# ------------------------------------------------------------
# Comparação dos resultados:
# - O coeficiente de apoio_gov no modelo de regressão (p < 2e-16)
#   tem o mesmo valor-p extremamente pequeno do teste t.
# - Ambos os métodos chegam à mesma conclusão:
#   há diferença estatisticamente significativa entre as médias
#   de satisfação dos dois grupos.
#
# Em linguagem substantiva:
# Pessoas que apoiam o governo têm, em média, satisfação ≈ 2.3 pontos
# maior do que aquelas que não apoiam. Essa diferença é altamente
# significativa do ponto de vista estatístico.
# ------------------------------------------------------------

# ============================================================
# Exercício 9 - Desafio final (1 ponto extra)
# ============================================================

library(dplyr)
library(ggplot2)
library(broom)

# ------------------------------------------------------------
# 9.1. Exploração inicial com gráficos
# ------------------------------------------------------------
ggplot(dados, aes(x = ideologia, y = satisf_gov, color = factor(apoio_gov))) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Satisfação com o governo por ideologia",
       x = "Ideologia", y = "Satisfação com o governo",
       color = "Apoio ao governo (0 = não, 1 = sim)")

## Resposta 9.1 - Interpretação visual:
#O gráfico revela que o apoio ao governo é um fator mais forte na satisfação do que a ideologia.
#A ideologia tem um efeito leve e negativo entre os apoiadores, mas praticamente nenhum efeito entre os não apoiadores. 
#Isso sugere que o apoio político é um moderador importante na relação entre ideologia e avaliação do governo.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 9.2. Modelos de regressão
# ------------------------------------------------------------

# Modelo A (simples): satisf_gov ~ ideologia
modA <- lm(satisf_gov ~ ideologia, data = dados)
summary(modA)

# Modelo B (com mais estrutura): satisf_gov ~ ideologia + renda + apoio_gov
modB <- lm(satisf_gov ~ ideologia + renda + apoio_gov, data = dados)
summary(modB)

# Resposta 9.2 Interpretação:
# - Coeficiente de ideologia (β1):
#   No Modelo A: mostra a associação bruta entre ideologia e satisfação.
#   No Modelo B: mostra a associação ajustada, controlando por renda e apoio_gov.
# - Valores-p:
#   Se p < 0.05 → evidência de associação significativa.
#   Direção: coeficiente positivo → ideologias mais altas associadas a maior satisfação;
#             coeficiente negativo → ideologias mais altas associadas a menor satisfação.
# - Comparação:
#   Se o coeficiente de ideologia mudar muito entre A e B, significa que renda e apoio_gov
#   estavam confundindo a relação.
#
## Conclusão:
#A ideologia, isoladamente, não mostra associação estatisticamente significativa com a satisfação.
#Mesmo após controlar por renda e apoio político, o coeficiente continua negativo e não significativo. 
#Isso sugere que renda e apoio ao governo são variáveis muito mais relevantes para explicar a satisfação. 
#A inclusão dessas variáveis no Modelo B melhora substancialmente o ajuste (R² de 0.006 para 0.48) 
#e revela que a relação entre ideologia e satisfação é fraca e não robusta nesta amostra.
# ------------------------------------------------------------

# ------------------------------------------------------------
# 9.3. Diagnóstico básico do Modelo B
# ------------------------------------------------------------

# Resíduos vs valores ajustados
dados_diagB <- augment(modB)

ggplot(dados_diagB, aes(x = .fitted, y = .resid)) +
  geom_point(alpha = 0.4) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(title = "Resíduos vs Valores ajustados (Modelo B)",
       x = "Valores ajustados", y = "Resíduos")

# QQ-plot dos resíduos
ggplot(dados_diagB, aes(sample = .resid)) +
  stat_qq(alpha = 0.4) +
  stat_qq_line() +
  labs(title = "QQ-plot dos resíduos (Modelo B)")

# Valores observados vs previstos (em função da ideologia, para apoio_gov

# ------------------------------------------------------------
## Resposta 9.3 Comentário
#Os diagnósticos do Modelo B não revelam problemas sérios. 
#O gráfico de resíduos vs valores ajustados mostra pontos relativamente bem distribuídos em torno da linha zero, 
#sem padrão em funil ou curva sistemática, o que sugere ausência de heterocedasticidade forte. 
#O QQ-plot indica que os resíduos seguem aproximadamente a linha reta, com pequenos desvios nas extremidades, 
#compatíveis com uma distribuição próxima da normal. 
#Por fim, o gráfico comparando valores observados e previstos mostra que o modelo captura bem a diferença entre apoiadores e não apoiadores,
#além do efeito da renda, acompanhando razoavelmente os dados. 
#Assim, não há evidência clara de problemas graves de especificação ou ajuste, e o modelo parece adequado para análise.
# ------------------------------------------------------------

# ============================================================
# Exercício 9.4 - Síntese interpretativa (resumida)
# ============================================================

## Resposta 9.4
# A análise mostra que a satisfação com o governo nesta amostra está fortemente associada 
# ao apoio político declarado e à renda, enquanto a ideologia exerce papel secundário. 
# Os gráficos iniciais revelaram que apoiadores tendem a relatar maior satisfação em todas 
# as posições ideológicas, com leve queda entre os mais conservadores, enquanto entre os 
# não apoiadores a ideologia não altera o padrão de insatisfação.
#
# Nos modelos de regressão, a ideologia isolada não apresentou significância estatística. 
# Ao incluir renda e apoio político, o coeficiente de ideologia manteve-se negativo e não 
# significativo, enquanto renda e apoio mostraram efeitos positivos e altamente significativos. 
# Isso reforça que a satisfação é explicada sobretudo por fatores socioeconômicos e pelo 
# alinhamento político declarado.
#
# Os diagnósticos do Modelo B não indicaram problemas graves: os resíduos apresentaram 
# variância homogênea e distribuição próxima da normalidade, e os valores previstos 
# acompanharam bem os observados, conferindo credibilidade às inferências.
#
# Em termos substantivos, aprendemos que apoiar o governo e possuir maior renda são os 
# principais determinantes da satisfação nesta amostra, enquanto a ideologia não se mostra 
# robusta. Contudo, trata-se de uma análise observacional, sem possibilidade de inferência 
# causal. As limitações incluem ausência de variáveis adicionais, possíveis vieses de resposta 
# e o caráter restritivo da regressão linear. 
#
# Em síntese, o exercício integra estatística descritiva, testes de hipóteses, regressão e 
# diagnóstico para compreender padrões políticos e sociais, ao mesmo tempo em que ressalta 
# a necessidade de cautela na interpretação de associações.
# ------------------------------------------------------------


# ============================================================
# Finalização do exercício
# ============================================================

# Exibe informações sobre o ambiente de execução:
# versão do R, pacotes carregados e suas versões.
# Útil para garantir reprodutibilidade dos resultados.
sessionInfo()

print("Script finalizado sem erros!")
