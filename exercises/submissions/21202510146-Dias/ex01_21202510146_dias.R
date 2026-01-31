# ============================================================
# MQ101 — Métodos Quantitativos para Políticas Públicas
# Lista de Exercícios #01 — TEMPLATE DE SCRIPT (R)
# ------------------------------------------------------------
# Preencha os campos abaixo antes de começar.
# Nome: DEBORA RODRIGUES PEREIRA DIAS
# Matrícula/RA: 21202510146
# Turma: PPU00920253
# Data: 13/01/2026
# Descrição: Respostas da Lista #01 (HOPR Partes I–II, caps. 1–8)
# ============================================================

# ======================== ORIENTAÇÕES ========================
# 1) Este script é o MODELO para a sua entrega (.R).
# 2) Execute o script de cima para baixo (Ctrl/Cmd + Shift + Enter no RStudio)
#    ou use o botão "Source".
# 3) Onde estiver escrito TODO ou ENTREGA, substitua pelos seus códigos e respostas.
# 4) Use comentários explicativos: diga o que o código faz e por que isso é útil.
# 5) Use set.seed() quando fizer simulações para garantir reprodutibilidade.
# 6) Gere também um PDF com os resultados e interpretações (via Rmd ou outro).
# 7) Não utilize pacotes além de 'ggplot2' (opcional). Prefira base R.
# ============================================================

# ===================== 0) PREPARAÇÃO =========================
# (Obrigatório, sem pontuação — verificação do ambiente)

# Versão do R
R.version.string
# Em RStudio:
# RStudio.Version()$mode

# Se estiver no RStudio, esta chamada retorna informações do RStudio (pode falhar fora do RStudio).
# tryCatch(RStudio.Version()$mode, error = function(e) "RStudio não detectado")

# Reprodutibilidade global para esta lista (você pode mudar, mas mantenha constante)
set.seed(202501)

# Carregamento opcional do ggplot2 (apenas se desejar usar ggplot para gráficos)
if (requireNamespace("ggplot2", quietly = TRUE)) {
  library(ggplot2)
} else {
  message("Pacote 'ggplot2' não encontrado. Usando gráficos base R (hist).")
}

# Dica: escolha uma pasta de trabalho, se necessário (descomente e ajuste):
# setwd(\"~/caminho/para/sua/pasta\")
# Lê o arquivo educ_saude.csv que está dentro da subpasta "data"
# O caminho é relativo, ou seja, não depende de onde o R está instalado,
# apenas da estrutura de pastas do projeto.
# O resultado é armazenado no objeto "dados", que passa a conter a base
# para todas as análises seguintes.
dados <- read_csv("data/educ_saude.csv")


# ============================================================
# ===================== EXERCÍCIO 1 (10 pts) =================
# R como calculadora — ordem das operações (HOPR cap. 1)
# Objetivo: executar expressões e entender a ordem das operações.
# ENTREGA: cole os resultados no PDF e escreva, em 3–5 linhas, como os parênteses afetam o resultado.

# TODO: Execute as linhas a seguir e observe os resultados.
10 + 2
(10 + 2) * 3
((10 + 2) * 3 - 6) / 3

##Resposta: 
#> 10 + 2
#[1] 12
#> (10 + 2) * 3
#[1] 36
#> ((10 + 2) * 3 - 6) / 3
#[1] 10
## Os parênteses mudam a ordem das operações, garantindo que certas partes da expressão sejam resolvidas antes das demais.
## Isso altera diretamente o resultado final, como vimos nos exemplos. Em políticas públicas e ciências sociais,
##esse controle é essencial porque indicadores compostos (como taxas ajustadas ou índices) 
##dependem de cálculos bem estruturados para evitar interpretações equivocadas.


# ============================================================
# ===================== EXERCÍCIO 2 (10 pts) =================
# Objetos e nomeação (HOPR cap. 1)
# ENTREGA: explique em 2–4 linhas a diferença entre 'Name' e 'name'.

# TODO: Execute e observe os resultados. R diferencia maiúsculas/minúsculas.
x <- 1:6
Name <- 1
name <- 0
Name + 1
name + 1

# Explique em 2–4 linhas a diferença entreName e name.
##Resposta:
#> x <- 1:6
#> Name <- 1
#> name <- 0
#> Name + 1
#[1] 2
#> name + 1
#[1] 1
#>
## No R, Name e name são objetos diferentes porque o software distingue maiúsculas de minúsculas.
##Assim, Name <- 1 cria um objeto independente de name <- 0.
##Quando somamos, cada um retorna resultados distintos (2 e 1), 
##mostrando que a nomeação precisa ser consistente para evitar erros em análises.
##Portanto, a distinção entre maiúsculas e minúsculas é fundamental para evitar confusões na criação e manipulação de objetos


# ============================================================
# ===================== EXERCÍCIO 3 (10 pts) =================
# Sorteio (sample) e reprodutibilidade (HOPR cap. 1–2)
# ENTREGA: descreva o papel de set.seed() em 3–5 linhas.

# TODO: Compare com e sem set.seed()
set.seed(123)
die <- 1:6
sample(die, size = 2, replace = TRUE)

##Resposta 1: 
#> set.seed(123)
#> die <- 1:6
#> sample(die, size = 2, replace = TRUE)
#[1] 3 6
##A função set.seed() fixa a sequência aleatória usada pelo R, garantindo que os resultados sejam sempre os mesmos quando o código é repetido.
##Isso é importante para reprodutibilidade: com a semente definida, todos que rodarem o script terão o mesmo sorteio.
##Sem set.seed(), cada execução gera números diferentes, o que dificulta comparar análises em políticas públicas ou ciências sociais.

# (Teste: reexecute as linhas acima e verifique se o resultado se repete)
set.seed(123)
die <- 1:6
sample(die, size = 2, replace = TRUE)

##Resposta 2: Teste de reprodutibilidade realizado, o resultado se repete
#> set.seed(123)
#> die <- 1:6
#> sample(die, size = 2, replace = TRUE)
#[1] 3 6

# (Agora remova ou mude o set.seed e compare)

##Removendo set.seed
die <- 1:6
sample(die, size = 2, replace = TRUE)

##Resposta 3: Removendo o set.seed altera o resultado.
#> die <- 1:6
#> sample(die, size = 2, replace = TRUE)
#> [1] 3 2

##Mudando o set.seed
set.seed(999)
die <- 1:6
sample(die, size = 2, replace = TRUE)

##Resposta 4: Mudando o set.seed altera o resultado.
#> set.seed(999)
#> die <- 1:6
#> sample(die, size = 2, replace = TRUE)
#> [1] 3 4

##Resposta comparativa do exercício 3: Com set.seed(123), o sorteio sempre repete [1] 3 6.
##Sem set.seed(), o resultado muda a cada execução. 
##Alterando a semente (ex.: set.seed(999)), o resultado volta a ser fixo, mas diferente. 
##Isso mostra que set.seed() garante reprodutibilidade.
##Assim, o uso de set.seed() é essencial em simulações e pesquisas, 
##pois garante que os resultados possam ser replicados e validados por diferentes pessoas.

# ============================================================
# ===================== EXERCÍCIO 4 (10 pts) =================
# Sua primeira função (HOPR cap. 1)
# ENTREGA: explique o que faz cada linha da função em 4–6 linhas.
# BÔNUS: implemente 'soma3()' (sorteia 3 números entre 1 e 6 e retorna a soma).

# TODO: Defina a função e teste:
roll2 <- function(bones = 1:6) {
  # Sorteia dois valores do vetor 'bones' com reposição e soma.
  # 'bones' por padrão é 1:6 (um dado comum).
  dice <- sample(bones, size = 2, replace = TRUE)
  sum(dice)
}
# Testes sugeridos:
roll2()
roll2(1:20)

##Resposta: > # Testes sugeridos:
#> roll2()
#[1] 6
#> roll2(1:20)
#[1] 15
#> 
##A função roll2 define um conjunto de valores possíveis (bones = 1:6).
##Em seguida, sample() sorteia dois números desse vetor com reposição e guarda em dice. 
##A linha sum(dice) soma os dois valores e retorna o resultado. 
##Assim, roll2() simula dois dados comuns, 
##e roll2(1:20) mostra que podemos alterar o intervalo de valores.

# TODO (BÔNUS): implementar soma3()
# soma3 <- function() {
#   # sua implementação aqui
# }

soma3 <- function(bones = 1:6) {
  dice <- sample(bones, size = 3, replace = TRUE)
  sum(dice)
}
# Teste:
soma3()

##Resposta do teste:
# > soma3()
#[1] 10
#>


# ============================================================
# ===================== EXERCÍCIO 5 (10 pts) =================
# Ajuda e exemplos (HOPR cap. 2)
# ENTREGA: resuma argumentos de sample() e como consultar ajuda (4–6 linhas).

# TODO: Consulte a ajuda e rode exemplos
# ?sample
# example(sample)

# Dica: leia os argumentos 'x', 'size', 'replace', 'prob', etc.

?sample
example(sample)

##Resposta: A função sample() sorteia elementos de um vetor. 
##Seus principais argumentos são: x (vetor base), size (quantidade de elementos), 
##replace (se pode repetir valores) (TRUE = com reposição, FALSE = sem reposição).e prob (probabilidades associadas).
##Para consultar ajuda, usa-se ?sample, e para ver exemplos práticos, example(sample).
##Esses recursos permitem compreender e testar funções no R de forma prática, 
##garantindo que o usuário saiba como aplicar corretamente os argumentos em diferentes contextos.

# ============================================================
# ===================== EXERCÍCIO 6 (15 pts) =================
# Simulação e histograma (HOPR cap. 1–2)
# ENTREGA: histograma, média, desvio-padrão; interpretação (4–6 linhas).
# Dica: você pode salvar o gráfico com png()... dev.off()

set.seed(42)
somas <- replicate(10000, roll2())
length(somas)
hist(somas, main = "Soma de dois dados (10.000 lançamentos)", xlab = "Soma")
mean(somas); sd(somas)


# (Opcional) Salvar figura:
# png(\"ex6_hist_somas.png\", width = 900, height = 600)
# hist(somas, main = \"Soma de dois dados (10.000 lançamentos)\", xlab = \"Soma\")
# dev.off()
png("ex6_hist_somas.png", width = 900, height = 600)
hist(somas, main = "Soma de dois dados (10.000 lançamentos)", xlab = "Soma")
dev.off()

# Extensão (opcional, sem pontos extras): dado viciado favorecendo o 6
prob_vies <- c(rep(1/8, 5), 3/8)
somas_vies <- replicate(10000, sum(sample(1:6, size = 2, replace = TRUE, prob = prob_vies)))
# hist(somas_vies, main = \"Dado viciado (6 favorecido)\", xlab = \"Soma\")

##Resposta: O histograma mostra que a soma de dois dados segue uma distribuição triangular, 
##concentrada em torno de 7. A média obtida é próxima de 7 e o desvio-padrão em torno de 2,4,
##refletindo a variabilidade moderada. Valores extremos (2 e 12) aparecem pouco,
## enquanto 6–8 são os mais frequentes.
##Esse comportamento confirma que a soma de dois dados segue uma distribuição simétrica, 
##com maior probabilidade de valores médios, o que é útil para compreender fenômenos aleatórios em políticas públicas.
##> set.seed(42)
#> somas <- replicate(10000, roll2())
#> length(somas)
#[1] 10000
#> hist(somas, main = "Soma de dois dados (10.000 lançamentos)", xlab = "Soma")
#> mean(somas); sd(somas)
#[1] 6.9532
#[1] 2.4329
#> png("ex6_hist_somas.png", width = 900, height = 600)
#> hist(somas, main = "Soma de dois dados (10.000 lançamentos)", xlab = "Soma")
#> dev.off()
#RStudioGD 
#2 


# ============================================================
# ===================== EXERCÍCIO 7 (10 pts) =================
# Tipos básicos (HOPR cap. 3)
# ENTREGA: explique o que 'str()' revela sobre cada tipo e cite um uso prático.

dbl <- c(1.5, 2.0)            # numéricos (double)
int <- c(1L, 2L)              # inteiros
chr <- c("saude", "educacao") # texto (character)
lgl <- c(TRUE, FALSE)         # lógico (booleano)

str(list(dbl = dbl, int = int, chr = chr, lgl = lgl))

##Resposta:
#List of 4
#$ dbl: num [1:2] 1.5 2
#$ int: int [1:2] 1 2
#$ chr: chr [1:2] "saude" "educacao"
#$ lgl: logi [1:2] TRUE FALSE
## Assim, str() é uma ferramenta essencial para inspecionar rapidamente a estrutura de objetos e 
##garantir que os tipos de dados estejam corretos antes de análises mais complexas
## Aqui vemos quatro tipos básicos: numérico (double), inteiro, texto (character) e lógico.
## Cada tipo tem usos práticos: números para cálculos, inteiros para contagens,
## texto para categorias e lógicos para condições em análises.


# ============================================================
# ===================== EXERCÍCIO 8 (10 pts) =================
# Data.frame (baralho) e mini-base municipal (HOPR cap. 3)
# ENTREGA: nº de linhas/colunas e breve interpretação do summary().

# Baralho
faces <- c("ace","two","three","four","five","six","seven",
           "eight","nine","ten","jack","queen","king")
suits <- c("spades","hearts","diamonds","clubs")
deck  <- data.frame(
  face  = rep(faces, times = 4),
  suit  = rep(suits, each = 13),
  value = rep(1:13, times = 4)
)
nrow(deck); ncol(deck)
head(deck, 10)

# Mini-base municipal (dados simulados)
set.seed(2025)
municipios <- paste0("Mun_", sprintf("%02d", 1:10))
dados_munic <- data.frame(
  municipio        = municipios,
  gasto_saude_pc   = round(runif(10, 200, 1200), 2),
  taxa_evasao      = round(runif(10, 0.00, 0.20), 3),
  taxa_desemprego  = round(rnorm(10, 0.12, 0.03), 3)
)
head(dados_munic); summary(dados_munic)

##Resposta:O baralho tem 52 linhas e 3 colunas, representando todas as cartas com seus valores e naipes.
##Já a mini-base municipal possui 10 linhas e 4 colunas, simulando indicadores de saúde, evasão escolar e desemprego. 
##O summary() revela estatísticas descritivas (mínimo, máximo, média e quartis),
##permitindo comparar municípios e identificar variações nos indicadores.
##Essas estatísticas iniciais são fundamentais para análises exploratórias, 
##pois permitem identificar padrões e possíveis outliers antes de aplicar modelos mais complexos.

# ============================================================
# ===================== EXERCÍCIO 9 (10 pts) =================
# Seleção e filtros (HOPR cap. 4)
# ENTREGA: descreva os retornos e quantos municípios têm taxa_evasao > 0.10.

deck[1, ]                                # primeira linha do baralho
deck[c(1,3,5), c("face","suit")]         # linhas 1,3,5 e colunas face/suit
deck[-(1:48), ]                          # últimas 4 linhas (cartas 49 a 52)

subset_hearts <- deck[ deck$suit == "hearts", ]  # todas as cartas de copas
nrow(subset_hearts)                      # número de cartas de copas (13)

evaz_alta <- dados_munic[ dados_munic$taxa_evasao > 0.10, ] # municípios com evasão > 10%
evaz_alta
nrow(evaz_alta)                          # quantos municípios atendem ao filtro

##Resposta:
# deck[1, ] retorna a primeira carta do baralho.
# deck[c(1,3,5), c("face","suit")] mostra apenas face e naipe das linhas 1, 3 e 5.
# deck[-(1:48), ] retorna as últimas quatro cartas.
# O filtro subset_hearts seleciona todas as cartas de copas (13 no total).
# Já evaz_alta mostra os municípios com taxa de evasão > 0.10 (10%); 
#foram encontrados 4 municípios.Esses filtros permitem selecionar subconjuntos relevantes de dados, 
#o que é essencial em análises aplicadas, como identificar municípios com indicadores críticos.


# ============================================================
# ===================== EXERCÍCIO 10 (10 pts) ================
# Modificando valores e NA (HOPR cap. 5)
# ENTREGA: explique o efeito de na.rm = TRUE e quando usá-lo.

# --- Pré-requisitos (do Exercício 8) --- com auxílio do Copilot
faces <- c("ace","two","three","four","five","six","seven",
           "eight","nine","ten","jack","queen","king")
suits <- c("spades","hearts","diamonds","clubs")
deck  <- data.frame(
  face  = rep(faces, times = 4),
  suit  = rep(suits, each = 13),
  value = rep(1:13, times = 4)
)

set.seed(2025)
municipios <- paste0("Mun_", sprintf("%02d", 1:10))
dados_munic <- data.frame(
  municipio        = municipios,
  gasto_saude_pc   = round(runif(10, 200, 1200), 2),
  taxa_evasao      = round(runif(10, 0.00, 0.20), 3),
  taxa_desemprego  = round(rnorm(10, 0.12, 0.03), 3)
)

# Modificando valores (ases = 14)
deck2 <- deck
deck2$value[c(13, 26, 39, 52)] <- 14
head(deck2, 13)

# Valores ausentes
vals <- c(NA, 1:5)
mean(vals)                 # retorna NA
mean(vals, na.rm = TRUE)   # ignora NA

dados_m2 <- dados_munic
dados_m2$taxa_evasao[3] <- NA
dados_m2$gasto_saude_pc[7] <- NA

mean(dados_m2$taxa_evasao)             # NA
mean(dados_m2$taxa_evasao, na.rm=TRUE) # média sem NA

##Resposta:
# O argumento na.rm = TRUE faz com que funções como mean() ignorem valores ausentes (NA).
# Sem ele, o resultado é NA. Usamos quando queremos calcular estatísticas apenas com dados válidos.
# Esse recurso é essencial em bases reais de políticas públicas, 
#onde dados incompletos são comuns e precisam ser tratados para evitar resultados inválidos.


# ============================================================
# ========== EXERCÍCIO 11 (OPCIONAL, até 10 pts) ============
# Funções que \"guardam estado\" (HOPR cap. 6)
# ENTREGA: explique o conceito e dê exemplo análogo em PP/CS.

setup <- function(deck_init) {
  DECK <- deck_init  # cópia interna (estado)
  
  DEAL <- function() {
    # Devolve a primeira carta e atualiza o baralho interno removendo-a.
    card <- DECK[1, , drop = FALSE]
    DECK <<- DECK[-1, , drop = FALSE]
    return(card)
  }
  
  SHUFFLE <- function() {
    # Reembaralha o baralho interno
    idx <- sample(seq_len(nrow(deck_init)), size = nrow(deck_init))
    DECK <<- deck_init[idx, , drop = FALSE]
    invisible(NULL)
  }
  
  list(deal = DEAL, shuffle = SHUFFLE)
}

cards <- setup(deck)
cards$deal(); cards$deal(); cards$shuffle(); cards$deal()

##Resposta
#Funções que guardam estado mantêm uma memória interna que é atualizada a cada chamada.
#No exemplo, deal() remove cartas do baralho e shuffle() reembaralha, alterando o objeto interno DECK. 
#Isso permite que o baralho “lembre” quais cartas já saíram. Em políticas públicas ou ciência de dados, 
#um análogo seria uma função que simula um orçamento ou fila de atendimento, atualizando o saldo ou a lista de espera a cada operação.
#“Esse tipo de função é útil em simulações dinâmicas, 
#pois permite modelar processos que evoluem ao longo do tempo sem perder o histórico.


# ============================================================
# Finalização do exercício
# ============================================================

# Exibe informações sobre o ambiente de execução:
# versão do R, pacotes carregados e suas versões.
# Útil para garantir reprodutibilidade dos resultados.
sessionInfo()

print("Script finalizado sem erros!")
