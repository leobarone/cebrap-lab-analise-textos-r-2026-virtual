---
output:
  github_document: default
  html_document:
    toc: true
    toc_depth: 2
lang: pt-BR
encoding: UTF-8
---

# Tutorial 6 — Do vocabulário à classificação supervisionada de textos

**Aula 3 · Aprendizado supervisionado de textos**

No Tutorial 3, utilizamos a modelagem de tópicos para descobrir estruturas latentes em uma coleção de discursos de Lula. Não informamos ao modelo quais eram os assuntos dos discursos, ou seja, nosso objetivo foi desvendar os assuntos sobre os quais cada um discurso versava de maneira automatizada. Sem conhecer um conjunto dos discurso, buscamos classificar todos os discursos.

Agora vamos trabalhar com uma outra coleção de discursos, desta vez da Câmara dos Deputados na atual Legislatura (57ª legislatura, de 2023-2026). Partiremos do conhecimento inicial sobre qual é o partido do discursante e vamos gerar um modelo capaz de estimar qual é o partido do discurso um(a) parlamentar cujo partido desconhecemos.

Para simplificar o nosso exemplo trabalharemos apenas com 2 partidos: PT e PL. Vamos criar um classificador de discursos que nos dirá se um discurso é mais provável de ter sido proferido por um parlamentar do PT ou do PL.  Também não vamos trabalhar, por razões didáticas, com o discurso completo, o que seria perfeitamente possível. Vamos usar apenas o sumário dos discursos, que são facilmente obtidos na API de Dados Abertos da Câmara dos Deputados.

Não vamos procurar a melhor classificação possível a qualquer custo. Nosso interesse é compreender o percurso que vai da representação de um texto à construção e à avaliação de um modelo. Seguiremos a lógica do [capítulo 7 de Emil Hvitfeldt e Julia Silge](https://smltar.com/mlclassification), adaptada ao nosso exemplo.

## O que vamos aprender

- Distinguir aprendizado supervisionado e não supervisionado.
- Definir uma variável resposta e um preditor textual.
- Dividir uma coleção de textos em conjunto de treino e teste.
- Retomar frequências relativas e TF-IDF para comparar os vocabulários de dois grupos.
- Construir uma representação textual aplicável a novos documentos, um modelo nulo, um modelo Naive Bayes e uma regressão logística regularizada.

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE,
  fig.width = 8, fig.height = 5, out.width = "100%"
)
```

## 1. O que significa aprender de maneira supervisionada?

Na aula anterior, fornecemos ao modelo de tópicos (LDA) apenas os textos, representados por uma matriz de contagens. Os tópicos de nenhum dos documentos não era conhecidos previamente: precisávamos inferí-los. Tratava-se, então, de um problema não-supervisionado.

Em um problema supervisionado, como o que trataremos agora, observamos, além dos textos, uma variável que queremos prever. São os **rótulos** dos documentos, ou seja, uma informação sobre a classificação de cada documento. Chamamos essa variável de **resposta** (ou *outcome*) e as informações que utilizamos para fazer a previsão de **preditores** (*predictors*). Em cursos de metodologia de pesquisa também damos os nomes de variável dependente e independentes a esses dois tipos de variáveis, respectivamente.

| Aprendizado não supervisionado (Tutorial 3) | Aprendizado supervisionado (Tutorial 6) |
|---|---|
| Textos sem rótulos de interesse | Textos acompanhados de rótulos conhecidos |
| O modelo estima tópicos e misturas | O modelo aprende a prever a resposta |
| Interpretamos estruturas descobertas | Avaliamos previsões em dados novos |

A variável resposta pode ser numérica (como em um problema simples de regressão), ou categórica, como o partido de um parlamentar (um problema de classificação), classificação ideológica, tratar ou não de um assunto, ser do autor A ou B, ou etc.

No exemplo deste tutorial temos apenas duas categorias, portanto vamos trabalhar com **classificação binária**.

Imagine uma tabela com três colunas: o identificador do discurso, seu sumário e seu partido. Para treinar o modelo, conhecemos o partido. Para fazer uma previsão, forneceremos somente o sumário. Queremos aprender relações entre as características dos sumários e os rótulos dos exemplos de treinamento, para então fazer previsões sobre textos que o modelo não conhecia.

Uma observação importante: vamos prever o partido que consta do cadastro do deputado na coleta, não a ideologia da pessoa nem sua filiação necessariamente na data do discurso. Neste exemplo simplificado, a classificação também não pretende generalizar para todos os partidos. Trabalhamos com dois partidos previamente selecionados e uma modalidade de pronunciamento. É um exercício sobre análise de textos públicos e avaliação de modelos.

## 2. Pacotes e corpus

Vamos utilizar o `tidyverse` e o `tidytext`, que já conhecemos. O `tidymodels` reúne ferramentas para construir, treinar e avaliar modelos. Utilizaremos operações do `tidytext` e do `dplyr` para preparar os textos diretamente e o `pivot_wider()` para construir nossas tabelas de preditores. O `discrim` registra modelos como o Naive Bayes; `naivebayes` e `glmnet` realizam os cálculos.

```{r instalacao, eval=FALSE}
# Instalar apenas os pacotes ainda não instalados.
pacotes <- c("tidyverse", "tidytext", "stopwords", "tidymodels",
             "discrim", "naivebayes", "glmnet")
pacotes_faltantes <- setdiff(pacotes, rownames(installed.packages()))

if (length(pacotes_faltantes) > 0) {
  install.packages(pacotes_faltantes)
}
```

```{r pacotes}
library(tidyverse)
library(tidytext)
library(tidymodels)
library(discrim)
```

```{r parametros}
semente <- 20261126L
n_termos <- 1000L
proporcao_treino <- 0.75
lambda_lasso <- 0.01
```

O corpus original foi coletado a partir dos Dados Abertos da Câmara dos Deputados e contém 52.957 registros de discursos da 57ª legislatura, de fevereiro de 2023 a novembro de 2026. Vamos partir desse corpus completo, com todos os partidos, tipos de pronunciamento e transcrições, e construir o recorte de interesse dentro do próprio tutorial. Assim, a escolha da população que vamos analisar também faz parte do exercício.

Vamos começar abrindo o arquivo [`corpus_discursos.csv`](https://github.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/raw/refs/heads/main/data/corpus_discursos.csv) diretamente do nosso repositório. O CSV é separado por ponto e vírgula. Não precisamos baixar ou descompactar o arquivo manualmente:

```{r leitura}
corpus_original <- read_delim(
  "https://github.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/raw/refs/heads/main/data/corpus_discursos.csv",
  delim = ";",
  col_types = cols(.default = col_character())
)
```

E examinar rapidamente:

```{r inspecao}
glimpse(corpus_original)
corpus_original %>% count(partido_cadastro, sort = TRUE)
corpus_original %>% count(tipo_discurso, sort = TRUE)
corpus_original %>%
  select(id_documento, id_deputado, partido_cadastro, sumario) %>%
  slice_head(n = 3)
```

### 2.1. Seleção e preparação dos documentos

Usaremos apenas `BREVES COMUNICAÇÕES`, uma modalidade que permite comparar intervenções relativamente semelhantes em sua função parlamentar. O preditor é `sumario`, e não a transcrição integral, que pode trazer uma identificação direta do orador. O sumário é uma síntese editorial do conteúdo, não necessariamente as palavras exatas do deputado.

Primeiro, vamos escolher PT e PL e filtrar os discursos do tipo `BREVES COMUNICAÇÕES`. Depois, vamos retirar ausências e criar um `factor` com dois níveis. Colocamos PT como primeiro nível, apenas para padronizar o cálculo das métricas binárias. Isso não atribui nenhum valor substantivo à categoria.

```{r selecao}
# Selecionamos os dois partidos e a modalidade de discurso.
discursos_selecionados <- corpus_original %>%
  filter(partido_cadastro %in% c("PT", "PL"),
         tipo_discurso == "BREVES COMUNICAÇÕES")

# Retiramos os documentos sem sumario ou identificadores.
discursos_validos <- discursos_selecionados %>%
  filter(!is.na(id_documento), !is.na(id_deputado),
         !is.na(sumario), str_squish(sumario) != "") %>%
  mutate(partido = factor(partido_cadastro, levels = c("PT", "PL")),
         chave_sumario = str_to_lower(str_squish(sumario), locale = "pt"))
```

### 2.2. Um problema empírico: textos repetidos

Há sumários idênticos, inclusive para pronunciamentos de partidos distintos. Se o mesmo texto aparecer em treino e teste, estaremos em parte avaliando a memorização de documentos. Se textos idênticos tiverem rótulos diferentes, não há como distingui-los usando apenas o sumário.

Vamos comparar os sumários depois de uniformizar espaços e maiúsculas/minúsculas. Essa chave serve **apenas** para localizar repetições. O sumário original continua intacto.

```{r repeticoes}
repetidos <- discursos_validos %>%
  group_by(chave_sumario) %>%
  summarise(n = n(), partidos = n_distinct(partido), .groups = "drop") %>%
  filter(n > 1) %>%
  arrange(desc(n))

repetidos %>% slice_head(n = 5)
```

Neste exercício retiraremos todos os registros dos grupos repetidos (não apenas a segunda ocorrência). É uma regra de definição do corpus, aplicada antes da divisão, sem escolher com base no partido ou no desempenho. Ela não é uma solução universal para duplicatas e não identifica paráfrases ou textos quase iguais. A população avaliada passa a ser a de sumários não repetidos neste recorte.

```{r corpus}
discursos <- discursos_validos %>%
  group_by(chave_sumario) %>%
  filter(n() == 1L) %>%
  ungroup() %>%
  select(id_documento, id_deputado, nome_deputado, partido,
         uf_cadastro, data, sumario) %>%
  arrange(id_documento)

stopifnot(!anyDuplicated(discursos$id_documento))
stopifnot(all((discursos %>% distinct(id_deputado, partido) %>%
                count(id_deputado))$n == 1L))
```

```{r quantidades}
discursos %>%
  group_by(partido) %>%
  summarise(discursos = n(), deputados = n_distinct(id_deputado),
            .groups = "drop")

tibble(corpus_completo = nrow(corpus_original),
       selecao = nrow(discursos_selecionados),
       com_sumario = nrow(discursos_validos),
       sem_repeticoes = nrow(discursos))
```

Partindo dos 52.957 registros do corpus completo, vamos selecionar 7.377 discursos destes, obter 7.348 sumários válidos e 7.295 registros após a retirada de 53 ocorrências pertencentes a grupos repetidos: 3.878 do PT e 3.417 do PL. Há 72 deputados do PT e 106 do PL (inclui suplentes que tenham eventualmente assumido no meio do mandato ou por um tempo limitado).

### 2.3. O tamanho dos documentos

```{r comprimento}
discursos %>%
  mutate(n_palavras = tokenizers::count_words(sumario)) %>%
  ggplot(aes(partido, n_palavras, fill = partido)) +
  geom_boxplot(show.legend = FALSE, outlier.alpha = 0.15) +
  scale_y_log10() +
  labs(x = NULL, y = "Número de palavras (escala log)",
       title = "Comprimento dos sumários") +
  theme_minimal()
```

A extensão dos textos pode variar bastante. As frequências relativas e o TF-IDF que conhecemos das aulas anteriores permitem levar essa diferença em consideração. Neste gráfico usamos todo o corpus apenas para descrever a amostra, não para ajustar modelos ou selecionar termos.

## 3. Treino e teste

Como saber se um modelo aprendeu algo útil? Não basta acertar os rótulos dos documentos que utilizamos para treiná-lo. Um algoritmo pode memorizar características muito particulares de sua amostra. Queremos saber se a relação que aprendeu pode ser utilizada em documentos novos.

Por isso vamos separar os dados em dois conjuntos. O **treinamento** será utilizado para explorar o vocabulário, construir a representação dos textos e ajustar os modelos. O **teste** será utilizado para avaliar as previsões dos modelos já ajustados. Neste tutorial, vamos trabalhar apenas com esses dois conjuntos. As configurações dos modelos serão definidas antes de examinarmos seu desempenho no teste.

### Por que separar parlamentares e não apenas discursos?

Cada deputado escreveu ou proferiu diversos discursos. Se dividíssemos as linhas aleatoriamente, sumários de um mesmo parlamentar provavelmente estariam nos dois conjuntos. Uma alta acurácia poderia refletir a capacidade de reconhecer a agenda recorrente, nomes, regiões ou expressões ligadas a esses parlamentares. Queremos formular uma pergunta mais exigente: conseguimos prever o partido de discursos de parlamentares **que não estavam no conjunto do treinamento**?

A função `group_initial_split()` do pacote `rsample` mantém todas as observações de um mesmo grupo juntas. Utilizamos `id_deputado` como grupo e `partido` como estrato, para preservar a representação dos dois partidos. A proporção desejada para o treinamento é de 75%. Como cada parlamentar produziu uma quantidade diferente de discursos e os grupos precisam permanecer inteiros, as proporções efetivas de discursos e de parlamentares podem ser diferentes de 75%.

```{r divisao}
set.seed(semente)
divisao <- group_initial_split(
  discursos, group = id_deputado,
  strata = partido, prop = proporcao_treino
)

treino <- training(divisao)
teste <- testing(divisao)

```

```{r conferir-divisao}
bind_rows(treino %>% mutate(conjunto = "Treino"),
          teste %>% mutate(conjunto = "Teste")) %>%
  group_by(conjunto, partido) %>%
  summarise(deputados = n_distinct(id_deputado), discursos = n(),
            .groups = "drop")
```

Na execução deste tutorial, ficaram 5.550 discursos de 143 parlamentares no treinamento e 1.745 discursos de 35 parlamentares no teste. O que importa aqui é que nenhum parlamentar pertence aos dois conjuntos. A divisão reserva cerca de 24% dos discursos, e não exatamente 25% dos parlamentares. A partir de agora, exploraremos apenas os textos do conjunto de `treino`.

## 4. Quais palavras distinguem os partidos?

Antes de estimar um modelo, vamos observar os dados com as mesmas técnicas que utilizamos nas duas primeiras aulas. O que muda é que agora conhecemos uma categoria para cada discurso. Podemos comparar vocabulários de grupos definidos pelos rótulos.

### 4.1. Tokenização e contagem

Primeiro, tokenizamos os sumários do treinamento. Como no Tutorial 3, retiramos as stopwords em português e tokens puramente numéricos. Não vamos utilizar adicionar, por exemplo marcadores orais adicionais, como fizemos no caso dos discursos de Lula, pois os sumários são de outro gênero textual.

```{r tokens}
stop_pt <- get_stopwords(language = "pt", source = "snowball")

tokens <- treino %>%
  select(id_documento, partido, sumario) %>%
  unnest_tokens(palavra, sumario) %>%
  anti_join(stop_pt, by = c("palavra" = "word")) %>%
  filter(!str_detect(palavra, "^\\d+$"))

contagens_partido <- tokens %>% count(partido, palavra, name = "n")

contagens_partido %>%
  group_by(partido) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  print(n = Inf)
```

O número de ocorrências de uma palavra depende também da quantidade e do tamanho dos discursos. Para comparar os partidos, vamos dividir o número de ocorrências de cada termo pelo total de tokens de cada partido.

```{r frequencias-relativas, fig.height=6}
freq_partido <- contagens_partido %>%
  group_by(partido) %>%
  mutate(tf = n / sum(n)) %>%
  ungroup()

freq_partido %>%
  group_by(partido) %>%
  slice_max(tf, n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(palavra = reorder_within(palavra, tf, partido)) %>%
  ggplot(aes(palavra, tf, fill = partido)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ partido, scales = "free_y") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = NULL, y = "Frequência relativa",
       title = "Palavras mais frequentes no treinamento") +
  theme_minimal()
```

### 4.2. Colocando os dois vocabulários lado a lado

Para cada palavra, queremos uma linha com sua frequência no PT e no PL. Usaremos `pivot_wider()`; palavras que não aparecem em um partido recebem frequência zero.

Para tornar as diferenças mais legíveis, vamos olhar os termos com pelo menos 30 ocorrências nos dois grupos somados e que aparecem nos dois partidos. A escala logarítmica permite enxergar diferenças relativas entre frequências de ordens de grandeza distintas. O gráfico é descritivo: esse filtro não determina as variáveis do classificador.

```{r comparar-frequencias, fig.width=8, fig.height=7}
freq_lado_a_lado <- freq_partido %>%
  select(partido, palavra, n, tf) %>%
  pivot_wider(names_from = partido, values_from = c(n, tf),
              values_fill = 0) %>%
  filter(n_PT + n_PL >= 30, tf_PT > 0, tf_PL > 0) %>%
  mutate(diferenca = log2(tf_PT / tf_PL))

rotulos <- bind_rows(
  freq_lado_a_lado %>% slice_max(diferenca, n = 8, with_ties = FALSE),
  freq_lado_a_lado %>% slice_min(diferenca, n = 8, with_ties = FALSE)
)

freq_lado_a_lado %>%
  ggplot(aes(tf_PL, tf_PT)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2, colour = "grey50") +
  geom_point(alpha = 0.25) +
  geom_point(data = rotulos, colour = "steelblue", size = 2) +
  geom_text(data = rotulos, aes(label = palavra),
            size = 3, check_overlap = TRUE, vjust = -0.5) +
  scale_x_log10(labels = scales::label_percent()) +
  scale_y_log10(labels = scales::label_percent()) +
  coord_equal() +
  labs(x = "Frequência relativa no PL", y = "Frequência relativa no PT",
       title = "Vocabulários dos dois partidos",
       subtitle = "Acima da diagonal: frequência proporcionalmente maior no PT") +
  theme_minimal()
```

Na diagonal, a frequência relativa é igual nos dois partidos. Quanto mais distante da diagonal, mais diferente é a frequência relativa. A coluna `diferenca` é o logaritmo de base dois da razão entre as frequências. Se uma palavra tem `diferenca = 1`, sua frequência relativa no PT é o dobro daquela no PL; se tem `diferenca = -1`, é a metade.

```{r termos-distintivos}
rotulos %>%
  arrange(desc(diferenca)) %>%
  select(palavra, n_PT, n_PL, tf_PT, tf_PL, diferenca) %>%
  print(n = Inf)
```


### 4.3. Retomando o TF-IDF por discurso

Um termo pode ser característico de um discurso por ser pouco frequente nos demais. Vamos calcular TF-IDF considerando cada discurso como um documento, como fizemos no Tutorial 3. Depois, vamos agregar os pesos por partido. Aqui `bind_tf_idf()` considera os discursos que têm tokens após a limpeza. Vamos aproveitar os valores de IDF calculados neste conjunto de treinamento para representar os textos novos.

Para obter a média de TF-IDF de uma palavra em todos os discursos de um partido, somamos os pesos e dividimos pelo número de documentos desse partido. Os discursos em que o termo não aparece contribuem com zero. Isso é diferente de calcular a média apenas entre os documentos em que a palavra aparece.

```{r tfidf-exploratorio, fig.height=6}
tfidf_discursos <- tokens %>%
  count(id_documento, partido, palavra, name = "n") %>%
  bind_tf_idf(palavra, id_documento, n)

n_docs <- treino %>% count(partido, name = "n_docs")

tfidf_partido <- tfidf_discursos %>%
  group_by(partido, palavra) %>%
  summarise(soma_tfidf = sum(tf_idf), .groups = "drop") %>%
  left_join(n_docs, by = "partido") %>%
  mutate(media_tfidf = soma_tfidf / n_docs)

tfidf_partido %>%
  group_by(partido) %>%
  slice_max(media_tfidf, n = 12, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(palavra = reorder_within(palavra, media_tfidf, partido)) %>%
  ggplot(aes(palavra, media_tfidf, fill = partido)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  scale_x_reordered() +
  facet_wrap(~ partido, scales = "free_y") +
  labs(x = NULL, y = "TF-IDF médio por documento",
       title = "Termos com maior TF-IDF médio no treinamento") +
  theme_minimal()
```

Esse gráfico não fornece um classificador. Ele oferece pistas sobre diferenças de conteúdo. O TF-IDF médio, por si só, também não mede quanto um termo distingue as duas classes: o mesmo termo pode ter peso médio elevado nos dois partidos.

Podemos fazer uma pergunta nova: se determinadas palavras são mais características dos discursos de um partido, será que podemos aprender a utilizá-las para classificar novos discursos?

### 4.4. Construindo as matrizes de preditores

Já temos quase tudo de que precisamos para ajustar os modelos: o objeto `tfidf_discursos` contém os pesos TF-IDF das palavras de cada discurso do treinamento. Vamos selecionar as mil palavras mais frequentes nesse conjunto e transformar os dados em uma tabela com um discurso por linha e uma palavra por coluna, como fizemos ao construir uma matriz documento-termo nas primeiras aulas.

```{r vocabulario}
vocabulario <- tfidf_discursos %>%
  group_by(palavra) %>%
  summarise(total = sum(n), idf = first(idf), .groups = "drop") %>%
  filter(idf > 0) %>%
  arrange(desc(total), palavra) %>%
  slice_head(n = n_termos) %>%
  mutate(coluna = sprintf("tfidf_%04d", row_number()))

vocabulario %>% slice_head(n = 10)
```

Guardamos o IDF de cada termo e atribuímos um nome simples à coluna correspondente. Por exemplo, `tfidf_0001` representa a primeira palavra do vocabulário. Termos com IDF igual a zero não são úteis aqui, pois seus pesos seriam sempre zero.

**Matriz de treinamento.** Com `pivot_wider()` transformamos as palavras em colunas. Se uma palavra não aparece em determinado discurso, seu peso é zero. A junção com `treino` garante que todos os discursos permaneçam na tabela, inclusive aqueles que eventualmente não contêm nenhuma palavra do vocabulário.

```{r matriz-treino}
pesos_treino <- tfidf_discursos %>%
  inner_join(vocabulario %>% select(palavra, coluna), by = "palavra") %>%
  select(id_documento, coluna, tf_idf) %>%
  pivot_wider(names_from = coluna, values_from = tf_idf,
              values_fill = 0)

matriz_treino <- treino %>%
  select(id_documento, partido) %>%
  left_join(pesos_treino, by = "id_documento") %>%
  mutate(across(starts_with("tfidf_"), ~ replace_na(.x, 0))) %>%
  select(partido, all_of(vocabulario$coluna))

matriz_treino %>% select(1:6) %>% slice_head(n = 5)
dim(matriz_treino)
```

**Matriz de teste.** Para representar os discursos novos, utilizaremos o mesmo vocabulário e o mesmo IDF. Podemos calcular a frequência relativa (TF) de cada palavra dentro de um discurso novo, mas não devemos recalcular o IDF a partir dos textos do teste. Isso permitiria que informações do conjunto reservado influenciassem a representação. Chamamos esse problema de **vazamento de informação** (*data leakage*).

```{r matriz-teste}
pesos_teste <- teste %>%
  select(id_documento, sumario) %>%
  unnest_tokens(palavra, sumario) %>%
  anti_join(stop_pt, by = c("palavra" = "word")) %>%
  filter(!str_detect(palavra, "^\\d+$")) %>%
  count(id_documento, palavra, name = "n") %>%
  group_by(id_documento) %>%
  mutate(tf = n / sum(n)) %>%
  ungroup() %>%
  inner_join(vocabulario, by = "palavra") %>%
  mutate(tf_idf = tf * idf,
         coluna = factor(coluna, levels = vocabulario$coluna)) %>%
  select(id_documento, coluna, tf_idf) %>%
  pivot_wider(names_from = coluna, values_from = tf_idf,
              values_fill = 0, names_expand = TRUE)

matriz_teste <- teste %>%
  select(id_documento, partido) %>%
  left_join(pesos_teste, by = "id_documento") %>%
  mutate(across(starts_with("tfidf_"), ~ replace_na(.x, 0))) %>%
  select(partido, all_of(vocabulario$coluna))

matriz_teste %>% select(1:6) %>% slice_head(n = 5)
dim(matriz_teste)
```

Calculamos o TF antes de restringir os termos ao vocabulário, da mesma forma que no treinamento. `names_expand = TRUE` garante que todas as colunas do vocabulário sejam criadas, mesmo que um termo não apareça em nenhum discurso do teste. A ordem das colunas é a mesma nos dois conjuntos.

```{r conferir-matrizes}
identical(names(matriz_treino), names(matriz_teste))
```

Agora temos duas tabelas com a mesma estrutura: a variável resposta `partido` e até mil preditores numéricos. A partir daqui vamos trabalhar com elas como trabalharíamos com qualquer outro conjunto de dados quantitativos. Não incluímos o nome, a UF ou o identificador do deputado como preditores.

Quando fizermos as previsões, vamos fornecer ao modelo apenas os preditores. O partido conhecido dos discursos do teste será utilizado somente para conferir os acertos e os erros.

```{r preditores-teste}
x_teste <- matriz_teste %>% select(-partido)
```

## 5. Modelo 0 — Uma previsão que ignora os textos

Antes de tentar classificar os discursos usando as palavras, vamos construir a regra de previsão mais simples possível: **prever sempre a classe mais frequente no treinamento**. Chamamos essa regra de modelo nulo (*baseline*). Ela nos dá uma referência para saber se utilizar os textos realmente ajuda.

```{r maioria}
treino %>% count(partido) %>% mutate(proporcao = n / sum(n))
```

Na nossa execução, 52,6% dos discursos do treinamento têm o rótulo PT. Portanto, o modelo nulo vai prever PT para qualquer discurso novo, independentemente das palavras que ele contém.

### 5.1. Ajustando o modelo

Vamos aprender três operações que se repetirão nos próximos modelos:

1. **Especificar:** escolher o tipo de modelo.
2. **Ajustar:** usar `fit()` para que o modelo aprenda com os dados de treinamento.
3. **Prever:** usar `predict()` para obter uma resposta para cada documento novo.

O pacote `tidymodels` nos permite repetir essas operações com diferentes algoritmos, quase sem alterar o código. O `engine` indica qual implementação realizará os cálculos e `set_mode("classification")` indica que queremos prever uma categoria.

```{r nulo-ajuste}
modelo_nulo <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

ajuste_nulo <- fit(modelo_nulo, partido ~ ., data = matriz_treino)
```

A fórmula `partido ~ .` significa que queremos prever `partido` utilizando todas as outras colunas da tabela. No modelo nulo, essas colunas são ignoradas. Repare que utilizamos somente `matriz_treino` no ajuste.

### 5.2. Fazendo previsões

A função `predict()` pode devolver duas coisas: a classe prevista ou as probabilidades de cada classe. Para obter as probabilidades, basta acrescentar `type = "prob"`. Em ambos os casos, fornecemos `x_teste`, a tabela que contém os pesos TF-IDF dos novos discursos, **sem os seus rótulos**.

```{r nulo-previsoes}
classes_nulo <- predict(ajuste_nulo, new_data = x_teste)
probabilidades_nulo <- predict(ajuste_nulo, new_data = x_teste,
                               type = "prob")

previsoes_nulo <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_nulo, probabilidades_nulo)

previsoes_nulo %>% slice_head(n = 5)
```

A última operação apenas junta as previsões aos rótulos conhecidos do teste. Vamos usar essas informações para avaliar o modelo. A coluna `partido` contém o rótulo real; `.pred_class` contém a classe prevista; `.pred_PT` e `.pred_PL` contêm as probabilidades estimadas, que somam 1 em cada linha.

**Interpretando o resultado.** Todas as linhas recebem exatamente as mesmas probabilidades: 0,526 para PT e 0,474 para PL. A classe prevista é sempre PT. O modelo aprendeu apenas as proporções das classes no treinamento, e não alguma característica dos discursos. Assim, o discurso de ID 1000, cujo rótulo real é PL, é classificado incorretamente; o de ID 10049, cujo rótulo real é PT, é classificado corretamente. Nenhuma das duas previsões depende do conteúdo do sumário.

Dos 1.745 discursos reservados para o teste, 961 têm o rótulo PT. Como o modelo nulo prevê sempre essa categoria, acerta 961 discursos (55,1%) e erra os outros 784. Parece um resultado um pouco superior a 50%, mas o modelo nunca acerta a outra classe. Voltaremos a isso na seção 7.

## 6. Modelo 1 — Naive Bayes

### 6.1. A intuição do modelo

Na seção 4, observamos que as palavras não aparecem com as mesmas frequências nos dois grupos de discursos. O Naive Bayes utiliza diferenças desse tipo para classificar documentos novos.

Vamos imaginar um exemplo com apenas duas palavras, A e B. Os números da tabela são fictícios: representam a proporção de documentos de cada classe que contém a palavra.

| Palavra | P(palavra presente dado PT) | P(palavra presente dado PL) |
|---|---:|---:|
| A | 0,30 | 0,10 |
| B | 0,20 | 0,05 |

Imagine que um texto novo contém A e B. Supondo que as classes sejam igualmente frequentes (50% cada) e que a presença das duas palavras seja independente dentro de cada classe, calculamos dois escores:

- PT: 0,5 × 0,30 × 0,20 = 0,030.
- PL: 0,5 × 0,10 × 0,05 = 0,0025.

O primeiro escore é maior. Dividindo-o pela soma dos dois, obtemos 0,030/(0,030 + 0,0025), aproximadamente 92,3%. Nesse exemplo simplificado, a previsão seria PT.

Essa é a ideia do **teorema de Bayes**: combinar uma probabilidade inicial de cada classe com a evidência que observamos no documento. O termo *naive* (ingênuo) vem da hipótese de que, dentro de cada classe, as características das palavras são independentes. Sabemos que essa é uma simplificação: muitas palavras aparecem juntas e podem trazer informações semelhantes.

Há diferentes versões do Naive Bayes. Uma versão multinomial utiliza contagens de palavras; a versão que ajustaremos aqui usa distribuições dos pesos numéricos TF-IDF dentro de cada classe. O exemplo com a presença de A e B serve para explicar a lógica, não para reproduzir exatamente o cálculo dessa implementação.

### 6.2. Ajustando o modelo

Agora repetimos os mesmos passos do modelo nulo. Desta vez, o modelo utiliza os valores TF-IDF para aprender a distinguir as classes.

```{r nb-ajuste}
modelo_nb <- naive_Bayes() %>%
  set_engine("naivebayes") %>%
  set_mode("classification")

ajuste_nb <- fit(modelo_nb, partido ~ ., data = matriz_treino)
```

Compare os dois blocos de código: a fórmula e os dados de treinamento são os mesmos. O que mudamos foi o algoritmo utilizado para aprender a relação entre os textos e os rótulos.

### 6.3. Fazendo previsões

```{r nb-previsoes}
classes_nb <- predict(ajuste_nb, new_data = x_teste)
probabilidades_nb <- predict(ajuste_nb, new_data = x_teste,
                             type = "prob")

previsoes_nb <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_nb, probabilidades_nb)

previsoes_nb %>% slice_head(n = 8)
```

**Interpretando o resultado.** Nas oito primeiras linhas da nossa execução, o Naive Bayes prevê PT em todos os casos. Ao contrário do modelo nulo, suas probabilidades variam de um documento para outro. No entanto, são muito próximas de 1 para PT e de 0 para PL nesses exemplos. O modelo acerta quatro desses oito documentos e erra quatro.

Observe o discurso de ID 1000: o rótulo real é PL, mas o modelo prevê PT com probabilidade estimada praticamente igual a 100%. Um número tão alto pode sugerir que o modelo está muito seguro de sua previsão, mas isso não garante que ela esteja correta. Quando muitas palavras correlacionadas são tratadas como evidências independentes, como no Naive Bayes, o modelo pode produzir probabilidades excessivamente extremas. Para investigar sua calibração de maneira sistemática, precisaríamos de uma análise adicional.

Oito linhas são insuficientes para avaliar o modelo. Precisamos examinar os 1.745 discursos do teste, verificando tanto os acertos quanto os tipos de erro.

## 7. Como avaliar um classificador?

### 7.1. Examinando a matriz de confusão

A **matriz de confusão** é uma tabela que cruza as classes previstas com as classes verdadeiras. Vamos construí-la para o Naive Bayes e utilizar o próprio resultado do exercício para aprender as métricas de avaliação.

```{r nb-matriz}
matriz_confusao_nb <- conf_mat(previsoes_nb, truth = partido,
                               estimate = .pred_class)
matriz_confusao_nb
```

```{r nb-heatmap}
matriz_confusao_nb %>% autoplot(type = "heatmap")
```

Na saída de `conf_mat()`, as linhas representam as previsões (*Prediction*) e as colunas representam os rótulos verdadeiros (*Truth*). Na execução deste tutorial, obtivemos a seguinte tabela:

| Classe prevista | Real PT | Real PL |
|---|---:|---:|
| PT | 839 | 632 |
| PL | 122 | 152 |

Os números da diagonal são os acertos (839 e 152), e os outros dois são os erros (632 e 122). Veja que os erros não se distribuem igualmente entre os dois tipos de documento.

- **Acurácia:** proporção de acertos entre todos os documentos. O Naive Bayes acerta (839 + 152)/1.745 = 56,8%.
- **Sensibilidade:** proporção de acertos entre os documentos cujo rótulo real é PT: 839/961 = 87,3%.
- **Especificidade:** proporção de acertos entre os documentos cujo rótulo real é PL: 152/784 = 19,4%.
- **Precisão:** entre os documentos previstos como PT, quantos realmente têm esse rótulo? 839/1.471 = 57,0%.
- **Acurácia balanceada:** média da sensibilidade e da especificidade, ou seja, aproximadamente (87,3% + 19,4%)/2 = 53,3%, calculada a partir das proporções antes do arredondamento.

Utilizamos PT como a classe positiva por convenção estatística. Isso determina o significado de sensibilidade e precisão, sem atribuir nenhum valor à categoria.

Em vez de calcular as medidas à mão, podemos utilizar `yardstick`, que faz parte do `tidymodels`. `metric_set()` permite reunir as funções que calculam as métricas que acabamos de conhecer. O argumento `truth` indica a coluna com o rótulo real e `estimate` indica a coluna com a classe prevista.

```{r metricas-nb}
metricas_classes <- metric_set(accuracy, bal_accuracy, sens, spec, precision)

metricas_classes(previsoes_nb, truth = partido, estimate = .pred_class)
```

**O que aprendemos com essa matriz?** O Naive Bayes classifica 1.471 dos 1.745 discursos como PT (84,3% das previsões), apesar de apenas 55,1% dos documentos terem esse rótulo. O resultado é uma taxa de acerto de 87,3% em uma classe e de somente 19,4% na outra. A acurácia geral de 56,8% esconde essa assimetria.

Por isso escolhemos a **acurácia balanceada** como nossa métrica principal. Ela atribui o mesmo peso à taxa de acertos de cada classe. Um modelo que acerta todos os documentos de uma classe e nenhum da outra obtém 50% de acurácia balanceada, mesmo que acerte mais da metade dos documentos do teste.

### 7.2. A curva ROC

Até aqui avaliamos a classe prevista, que depende do limiar utilizado para transformar uma probabilidade em uma decisão. Normalmente, em uma classificação binária, escolhemos uma classe quando sua probabilidade ultrapassa 0,5. Mas podemos perguntar algo um pouco diferente: o modelo atribui, em geral, probabilidades maiores de PT aos documentos cujo rótulo é realmente PT?

A curva ROC ajuda a responder a essa pergunta. Para diferentes limiares de decisão, ela relaciona a proporção de verdadeiros positivos à proporção de falsos positivos (1 menos a especificidade). Quanto mais a curva se aproxima do canto superior esquerdo do gráfico, melhor é a capacidade de discriminação. A área sob a curva, chamada **ROC AUC**, resume essa informação em um número: 0,5 é a referência de um classificador sem capacidade de discriminação e 1 representa uma ordenação perfeita.

```{r nb-roc}
previsoes_nb %>%
  roc_curve(truth = partido, .pred_PT) %>%
  autoplot() +
  labs(title = "Naive Bayes: curva ROC no teste")
```

Vamos reunir agora as três métricas que utilizaremos na comparação dos modelos. A AUC usa as probabilidades (`.pred_PT`), enquanto a acurácia e a acurácia balanceada usam as classes previstas (`.pred_class`).

```{r nb-nulo-comparacao}
metricas <- metric_set(bal_accuracy, accuracy, roc_auc)

bind_rows(
  previsoes_nulo %>% mutate(modelo = "Nulo"),
  previsoes_nb %>% mutate(modelo = "Naive Bayes")
) %>%
  group_by(modelo) %>%
  metricas(truth = partido, estimate = .pred_class, .pred_PT)
```

O Naive Bayes alcançou uma acurácia balanceada de 53,3%, apenas 3,3 pontos percentuais acima do modelo nulo (50%). Em número de acertos, são 991 contra 961: apenas 30 acertos adicionais. A AUC, porém, é de 0,653. Isso indica alguma informação útil para ordenar os documentos pelas probabilidades estimadas, embora a classificação feita com o limiar padrão não aproveite bem essa informação. Uma AUC superior a 0,5 não significa que as probabilidades sejam bem calibradas.

**Exercício:** explique por que o Naive Bayes pode ter uma AUC de 0,653 e uma acurácia balanceada de apenas 53,3%. Por que seria inadequado avaliar esse modelo apenas com a acurácia geral? Não vamos alterar o limiar de classificação a partir dos resultados do teste; para isso, precisaríamos de dados de validação.

## 8. Modelo 2 — Regressão logística regularizada

### 8.1. Como as palavras entram em uma regressão logística?

A regressão logística é outra maneira de resolver o mesmo problema. Ela combina os valores TF-IDF das palavras em uma soma ponderada, atribuindo um coeficiente a cada palavra. Imagine que tivéssemos apenas três preditores, `x1`, `x2` e `x3`:

$$z = b_0 + b_1 x_1 + b_2 x_2 + b_3 x_3$$

Os coeficientes são aprendidos no treinamento. A função logística transforma o escore `z` em uma probabilidade entre zero e um:

$$P(Y=\text{classe modelada}\mid X) = \frac{1}{1+\exp(-z)}$$

Se o escore é zero, a probabilidade é 0,5. Se é positivo, a probabilidade é maior que 0,5; se é negativo, é menor. A previsão resulta dessa probabilidade. Um coeficiente positivo aumenta o escore da classe modelada quando o TF-IDF da palavra aumenta, mantendo os demais valores constantes. Na seção 10 veremos qual classe corresponde ao escore na implementação que utilizamos.

### 8.2. Por que utilizar o LASSO?

Nosso problema tem mil preditores. A regressão poderia ajustar muitos coeficientes para reproduzir detalhes particulares dos discursos do treinamento e acabar errando nos documentos novos. Chamamos esse problema de **sobreajuste** (*overfitting*).

O LASSO é uma forma de regularização: ele penaliza a soma dos valores absolutos dos coeficientes. Com essa penalização, o modelo pode reduzir alguns coeficientes a zero, deixando de utilizar determinadas palavras na previsão. A intensidade da penalização é chamada de `lambda` (ou `penalty` no `tidymodels`). Uma penalização mais forte tende a produzir um modelo com menos coeficientes não nulos.

A diferença entre parâmetros e hiperparâmetros é simples: os coeficientes são estimados pelo modelo; a intensidade da penalização é uma configuração que definimos para o ajuste. Neste exercício utilizaremos o valor fixo de 0,01, escolhido com os dados de treinamento na versão anterior do tutorial, sem consultar o teste. Não vamos procurar outro valor agora.

### 8.3. Ajustando o modelo

```{r lasso-ajuste}
modelo_lasso <- logistic_reg(penalty = lambda_lasso, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

ajuste_lasso <- fit(modelo_lasso, partido ~ ., data = matriz_treino)
```

`mixture = 1` indica que queremos a penalização LASSO; `penalty` indica sua intensidade. Apesar de o algoritmo ser diferente do Naive Bayes, a forma de ajustar o modelo e a tabela de treinamento continuam as mesmas.

### 8.4. Fazendo previsões

```{r lasso-previsoes}
classes_lasso <- predict(ajuste_lasso, new_data = x_teste)
probabilidades_lasso <- predict(ajuste_lasso, new_data = x_teste,
                                type = "prob")

previsoes_lasso <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_lasso, probabilidades_lasso)

previsoes_lasso %>% slice_head(n = 8)
```

**Interpretando as primeiras previsões.** O LASSO acerta sete dos oito primeiros discursos apresentados, contra quatro acertos do Naive Bayes. O discurso de ID 10045, por exemplo, tem rótulo real PL e recebe do LASSO uma probabilidade de 0,725 para PL. O Naive Bayes havia classificado esse mesmo documento como PT.

Ainda assim, o LASSO também comete erros. O discurso 1000 continua classificado como PT, apesar de o rótulo real ser PL, mas a probabilidade estimada para PT é agora de 0,604 e não praticamente 1. Nos exemplos apresentados, as probabilidades da regressão logística são menos extremas. Isso, por si só, não demonstra melhor calibração; apenas mostra como os modelos expressam suas previsões de maneiras diferentes.

### 8.5. Avaliando o modelo

Vamos repetir as mesmas operações que utilizamos com o Naive Bayes.

```{r lasso-matriz}
matriz_confusao_lasso <- conf_mat(previsoes_lasso, truth = partido,
                                  estimate = .pred_class)
matriz_confusao_lasso
```

```{r lasso-heatmap}
matriz_confusao_lasso %>% autoplot(type = "heatmap")
```

```{r lasso-metricas}
metricas_classes(previsoes_lasso, truth = partido, estimate = .pred_class)
```

A matriz de confusão agora apresenta 682 acertos e 279 erros entre os discursos com rótulo PT, e 619 acertos e 165 erros entre os discursos com rótulo PL. No total, são 1.301 acertos e 444 erros, resultando em uma acurácia de 74,6%.

Mais importante do que o aumento da acurácia geral é a mudança na distribuição dos erros. A sensibilidade é de 71,0% e a especificidade é de 79,0%, produzindo uma acurácia balanceada de 75,0%. Com o Naive Bayes, essas taxas eram de 87,3% e 19,4%, respectivamente. O LASSO deixa de acertar alguns documentos de uma classe, mas ganha muitos acertos na outra. Sua precisão para PT é de 80,5%: entre os 847 discursos previstos como PT, 682 realmente têm esse rótulo.

Portanto, neste teste, o LASSO consegue distinguir os dois tipos de documento de maneira mais equilibrada. Não precisamos alterar a representação dos textos para observar essa diferença: os dois modelos utilizaram os mesmos mil preditores TF-IDF.

## 9. Comparando os modelos

Agora temos as previsões dos três modelos para exatamente os mesmos 1.745 discursos de parlamentares que não participaram do treinamento. Vamos reunir as previsões em uma única tabela e calcular as métricas por modelo.

```{r comparacao}
previsoes <- bind_rows(
  previsoes_nulo %>% mutate(modelo = "Nulo"),
  previsoes_nb %>% mutate(modelo = "Naive Bayes"),
  previsoes_lasso %>% mutate(modelo = "LASSO TF-IDF")
)

comparacao <- previsoes %>%
  group_by(modelo) %>%
  metricas(truth = partido, estimate = .pred_class, .pred_PT) %>%
  ungroup()

comparacao %>%
  select(modelo, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(desc(bal_accuracy))
```

```{r comparacao-grafico}
comparacao %>%
  filter(.metric == "bal_accuracy") %>%
  ggplot(aes(reorder(modelo, .estimate), .estimate)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "grey50") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  labs(x = NULL, y = "Acurácia balanceada",
       title = "Comparando os classificadores no conjunto de teste") +
  theme_minimal()
```

Na execução com o corpus e a semente indicados, a acurácia balanceada passou de 50,0% no modelo nulo para 53,3% no Naive Bayes e 75,0% no LASSO. O LASSO superou o modelo nulo em 25 pontos percentuais nessa métrica, e o Naive Bayes em 21,7 pontos. Em números absolutos, são 310 acertos a mais que o Naive Bayes. A representação TF-IDF contém informação preditiva que o modelo nulo não utiliza, e os algoritmos diferem na maneira como aproveitam essa informação.

```{r comparacao-roc}
previsoes %>%
  group_by(modelo) %>%
  roc_curve(truth = partido, .pred_PT) %>%
  autoplot() +
  labs(title = "Curvas ROC dos classificadores no teste", colour = "Modelo")
```

A AUC do LASSO (0,822) também foi superior à do Naive Bayes (0,653) e à do modelo nulo (0,500). Uma maneira de interpretar 0,822 é imaginar que sorteamos um discurso de cada classe: em aproximadamente 82,2% dos pares, contando empates pela metade, o escore do LASSO ordenaria corretamente os dois documentos em relação ao rótulo PT. **AUC de 0,822 não significa 82,2% de classificações corretas**: a acurácia observada foi 74,6%.

Podemos resumir o exercício em três resultados: o modelo nulo não utiliza nenhuma informação textual; o Naive Bayes aproveita parte dessa informação, mas produz previsões bastante assimétricas; a regressão regularizada obtém o melhor desempenho observado entre as três configurações avaliadas.

O resultado não demonstra que o LASSO seja sempre superior ao Naive Bayes. Refere-se a este corpus, esta representação, estas configurações e esta divisão dos dados. Também não significa que a identificação de rótulos partidários seja equivalente à identificação de ideologia. O modelo pode utilizar menções a pessoas, instituições, lugares, siglas ou padrões da redação dos sumários. Além disso, as métricas atribuem o mesmo peso a cada discurso; parlamentares com muitos discursos têm maior participação no resultado. Não estimamos a variabilidade entre diferentes divisões.

### Uma alternativa: treino, validação e teste

Neste tutorial, fixamos as configurações antes do teste e comparamos os resultados de maneira descritiva. Caso quiséssemos escolher o algoritmo ou experimentar hiperparâmetros, poderíamos criar um terceiro conjunto de dados, a **validação**. Ajustaríamos os modelos no treino, escolheríamos as configurações na validação e só então avaliaríamos o modelo selecionado no teste. A separação continuaria sendo por parlamentar. Não faremos isso aqui. Se utilizarmos os resultados do teste atual para escolher um modelo, precisaremos de novos dados independentes para avaliar essa escolha sem otimismo de seleção.

## 10. Interpretando os coeficientes do LASSO

Além de fazer previsões, podemos examinar os coeficientes aprendidos pela regressão logística. O LASSO utiliza mil palavras como possíveis preditores, mas pode deixar de utilizar muitas delas. Vamos descobrir quantas permaneceram com coeficientes diferentes de zero.

```{r coeficientes-lasso}
motor_lasso <- extract_fit_engine(ajuste_lasso)
classes_motor <- motor_lasso$classnames

mat_coef <- as.matrix(coef(motor_lasso, s = lambda_lasso))

coeficientes <- tibble(
  termo = rownames(mat_coef),
  coeficiente = as.numeric(mat_coef[, 1])
) %>%
  filter(termo != "(Intercept)") %>%
  left_join(vocabulario %>% select(coluna, palavra),
            by = c("termo" = "coluna"))

coeficientes %>% filter(coeficiente != 0) %>%
  summarise(n_palavras = n())
```

Esse bloco apenas recupera os coeficientes do modelo já ajustado e associa cada coluna (`tfidf_0001`, `tfidf_0002` etc.) à palavra correspondente no vocabulário. Retiramos o intercepto porque ele não corresponde a uma palavra.

O resultado foi de **287 palavras com coeficientes não nulos**, de um total de mil. Isso significa que 713 coeficientes foram reduzidos a zero pela regularização: 71,3% dos preditores não contribuem diretamente para a previsão nesse ajuste. É um exemplo concreto de seleção de variáveis. Não significa que as outras 713 palavras sejam irrelevantes em qualquer análise; algumas podem trazer informações que também estão presentes em outras palavras ou que não foram úteis para esta classificação.

Vamos visualizar os dez maiores coeficientes positivos e os dez mais negativos. No ajuste que obtivemos, `classes_motor` contém PT e PL, nessa ordem. O `glmnet` binomial modela o escore correspondente à segunda classe: os coeficientes positivos aumentam o escore de PL e os negativos o reduzem, aumentando a probabilidade de PT, mantendo os demais preditores constantes.

```{r coeficientes-grafico, fig.height=7}
classes_motor

bind_rows(
  coeficientes %>% filter(coeficiente > 0) %>%
    slice_max(coeficiente, n = 10, with_ties = FALSE),
  coeficientes %>% filter(coeficiente < 0) %>%
    slice_min(coeficiente, n = 10, with_ties = FALSE)
) %>%
  ggplot(aes(reorder(palavra, coeficiente), coeficiente,
             fill = coeficiente > 0)) +
  geom_col(show.legend = FALSE) + coord_flip() +
  labs(x = NULL, y = "Coeficiente",
       title = "Palavras selecionadas pelo LASSO",
       subtitle = paste("Coeficientes positivos aumentam o escore de", classes_motor[2],
                        "e negativos, o de", classes_motor[1])) +
  theme_minimal()
```

Volte ao gráfico de frequências relativas da seção 4.2. Naquela análise, comparávamos cada palavra isoladamente. Agora o modelo combina centenas de palavras: um coeficiente depende também da informação que as outras variáveis fornecem e da regularização. Por isso, não precisamos encontrar as mesmas palavras nas duas análises. A magnitude de um coeficiente também deve ser entendida em relação à escala do seu preditor: um coeficiente grande não implica, por si só, que a palavra seja a mais importante em todos os documentos.

**Exercício:** compare as palavras destacadas no gráfico com os termos mais distintivos da análise exploratória. Que tipos de pistas aparecem? Leia alguns sumários do treinamento para entender o uso dessas palavras. Lembre-se de que os coeficientes representam associações preditivas, e não efeitos causais ou medidas de ideologia. As palavras foram selecionadas para uma tarefa específica de previsão de rótulos, não para explicar o comportamento dos parlamentares.

## 11. O que aprendemos?

Neste tutorial passamos de frequências de palavras e matrizes numéricas para um problema de aprendizado supervisionado. Aprendemos a especificar um modelo, ajustá-lo utilizando dados rotulados e fazer previsões sobre documentos novos.

O contraste entre os resultados é especialmente útil. Com a mesma representação textual, o Naive Bayes e o LASSO produziram padrões de erro diferentes. O modelo nulo nos deu uma referência, a matriz de confusão permitiu entender onde cada classificador acertou ou errou, e a regularização mostrou que podemos utilizar menos palavras efetivas do que o vocabulário inicial. A comparação foi feita em discursos de parlamentares reservados, sem mudar as configurações com base no teste.

No [Tutorial 7](07_comparando_modelos_representacoes.Rmd) voltaremos à questão da escolha dos algoritmos e das representações textuais. Vamos utilizar a mesma divisão dos dados e comparar configurações fixas de outros modelos, sem escolher hiperparâmetros a partir do teste já examinado aqui.

**Leituras:** Hvitfeldt e Silge, [visão geral do aprendizado supervisionado](https://smltar.com/mloverview) e [capítulo 7: classificação](https://smltar.com/mlclassification); documentação de [`rsample`](https://rsample.tidymodels.org/reference/group_initial_split.html), [`tidytext`](https://juliasilge.github.io/tidytext/) e [`parsnip`](https://parsnip.tidymodels.org/).
