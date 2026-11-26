---
output:
  github_document: default
  html_document:
    toc: true
    toc_depth: 2
lang: pt-BR
encoding: UTF-8
---

# Tutorial 7 — Comparando modelos e representações textuais

**Aula 3 · Aprendizado supervisionado de textos**

No [Tutorial 6](06_classificacao_supervisionada_discursos.Rmd), vimos como transformar os sumários dos discursos da Câmara dos Deputados em uma tabela numérica e como usar essa tabela para classificar textos. Comparamos um modelo que ignora os textos (o modelo nulo), o Naive Bayes e uma regressão logística com regularização LASSO. Os dois classificadores que utilizam os textos receberam exatamente a mesma informação: os pesos TF-IDF de mil palavras.

Na execução anterior, a regressão logística acertou 74,6% dos discursos reservados para o teste, contra 56,8% do Naive Bayes e 55,1% do modelo nulo. A acurácia balanceada foi, respectivamente, 75,0%, 53,3% e 50,0%. Isso nos mostra que mudar o algoritmo, mesmo sem mudar a representação do texto, pode alterar bastante as previsões.

Vamos continuar o exercício de duas maneiras. Primeiro, vamos experimentar mais dois classificadores usando a mesma representação: a máquina de vetores de suporte (SVM) e a Random Forest. Depois, vamos mudar a representação dos discursos, recuperando a modelagem de tópicos (LDA) da segunda aula. Em vez de mil pesos TF-IDF, entregaremos seis proporções de tópicos à regressão logística.

## O que vamos aprender

- Ajustar uma SVM linear e uma Random Forest com a mesma interface que aprendemos no Tutorial 6.
- Comparar diferentes algoritmos mantendo a representação dos textos constante.
- Usar um modelo de tópicos para construir preditores de um modelo supervisionado.
- Avaliar o que muda nas previsões quando mudamos a representação dos documentos.
- Voltar aos sumários originais para entender os erros dos classificadores.

```{r setup, include=FALSE}
knitr::opts_chunk$set(
  echo = TRUE, message = FALSE,
  fig.width = 8, fig.height = 5, out.width = "100%"
)
```

## 1. Retomando o exercício anterior

Para facilitar, o código inicial abaixo reproduz a preparação dos dados do Tutorial 6. Assim, podemos executar este R Markdown mesmo em uma nova sessão do R, sem depender de objetos que tenham ficado na memória. Não vamos repetir a análise exploratória do vocabulário, que já fizemos.

### 1.1. Pacotes e corpus

Usaremos os mesmos pacotes do tutorial anterior, acrescentando `kernlab` (SVM), `ranger` (Random Forest), `topicmodels` (LDA) e `slam` (matrizes de contagens). `topicmodels` já foi utilizado no Tutorial 3.

```{r instalacao, eval=FALSE}
# Execute este bloco se precisar instalar algum pacote.
pacotes <- c("tidyverse", "tidytext", "stopwords", "tidymodels",
             "discrim", "naivebayes", "glmnet", "kernlab",
             "ranger", "topicmodels", "slam")
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
k_topicos <- 6L
custo_svm <- 1
n_arvores <- 200L
mtry_rf <- 32L
min_n_rf <- 10L
```

Partimos do mesmo CSV completo, disponibilizado na pasta `data/` do repositório. O arquivo contém todos os partidos e modalidades de discurso. A seleção continua sendo feita dentro do tutorial: apenas PT e PL, na modalidade `BREVES COMUNICAÇÕES`, com sumários válidos e sem grupos de textos idênticos.

```{r leitura}
corpus_original <- read_delim(
  "https://github.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/raw/refs/heads/main/data/corpus_discursos.csv",
  delim = ";",
  col_types = cols(.default = col_character())
)
```

```{r corpus}
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

### 1.2. Os mesmos conjuntos de treino e teste

Vamos manter a semente, a ordem dos documentos e a divisão por parlamentar. Assim, os resultados dos modelos podem ser comparados nos mesmos documentos. A proporção efetiva de discursos em cada conjunto pode não ser exatamente 75% e 25%, porque um parlamentar inteiro precisa permanecer em um único conjunto.

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
stopifnot(length(intersect(treino$id_deputado, teste$id_deputado)) == 0L)

bind_rows(treino %>% mutate(conjunto = "Treino"),
          teste %>% mutate(conjunto = "Teste")) %>%
  group_by(conjunto, partido) %>%
  summarise(deputados = n_distinct(id_deputado), discursos = n(),
            .groups = "drop")
```

Com a mesma base e a mesma divisão, esperamos obter 5.550 discursos no treinamento e 1.745 no teste. O teste reúne discursos de 35 parlamentares que não estão no treinamento.

**Um cuidado com a avaliação.** O conjunto de teste já foi examinado no Tutorial 6. Vamos utilizá-lo novamente para comparar, de maneira descritiva, algumas configurações fixas de modelos. Não vamos procurar valores de hiperparâmetros que maximizem o desempenho nesse conjunto. Se quiséssemos escolher o melhor classificador e obter uma avaliação independente dessa escolha, separaríamos uma amostra de validação e reservaríamos um novo teste.

### 1.3. Retomando a representação TF-IDF

Vamos repetir a tokenização, retirar as stopwords e os tokens exclusivamente numéricos, e calcular os pesos TF-IDF do treinamento. Selecionamos as mesmas mil palavras. Guardamos o vocabulário e o IDF, que serão usados também nos novos textos.

```{r tokens}
stop_pt <- get_stopwords(language = "pt", source = "snowball")

tokens <- treino %>%
  select(id_documento, partido, sumario) %>%
  unnest_tokens(palavra, sumario) %>%
  anti_join(stop_pt, by = c("palavra" = "word")) %>%
  filter(!str_detect(palavra, "^\\d+$"))

tfidf_discursos <- tokens %>%
  count(id_documento, partido, palavra, name = "n") %>%
  bind_tf_idf(palavra, id_documento, n)
```

```{r vocabulario}
vocabulario <- tfidf_discursos %>%
  group_by(palavra) %>%
  summarise(total = sum(n), idf = first(idf), .groups = "drop") %>%
  filter(idf > 0) %>%
  arrange(desc(total), palavra) %>%
  slice_head(n = n_termos) %>%
  mutate(coluna = sprintf("tfidf_%04d", row_number()))
```

Com `pivot_wider()` transformamos os pesos em uma tabela com uma linha por discurso e uma coluna por palavra. A variável resposta é `partido`. Repare que os identificadores não entram como preditores.

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
```

A tabela de teste é construída com o mesmo vocabulário e IDF do treinamento. Aqui apenas guardamos também as contagens dos textos novos em `contagens_teste`, porque vamos reaproveitá-las no LDA mais adiante. O cálculo dos pesos é o mesmo do Tutorial 6.

```{r matriz-teste}
contagens_teste <- teste %>%
  select(id_documento, sumario) %>%
  unnest_tokens(palavra, sumario) %>%
  anti_join(stop_pt, by = c("palavra" = "word")) %>%
  filter(!str_detect(palavra, "^\\d+$")) %>%
  count(id_documento, palavra, name = "n")

pesos_teste <- contagens_teste %>%
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
```

```{r conferir-matrizes}
stopifnot(identical(names(matriz_treino), names(matriz_teste)),
          nrow(matriz_treino) == nrow(treino),
          nrow(matriz_teste) == nrow(teste))

dim(matriz_treino)
dim(matriz_teste)
x_teste <- matriz_teste %>% select(-partido)
```

### 1.4. Reproduzindo as previsões dos primeiros três modelos

Como aprendemos no Tutorial 6, há três passos: especificar o modelo, ajustá-lo com `fit()` e prever com `predict()`. Vamos repetir os ajustes para poder comparar as previsões de cada discurso, sem precisar ler resultados de um outro arquivo.

```{r nulo-ajuste}
modelo_nulo <- null_model() %>%
  set_engine("parsnip") %>%
  set_mode("classification")

ajuste_nulo <- fit(modelo_nulo, partido ~ ., data = matriz_treino)
```

```{r nulo-previsoes}
classes_nulo <- predict(ajuste_nulo, new_data = x_teste)
probabilidades_nulo <- predict(ajuste_nulo, new_data = x_teste,
                               type = "prob")

previsoes_nulo <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_nulo, probabilidades_nulo)
```

```{r nb-ajuste}
modelo_nb <- naive_Bayes() %>%
  set_engine("naivebayes") %>%
  set_mode("classification")

ajuste_nb <- fit(modelo_nb, partido ~ ., data = matriz_treino)
```

```{r nb-previsoes}
classes_nb <- predict(ajuste_nb, new_data = x_teste)
probabilidades_nb <- predict(ajuste_nb, new_data = x_teste,
                             type = "prob")

previsoes_nb <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_nb, probabilidades_nb)
```

```{r lasso-ajuste}
modelo_lasso <- logistic_reg(penalty = lambda_lasso, mixture = 1) %>%
  set_engine("glmnet") %>%
  set_mode("classification")

ajuste_lasso <- fit(modelo_lasso, partido ~ ., data = matriz_treino)
```

```{r lasso-previsoes}
classes_lasso <- predict(ajuste_lasso, new_data = x_teste)
probabilidades_lasso <- predict(ajuste_lasso, new_data = x_teste,
                                type = "prob")

previsoes_lasso <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_lasso, probabilidades_lasso)
```

Definimos as mesmas métricas do tutorial anterior. `accuracy` é a proporção de acertos; `bal_accuracy` é a média das taxas de acerto das duas classes; `roc_auc` mede a capacidade de ordenar os documentos pelas probabilidades estimadas. Continuaremos utilizando a acurácia balanceada como principal medida de comparação.

```{r metricas}
metricas <- metric_set(bal_accuracy, accuracy, roc_auc)
metricas_classes <- metric_set(accuracy, bal_accuracy, sens, spec, precision)

previsoes_tutorial6 <- bind_rows(
  previsoes_nulo %>% mutate(modelo = "Nulo"),
  previsoes_nb %>% mutate(modelo = "Naive Bayes"),
  previsoes_lasso %>% mutate(modelo = "LASSO TF-IDF")
)

previsoes_tutorial6 %>%
  group_by(modelo) %>%
  metricas(truth = partido, estimate = .pred_class, .pred_PT)
```

## 2. Modelo 3 — Máquina de vetores de suporte (SVM)

### 2.1. A intuição do modelo

Na primeira aula, aprendemos que cada documento pode ser representado como um vetor de números. Imagine que utilizamos apenas duas palavras e, portanto, podemos representar cada discurso como um ponto em um gráfico de duas dimensões. Alguns pontos têm o rótulo A, outros o rótulo B. Poderíamos tentar desenhar uma linha que separasse os dois grupos.

A SVM (*Support Vector Machine*) parte dessa ideia. Mas não basta traçar qualquer linha: procuramos uma fronteira com uma margem de separação tão ampla quanto possível entre os grupos, admitindo alguns erros quando os pontos se misturam. Os pontos que influenciam a localização dessa fronteira são chamados de vetores de suporte.

```{r svm-ilustracao, fig.height=3.5}
# Exemplo fictício com apenas dois preditores.
exemplo <- tibble(
  x1 = c(1, 2, 2, 3, 1.5, 2.5, 5, 6, 6, 7, 5.5, 6.5),
  x2 = c(1, 3, 1.5, 2.5, 2, 1, 3, 1.5, 2.5, 2, 1, 3),
  grupo = rep(c("A", "B"), each = 6)
)

ggplot(exemplo, aes(x1, x2, colour = grupo)) +
  geom_point(size = 3) +
  geom_vline(xintercept = 4) +
  geom_vline(xintercept = c(3, 5), linetype = 2) +
  labs(x = "Preditor 1", y = "Preditor 2", colour = "Classe",
       title = "Uma fronteira e suas margens de separação") +
  theme_minimal()
```

O gráfico é apenas uma ilustração: não ajustamos uma SVM para construí-lo. No nosso problema existem mil dimensões em vez de duas. Utilizaremos uma **SVM linear**, ou seja, uma fronteira linear nesse espaço. Assim como na regressão logística, as palavras são combinadas em um escore, mas o critério utilizado para aprender a fronteira é diferente.

O hiperparâmetro `cost` controla quanto penalizamos os erros e as violações da margem durante o ajuste. Fixamos `cost = 1` neste exercício. Não vamos testar diferentes valores para escolher aquele que funciona melhor no teste.

### 2.2. Ajustando o modelo

Não precisamos modificar a tabela de entrada. Utilizaremos os pesos TF-IDF que já tínhamos. A implementação `kernlab` padroniza os preditores internamente durante o ajuste e reaplica essa transformação aos novos documentos.

```{r svm-ajuste}
set.seed(semente + 6L)

modelo_svm <- svm_linear(cost = custo_svm) %>%
  set_engine("kernlab") %>%
  set_mode("classification")

ajuste_svm <- fit(modelo_svm, partido ~ ., data = matriz_treino)
```

### 2.3. Fazendo previsões

```{r svm-previsoes}
classes_svm <- predict(ajuste_svm, new_data = x_teste)
probabilidades_svm <- predict(ajuste_svm, new_data = x_teste,
                              type = "prob")

previsoes_svm <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_svm, probabilidades_svm)

previsoes_svm %>% slice_head(n = 8)
```

A SVM não produz probabilidades diretamente da mesma maneira que a regressão logística. O motor `kernlab` acrescenta uma etapa para converter seus escores em probabilidades. Essa etapa pode variar um pouco entre execuções mesmo com a semente fixada. As classes previstas são as devolvidas pelo próprio modelo; não vamos recalculá-las a partir de um limiar escolhido por nós.

### 2.4. Avaliando o modelo

```{r svm-metricas}
conf_mat(previsoes_svm, truth = partido, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

metricas_classes(previsoes_svm, truth = partido, estimate = .pred_class)
metricas_svm <- metricas(previsoes_svm, truth = partido,
                        estimate = .pred_class, .pred_PT)
metricas_svm
```

**Interpretando os resultados.** A SVM acertou `r sum(previsoes_svm$partido == previsoes_svm$.pred_class)` dos `r nrow(previsoes_svm)` documentos. Sua acurácia balanceada foi de `r scales::percent(metricas_svm$.estimate[metricas_svm$.metric == "bal_accuracy"], accuracy = 0.1, decimal.mark = ",")`. Para interpretar esse valor, observe a diagonal da matriz de confusão e compare a sensibilidade e a especificidade. A taxa de acertos é semelhante nas duas classes ou o modelo concentra os erros em uma delas, como ocorreu com o Naive Bayes no Tutorial 6?

## 3. Modelo 4 — Random Forest

### 3.1. Da árvore à floresta

Vamos imaginar outra maneira de classificar um discurso. Em vez de combinar todos os pesos das palavras em uma soma, podemos fazer perguntas sucessivas: o valor TF-IDF da palavra A é maior do que certo número? Se sim, passamos a uma pergunta; se não, a outra. No final, chegamos a um grupo de documentos e fazemos uma previsão. Essa é a ideia de uma **árvore de decisão**.

Uma única árvore pode ser muito sensível aos dados que utilizamos no treinamento. A Random Forest (*floresta aleatória*) constrói várias árvores, usando amostras dos documentos e subconjuntos aleatórios dos preditores nas divisões. A previsão combina as informações dessas árvores. É o que chamamos de *ensemble*: um conjunto de modelos cujas previsões são combinadas.

O interessante é que a floresta pode aprender relações não lineares e interações entre as palavras. Uma palavra pode ser útil em uma parte da árvore e pouco útil em outra. Isso não significa que a floresta necessariamente funcionará melhor do que um modelo linear.

### 3.2. Ajustando o modelo

Vamos construir uma floresta de 200 árvores. `mtry = 32` significa que, em cada divisão de uma árvore, até 32 preditores são considerados como candidatos. `min_n = 10` controla o tamanho mínimo de um nó para continuar o crescimento, na implementação utilizada. Fixamos esses valores para não transformar o exercício em uma busca de hiperparâmetros.

```{r rf-ajuste}
set.seed(semente + 7L)

modelo_rf <- rand_forest(trees = n_arvores, mtry = mtry_rf,
                         min_n = min_n_rf) %>%
  set_engine("ranger", num.threads = 1) %>%
  set_mode("classification")

ajuste_rf <- fit(modelo_rf, partido ~ ., data = matriz_treino)
```

O motor ajusta uma floresta de probabilidades. Utilizamos uma única thread para limitar o uso de recursos; o ajuste pode ser mais demorado que a regressão logística. Repare que continuamos utilizando a mesma fórmula e os mesmos dados.

### 3.3. Fazendo previsões

```{r rf-previsoes}
classes_rf <- predict(ajuste_rf, new_data = x_teste)
probabilidades_rf <- predict(ajuste_rf, new_data = x_teste,
                             type = "prob")

previsoes_rf <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_rf, probabilidades_rf)

previsoes_rf %>% slice_head(n = 8)
```

### 3.4. Avaliando o modelo

```{r rf-metricas}
conf_mat(previsoes_rf, truth = partido, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

metricas_classes(previsoes_rf, truth = partido, estimate = .pred_class)
metricas_rf <- metricas(previsoes_rf, truth = partido,
                       estimate = .pred_class, .pred_PT)
metricas_rf
```

**Interpretando os resultados.** A floresta acertou `r sum(previsoes_rf$partido == previsoes_rf$.pred_class)` documentos, com acurácia balanceada de `r scales::percent(metricas_rf$.estimate[metricas_rf$.metric == "bal_accuracy"], accuracy = 0.1, decimal.mark = ",")`. Olhe também as probabilidades das primeiras previsões. Um modelo mais flexível pode produzir mais acertos, mas também pode aprender padrões muito particulares dos textos do treinamento. Só conseguimos examinar sua capacidade preditiva quando o aplicamos a documentos novos.

## 4. Experimento 1 — Comparando os algoritmos

Agora temos cinco conjuntos de previsões para os mesmos discursos do teste. O modelo nulo não utiliza os textos; os outros quatro classificadores recebem os mesmos mil pesos TF-IDF. Vamos juntar as previsões e comparar os resultados, como no Tutorial 6.

```{r comparacao-algoritmos}
previsoes_tfidf <- bind_rows(
  previsoes_tutorial6,
  previsoes_svm %>% mutate(modelo = "SVM linear"),
  previsoes_rf %>% mutate(modelo = "Random Forest")
)

comparacao_tfidf <- previsoes_tfidf %>%
  group_by(modelo) %>%
  metricas(truth = partido, estimate = .pred_class, .pred_PT) %>%
  ungroup() %>%
  select(modelo, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(desc(bal_accuracy))

comparacao_tfidf
```

```{r grafico-algoritmos}
comparacao_tfidf %>%
  ggplot(aes(reorder(modelo, bal_accuracy), bal_accuracy)) +
  geom_col(fill = "steelblue", width = 0.6) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "grey50") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  labs(x = NULL, y = "Acurácia balanceada",
       title = "Diferentes algoritmos, mesmos documentos") +
  theme_minimal()
```

O maior valor observado da acurácia balanceada nesta tabela é `r scales::percent(max(comparacao_tfidf$bal_accuracy), accuracy = 0.1, decimal.mark = ",")`, para `r paste(comparacao_tfidf$modelo[comparacao_tfidf$bal_accuracy == max(comparacao_tfidf$bal_accuracy)], collapse = ", ")`. Isso descreve o resultado desta execução, não é uma recomendação de algoritmo para qualquer corpus. Vamos observar também as curvas ROC.

```{r roc-algoritmos}
previsoes_tfidf %>%
  group_by(modelo) %>%
  roc_curve(truth = partido, .pred_PT) %>%
  autoplot() +
  labs(title = "Curvas ROC dos cinco classificadores", colour = "Modelo")
```

Um modelo com AUC maior pode não ter a maior acurácia no limiar de classificação que utilizamos. Observe as duas medidas. Como os documentos e a informação de entrada são os mesmos, as diferenças observadas entre os quatro classificadores de texto decorrem dos algoritmos e de suas configurações, não da inclusão de mais palavras em um deles.

**Exercício:** compare a matriz de confusão da SVM e da Random Forest com a do LASSO do Tutorial 6. O resultado que parece melhor considerando somente a acurácia é também aquele com melhor equilíbrio entre os acertos nas duas classes? A maior complexidade da floresta produziu necessariamente maior capacidade preditiva?

## 5. Experimento 2 — E se utilizássemos tópicos em vez de palavras?

No Tutorial 3, estudamos a modelagem de tópicos com LDA. O modelo nos devolveu duas coisas: `beta`, a distribuição de palavras em cada tópico, e `gamma`, a participação de cada tópico em um documento. Naquela ocasião nosso objetivo era descobrir os assuntos presentes no corpus. Agora vamos usar `gamma` para construir os preditores de um modelo supervisionado.

Imagine a seguinte tabela (os números são fictícios):

| Discurso | Partido | T1 | T2 | T3 | T4 | T5 | T6 |
|---|---|---:|---:|---:|---:|---:|---:|
| A | PT | 0,40 | 0,10 | 0,20 | 0,05 | 0,15 | 0,10 |
| B | PL | 0,05 | 0,35 | 0,10 | 0,30 | 0,05 | 0,15 |
| C | PT | 0,20 | 0,20 | 0,10 | 0,10 | 0,30 | 0,10 |

O que mudou em relação às tabelas do Tutorial 6? Ainda temos uma linha por discurso e a mesma variável resposta. Mas, em vez de mil palavras, temos seis colunas: as proporções dos tópicos no documento. O LDA constrói essa representação **sem receber os rótulos dos partidos**. Só depois utilizaremos os rótulos para ajustar a regressão logística.

Essa é uma maneira de combinar o aprendizado não supervisionado com o supervisionado. Vamos fazer a comparação mantendo a família de modelo (regressão logística regularizada) e alterando a representação dos textos.

### 5.1. Aprendendo os tópicos com o treinamento

O LDA clássico utiliza contagens de palavras, não TF-IDF. Já temos as contagens na coluna `n` de `tfidf_discursos`. Vamos construir uma DTM de contagens com `cast_dtm()`, usando o mesmo vocabulário do treinamento. Essa matriz não contém a coluna `partido`.

```{r lda-dtm-treino}
dtm_treino <- tfidf_discursos %>%
  filter(palavra %in% vocabulario$palavra) %>%
  cast_dtm(id_documento, palavra, n)

dim(dtm_treino)
```

Vamos ajustar um LDA com seis tópicos, como no Tutorial 3. Usamos o método VEM, com até 100 iterações externas para reduzir o tempo de processamento. Esse limite é uma escolha didática, não uma garantia de convergência nem uma busca pelo melhor número de tópicos.

```{r lda-ajuste}
set.seed(semente + 8L)

lda <- topicmodels::LDA(
  dtm_treino, k = k_topicos, method = "VEM",
  control = list(seed = semente + 8L,
                 em = list(iter.max = 100L, tol = 1e-4))
)
```

Vamos olhar as palavras mais associadas aos tópicos, como fizemos na aula anterior. Esses tópicos foram aprendidos apenas com os textos do treinamento.

```{r lda-beta, fig.width=9, fig.height=7}
beta <- tidy(lda, matrix = "beta")

beta %>%
  group_by(topic) %>%
  slice_max(beta, n = 8, with_ties = FALSE) %>%
  ungroup() %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ topic, scales = "free_y") +
  scale_x_reordered() +
  labs(x = NULL, y = "Beta", title = "Palavras associadas aos seis tópicos") +
  theme_minimal()
```

O gráfico nos ajuda a interpretar o conteúdo de cada tópico. Não devemos dar um nome ao tópico com base em apenas uma palavra. Os tópicos são misturas de palavras; um mesmo documento pode conter vários tópicos.

### 5.2. De gamma a uma tabela de preditores

Vamos recuperar as proporções de tópicos do treinamento. `posterior(lda)$topics` devolve uma matriz em que cada linha corresponde a um discurso e cada coluna a um tópico. Assim como fizemos com a representação TF-IDF, juntamos a matriz aos rótulos pelo identificador do discurso.

```{r gamma-treino}
gamma_treino <- topicmodels::posterior(lda)$topics
colnames(gamma_treino) <- paste0("topico_", seq_len(k_topicos))

stopifnot(setequal(rownames(gamma_treino), rownames(dtm_treino)),
          all(is.finite(gamma_treino)))

topicos_treino <- treino %>%
  select(id_documento, partido) %>%
  left_join(as_tibble(gamma_treino, rownames = "id_documento"),
            by = "id_documento") %>%
  mutate(across(starts_with("topico_"), ~ replace_na(.x, 1 / k_topicos)))

matriz_topicos_treino <- topicos_treino %>% select(-id_documento)
matriz_topicos_treino %>% slice_head(n = 5)
dim(matriz_topicos_treino)
```

Um discurso que não contém nenhuma palavra do vocabulário não pode ser representado pelo LDA a partir das palavras. Para não excluir documentos apenas deste modelo, atribuímos a esses casos uma mistura uniforme de 1/6 em cada tópico (a média da distribuição a priori simétrica, sem evidência textual). Vamos conferir quantos são.

```{r lda-vazios-treino}
nrow(treino) - nrow(gamma_treino)
```

Podemos comparar, descritivamente, a participação média dos tópicos nos dois grupos de discursos do treinamento. Como cada documento tem seis proporções que somam 1, a média dessas proporções em cada grupo também soma 1.

```{r gamma-medias}
topicos_treino %>%
  pivot_longer(starts_with("topico_"), names_to = "topico",
               values_to = "gamma") %>%
  group_by(partido, topico) %>%
  summarise(gamma_medio = mean(gamma), .groups = "drop") %>%
  ggplot(aes(topico, gamma_medio, fill = partido)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(x = "Tópico", y = "Participação média", fill = "Rótulo",
       title = "Distribuição média dos tópicos no treinamento") +
  theme_minimal()
```

Esse gráfico resume o conteúdo dos documentos. Assim como a comparação de frequências de palavras, ele não é por si só um classificador. Vamos utilizar a tabela de proporções como entrada da regressão logística.

### 5.3. Representando os novos textos sem reaprender os tópicos

No Tutorial 6, calculamos o IDF no treinamento e utilizamos esse mesmo IDF no teste. Agora temos um problema semelhante. Não podemos ajustar um novo LDA aos discursos do teste: os tópicos estimados seriam outros. Também não devemos ajustar um LDA juntando treino e teste, pois os documentos reservados participariam da aprendizagem da representação.

Precisamos construir uma DTM de contagens dos textos novos com as mesmas palavras e na mesma ordem da DTM de treinamento. `cast_dtm()` sozinho poderia deixar de criar alguma coluna que não aparece no teste. Usaremos `slam::simple_triplet_matrix()` para definir as linhas, colunas e contagens explicitamente. É a mesma ideia da matriz esparsa da Aula 1: armazenamos apenas as contagens diferentes de zero.

```{r lda-dtm-teste}
contagens_lda_teste <- contagens_teste %>%
  filter(palavra %in% colnames(dtm_treino))

ids_teste_lda <- teste$id_documento[
  teste$id_documento %in% contagens_lda_teste$id_documento
]

dtm_teste <- slam::simple_triplet_matrix(
  i = match(contagens_lda_teste$id_documento, ids_teste_lda),
  j = match(contagens_lda_teste$palavra, colnames(dtm_treino)),
  v = contagens_lda_teste$n,
  nrow = length(ids_teste_lda),
  ncol = ncol(dtm_treino),
  dimnames = list(ids_teste_lda, colnames(dtm_treino))
)

stopifnot(identical(colnames(dtm_teste), colnames(dtm_treino)))
```

A função `posterior()` permite utilizar o LDA já ajustado para inferir as proporções dos tópicos em novos documentos. Mantemos as distribuições de palavras (`beta`) fixas e também o parâmetro `alpha` aprendido no treinamento. Assim, os seis tópicos da nova tabela correspondem exatamente aos seis tópicos da tabela de treinamento.

```{r gamma-teste}
gamma_teste <- topicmodels::posterior(
  lda, newdata = dtm_teste,
  control = list(seed = semente + 8L,
                 estimate.alpha = FALSE,
                 alpha = lda@alpha)
)$topics

colnames(gamma_teste) <- colnames(gamma_treino)
stopifnot(setequal(rownames(gamma_teste), ids_teste_lda),
          all(is.finite(gamma_teste)))

topicos_teste <- teste %>%
  select(id_documento, partido) %>%
  left_join(as_tibble(gamma_teste, rownames = "id_documento"),
            by = "id_documento") %>%
  mutate(across(starts_with("topico_"), ~ replace_na(.x, 1 / k_topicos)))

matriz_topicos_teste <- topicos_teste %>% select(-id_documento)
x_topicos_teste <- matriz_topicos_teste %>% select(-partido)

matriz_topicos_teste %>% slice_head(n = 5)
dim(matriz_topicos_teste)
nrow(teste) - nrow(gamma_teste)
```

Como no treinamento, preenchemos com 1/6 os documentos sem termos conhecidos. Isso garante que o teste de todos os modelos continue contendo os mesmos discursos. Conferimos que as colunas são iguais e que as proporções de cada linha somam aproximadamente 1.

```{r conferir-topicos}
stopifnot(identical(names(matriz_topicos_treino), names(matriz_topicos_teste)),
          nrow(matriz_topicos_treino) == nrow(treino),
          nrow(matriz_topicos_teste) == nrow(teste),
          all(abs(rowSums(matriz_topicos_treino %>% select(-partido)) - 1) < 1e-5),
          all(abs(rowSums(x_topicos_teste) - 1) < 1e-5))
```

### 5.4. Modelo 5 — Regressão logística com tópicos

Agora temos duas tabelas numéricas com seis preditores em vez de mil. O restante é exatamente o que já fizemos. Vamos reutilizar a especificação do LASSO do Tutorial 6 (`lambda = 0,01`) como uma configuração fixa. Esse valor não foi otimizado para a representação por tópicos: estamos fazendo uma comparação didática, não uma busca pelo melhor desempenho possível de cada representação.

```{r lasso-lda-ajuste}
ajuste_lda <- fit(modelo_lasso, partido ~ .,
                  data = matriz_topicos_treino)
```

```{r lasso-lda-previsoes}
classes_lda <- predict(ajuste_lda, new_data = x_topicos_teste)
probabilidades_lda <- predict(ajuste_lda, new_data = x_topicos_teste,
                              type = "prob")

previsoes_lda <- teste %>%
  select(id_documento, partido) %>%
  bind_cols(classes_lda, probabilidades_lda)

previsoes_lda %>% slice_head(n = 8)
```

```{r lasso-lda-metricas}
conf_mat(previsoes_lda, truth = partido, estimate = .pred_class) %>%
  autoplot(type = "heatmap")

metricas_classes(previsoes_lda, truth = partido, estimate = .pred_class)
metricas_lda <- metricas(previsoes_lda, truth = partido,
                       estimate = .pred_class, .pred_PT)
metricas_lda
```

**Interpretando os resultados.** Com apenas seis preditores, acertamos `r sum(previsoes_lda$partido == previsoes_lda$.pred_class)` documentos do teste e obtivemos acurácia balanceada de `r scales::percent(metricas_lda$.estimate[metricas_lda$.metric == "bal_accuracy"], accuracy = 0.1, decimal.mark = ",")`. Para saber se isso é muito ou pouco, precisamos colocar esse resultado lado a lado com o da regressão que utiliza as palavras.

## 6. O que muda quando mudamos a representação?

Vamos comparar especificamente as duas regressões logísticas. Utilizamos os mesmos discursos e rótulos, a mesma família de modelo e a mesma penalização nominal. O que muda são os preditores. Com a representação LDA, comprimimos a informação de mil palavras em seis proporções, que somam 1 (portanto apenas cinco variam livremente).

```{r comparacao-representacoes}
comparacao_representacoes <- bind_rows(
  previsoes_lasso %>% mutate(representacao = "TF-IDF"),
  previsoes_lda %>% mutate(representacao = "LDA")
) %>%
  group_by(representacao) %>%
  metricas(truth = partido, estimate = .pred_class, .pred_PT) %>%
  ungroup() %>%
  select(representacao, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate)

comparacao_representacoes

ba_tfidf <- comparacao_representacoes %>%
  filter(representacao == "TF-IDF") %>% pull(bal_accuracy)
ba_lda <- comparacao_representacoes %>%
  filter(representacao == "LDA") %>% pull(bal_accuracy)
```

A acurácia balanceada da regressão com TF-IDF foi `r scales::percent(ba_tfidf, accuracy = 0.1, decimal.mark = ",")`; com LDA foi `r scales::percent(ba_lda, accuracy = 0.1, decimal.mark = ",")`. A diferença (LDA menos TF-IDF) foi de `r round(100 * (ba_lda - ba_tfidf), 1)` pontos percentuais.

`r if (ba_lda < ba_tfidf) "Neste ajuste, a representação por tópicos teve menor capacidade de classificação. Isso é compatível com a perda de informação lexical específica." else if (ba_lda > ba_tfidf) "Neste ajuste, a representação por tópicos teve maior acurácia balanceada. Uma representação mais compacta pode conservar padrões úteis para a previsão." else "Neste ajuste, as duas representações tiveram a mesma acurácia balanceada, apesar da diferença no número de preditores."`

A comparação retoma uma questão importante da primeira aula. Toda representação conserva algumas informações e perde outras. Com o TF-IDF, conservamos o peso de palavras individuais, mas não sua ordem. Com LDA, conservamos as misturas de tópicos e perdemos parte da especificidade do vocabulário. Se um termo pouco frequente for muito informativo para a previsão, a representação por tópicos pode diluir esse sinal. Por outro lado, um tópico pode reunir palavras diferentes que trazem informações semelhantes.

Não podemos atribuir qualquer diferença exclusivamente à dimensionalidade: o vocabulário, a estimação dos tópicos, o número de tópicos e a regularização também importam. Uma penalização nominal igual não garante que os dois modelos tenham o mesmo grau de complexidade. Nosso objetivo é entender o efeito de duas escolhas concretas, não determinar qual representação é universalmente superior.

## 7. Reunindo todos os resultados

```{r comparacao-final}
previsoes <- bind_rows(
  previsoes_tfidf,
  previsoes_lda %>% mutate(modelo = "LASSO LDA")
)

# Todos os modelos fizeram uma previsão para cada discurso reservado.
stopifnot(nrow(previsoes) == 6L * nrow(teste),
          !anyDuplicated(previsoes %>% select(modelo, id_documento)))

comparacao <- previsoes %>%
  group_by(modelo) %>%
  metricas(truth = partido, estimate = .pred_class, .pred_PT) %>%
  ungroup()

comparacao %>%
  select(modelo, .metric, .estimate) %>%
  pivot_wider(names_from = .metric, values_from = .estimate) %>%
  arrange(desc(bal_accuracy))
```

```{r grafico-final}
comparacao %>%
  filter(.metric == "bal_accuracy") %>%
  mutate(representacao = case_when(
    modelo == "Nulo" ~ "Sem texto",
    modelo == "LASSO LDA" ~ "LDA",
    TRUE ~ "TF-IDF"
  )) %>%
  ggplot(aes(reorder(modelo, .estimate), .estimate,
             fill = representacao)) +
  geom_col(width = 0.6) +
  geom_hline(yintercept = 0.5, linetype = 2, colour = "grey50") +
  coord_flip() +
  scale_y_continuous(limits = c(0, 1), labels = scales::label_percent()) +
  labs(x = NULL, y = "Acurácia balanceada", fill = "Representação",
       title = "Comparando seis configurações de classificação") +
  theme_minimal()
```

```{r roc-final}
previsoes %>%
  group_by(modelo) %>%
  roc_curve(truth = partido, .pred_PT) %>%
  autoplot() +
  labs(title = "Curvas ROC dos classificadores", colour = "Modelo")
```

O gráfico permite distinguir duas comparações que às vezes são confundidas. Entre os modelos com TF-IDF, mudamos o algoritmo, não os textos nem seus preditores. Entre as regressões com TF-IDF e LDA, mudamos a representação. O modelo nulo é nossa referência: ele não utiliza nenhuma informação textual e sua acurácia balanceada é 50%.

Os resultados referem-se a esta divisão por parlamentar e a estas configurações. Como cada discurso conta uma vez, parlamentares que têm muitos discursos têm maior peso nas métricas. Não estimamos a variabilidade entre divisões. Tampouco vamos ajustar novamente os modelos em função desses resultados: o teste já foi utilizado para uma comparação descritiva no Tutorial 6 e aqui.

## 8. Voltando aos textos: quais documentos foram classificados incorretamente?

Em análise de textos, uma boa maneira de entender os resultados de um modelo é voltar aos documentos. Mesmo um classificador com acurácia alta pode cometer erros importantes. Para facilitar a comparação, vamos utilizar as duas regressões logísticas, uma com cada representação, sem escolher uma nova configuração com base no teste.

Primeiro, reunimos as classes previstas para cada discurso, preservando seu identificador. Depois, comparamos as situações em que as duas representações acertam, as duas erram ou apenas uma acerta.

```{r erros-comparacao}
erros <- previsoes_lasso %>%
  transmute(id_documento, partido, classe_tfidf = .pred_class,
            prob_tfidf = pmax(.pred_PT, .pred_PL)) %>%
  left_join(
    previsoes_lda %>%
      transmute(id_documento, classe_lda = .pred_class,
                prob_lda = pmax(.pred_PT, .pred_PL)),
    by = "id_documento"
  ) %>%
  mutate(acerto_tfidf = classe_tfidf == partido,
         acerto_lda = classe_lda == partido)

erros %>% count(acerto_tfidf, acerto_lda)
```

Se os dois modelos acertassem e errassem sempre os mesmos documentos, mudar a representação pouco alteraria as previsões individuais. Se há documentos que uma representação classifica corretamente e a outra não, isso nos dá exemplos concretos para estudar as informações preservadas ou perdidas em cada transformação.

Vamos recuperar alguns sumários classificados incorretamente pelo LASSO com TF-IDF, priorizando os erros com maior probabilidade prevista. A probabilidade elevada não significa que o resultado seja necessariamente confiável.

```{r erros-textos}
erros %>%
  filter(!acerto_tfidf) %>%
  group_by(partido) %>%
  slice_max(prob_tfidf, n = 2, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(teste %>% select(id_documento, sumario), by = "id_documento") %>%
  select(id_documento, partido, classe_tfidf, prob_tfidf,
         classe_lda, sumario) %>%
  knitr::kable(digits = 3)
```

Leia os textos. Há informações que a representação por palavras não consegue preservar? Há menções explícitas a partidos, nomes, lugares ou instituições? Uma palavra aparece em contextos diferentes? Lembre-se também de que o sumário é uma síntese editorial e pode omitir o posicionamento expresso no discurso original.

**Exercício:** procure também exemplos em que a regressão com tópicos erra e a regressão com TF-IDF acerta, e vice-versa. A leitura desses exemplos ajuda a formular hipóteses, mas não autoriza alterar o modelo e voltar a apresentar o mesmo teste como uma avaliação independente.

## 9. O que aprendemos?

Começamos o curso contando palavras e construindo representações de textos. Na segunda aula aprendemos a estimar tópicos sem conhecer previamente os assuntos de cada documento. Nesta última aula utilizamos rótulos conhecidos para construir classificadores e avaliar suas previsões em documentos de parlamentares reservados.

Aprendemos que uma mesma representação pode ser utilizada por vários algoritmos, e que uma mesma família de modelo pode receber diferentes representações. As duas escolhas importam. Também aprendemos que um modelo não precisa compreender um texto como nós o compreendemos para usar padrões do vocabulário na previsão de uma categoria.

O exercício tem limites: utiliza somente dois rótulos, uma modalidade de discurso e sumários de um período específico. As associações aprendidas não são medidas de ideologia, não são efeitos causais e não garantem a generalização para outras coleções. Para desenvolver um classificador voltado à aplicação em novos dados, poderíamos trabalhar com três conjuntos (treino, validação e teste), comparar outras configurações na validação e reservar o teste para o final. A validação cruzada agrupada por autor é outra possibilidade.

Como extensão, podemos experimentar bigramas, comparar contagens com TF-IDF ou examinar a influência de referências explícitas nos textos. Essas experiências devem ser feitas com dados de desenvolvimento e avaliadas posteriormente em novos dados reservados.

**Leituras:** Hvitfeldt e Silge, [capítulo 7 — Classificação](https://smltar.com/mlclassification); Silge e Robinson, [capítulo 6 — Modelagem de tópicos](https://www.tidytextmining.com/topicmodeling); documentação de [`svm_linear()`](https://parsnip.tidymodels.org/reference/details_svm_linear_kernlab.html), [`rand_forest()`](https://parsnip.tidymodels.org/reference/details_rand_forest_ranger.html) e [`topicmodels::posterior()`](https://rdrr.io/cran/topicmodels/man/posterior-methods.html).
