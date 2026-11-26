---
output:
  github_document: default
  html_document:
    toc: true
    toc_depth: 2
lang: pt-BR
encoding: UTF-8
---

# Tutorial 4 — Análise de sentimentos nos discursos de Lula

**Aula 2 · Análise de sentimentos com dicionários**

*Material complementar opcional, para exploração ao final ou após o segundo encontro.*

No tutorial anterior, usamos as palavras para descobrir tópicos nos discursos. Agora vamos fazer outra coisa: associar as palavras a um dicionário que já contém uma classificação de polaridade.

Seguiremos a lógica do [capítulo 2 de *Text Mining with R*, de Julia Silge e David Robinson](https://www.tidytextmining.com/sentiment.html). Em lugar de romances em inglês, temos discursos em português. Vamos usar essa técnica para comparar o vocabulário de duas falas sobre o combate à fome e para observar como esse vocabulário muda ao longo de cada fala.

## 1. Pacotes e corpus

```{r instalacao, eval=FALSE}
# Execute apenas se precisar instalar os pacotes.
install.packages(c("tidyverse", "tidytext", "lexiconPT", "stringi"))
```

```{r pacotes, message=FALSE}
library(tidyverse)
library(tidytext)
library(lexiconPT)
```

Vamos abrir o mesmo RDS do tutorial de tópicos diretamente do GitHub, sem precisar baixar uma cópia local ou ajustar o diretório de trabalho.

```{r corpus}
corpus_original <- read_rds("https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/corpus_discursos_lula.rds") %>%
  mutate(id = as.character(id))

metadados <- corpus_original %>%
  select(id, titulo, data_publicacao, url)
```

Neste exercício, mantemos o conjunto de registros do tutorial de tópicos. Não fazemos uma auditoria completa de autoria, duplicatas ou transcrições com mais de um locutor. Portanto, os resultados descrevem o acervo utilizado, e não necessariamente apenas palavras proferidas por Lula. A data disponível é a de publicação.

O RDS contém uma coluna `paragrafos`, que guarda uma lista de parágrafos para cada discurso. Vamos usar `unnest_longer()` para transformar essa lista em uma tabela com um parágrafo por linha, preservando a ordem e o identificador do discurso. Retiramos parágrafos vazios e os rótulos editoriais que começam com `TRANSMISSÃO |`.

```{r paragrafos}
paragrafos <- corpus_original %>%
  select(id, paragrafos) %>%
  unnest_longer(paragrafos, values_to = "texto", indices_to = "paragrafo") %>%
  filter(!is.na(texto), str_squish(texto) != "") %>%
  filter(!str_detect(texto, regex("^\\s*TRANSMISSÃO\\s*\\|", ignore_case = TRUE)))

paragrafos %>% select(id, paragrafo, texto) %>% head(3)
```

## 2. Tokenização e dicionário de sentimentos

Como já fizemos nos tutoriais anteriores, uma linha de `tokens_lula` representa uma ocorrência de uma palavra. Agora **não vamos retirar stopwords**: palavras como "não" são importantes para a interpretação de sentimentos, ainda que nem todas recebam uma classificação. Mantemos os tokens que contêm pelo menos uma letra.

```{r tokens}
tokens_lula <- paragrafos %>%
  unnest_tokens(palavra, texto) %>%
  filter(str_detect(palavra, "\\p{L}")) %>%
  mutate(chave = stringi::stri_trans_general(palavra, "Latin-ASCII"))

head(tokens_lula)
```

Por que criar uma `chave`? A versão do OpLexicon disponibilizada pelo pacote `lexiconPT` usa termos sem acentos. Para encontrar correspondências, criamos uma coluna auxiliar sem acentos, mas guardamos a palavra e o parágrafo originais. Essa simplificação pode aproximar palavras com sentidos diferentes.

O léxico Bing usado por Silge e Robinson classifica palavras em positivas e negativas, mas foi construído para o inglês. Vamos usar o [OpLexicon 3.0](https://rdrr.io/cran/lexiconPT/man/oplexicon_v3.0.html), que contém termos em português com polaridade -1 (negativa), 0 (neutra) ou +1 (positiva).

```{r lexico}
data("oplexicon_v3.0", package = "lexiconPT")

lexico <- oplexicon_v3.0 %>%
  transmute(
    chave = stringi::stri_trans_general(str_to_lower(term), "Latin-ASCII"),
    polaridade = polarity
  ) %>%
  filter(str_detect(chave, "^[a-z]+$"), polaridade %in% c(-1, 0, 1)) %>%
  distinct(chave, polaridade) %>%
  group_by(chave) %>%
  filter(n() == 1) %>%
  ungroup() %>%
  filter(polaridade != 0) %>%
  mutate(sentimento = if_else(polaridade == 1, "positiva", "negativa"))

lexico %>% count(sentimento)
```

Uma palavra pode aparecer mais de uma vez no dicionário. Para que uma ocorrência não seja contada duas vezes na junção, retiramos as chaves que têm classificações conflitantes e mantemos apenas as de polaridade positiva ou negativa. Essa é uma escolha conservadora para a aula; uma palavra excluída não é necessariamente neutra.

## 3. Associando palavras a sentimentos

A função `inner_join()` conserva somente as ocorrências encontradas nas duas tabelas. É a operação central deste tutorial: juntamos os tokens do corpus ao dicionário pela coluna `chave`.

```{r juntar}
palavras_avaliadas <- tokens_lula %>%
  inner_join(lexico, by = "chave")

head(palavras_avaliadas)

palavras_avaliadas %>% count(sentimento)
```

Nem todas as palavras de um discurso estão no dicionário. Podemos calcular a **cobertura**: a proporção de tokens que receberam uma classificação positiva ou negativa neste exercício.

```{r cobertura}
nrow(palavras_avaliadas) / nrow(tokens_lula)
```

Cobertura não significa acurácia. Para saber se a classificação é correta no contexto, precisaríamos comparar as classificações do dicionário com a leitura humana dos textos.

### Quais palavras mais contribuem para o resultado?

```{r palavras-frequentes, fig.width=9, fig.height=5}
palavras_frequentes <- palavras_avaliadas %>%
  count(sentimento, palavra, sort = TRUE) %>%
  group_by(sentimento) %>%
  slice_max(n, n = 10, with_ties = FALSE) %>%
  ungroup()

palavras_frequentes %>%
  mutate(palavra = reorder_within(palavra, n, sentimento)) %>%
  ggplot(aes(palavra, n, fill = sentimento)) +
  geom_col(show.legend = FALSE) +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ sentimento, scales = "free_y") +
  labs(title = "Palavras positivas e negativas mais frequentes",
       x = NULL, y = "Ocorrências") +
  theme_minimal()
```

O gráfico não identifica emoções dos oradores. Ele informa quais palavras classificadas pelo léxico aparecem mais vezes no acervo. Observe se algumas palavras parecem positivas ou negativas em um contexto, mas não em outro.

## 4. Comparando discursos

Seguindo a ideia do livro, podemos subtrair a quantidade de palavras negativas da quantidade de positivas. Mas há um cuidado importante: uma fala longa tem mais oportunidades de conter palavras das duas categorias. Vamos dividir o saldo pelo total de tokens do discurso e multiplicar por mil.

```{r saldo}
totais <- tokens_lula %>% count(id, name = "total")

contagens_sentimentos <- palavras_avaliadas %>%
  group_by(id) %>%
  summarise(positivas = sum(polaridade == 1),
            negativas = sum(polaridade == -1),
            .groups = "drop")

sentimentos_lula <- totais %>%
  left_join(contagens_sentimentos, by = "id") %>%
  mutate(positivas = replace_na(positivas, 0L),
         negativas = replace_na(negativas, 0L),
         saldo = positivas - negativas,
         saldo_por_mil = 1000 * saldo / total,
         cobertura = (positivas + negativas) / total) %>%
  left_join(metadados, by = "id")

sentimentos_lula %>% select(titulo, total, positivas, negativas,
                            saldo_por_mil, cobertura) %>% head()
```

Um saldo de +20 por mil tokens significa que houve 20 ocorrências positivas a mais que negativas, a cada mil tokens do texto. Não significa que 20% do discurso seja positivo. Um saldo zero pode resultar de equilíbrio entre as classes ou de ausência de palavras classificadas; por isso guardamos a cobertura.

Vamos comparar duas falas do acervo sobre o combate à fome: o lançamento do Plano Brasil Sem Fome e o lançamento da Aliança Global contra a Fome e a Pobreza. Selecionamos os discursos pelo identificador do RDS, não pelo resultado do método.

```{r casos}
casos <- tibble(
  id = c("36228147083906c706c7976434a92c6e15347902",
         "03606f241616f853760dce18c2e236a586efdd03"),
  discurso = c("Brasil Sem Fome", "Aliança Global")
)

stopifnot(all(casos$id %in% sentimentos_lula$id))

sentimentos_lula %>%
  inner_join(casos, by = "id") %>%
  select(discurso, total, positivas, negativas, saldo_por_mil, cobertura)
```

```{r comparar-casos, fig.width=7, fig.height=4}
sentimentos_lula %>%
  inner_join(casos, by = "id") %>%
  mutate(positivas = 1000 * positivas / total,
         negativas = 1000 * negativas / total) %>%
  pivot_longer(c(positivas, negativas), names_to = "sentimento",
               values_to = "por_mil") %>%
  ggplot(aes(discurso, por_mil, fill = sentimento)) +
  geom_col(position = "dodge") +
  labs(title = "Vocabulário avaliativo em duas falas sobre a fome",
       x = NULL, y = "Ocorrências por mil tokens", fill = NULL) +
  theme_minimal()
```

Qual fala tem mais palavras positivas por mil tokens? E negativas? Ter mais palavras negativas significa ser contrário ao programa apresentado? Para interpretar o gráfico, precisamos saber quais palavras foram contadas.

```{r contribuicoes-casos}
palavras_avaliadas %>%
  inner_join(casos, by = "id") %>%
  count(discurso, sentimento, palavra, sort = TRUE) %>%
  group_by(discurso, sentimento) %>%
  slice_max(n, n = 5, with_ties = FALSE) %>%
  ungroup() %>%
  arrange(discurso, sentimento, desc(n)) %>%
  print(n = Inf)
```

## 5. Como o sentimento varia ao longo da fala?

No capítulo 2, Silge e Robinson dividem os romances em partes para acompanhar o saldo ao longo da narrativa. Faremos algo semelhante, dividindo cada uma das duas falas em blocos consecutivos de 200 tokens. Diferentemente de um romance, o discurso é curto e a série terá poucos pontos.

```{r blocos, fig.width=9, fig.height=5}
sentimentos_blocos <- tokens_lula %>%
  inner_join(casos, by = "id") %>%
  group_by(id) %>%
  mutate(bloco = (row_number() - 1L) %/% 200L + 1L) %>%
  ungroup() %>%
  left_join(lexico, by = "chave") %>%
  group_by(discurso, bloco) %>%
  summarise(total = n(),
            saldo = sum(polaridade, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(saldo_por_mil = 1000 * saldo / total)

sentimentos_blocos %>%
  ggplot(aes(bloco, saldo_por_mil)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey50") +
  geom_col(fill = "steelblue") +
  facet_wrap(~ discurso, ncol = 1, scales = "free_x") +
  labs(title = "Saldo lexical ao longo de cada discurso",
       x = "Bloco consecutivo de 200 tokens", y = "Saldo por mil tokens") +
  theme_minimal()
```

O operador `%/%` é a divisão inteira. O bloco 1 reúne os primeiros 200 tokens, o bloco 2 os 200 seguintes, e assim por diante. Criamos os blocos **antes** da junção com o léxico, para não agrupar apenas palavras avaliadas. O último bloco pode ter menos de 200 tokens; sua taxa é mais sensível a poucas ocorrências. Um bloco não equivale a um intervalo de tempo nem necessariamente a uma unidade de sentido.

## 6. Voltando aos textos

Vamos procurar uma palavra negativa frequente entre as duas falas e recuperar um parágrafo de cada uma em que ela aparece. Assim podemos verificar o significado que ela assume no texto original.

```{r contexto}
termo_exemplo <- palavras_avaliadas %>%
  filter(id %in% casos$id, sentimento == "negativa") %>%
  count(palavra, sort = TRUE) %>%
  slice_head(n = 1) %>%
  pull(palavra)

termo_exemplo

contexto <- tokens_lula %>%
  filter(id %in% casos$id, palavra == termo_exemplo) %>%
  distinct(id, paragrafo) %>%
  inner_join(paragrafos, by = c("id", "paragrafo")) %>%
  inner_join(casos, by = "id") %>%
  group_by(discurso) %>%
  slice_head(n = 1) %>%
  ungroup()

for (i in seq_len(nrow(contexto))) {
  cat("\n", contexto$discurso[i], "— parágrafo", contexto$paragrafo[i],
      "\n", contexto$texto[i], "\n\n")
}
```

Uma palavra negativa pode nomear uma situação que o orador deseja combater. Também há casos em que uma palavra positiva é negada, como em "não é justo". A classificação de palavras isoladas não resolve esses problemas. No próximo tutorial, aprenderemos a olhar para as relações entre palavras.

## 7. Exercício e conclusão

Escolha outra fala do acervo ou altere os IDs em `casos`. Compare as duas contagens por mil tokens e as palavras que mais contribuíram para elas. Leia um parágrafo e explique se o resultado do léxico representa adequadamente seu sentido.

Aprendemos a relacionar tokens a um léxico, contar polaridades, comparar discursos de comprimentos diferentes e acompanhar as contagens ao longo de uma fala. O resultado é uma **medida lexical**, não uma medida de aprovação do presidente, intenção, emoção real ou avaliação de uma política.

**Referências:** [Silge e Robinson, capítulo 2](https://www.tidytextmining.com/sentiment.html); [documentação do OpLexicon 3.0](https://rdrr.io/cran/lexiconPT/man/oplexicon_v3.0.html).
