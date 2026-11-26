---
output:
  github_document: default
  html_document:
    toc: true
    toc_depth: 2
lang: pt-BR
encoding: UTF-8
---

# Tutorial 5 — Relações entre palavras nos discursos de Lula

**Aula 2 · N-gramas, coocorrências e correlação**

*Material complementar opcional, para exploração ao final ou após o segundo encontro.*

Nos tutoriais anteriores, contamos palavras isoladamente. Ao fazer isso, perdemos sua ordem e parte de seu contexto. Agora vamos estudar como as palavras se relacionam, seguindo o [capítulo 4 de *Text Mining with R*, de Julia Silge e David Robinson](https://www.tidytextmining.com/ngrams.html).

Vamos olhar para a palavra **fome**. Quais palavras aparecem imediatamente antes ou depois dela? Quais aparecem nos mesmos parágrafos? E quais aparecem junto dela mais do que esperaríamos pela frequência de cada uma? São três perguntas diferentes.

## 1. Pacotes e corpus

```{r instalacao, eval=FALSE}
# Execute apenas se precisar instalar os pacotes.
install.packages(c("tidyverse", "tidytext", "stopwords", "widyr"))
```

```{r pacotes, message=FALSE}
library(tidyverse)
library(tidytext)
library(widyr)
```

Vamos ler o mesmo RDS dos tutoriais anteriores diretamente do GitHub, sem depender de um arquivo local.

```{r corpus}
corpus_original <- read_rds("https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/corpus_discursos_lula.rds") %>%
  mutate(id = as.character(id))

metadados <- corpus_original %>%
  select(id, titulo, data_publicacao, url)
```

Usamos o mesmo acervo dos tutoriais anteriores. Não realizamos aqui a auditoria de autoria, textos repetidos ou transcrições com vários locutores. Retiramos apenas parágrafos vazios e rótulos editoriais iniciados por `TRANSMISSÃO |`. Para uma pesquisa substantiva, precisaríamos examinar a composição do corpus com mais cuidado.

Vamos organizar os parágrafos. Cada discurso tem sua própria numeração de parágrafos; para identificar um parágrafo em todo o corpus, reunimos o ID do discurso e o número do parágrafo.

```{r paragrafos}
paragrafos <- corpus_original %>%
  select(id, paragrafos) %>%
  unnest_longer(paragrafos, values_to = "texto", indices_to = "paragrafo") %>%
  filter(!is.na(texto), str_squish(texto) != "") %>%
  filter(!str_detect(texto, regex("^\\s*TRANSMISSÃO\\s*\\|", ignore_case = TRUE))) %>%
  mutate(paragrafo_id = paste(id, paragrafo, sep = "::"))

nrow(paragrafos)
head(paragrafos, 3)
```

## 2. De palavras a bigramas

Um n-grama é uma sequência de n palavras consecutivas. Por exemplo, na expressão *combate à fome*, há dois bigramas: `combate à` e `à fome`. Há também um trigrama: `combate à fome`.

A função `unnest_tokens()` que já conhecemos pode gerar n-gramas. Primeiro separamos as frases, para evitar que o fim de uma frase seja ligado ao início da seguinte. Depois usamos `token = "ngrams"` e `n = 2`.

```{r bigramas}
frases <- paragrafos %>%
  unnest_tokens(frase, texto, token = "sentences", to_lower = FALSE)

bigramas <- frases %>%
  unnest_tokens(bigrama, frase, token = "ngrams", n = 2) %>%
  filter(!is.na(bigrama))

bigramas %>% count(bigrama, sort = TRUE) %>% slice_head(n = 15)
```

Cada linha de `bigramas` contém uma ocorrência de duas palavras consecutivas. Como na contagem de palavras isoladas, aparecem muitas expressões de uso comum. Para retirar aquelas que contêm stopwords, precisamos separar o bigrama em duas colunas.

```{r separar}
bigramas_separados <- bigramas %>%
  separate(bigrama, into = c("palavra1", "palavra2"), sep = " ", remove = FALSE)

head(bigramas_separados)
```

### Retirando stopwords dos bigramas

Vamos recuperar a lista de stopwords e as marcas de oralidade já utilizadas no tutorial de tópicos.

```{r stopwords}
stopwords_extra <- get_stopwords(language = "pt", source = "snowball")

marcadores_orais <- c(
  "gente", "porque", "vai", "vamos", "vão", "vou", "fazer", "ser", "pode",
  "pra", "pro", "pras", "pros", "tá", "tô", "tava", "né", "aí",
  "coisa", "coisas", "sei", "sabe", "dizer", "disse", "ter", "quer",
  "queria", "quero"
)

stopwords_extra <- stopwords_extra %>%
  bind_rows(tibble(word = marcadores_orais, lexicon = "marcadores_orais")) %>%
  distinct(word, .keep_all = TRUE)

bigramas_filtrados <- bigramas_separados %>%
  anti_join(stopwords_extra, by = c("palavra1" = "word")) %>%
  anti_join(stopwords_extra, by = c("palavra2" = "word")) %>%
  filter(str_detect(palavra1, "\\p{L}"), str_detect(palavra2, "\\p{L}"))

bigramas_filtrados %>% count(bigrama, sort = TRUE) %>% slice_head(n = 15)
```

**Atenção:** aqui retiramos stopwords **depois** de formar os bigramas. Se retirássemos "do" antes, poderíamos criar a sequência artificial `banco brasil` a partir de *Banco do Brasil*. O filtro elimina pares, mas não cria novas adjacências. Alguns nomes institucionais completos também desaparecem com esse filtro.

## 3. Que expressões contêm a palavra fome?

Para estudar uma palavra específica, podemos recuperar os bigramas que contêm essa palavra na primeira ou na segunda posição. Desta vez usaremos `bigramas_separados`, antes do filtro de stopwords: queremos preservar preposições e outras palavras que formam expressões.

```{r bigramas-fome, fig.width=8, fig.height=5}
termo_foco <- "fome"

expressoes_fome <- bigramas_separados %>%
  filter(palavra1 == termo_foco | palavra2 == termo_foco) %>%
  count(bigrama, sort = TRUE) %>%
  slice_head(n = 12)

expressoes_fome %>%
  ggplot(aes(reorder(bigrama, n), n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Bigramas que contêm a palavra fome",
       x = NULL, y = "Ocorrências") +
  theme_minimal()
```

Uma sequência de duas palavras às vezes é curta demais. Vamos fazer a mesma análise para três palavras. `str_detect()` procura a palavra inteira dentro do trigrama.

```{r trigramas}
trigramas_fome <- frases %>%
  unnest_tokens(trigrama, frase, token = "ngrams", n = 3) %>%
  filter(!is.na(trigrama), str_detect(trigrama, str_c("\\b", termo_foco, "\\b"))) %>%
  count(trigrama, sort = TRUE)

trigramas_fome %>% slice_head(n = 15)
```

Compare expressões como *passando fome*, *combate à fome* e referências ao nome da Aliança Global. A mesma palavra pode aparecer em uma descrição de uma situação ou em uma proposta de ação.

### Uma ligação com análise de sentimentos

No tutorial anterior, o dicionário não sabia se uma palavra estava sendo negada. Podemos usar os bigramas para localizar palavras imediatamente precedidas por "não".

```{r negacao}
bigramas_separados %>%
  filter(palavra1 == "não") %>%
  count(palavra2, sort = TRUE) %>%
  slice_head(n = 15)
```

Agora temos informação sobre o contexto imediato. Ainda não resolvemos o problema da negação: em "não é justo", por exemplo, o bigrama `não é` não inclui "justo". O capítulo 4 usa esse tipo de informação para mostrar os limites da polaridade isolada. Não vamos inverter automaticamente o sentimento dos bigramas.

## 4. Palavras que aparecem no mesmo parágrafo

Diferentemente dos bigramas, uma coocorrência não exige que as palavras sejam vizinhas. Basta que as duas apareçam na mesma unidade de contexto. Vamos utilizar os parágrafos.

Voltamos à tokenização por palavra. Retiramos as stopwords e mantemos apenas tokens que contêm letras. A função `distinct()` garante que, se uma palavra aparecer cinco vezes no mesmo parágrafo, ela conte como uma única presença.

```{r presenca}
presencas <- paragrafos %>%
  select(paragrafo_id, texto) %>%
  unnest_tokens(palavra, texto) %>%
  anti_join(stopwords_extra, by = c("palavra" = "word")) %>%
  filter(str_detect(palavra, "\\p{L}")) %>%
  distinct(paragrafo_id, palavra)

head(presencas)
```

Queremos comparar algumas palavras substantivamente interessantes. Para manter a tabela pequena, escolhemos dez termos. Esta lista é uma escolha didática: não representa todo o vocabulário do corpus.

```{r vocabulario}
palavras_foco <- c("fome", "pobreza", "comer", "alimentação", "emprego",
                   "renda", "brasil", "mundo", "aliança", "guerra")

presencas_foco <- presencas %>%
  filter(palavra %in% palavras_foco)

presencas_foco %>% count(palavra, sort = TRUE)
```

A função `pairwise_count()` do pacote `widyr` conta em quantos parágrafos cada par de palavras aparece. A coluna `n` do resultado é uma contagem de parágrafos, não de ocorrências de bigramas.

```{r coocorrencias}
coocorrencias <- presencas_foco %>%
  pairwise_count(palavra, paragrafo_id, sort = TRUE)

coocorrencias %>% filter(item1 == termo_foco)
```

Observe a posição de "brasil" e "pobreza" na tabela. Uma palavra muito comum tem mais oportunidades de aparecer com "fome". A contagem conjunta, por si só, não indica se duas palavras têm uma associação especial.

## 5. Da coocorrência à correlação

Para distinguir uma palavra comum de uma palavra associada à fome, precisamos considerar também os parágrafos onde cada palavra **não** aparece. Vamos montar uma pequena tabela de presenças e ausências para os dez termos.

```{r zeros}
presenca_ausencia <- expand_grid(
  paragrafo_id = paragrafos$paragrafo_id,
  palavra = palavras_foco
) %>%
  left_join(presencas_foco %>% mutate(presente = 1L),
            by = c("paragrafo_id", "palavra")) %>%
  mutate(presente = replace_na(presente, 0L))

head(presenca_ausencia, 12)
```

`expand_grid()` cria todas as combinações de parágrafo e palavra. A coluna `presente` vale 1 quando a palavra aparece e 0 quando não aparece. Incluímos inclusive os parágrafos que não contêm nenhuma das dez palavras, pois fazem parte do universo de comparação.

### Um exemplo: fome e pobreza

Podemos comparar diretamente a presença de duas palavras. Transformamos as duas em colunas usando `pivot_wider()` e construímos uma tabela de contingência.

```{r tabela-dois-termos}
duas_palavras <- presenca_ausencia %>%
  filter(palavra %in% c("fome", "pobreza")) %>%
  pivot_wider(names_from = palavra, values_from = presente)

tabela <- table(fome = duas_palavras$fome,
                pobreza = duas_palavras$pobreza)
tabela

# A proporção de parágrafos com pobreza em cada grupo (linhas).
prop.table(tabela, margin = 1)
```

Leia a coluna `pobreza = 1` nos dois grupos. Ela mostra a proporção de parágrafos com pobreza dentre os parágrafos sem fome (`fome = 0`) e dentre os parágrafos com fome (`fome = 1`). Essa comparação ajuda a enxergar a associação que uma simples contagem conjunta esconde.

A correlação de Pearson entre duas variáveis binárias também é chamada de coeficiente **phi**. Podemos calculá-la para as duas palavras usando `cor()`.

```{r phi}
cor(duas_palavras$fome, duas_palavras$pobreza)
```

O valor varia entre -1 e 1. Valores positivos indicam tendência à presença conjunta, valores negativos indicam que a presença de uma tende a acompanhar a ausência da outra e valores próximos de zero indicam pouca associação linear entre as presenças. A correlação não indica causalidade ou que as palavras apareçam lado a lado.

### Comparando todos os pares

Agora vamos usar `pairwise_cor()`, também do pacote `widyr`, para calcular essa medida para todos os pares do pequeno vocabulário. Usamos a coluna `presente` para representar explicitamente zeros e uns.

```{r correlacoes}
correlacoes <- presenca_ausencia %>%
  pairwise_cor(palavra, paragrafo_id, presente, sort = TRUE)

associacoes_fome <- correlacoes %>%
  filter(item1 == termo_foco) %>%
  left_join(coocorrencias, by = c("item1", "item2")) %>%
  mutate(n = replace_na(n, 0)) %>%
  arrange(desc(correlation))

associacoes_fome
```

```{r grafico-correlacao, fig.width=8, fig.height=4.5}
associacoes_fome %>%
  ggplot(aes(reorder(item2, correlation), correlation)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(title = "Associação entre fome e outras palavras",
       x = NULL, y = "Correlação phi entre presenças nos parágrafos") +
  theme_minimal()
```

Compare agora o ranking por `n` com o ranking por `correlation`. No acervo, "brasil" aparece em muitos parágrafos com fome; "pobreza" e "aliança" são termos que merecem atenção pela associação relativa, não só pela quantidade de ocorrências conjuntas. Confira isso nas tabelas produzidas. A medida depende da unidade de contexto: parágrafos longos têm mais oportunidades de reunir palavras do que parágrafos curtos.

## 6. Voltando aos parágrafos

Precisamos verificar por que duas palavras aparecem juntas. A associação entre fome e pobreza pode refletir, em parte, o nome de uma iniciativa política: *Aliança Global contra a Fome e a Pobreza*.

Vamos recuperar alguns parágrafos em que as duas palavras aparecem, com e sem menção literal a "Aliança Global". Dentro de cada grupo, `distinct(id, .keep_all = TRUE)` evita escolher vários exemplos do mesmo discurso.

```{r contexto}
ids_com_fome_pobreza <- duas_palavras %>%
  filter(fome == 1, pobreza == 1) %>%
  select(paragrafo_id)

exemplos <- paragrafos %>%
  semi_join(ids_com_fome_pobreza, by = "paragrafo_id") %>%
  left_join(metadados, by = "id") %>%
  mutate(menciona_alianca = str_detect(texto, regex("aliança global", ignore_case = TRUE))) %>%
  group_by(menciona_alianca) %>%
  distinct(id, .keep_all = TRUE) %>%
  slice_head(n = 2) %>%
  ungroup()

for (i in seq_len(nrow(exemplos))) {
  cat("\n", exemplos$titulo[i], "\nParágrafo", exemplos$paragrafo[i],
      "\n", exemplos$texto[i], "\n\n")
}
```

Uma associação pode refletir tanto um nome institucional quanto uma relação substantiva entre dois problemas. Uma medida de associação não distingue essas interpretações sozinha.

**Exercício:** retire os parágrafos que mencionam literalmente "Aliança Global" e refaça a correlação de fome com pobreza. Para fazer isso, execute o comando abaixo e repita as etapas 4 e 5. A associação desaparece ou permanece? Leia outros exemplos para explicar o resultado.

```{r exercicio, eval=FALSE}
paragrafos <- paragrafos %>%
  filter(!str_detect(texto, regex("aliança global", ignore_case = TRUE)))
```

Para voltar à análise original, execute novamente o bloco `paragrafos` da etapa 1.

## 7. O que aprendemos?

| Representação ou medida | O que descobrimos |
|---|---|
| Bigramas e trigramas | Palavras que aparecem consecutivamente e formam expressões. |
| Coocorrência | Quantos parágrafos contêm duas palavras, independentemente da ordem. |
| Correlação phi | Associação entre presenças e ausências, considerando a frequência individual das palavras. |

No exercício, passamos de uma expressão local para uma relação entre palavras no conjunto de parágrafos. Esse caminho mostra por que a escolha de representação determina o tipo de informação que podemos recuperar de um texto.

Para experimentar, escolha outras dez palavras e outro termo de interesse em `palavras_foco` e `termo_foco`, ajustando também o exemplo das duas palavras. Repita a comparação entre contagem e correlação.

**Referência:** [Silge e Robinson, capítulo 4](https://www.tidytextmining.com/ngrams.html).
