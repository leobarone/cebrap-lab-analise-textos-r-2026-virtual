# Tutorial 2 — Como representar textos para análise e modelagem?

**Aula 1 · Representações de dados textuais**  
**Referências:** Silge e Robinson, *Text Mining with R*, [capítulos 1](https://www.tidytextmining.com/tidytext), [3](https://www.tidytextmining.com/tfidf), [4](https://www.tidytextmining.com/ngrams) e [5](https://www.tidytextmining.com/dtm); Hvitfeldt e Silge, *Supervised Machine Learning for Text Analysis in R*, [capítulos 5](https://smltar.com/embeddings.html) e [8–10](https://smltar.com/dldnn.html).

No tutorial anterior, transformamos textos em palavras e construímos tabelas de frequências. Mas ainda precisamos responder a outra pergunta: **que tipo de informação sobre um documento queremos entregar para uma análise ou um modelo?**

Não há uma única maneira de representar textos como dados. Podemos armazenar frases completas, organizar uma tabela de palavras, contar quantas vezes cada termo aparece em cada documento ou representar palavras por vetores numéricos. As escolhas dependem do caminho analítico a ser seguido e dos modelos a serem utilizados.

Neste tutorial vamos comparar as alternativas de representação de dados textuais. Usaremos quatro romances de Machado de Assis, com **um livro por documento**, para estudar **bag-of-words**, **matrizes documento-termo**, **TF**, **IDF** e **TF-IDF**. Em seguida, avançaremos para **n-gramas**, **coocorrências**, **embeddings** e **sequências ordenadas**, sempre observando o que cada representação preserva, o que deixa de fora e em quais modelos podemos utilizá-la.

## O que vamos aprender

- Distinguir **corpus**, **documento**, **token**, **representação** e **modelo**.
- Entender por que uma tabela *tidy*, uma matriz documento-termo e uma sequência de tokens não preservam as mesmas informações.
- Construir uma representação **bag-of-words (BoW)** a partir das contagens de palavras.
- Reconhecer uma **DTM** (*document-term matrix*) e uma **DFM** (*document-feature matrix*) como estruturas de dados.
- Comparar contagem, presença/ausência, **TF**, **DF**, **IDF** e **TF-IDF**.
- Construir **n-gramas** e estimar um modelo simples de próxima palavra.
- Medir **coocorrências** de palavras em janelas e interpretá-las como uma rede ponderada.
- Construir **embeddings exploratórios** a partir de PMI e SVD e visualizar seus vetores.
- Preservar **sequências ordenadas** como IDs de tokens e reconhecer o tipo de entrada usado por redes recorrentes.
- Entender por que diferentes modelos podem exigir entradas diferentes.

## 1. Organização, representação e modelo não são a mesma coisa

Vamos começar separando três decisões. Ao preparar um corpus, precisamos escolher:

| Decisão | Pergunta | Exemplo |
|---|---|---|
| **Unidade de análise** | O que será considerado um documento? | Um livro, uma notícia, um capítulo, um discurso, um parágrafo, uma resposta a um questionário, etc |
| **Organização dos dados** | Como vamos guardar e manipular as observações? | Uma linha por documento, uma linha por token ou uma matriz |
| **Representação** | Que informações do texto se tornam variáveis? | Contagens, TF-IDF, n-gramas, embeddings ou sequências |
| **Modelo ou método** | Que operação analítica faremos sobre essas informações? | Comparação de vocabulário, LDA ou regressão logística |

O percurso mais comum para análise de um texto será:

**texto original → tokens → termos e contagens → representação numérica → análise ou modelo**.

Em algumas aplicações, porém, precisaremos preservar a sequência de palavras, e não apenas suas contagens. Neste tutorial, vamos começar a entender estes primeiros conceitos.

## 2. O que uma contagem de palavras consegue enxergar?

Começaremos com duas frases inspiradas em uma passagem de *Memórias Póstumas de Brás Cubas*, de Machado de Assis. Ambas utilizam **as mesmas palavras e as mesmas frequências**, mas atribuem posições diferentes a "autor" e "defunto". Incluiremos também a passagem original como terceiro documento para comparação.

```r
install.packages(c("tidyverse", "tidytext", "stopwords", "widyr", "Matrix"))
```

```r
library(tidyverse)
library(tidytext)

frases <- tibble(
  documento = c("A", "B", "C"),
  texto = c(
    "O autor era um defunto.",
    "O defunto era um autor.",
    "Eu não sou propriamente um autor defunto, mas um defunto autor, para quem a campa foi outro berço."
  )
)

frases
```

Vamos tokenizar as frases. Neste exemplo, cada palavra será um token. A função `unnest_tokens()` produz uma tabela com uma palavra por linha; em seguida, criaremos a coluna `posicao` para registrar a ordem dos termos.

Para o exemplo, manteremos artigos e outras palavras frequentes, pois não queremos introduzir ainda uma decisão de filtragem.

```r
tokens_frases <- frases %>%
  unnest_tokens(palavra, texto) %>%
  group_by(documento) %>%
  mutate(posicao = row_number()) %>%
  ungroup()

tokens_frases
```

A tabela tem uma ocorrência de palavra por linha. A coluna `posicao` registra a ordem original **neste exemplo**, em que cada documento tem apenas uma frase. Se um documento ocupasse várias linhas, precisaríamos preservar também a ordem dessas linhas antes de tokenizar.

Agora vamos agregar as ocorrências.

```r
contagens_frases <- tokens_frases %>%
  count(documento, palavra, name = "n")

contagens_frases
```

A tabela de contagens já é uma **representação bag-of-words**, pois conhecemos quais palavras aparecem em cada documento e quantas vezes aparecem. Contudo, deixamos de registrar onde aparecem e por isso é como se "picassemos" a frase e jogássemos todos as palavras (tokens) dentro de um saco, desconsiderando a ordem de ocorrência.

Vamos comparar os "sacos de palavras" das duas primeiras frases. Se as palavras e suas frequências são as mesmas, os dois sacos de palavras também são idênticos, mesmo que a ordem das palavras e o sentido das frases sejam diferentes.

```r

contagens_frases %>%
  filter(documento %in% c("A", "B")) %>%
  pivot_wider(
    names_from = documento,
    values_from = n,
    values_fill = 0
  ) %>%
  mutate(iguais = A == B)
```

Observe que todas as palavras apresentam as mesmas contagens nas duas frases. A coluna iguais será TRUE em todas as linhas. Isso significa que, para uma representação bag-of-words, as frases A e B são indistinguíveis. A representação preserva o vocabulário e as frequências, mas perde a ordem das palavras — justamente a informação que diferencia as duas primeiras frases.

## 3. De bag-of-words à matriz documento-termo (DTM)

Para visualizar essa representação, vamos passar da tabela de formato "longo" (*long*) para uma tabela de formato "largo" (*wide*).

```r
bow_frases <- contagens_frases %>%
  pivot_wider(
    names_from = palavra,
    values_from = n,
    values_fill = 0
  )

bow_frases
```

Cada linha representa um documento. Cada coluna representa uma palavra. Cada célula indica o número de ocorrências da palavra naquele documento. Podemos transformar rapidamente essa tabela em uma **matriz documento-termo (DTM, do inglês *document-term matrix*)** simples, com documentos nas linhas e palavras nas colunas. Em relação ao objeto criado acima, vamos apenas transformar a coluna dos documentos em nomes das linhas e retirá-la da matriz.

```r
matriz_frases <- bow_frases %>%
  column_to_rownames("documento") %>%
  as.matrix()

matriz_frases
```

Essa é uma DTM comum, com contagens numéricas. Como nossa matriz é pequena, podemos armazená-la diretamente. Para corpora extensos, costuma ser preferível empregar matrizes esparsas, que não armazenam todos os zeros explicitamente e, portanto, tem outro formato. Há uma breve seção sobre [matrizes esparsas](#matrizes-esparsas) ao final do tutorial.

Há diversos pacotes que têm funções específicas para essa transformação. Neste momento, porém, estamos preocupados com a intuição da organização dos dados e não com a preparação do objeto para seu uso em um modelo.

Vamos comparar os documentos A e B:

```r
matriz_frases["A", , drop = FALSE]
matriz_frases["B", , drop = FALSE]

all(matriz_frases["A", ] == matriz_frases["B", ])
```

O resultado final é `TRUE`: **a representação bag-of-words em uma DTM não distingue as duas frases**. Descartamos a ordem dos termos e, com ela, uma informação essencial para interpretar o sentido das frases.

Essa não é uma falha de implementação. É uma característica da representação que escolhemos.

### Document-term matrix (DTM) ou Document-feature matrix (DFM)?

Encontraremos outra expressão bastante semelhante: **DFM** (*document-feature matrix*), ou matriz documento-característica.

A diferença está no significado das colunas:

- **DTM (*document-term matrix*):** organiza documentos e termos. Normalmente, esses termos são palavras, mas também podem ser expressões compostas por mais de uma palavra.
- **DFM (*document-feature matrix*):** é uma denominação mais geral, pois as características (*features*) podem ser palavras, bigramas, expressões, indicadores ou outras informações extraídas dos documentos.

Na prática, os dois termos frequentemente são utilizados de maneira intercambiável. O pacote `quanteda`, por exemplo, utiliza a expressão DFM para designar a estrutura que armazena as características textuais dos documentos.

Vamos construir uma matriz cujas características são **bigramas**, isto é, pares de palavras consecutivas. Por enquanto, basta saber que essa representação preserva uma parte da ordem das palavras. Voltaremos a esse conceito mais adiante.

```r
matriz_dfm <- frases %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>%
  count(documento, bigrama) %>%
  filter(bigrama %in% c(
    "autor era", "defunto era",
    "autor defunto", "defunto autor"
  )) %>%
  pivot_wider(
    names_from = bigrama,
    values_from = n,
    values_fill = 0
  ) %>%
  column_to_rownames("documento") %>%
  as.matrix()

matriz_dfm
```

Agora, as duas primeiras frases já não produzem linhas idênticas. A primeira contém o bigrama `autor era`, enquanto a segunda contém `defunto era`. A terceira, por sua vez, contém as expressões `autor defunto` e `defunto autor`.

Isso mostra que **a informação disponível para análise depende das características que escolhemos representar**. Ao utilizar apenas palavras isoladas, perdemos sua ordem. Ao utilizar bigramas, preservamos parte dessa informação.

#### Afinal, qual é a diferença entre essas representações?

É importante distinguir três conceitos que frequentemente aparecem juntos:

| Conceito | O que descreve? |
|---|---|
| **Bag-of-words (BoW)** | Uma representação do texto que considera o vocabulário e as ocorrências das palavras, desconsiderando sua ordem. |
| **DTM / DFM** | Uma estrutura que organiza documentos nas linhas e termos ou características nas colunas. |
| **Matriz esparsa** | Uma maneira eficiente de armazenar matrizes que contêm muitos zeros. |

Esses conceitos não são excludentes. Podemos representar um corpus como bag-of-words, organizar as contagens em uma DTM e armazená-la como matriz esparsa. Também podemos construir uma DFM com valores de TF-IDF ou com frequências de bigramas.

#### Que modelos utilizam essas matrizes?

Essas representações são particularmente importantes porque muitos algoritmos de análise de textos trabalham com matrizes numéricas.

Um exemplo é o **LDA (*Latent Dirichlet Allocation*)**, modelo de tópicos que estudaremos na Aula 2. Ele utiliza contagens inteiras de palavras por documento para estimar conjuntos de palavras que tendem a aparecer juntas e a participação desses conjuntos em cada documento.

Outro exemplo são os **classificadores supervisionados**, que estudaremos na Aula 3. Nesse caso, as colunas da matriz podem funcionar como variáveis explicativas (*preditores*). O modelo procura aprender relações entre essas características e uma variável de resposta conhecida, como a categoria de um documento. As células podem conter contagens, valores binários ou pesos como TF-IDF, dependendo da preparação escolhida.

Assim, **DTM e DFM não são modelos estatísticos: são estruturas de dados que podem ser utilizadas por diferentes modelos**. O tipo de informação armazenado nas células e os pressupostos de cada algoritmo determinam como a matriz poderá ser utilizada.

Nos próximos exemplos, veremos que uma mesma coleção de documentos pode ser representada de diferentes maneiras e que essas escolhas influenciam diretamente o tipo de análise que conseguimos realizar.

## 4. A mesma estrutura pode receber pesos diferentes

Em uma DTM ou DFM, o valor da célula pode ser construído com diferentes métricas. Até agora, cada célula da matriz contém uma contagem absoluta. Mas poderíamos guardar outra informação. Veja a tabela abaixo:

| Valor da célula | O que significa | Pergunta que permite fazer |
|---|---|---|
| **Contagem (`n`)** | Quantas vezes a palavra aparece no documento | Quantas ocorrências temos? |
| **Binário (0/1)** | A palavra aparece ou não no documento | Em quais documentos ela está presente? |
| **TF** | Qual a frequência relativa do termo no documento | Que parcela dos tokens do documento corresponde à palavra? |
| **TF-IDF** | Qual a frequência do termo ponderada por sua difusão no corpus | Quais termos ajudam a distinguir um documento dos outros? |

Por exemplo, poderíamos simplesmente indicar de maneira binária se o termo está ou não presente no documento, independentemente de sua contagem. No nosso exemplo uma simples transformação produz a matriz com essa outra métrica:

```r
matriz_binaria <-  (matriz_frases > 0) * 1L

matriz_binaria
```

Quando usamos uma representação binária, uma palavra que aparece vinte vezes é tratada da mesma maneira que uma palavra que aparece apenas uma vez. Essa escolha pode ser adequada para algumas perguntas e inadequada para outras.

Nas próximas seções vamos explorar essas duas outras métricas: TF e TF-IDF. Para isso, precisaremos ir para exemplos com documentos com mais termos.

## 5. Comparando documentos: obras de Machado de Assis

Agora vamos trabalhar com quatro romances de Machado de Assis: *Memórias Póstumas de Brás Cubas*, *Quincas Borba*, *Dom Casmurro* e *Esaú e Jacó*. O arquivo de dados contém os textos completos, com **um livro por documento**. Ele está na pasta `data/` e também pode ser acessado pelo endereço público do repositório.

```r
url_dados <- paste0(
  "https://raw.githubusercontent.com/leobarone/",
  "cebrap-lab-analise-textos-r-2026-virtual/main/",
  "data/corpus_machado_4_romances.rds"
)

arquivo_dados <- "data/corpus_machado_4_romances.rds"

if (file.exists(arquivo_dados)) {
  livros <- readr::read_rds(arquivo_dados)
} else {
  conexao <- url(url_dados, open = "rb")
  livros <- readRDS(conexao)
  close(conexao)
}

livros <- livros %>%
  transmute(livro = as.character(book), texto = text)

livros %>%
  transmute(livro, caracteres = nchar(texto))
```

Vamos tokenizar os livros e contar as palavras por documento, tal como fizemos com as frases:

```r
tokens_machado <- livros %>%
  unnest_tokens(palavra, texto)

contagens_machado <- tokens_machado %>%
  count(livro, palavra, name = "n")

contagens_machado %>%
  arrange(desc(n)) %>%
  slice_head(n = 12)
```

**Não retiraremos stopwords antes de calcular o TF.** Assim, o denominador da frequência relativa corresponderá ao total de tokens resultantes desta tokenização, e não apenas aos tokens que sobraram depois de um filtro. Mais tarde poderemos selecionar termos para apresentar os resultados.

### 5.1. TF — Frequência de termos

Uma palavra que aparece cem vezes pode ter importância relativa muito diferente em um livro curto e em um livro longo. Por isso, vamos dividir a contagem de cada palavra pelo total de tokens do documento.

```r
frequencias_machado <- contagens_machado %>%
  group_by(livro) %>%
  mutate(
    total = sum(n),
    tf_manual = n / total
  ) %>%
  ungroup()

frequencias_machado %>%
  filter(palavra %in% c("morto", "defunto", "capitu")) %>%
  select(livro, palavra, n, total, tf_manual) %>%
  arrange(palavra, desc(tf_manual))
```

Chamamos essa medida de **TF (*term frequency*)**. Nesta definição, TF é uma proporção, mas há outras convenções de ponderação em diferentes pacotes e aplicações.

### 5.2. DF e IDF — A palavra aparece em quantos documentos?

Uma palavra pode ser frequente em um livro e também aparecer em todos os outros. Para distinguir documentos, precisamos de uma segunda informação: **em quantos documentos a palavra aparece?**

**DF (*document frequency*)** é o número de documentos que contêm a palavra. **IDF (*inverse document frequency*)** atribui menor peso às palavras presentes em muitos documentos e maior peso às que aparecem em poucos.

Na formulação utilizada por `tidytext`:

\[
TF(t,d) = \frac{n_{t,d}}{\sum_j n_{j,d}}
\]

\[
IDF(t) = \ln\left(\frac{N}{DF(t)}\right)
\]

Aqui, `N` é o número de documentos do corpus, e `DF(t)` é a quantidade de documentos que contêm a palavra `t`.

```r
numero_documentos <- n_distinct(contagens_machado$livro)

difusao <- contagens_machado %>%
  count(palavra, name = "df") %>%
  mutate(idf_manual = log(numero_documentos / df))

difusao %>%
  filter(palavra %in% c("morto", "defunto", "amor", "capitu")) %>%
  arrange(desc(idf_manual))
```

Se uma palavra aparece em todas as obras analisadas, seu IDF é zero nessa formulação. Isso **não significa que ela seja desprovida de significado**: significa que não ajuda a diferenciar os livros desta coleção por esse critério.

### 5.3. TF-IDF — Frequência ponderada pela difusão no corpus

Combinando as duas medidas, temos:

\[
TF\text{-}IDF(t,d) = TF(t,d) \times IDF(t)
\]

Em R, a função `bind_tf_idf()` acrescenta TF, IDF e TF-IDF à tabela de contagens.

```r
tfidf_machado <- contagens_machado %>%
  bind_tf_idf(
    term = palavra,
    document = livro,
    n = n
  )

tfidf_machado %>%
  select(livro, palavra, n, tf, idf, tf_idf) %>%
  arrange(desc(tf_idf)) %>%
  slice_head(n = 12)
```

Podemos conferir se o TF calculado por `bind_tf_idf()` coincide com aquele que calculamos manualmente:

```r
frequencias_machado %>%
  select(livro, palavra, tf_manual) %>%
  left_join(
    tfidf_machado %>% select(livro, palavra, tf),
    by = c("livro", "palavra")
  ) %>%
  mutate(diferenca = abs(tf_manual - tf)) %>%
  summarise(maior_diferenca = max(diferenca))
```

Agora vamos comparar as palavras de maior TF-IDF entre os **quatro romances**. Como eles constituem todo o corpus, o IDF foi calculado considerando exatamente esses quatro documentos.

```r
termos_caracteristicos <- tfidf_machado %>%
  group_by(livro) %>%
  slice_max(tf_idf, n = 8, with_ties = FALSE) %>%
  ungroup()

termos_caracteristicos %>%
  mutate(palavra = reorder_within(palavra, tf_idf, livro)) %>%
  ggplot(aes(palavra, tf_idf)) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ livro, scales = "free_y") +
  labs(
    title = "Termos que distinguem quatro romances de Machado de Assis",
    x = NULL,
    y = "TF-IDF"
  )
```

Alguns nomes próprios podem se destacar. Eles podem ajudar a **identificar uma obra**, mas não necessariamente a descrever seus temas centrais. TF-IDF mede a relevância **distintiva em relação a uma coleção**; não é uma medida automática de relevância substantiva.

Se trocarmos as obras por trechos menores, as frequências de documento e os pesos IDF mudarão. A unidade de análise faz parte da definição da medida.

## 6. Duas DTMs, dois conteúdos diferentes

Vamos colocar lado a lado duas matrizes construídas com **os mesmos quatro romances e as mesmas palavras**. Para não converter todo o vocabulário em uma matriz densa, selecionaremos apenas quatro termos.

```r
# Uma palavra característica de cada romance
palavras_exemplo <- tfidf_machado %>%
  group_by(livro) %>%
  slice_max(tf_idf, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  pull(palavra) %>%
  c("casa", .) %>%
  unique()

# DTM com contagens
matriz_contagens <- contagens_machado %>%
  filter(palavra %in% palavras_exemplo) %>%
  pivot_wider(
    names_from = palavra,
    values_from = n,
    values_fill = 0
  ) %>%
  column_to_rownames("livro") %>%
  as.matrix()

# DTM com TF-IDF
matriz_tfidf <- tfidf_machado %>%
  filter(palavra %in% palavras_exemplo) %>%
  select(livro, palavra, tf_idf) %>%
  pivot_wider(
    names_from = palavra,
    values_from = tf_idf,
    values_fill = 0
  ) %>%
  column_to_rownames("livro") %>%
  as.matrix()

matriz_contagens
round(matriz_tfidf, 4)
```

Observe que as duas matrizes possuem a mesma organização: **os quatro romances nas linhas e as mesmas palavras nas colunas**. O que muda é a informação armazenada em cada célula.

Na primeira matriz, temos a quantidade de vezes que cada palavra aparece em cada romance. Na segunda, temos seu peso de TF-IDF.

Repare especialmente na palavra `casa`. Ela aparece muitas vezes nos quatro livros, mas recebe TF-IDF igual a zero. Isso acontece porque, embora seja frequente, ela não distingue nenhum dos romances dos demais.

As outras palavras foram escolhidas justamente por apresentarem os maiores valores de TF-IDF em suas respectivas obras. Elas não precisam ser as palavras mais frequentes: o que importa é sua frequência relativa no documento combinada com sua distribuição pelos demais livros.

Esse exemplo nos ajuda a perceber que **contagens e TF-IDF respondem a perguntas diferentes**. A contagem informa quanto uma palavra é utilizada; o TF-IDF procura identificar palavras que caracterizam um documento em relação aos outros.

As duas representações podem ser organizadas em uma DTM. Portanto, **DTM é uma estrutura de organização dos dados, enquanto contagens e TF-IDF são maneiras diferentes de representar numericamente as informações textuais**.


## 7. N-gramas — quando a ordem das palavras importa

Com bag-of-words, as frases "O autor era um defunto" e "O defunto era um autor" são indistinguíveis. Uma forma simples de recuperar **parte da ordem** é contar sequências de palavras adjacentes: dois termos formam um **bigrama**; três, um **trigrama**.

```r
frases %>%
  filter(documento %in% c("A", "B")) %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>%
  count(documento, bigrama)
```

Agora conseguimos distinguir `autor era` de `defunto era`. Podemos aplicar exatamente a mesma transformação aos romances:

```r
bigramos_machado <- livros %>%
  unnest_tokens(bigrama, texto, token = "ngrams", n = 2) %>%
  filter(!is.na(bigrama))

bigramos_machado %>%
  count(livro, bigrama, sort = TRUE) %>%
  group_by(livro) %>%
  slice_head(n = 5)
```

**Um modelo baseado em bigramas.** Podemos estimar a probabilidade de uma palavra aparecer **depois** de outra usando suas frequências observadas. É um modelo de linguagem de primeira ordem, bastante simples:

```r
modelo_bigrama <- bigramos_machado %>%
  filter(livro == "Dom Casmurro") %>%
  separate_wider_delim(bigrama, " ", names = c("atual", "seguinte")) %>%
  count(atual, seguinte, name = "n") %>%
  group_by(atual) %>%
  mutate(probabilidade = n / sum(n)) %>%
  ungroup()

modelo_bigrama %>%
  filter(atual == "de") %>%
  slice_max(probabilidade, n = 8, with_ties = FALSE)
```

O modelo aprende a distribuição da próxima palavra **condicional à palavra atual**. Ele aproveita a ordem local, mas não leva em conta o restante da frase; pares raros também produzem estimativas instáveis. Na Aula 2, voltaremos aos bigramas como ferramenta de exploração.

## 8. Coocorrência — palavras na mesma janela de contexto

Nem toda relação importante acontece entre palavras consecutivas. Duas palavras podem estar associadas porque aparecem com frequência no **mesmo trecho**, mesmo separadas por outras palavras.

Chamamos esse fenômeno de **coocorrência**. Precisamos escolher uma *janela de contexto*: neste exemplo, cada conjunto de 40 tokens consecutivos de um romance será uma janela. A ordem exata dentro dela não será usada.

Como um livro inteiro seria uma janela grande demais, vamos criar várias janelas dentro de cada obra. Para facilitar a leitura, manteremos apenas 50 palavras frequentes depois de retirar stopwords em português.

```r

stop_words_extra <- get_stopwords(language = "pt") %>% 
  bind_rows(
    tibble(
      word = c('é', 'ser', 'disse', 'capítulo', 'outra', 'cousa', 'alguma', 'vez'),
      lexicon = 'dominio_acrescido')
    )


palavras_frequentes <- tokens_machado %>%
  anti_join(stop_words_extra, by = c("palavra" = "word")) %>%
  count(palavra, sort = TRUE) %>%
  slice_head(n = 50) %>%
  pull(palavra)

janelas <- tokens_machado %>%
  group_by(livro) %>%
  mutate(janela = (row_number() - 1L) %/% 40L + 1L) %>%
  ungroup() %>%
  mutate(janela_id = paste(livro, janela, sep = "::")) %>%
  filter(palavra %in% palavras_frequentes) %>%
  distinct(janela_id, palavra)
```

Cada linha de `janelas` indica que uma palavra apareceu em determinado trecho. Vamos contar em quantas janelas cada par aparece junto, utilizando `widyr`, pacote desenvolvido no mesmo ecossistema de `tidytext`.

```r
pares <- janelas %>%
  widyr::pairwise_count(palavra, janela_id, sort = TRUE)

pares %>% slice_head(n = 10)
```

**Um modelo baseado em coocorrências.** Podemos interpretar essa tabela como uma **rede de palavras**: cada palavra é um nó, e cada par forma uma ligação cujo peso é o número de janelas compartilhadas. Uma medida simples nessa rede é a força de cada nó, isto é, a soma dos pesos de suas ligações.

```r
pares %>%
  group_by(item1) %>%
  summarise(forca = sum(n), .groups = "drop") %>%
  slice_max(forca, n = 10)
```

Uma palavra pode aparecer como nó importante porque é muito frequente. **Coocorrência não é sinônimo de associação forte**: para comparar relações levando em conta a frequência individual das palavras, podemos utilizar medidas como correlação ou PMI.

## 9. Embeddings — palavras representadas por vetores

Até aqui, cada termo apareceu como uma categoria ou como uma coluna de uma matriz. **Embeddings** representam palavras por vetores de números, de modo que padrões de uso semelhantes possam resultar em vetores próximos.

Há muitas maneiras de construir embeddings. No [capítulo 5 de Hvitfeldt e Silge](https://smltar.com/embeddings.html), uma delas começa com janelas de coocorrência, calcula **PMI** (*pointwise mutual information*) e usa **SVD** (*singular value decomposition*) para reduzir a dimensão da matriz.

A PMI compara a presença conjunta de duas palavras com aquilo que esperaríamos a partir de suas frequências individuais. Vamos calcular essa medida a partir das mesmas janelas:

```r
associacoes <- janelas %>%
  widyr::pairwise_pmi(palavra, janela_id)

associacoes %>%
  arrange(desc(pmi)) %>%
  slice_head(n = 10)
```

PMI elevada sugere que duas palavras aparecem juntas **mais do que o esperado**. Pares muito raros podem receber valores elevados, razão pela qual a interpretação deve considerar o tamanho do corpus.

**Um modelo que aprende vetores.** A SVD decompõe a matriz de associações e permite aproximá-la em menos dimensões. Vamos usar **duas dimensões**, apenas para visualizar os embeddings aprendidos:

```r
vetores <- associacoes %>%
  widyr::widely_svd(item1, item2, pmi, nv = 2)

vetores %>%
  pivot_wider(names_from = dimension, values_from = value, names_prefix = "d") %>%
  ggplot(aes(d1, d2, label = item1)) +
  geom_text(check_overlap = TRUE, size = 3) +
  labs(x = "Dimensão 1", y = "Dimensão 2")
```

Cada palavra passa a ser representada por um par de coordenadas. Com corpora maiores, usam-se frequentemente vetores com dezenas ou centenas de dimensões. **Esses dois eixos não são temas nem sentimentos**: são dimensões matemáticas de uma aproximação da matriz de associações.

Os vetores construídos aqui são **ilustrativos**, pois nosso corpus é pequeno e o vocabulário foi limitado. Em modelos supervisionados, embeddings podem ser usados como preditores, por exemplo em um classificador. Não devem ser interpretados como uma medida objetiva e universal de significado.

## 10. Sequências ordenadas — preservando a posição dos tokens

N-gramas recuperam apenas a ordem local. Para modelos que precisam acompanhar a **sequência de palavras**, podemos manter um identificador por token e registrar sua posição no texto.

Vamos converter algumas palavras iniciais de *Dom Casmurro* em números. Os números são **identificadores arbitrários**, não quantidades ou medidas de importância.

```r
vocabulário <- tokens_machado %>%
  distinct(palavra) %>%
  mutate(id = row_number())

sequencia <- tokens_machado %>%
  filter(livro == "Dom Casmurro") %>%
  mutate(posicao = row_number()) %>%
  left_join(vocabulário, by = "palavra") %>%
  select(posicao, palavra, id) %>%
  slice_head(n = 15)

sequencia
```

A mesma sequência pode ser organizada como uma matriz de **uma observação e quinze posições**, formato semelhante ao que uma arquitetura sequencial recebe após a preparação dos dados:

```r
matrix(sequencia$id, nrow = 1)
```

**Modelos que utilizam sequências.** Redes recorrentes, como **LSTM**, recebem uma sequência de IDs transformados em vetores por uma camada de embeddings e processam as posições considerando a ordem. Redes convolucionais de texto procuram padrões locais nessas sequências. O livro de Hvitfeldt e Silge apresenta essas arquiteturas nos capítulos 8 a 10; não vamos treiná-las nesta aula.

Uma sequência de quinze IDs não contém, por si só, o significado das palavras. Precisamos do **vocabulário que associa IDs a tokens** e de uma representação numérica aprendida. Além disso, o modelo continua condicionado às decisões de tokenização e aos limites de comprimento das sequências.

### Comparando as representações

| Representação | Informação privilegiada | Informação não preservada integralmente | Exemplo de uso |
|---|---|---|---|
| **Bag-of-words / contagens** | Quantidades de palavras por documento | Ordem | LDA; classificadores |
| **TF-IDF** | Frequência relativa e distinção entre documentos | Ordem e contexto | Regressão logística com textos |
| **N-gramas** | Ordem de termos adjacentes | Dependências distantes | Modelo probabilístico de próxima palavra |
| **Coocorrência** | Presença conjunta em janelas | Ordem e sintaxe | Redes e associações entre palavras |
| **Embeddings** | Similaridades distribucionais em vetores | Texto original e, em vetores estáticos, contexto particular | PMI + SVD; preditores para classificação |
| **Sequências ordenadas** | Identidade e posição dos tokens | Dependem de tokenização e limites de comprimento | LSTM e redes convolucionais |

**Perder informação nem sempre é um problema.** Uma representação simples pode atender à nossa pergunta usando menos dados e menos processamento. A escolha correta depende do problema: não existe uma representação sempre superior às demais.

Para as seções de coocorrência e embeddings, utilizamos `widyr` e `get_stopwords()` (do pacote `tidytext`, com listas providas por `stopwords`). Esses pacotes constam nas instruções de instalação. A ordem dos tokens já está preservada em `tokens_machado`, porque o corpus carregado contém **um texto por livro**.

## 11. Representação não é modelo: preparando as próximas aulas

Vamos comparar alguns procedimentos dos dois livros.

| Método ou modelo | Qual informação utiliza? | Exige uma resposta conhecida? |
|---|---|---|
| **Frequências e TF-IDF** | Contagens e distribuição das palavras pelos documentos | Não. São medidas e procedimentos descritivos, não modelos de aprendizado |
| **Sentimentos por dicionário** | Tokens associados a um léxico externo de sentimentos | Não usa rótulos de treinamento; utiliza regras predefinidas |
| **Relações entre palavras** | Bigramas, presença conjunta ou associação entre termos | Não, na exploração que faremos |
| **LDA** | **Matriz documento-termo de contagens inteiras** | Não. Estima distribuições de tópicos sem rótulos de assunto fornecidos |
| **Regressão logística para classificação** | **Variáveis numéricas** dos textos (por exemplo, TF-IDF) e rótulos conhecidos | Sim. Precisa conhecer a categoria de parte dos documentos para aprender |
| **Modelos com embeddings** | Vetores que representam termos ou documentos | Depende do método: embeddings podem alimentar modelos supervisionados ou não supervisionados |
| **Modelos sequenciais** | Tokens ordenados, frequentemente convertidos em índices e vetores | Depende do objetivo de treinamento |

O LDA e a regressão logística podem receber informações originadas do mesmo corpus, mas **não estão resolvendo a mesma pergunta**:

- No **LDA**, queremos descrever padrões de ocorrência de palavras e misturas de tópicos, **sem informar uma categoria correta** para cada documento.
- Na **classificação supervisionada**, queremos aprender uma relação entre características dos textos e uma **categoria de resposta observada**, para produzir previsões sobre documentos novos.

Um último cuidado: quando construirmos TF-IDF para alimentar um modelo supervisionado, o IDF deverá ser aprendido com os **dados de treinamento**, e não antecipadamente com toda a coleção, incluindo documentos reservados para teste. Esse é um exemplo de **vazamento de informação**, que examinaremos na Aula 3. Para nossa comparação descritiva de Machado de Assis, calculamos IDF a partir dos quatro romances porque eles constituem a coleção analisada.

## 12. Para experimentar

1. Volte à matriz das três frases. Compare `matriz_frases` com `matriz_binaria`. **Que informação foi perdida?**
2. Escolha uma obra de Machado de Assis. Liste as dez palavras mais frequentes e as dez de maior TF-IDF. **Essas listas respondem à mesma pergunta?**
3. Refaria algum dos cálculos usando **trechos de tamanho fixo** em vez de obras? Explique como a escolha mudaria o DF, o IDF e as coocorrências.
4. Imagine um corpus em que cada documento seja uma notícia. Para estudar expressões como "não aprovou", você escolheria palavras isoladas ou bigramas? O que poderia acontecer se retirasse stopwords antes de tokenizar os bigramas?
5. Um pesquisador constrói uma DTM com TF-IDF e tenta aplicá-la diretamente a um LDA que pressupõe contagens de ocorrências. **Qual é o problema conceitual dessa escolha?**
6. Outro pesquisador ajusta um classificador que deve prever a categoria de uma reclamação. Quais informações, além da representação textual, ele precisa reunir? **Quando deverá calcular o IDF?**
7. Compare as duas janelas de contexto: 20 e 80 palavras. **Como muda a interpretação das coocorrências?**
8. Por que uma rede LSTM não deve receber os IDs dos tokens como se fossem quantidades com distância numérica significativa?

## Ao final

Agora sabemos que **representar textos não significa apenas convertê-los em números**. Precisamos decidir o que conta como documento, que unidades textuais serão preservadas, quais informações serão agregadas e que partes do texto deixaremos de observar.

Na Aula 2 utilizaremos essas escolhas para comparar **sentimentos, relações entre palavras e tópicos**. Na Aula 3, veremos como construir representações textuais para **treinar e avaliar modelos de classificação**.

**Para continuar lendo:** [Silge e Robinson, capítulos 1, 3, 4 e 5](https://www.tidytextmining.com/) e [Hvitfeldt e Silge, capítulos 5 e 8–10](https://smltar.com/).


<a id="matrizes-esparsas"></a>

## Apêndice — O que é uma matriz esparsa?

Quando trabalhamos com um corpus pequeno, como o das três frases, podemos armazenar a matriz inteira sem maiores problemas. Mas imagine uma coleção de milhares de documentos contendo dezenas de milhares de palavras diferentes.

Provavelmente, a maioria dessas palavras não aparecerá na maioria dos documentos.

Por exemplo, uma palavra que aparece em *Dom Casmurro* pode não aparecer em *Quincas Borba*. Na matriz documento-termo, a célula correspondente terá valor zero.

Uma matriz que contém uma proporção elevada de zeros é chamada de **matriz esparsa** (*sparse matrix*).

Vamos verificar quantas células da nossa DTM contêm zeros:

```r
mean(matriz_frases == 0) * 100
```

O resultado é a porcentagem de células com valor zero.

Em coleções grandes de textos, essa proporção pode ser muito elevada. Imagine uma matriz com 10 mil documentos e 50 mil palavras diferentes. Teríamos 500 milhões de células, embora apenas uma pequena parcela delas provavelmente contivesse valores diferentes de zero.

Para evitar o desperdício de memória, podemos utilizar uma representação esparsa. Nesse formato, armazenamos principalmente as posições e os valores das células diferentes de zero, em vez de guardar explicitamente todos os zeros.

O pacote `Matrix` permite construir esse tipo de objeto:

```r
matriz_esparsa <- Matrix::Matrix(
  matriz_frases,
  sparse = TRUE
)

matriz_esparsa
```

Observe que o R apresenta os valores diferentes de zero e representa as demais posições por pontos (`.`).

Podemos recuperar a matriz convencional:

```r
as.matrix(matriz_esparsa)
```

E verificar que os valores não foram modificados:

```r
all(as.matrix(matriz_esparsa) == matriz_frases)
```

O resultado será `TRUE`.

Portanto, **a matriz esparsa não é uma nova maneira de interpretar os textos**. Ela é uma maneira mais eficiente de armazenar os mesmos dados. A vantagem aparece principalmente quando a matriz é muito grande e contém muitos zeros. Em exemplos pequenos, o armazenamento esparso pode até ocupar mais memória, devido à estrutura adicional necessária.