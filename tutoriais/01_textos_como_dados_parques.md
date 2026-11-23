# Tutorial 1 — Textos como dados: dos parques nacionais às palavras

**Aula 1 · Preparação e análise de textos em R com `stringr` e `tidytext`**

Se você participou do curso [Captura de dados na internet com R](https://github.com/leobarone/cebrap-lab-raspagem-r-2026-virtual), já teve contato com a tarefa de trazer dados da internet para o R. Coletar uma coleção de documentos pode ser um desafio: os textos podem estar em páginas da internet, relatórios, documentos administrativos, obras literárias ou arquivos PDF.

Neste curso, começaremos **depois da coleta**: partiremos de um *corpus* já organizado para aprender a preparar os textos e transformá-los em dados que possamos analisar. Nesta aula, utilizaremos os textos dos verbetes da Wikipédia sobre os parques nacionais brasileiros. A [lista de parques na Wikipédia](https://pt.wikipedia.org/wiki/Lista_de_parques_nacionais_do_Brasil) foi o ponto de partida da coleta.

Começaremos com o pacote **`stringr`**, que permite procurar, substituir e transformar trechos de texto. Depois usaremos **`tidytext`**, que organiza textos como um *data frame*, com um token por linha. Essa organização permite recorrer às mesmas ferramentas do `tidyverse` que utilizamos para analisar outros tipos de dados.

## O que vamos aprender

- Reconhecer um *corpus*, seus documentos, identificadores, textos e metadados.
- Inspecionar a base recebida antes de transformá-la.
- Remover acentos, padronizar caixa, tratar pontuação e espaços com `stringr`.
- Substituir e remover padrões e usar expressões regulares simples.
- Identificar quais documentos mencionam determinados termos e quantas vezes.
- Tokenizar textos com `tidytext`, mantendo a ligação com os documentos.
- Comparar listas de *stopwords*, ampliá-las com termos próprios, calcular frequências e produzir gráficos.
- Trabalhar com bigramas, trigramas e uma nuvem de palavras.

## 1. Preparação: pacotes e corpus

Os pacotes que utilizaremos são:

- **`stringr`**: manipulação de textos, com funções cujo nome começa por `str_`.
- **`tidyverse`**: leitura, organização, transformação e visualização de dados.
- **`tidytext`**: tokenização e organização de textos em formato *tidy*.
- **`tm`**: uma maneira de consultar listas de *stopwords* em português.
- **`stopwords`**: fornece os vocabulários consultados por `tidytext::get_stopwords()`.
- **`wordcloud`**: construção de nuvens de palavras.

Se for necessário instalá-los, execute uma vez:

```r
install.packages(c("stringr", "tidyverse", "tidytext", "tm", "stopwords", "wordcloud"))
```

Agora carregue os pacotes:

```r
library(stringr)
library(tidyverse)
library(tidytext)
library(tm)
library(wordcloud)
```

### O que é um corpus?

Chamamos de **corpus** uma coleção de documentos textuais escolhidos para determinado trabalho. Podemos armazená-la como uma lista, um objeto especializado ou um *data frame*. Na nossa tabela, cada linha será um documento: o texto de uma página de parque nacional.

Para trabalhar com os documentos, precisamos, no mínimo, de um **identificador** e do **conteúdo textual**. Outras variáveis — nome do parque, localização, data de criação, endereço da página — são **metadados** que poderão ser úteis em análises posteriores.

Vamos ler o **CSV completo** dos parques nacionais. O código procura primeiro o arquivo na pasta `data/` e, caso ele não esteja disponível, utiliza o endereço público deste repositório. Enquanto o arquivo não estiver publicado aqui, recorre à fonte original da coleta. Observe que o separador do CSV é ponto e vírgula; por isso, usamos `read_csv2()` em vez de `read_csv()`.

```r
url_dados <- paste0(
  "https://raw.githubusercontent.com/leobarone/",
  "cebrap-lab-analise-textos-r-2026-virtual/main/",
  "data/tabelas-parques-utf8.csv"
)

arquivo_dados <- "data/tabelas-parques-utf8.csv"
origem_dados <- if (file.exists(arquivo_dados)) arquivo_dados else url_dados

corpus_docs <- tryCatch(
  readr::read_csv2(
    origem_dados,
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  ),
  error = function(e) readr::read_csv2(
    "https://raw.githubusercontent.com/leobarone/aula-cci-webscraping-text/master/data/tabelas-parques-utf8.csv",
    locale = readr::locale(encoding = "UTF-8"),
    show_col_types = FALSE
  )
) %>%
  rename(
    url_parque = url_pagina_parque,
    texto = texto_parque
  )

glimpse(corpus_docs)
names(corpus_docs)
```

As colunas do arquivo original são `id`, `nome`, `imagem`, `localizacao`, `data_de_criacao`, `area`, `visitantes_2021_6`, `descricao`, `url_pagina_parque` e `texto_parque`. Renomeamos **somente** as duas últimas colunas para `url_parque` e `texto`.

Vamos observar as primeiras linhas da tabela:

```r
corpus_docs %>%
  select(id, nome, localizacao, data_de_criacao, url_parque, texto) %>%
  head()
```

### Conferindo o que recebemos

Antes de modificar as strings, convém identificar textos ausentes, identificadores repetidos e tamanhos muito diferentes.

```r
corpus_docs %>%
  summarise(
    documentos = n(),
    ids_distintos = n_distinct(id),
    textos_ausentes = sum(is.na(texto) | str_squish(texto) == "", na.rm = TRUE),
    textos_repetidos = n() - n_distinct(texto)
  )

corpus_docs %>%
  mutate(tamanho_original = str_length(texto)) %>%
  select(id, nome, tamanho_original) %>%
  arrange(desc(tamanho_original)) %>%
  slice_head(n = 10)
```

Nem toda página encontrada na lista tem necessariamente um verbete completo. Há, por exemplo, registros cujo conteúdo é uma mensagem do editor da Wikipédia, e não um texto sobre o parque. Podemos localizá-los sem excluí-los automaticamente:

```r
corpus_docs %>%
  filter(str_detect(texto, fixed("O editor será agora carregado"))) %>%
  select(id, nome, url_parque)
```

Uma análise real precisaria decidir como lidar com esse problema. **Neste tutorial, não eliminaremos linhas da base original**: o objetivo é aprender as operações usando os dados completos e saber identificar suas limitações.

Guardemos uma cópia para poder retornar ao texto tal como foi recebido:

```r
corpus_docs_original <- corpus_docs
```

## 2. Limpeza e preparação de textos com `stringr`

As funções mais utilizadas do `stringr` começam com `str_` e recebem um vetor de strings como primeiro argumento. Podemos usá-las diretamente em um vetor ou dentro de `mutate()`, para transformar uma coluna de um *data frame*.

A preparação não é neutra: retirar acentos, pontuação, números ou nomes próprios pode facilitar certas contagens, mas também eliminar informações importantes. Vamos observar o que cada transformação faz.

### Remover acentos

Comecemos por uma função simples baseada em `iconv()`, útil para padronizar diferentes formas de escrita de uma palavra:

```r
remove_acentos <- function(x) {
  iconv(x, to = "ASCII//TRANSLIT")
}

remove_acentos("Mata Atlântica, conservação e proteção")
```

Agora criaremos uma coluna que conserva o texto de origem e modificaremos apenas a coluna `texto`, utilizada nos exercícios:

```r
corpus_docs <- corpus_docs %>%
  mutate(
    texto_original = texto,
    texto = remove_acentos(texto)
  )
```

Assim, `texto_original` preserva a versão com acentos. A remoção de acentos é útil para estas buscas, mas não deve ser considerada obrigatória em toda análise de texto em português.

### Padronizar minúsculas e maiúsculas

O R distingue `Parque` de `parque` quando comparamos strings diretamente. A função `str_to_lower()` transforma os caracteres em minúsculas. `str_to_upper()` transforma em maiúsculas, e `str_to_title()` coloca a inicial de cada palavra em maiúscula.

```r
str_to_lower("Parque Nacional")
str_to_upper("Parque Nacional")
str_to_title("parque nacional do alto cariri")
```

Como queremos contar palavras independentemente de serem maíusculas ou minúsculas, aplicaremos a transformação ao corpus:

```r
corpus_docs <- corpus_docs %>%
  mutate(texto = str_to_lower(texto))
```

### Remover pontuação

As **expressões regulares** permitem definir padrões de caracteres. A classe `[[:punct:]]` representa caracteres de pontuação.

Podemos removê-los com `str_remove_all()`:

```r
str_remove_all("parque-nacional, no Brasil!", "[[:punct:]]")
```

Observe que a remoção pura do hífen pode **colar duas palavras** (`parquenacional`). Por essa razão, no corpus vamos substituir a pontuação por um espaço usando `str_replace_all()`. O efeito é semelhante para as frequências, mas preserva a separação entre termos:

```r
corpus_docs <- corpus_docs %>%
  mutate(texto = str_replace_all(texto, "[[:punct:]]", " "))
```

Esse tratamento também remove pontuações que poderiam interessar a outras pesquisas. Por exemplo, uma análise de números, citações ou abreviaturas pediria outro procedimento.

### Normalizar espaços

As páginas coletadas contêm quebras de linha, tabulações e múltiplos espaços. A função `str_squish()` remove os espaços no começo e no fim da string e comprime sequências internas de espaços em apenas um.

```r
str_squish("  Parque   Nacional \n do     Brasil  ")

corpus_docs <- corpus_docs %>%
  mutate(texto = str_squish(texto))
```

Vejamos os primeiros mil caracteres de um dos textos preparados. `slice(2)` escolhe a segunda linha para **inspeção**, sem restringir o restante do tutorial:

```r
corpus_docs %>%
  slice(2) %>%
  pull(texto) %>%
  str_sub(1, 1000)
```

### Substituir trechos com `str_replace()` e `str_replace_all()`

`str_replace()` modifica **apenas a primeira ocorrência** do padrão em cada string; `str_replace_all()` modifica **todas as ocorrências**.

Vamos comparar as funções em um texto retirado do corpus. **Este exercício não sobrescreve `corpus_docs`**, pois não queremos que a substituição por `PN` altere as próximas contagens.

```r
texto_exemplo <- corpus_docs %>%
  slice(2) %>%
  pull(texto)

str_sub(
  str_replace(texto_exemplo, "parque nacional", "PN"),
  1, 350
)

str_sub(
  str_replace_all(texto_exemplo, "parque nacional", "PN"),
  1, 350
)
```

Quando a tarefa é apenas retirar um padrão, podemos usar `str_remove()` (primeira ocorrência) ou `str_remove_all()` (todas). No exemplo abaixo, removemos a palavra `brasileiro` do texto de demonstração:

```r
str_sub(str_remove(texto_exemplo, "brasileiro"), 1, 350)
str_sub(str_remove_all(texto_exemplo, "brasileiro"), 1, 350)
```

Essas funções funcionam também dentro de `mutate()`. A escolha entre **criar uma nova coluna** e **sobrescrever a coluna existente** depende de nossa necessidade de preservar a informação original.

### Comprimento dos textos

Qual dos documentos do corpus é o maior? `str_length()` conta caracteres, e não palavras.

```r
corpus_docs <- corpus_docs %>%
  mutate(tamanho = str_length(texto))

corpus_docs %>%
  select(id, nome, tamanho) %>%
  arrange(desc(tamanho)) %>%
  slice_head(n = 10)
```

### Encontrar padrões com `str_detect()`, `str_which()` e `str_subset()`

Vamos procurar, como no tutorial original, as expressões **`mata atlantica`** e **`serra`**. Lembre-se de que, neste ponto, o conteúdo de `texto` está sem acentos e em minúsculas.

`str_detect()` devolve um vetor de valores lógicos, um para cada documento:

```r
str_detect(corpus_docs$texto, "mata atlantica")
str_detect(corpus_docs$texto, "serra")
```

`str_which()` retorna **as posições** das strings que contêm o padrão. São posições no vetor, não necessariamente os valores da coluna `id`:

```r
str_which(corpus_docs$texto, "mata atlantica")
str_which(corpus_docs$texto, "serra")
```

`str_subset()` devolve **os próprios textos** que correspondem à busca. Para não imprimir páginas inteiras, veremos apenas um pequeno trecho de cada resultado:

```r
str_subset(corpus_docs$texto, "mata atlantica") %>%
  str_sub(1, 300) %>%
  head(5)

str_subset(corpus_docs$texto, "serra") %>%
  str_sub(1, 300) %>%
  head(5)
```

Em um *data frame*, `str_detect()` é particularmente útil com `filter()`. Assim, preservamos os metadados dos documentos selecionados:

```r
docs_mata_atlantica <- corpus_docs %>%
  filter(str_detect(texto, "mata atlantica"))

docs_mata_atlantica %>%
  select(id, nome, tamanho) %>%
  slice_head(n = 10)
```

A seleção acima está em um **novo objeto**. Continuaremos trabalhando com o corpus completo.

Se quisermos apenas marcar os documentos, sem selecioná-los, podemos criar colunas lógicas. `str_count()` acrescenta outra informação: **quantas vezes** cada expressão aparece em cada documento.

```r
corpus_docs <- corpus_docs %>%
  mutate(
    tem_mata = str_detect(texto, "mata atlantica"),
    tem_serra = str_detect(texto, "serra"),
    n_mata = str_count(texto, "mata atlantica"),
    n_serra = str_count(texto, "serra")
  )

corpus_docs %>%
  select(id, nome, tem_mata, tem_serra, n_mata, n_serra) %>%
  arrange(desc(n_mata)) %>%
  slice_head(n = 15)
```

Uma observação importante: procurar simplesmente `serra` também encontra a sequência dentro de palavras mais longas. Uma expressão regular pode restringir a busca à palavra inteira. Em R, as barras precisam ser duplicadas:

```r
corpus_docs %>%
  mutate(
    serra_em_qualquer_posicao = str_count(texto, "serra"),
    serra_como_palavra = str_count(texto, "\\bserra\\b")
  ) %>%
  select(nome, serra_em_qualquer_posicao, serra_como_palavra) %>%
  arrange(desc(serra_como_palavra)) %>%
  slice_head(n = 10)
```

## 3. Texto em formato *tidy* com `tidytext`

Até aqui, uma linha do *data frame* representava um parque e continha um texto inteiro. Agora vamos mudar a unidade de observação.

No formato *tidy text*, **cada linha representa uma ocorrência de um token**. Um token pode ser uma palavra, uma sequência de palavras, uma frase ou outra unidade textual. Começaremos com palavras.

### Tokenização em palavras

A função `unnest_tokens()` recebe primeiro o nome da **nova coluna**, depois a coluna que contém o **texto de entrada**:

```r
docs_token <- corpus_docs %>%
  select(id, nome, url_parque, texto) %>%
  unnest_tokens(word, texto)

glimpse(docs_token)
docs_token %>% slice_head(n = 20)
```

Observe o que mudou:

- Em `corpus_docs`, cada linha é um **documento**, e `texto` contém o conteúdo completo.
- Em `docs_token`, cada linha é uma **ocorrência de palavra**, registrada em `word`.
- `id`, `nome` e `url_parque` acompanham cada ocorrência, permitindo recuperar o documento de origem.

`unnest_tokens()` converte palavras em minúsculas e descarta pontuações na tokenização padrão. Fizemos transformações semelhantes antes para aprender o funcionamento do `stringr`; em muitos projetos, algumas dessas etapas poderiam ser dispensadas.

Podemos contar quantos tokens foram produzidos por documento:

```r
docs_token %>%
  count(id, nome, sort = TRUE) %>%
  slice_head(n = 10)
```

### Stopwords: palavras muito frequentes

Palavras como `de`, `que`, `em`, `a` e `para` costumam aparecer com muita frequência. Dependendo de nossa pergunta, elas podem não ajudar a diferenciar os documentos. Por isso, algumas análises utilizam listas de **stopwords**, ou palavras de parada.

No tutorial original, recorremos à função `tm::stopwords()`. No **Tutorial 2**, utilizaremos outra função, `tidytext::get_stopwords()`. Vamos conhecer as duas e perceber que podemos ampliar qualquer uma dessas listas com termos escolhidos para uma pesquisa.

#### Duas funções para obter stopwords em português

A função `tm::stopwords("pt")` devolve um **vetor de caracteres**, isto é, uma sequência de palavras:

```r
stopwords_tm <- tm::stopwords("pt")

head(stopwords_tm, 30)
class(stopwords_tm)
```

Já `tidytext::get_stopwords()` devolve diretamente um **data frame no formato tidy**. Para obter palavras em português, também precisamos informar o idioma; caso contrário, o padrão é inglês. Sua coluna `word` contém a palavra, e `lexicon` indica a fonte da lista:

```r
stopwords_tidy <- tidytext::get_stopwords(
  language = "pt",
  source = "snowball"
)

stopwords_tidy %>% slice_head(n = 30)
glimpse(stopwords_tidy)
```

Observe as diferenças:

| Função | Resultado | Como podemos utilizá-lo |
|---|---|---|
| `tm::stopwords("pt")` | Vetor (`character`) | Podemos adicionar termos com `c()` e transformá-lo em tabela com `tibble()`. |
| `tidytext::get_stopwords(language = "pt")` | `tibble` com `word` e `lexicon` | Podemos combinar diretamente com outras tabelas usando `anti_join()` ou ampliar com `bind_rows()`. |

**As duas funções não representam necessariamente dois vocabulários diferentes.** A lista de português do `tm` e a fonte padrão `snowball` de `get_stopwords()` têm origem na família Snowball. Os resultados podem ser muito semelhantes, embora seja importante conferir diferenças entre fontes e versões dos pacotes. A função `get_stopwords()` consulta listas por meio do pacote `stopwords` e permite escolher outras fontes pelo argumento `source`.

#### Comparando o conteúdo das duas listas

Até aqui, removemos os acentos do corpus e transformamos suas palavras em minúsculas. Para comparar as listas com os tokens, precisamos aplicar **a mesma normalização** às stopwords. Isso evita contar como diferentes, por exemplo, uma palavra acentuada e a sua versão sem acento.

```r
stopwords_df <- tibble(word = stopwords_tm) %>%
  mutate(word = str_to_lower(remove_acentos(word))) %>%
  distinct(word)

stopwords_tidy_df <- stopwords_tidy %>%
  mutate(word = str_to_lower(remove_acentos(word))) %>%
  distinct(word, .keep_all = TRUE)

# Quantos termos distintos há em cada lista, após a normalização?
tibble(
  lista = c("tm", "tidytext (snowball)"),
  termos = c(nrow(stopwords_df), nrow(stopwords_tidy_df))
)
```

Contar os termos não é suficiente: duas listas podem ter o mesmo tamanho, mas conter palavras diferentes. Vamos verificar **quais termos estão exclusivamente em cada uma**, utilizando `anti_join()`:

```r
apenas_tm <- stopwords_df %>%
  anti_join(stopwords_tidy_df, by = "word")

apenas_tidytext <- stopwords_tidy_df %>%
  anti_join(stopwords_df, by = "word")

# Tamanho das diferenças
nrow(apenas_tm)
nrow(apenas_tidytext)

# Exemplos dos termos que aparecem em apenas uma das listas
apenas_tm %>% slice_head(n = 15)
apenas_tidytext %>% select(word) %>% slice_head(n = 15)
```

Se os dois resultados estiverem vazios, significa que, **depois da nossa normalização**, as listas têm os mesmos termos. Se houver linhas, teremos identificado diferenças concretas, e não apenas diferenças no formato dos objetos.

Podemos, inclusive, selecionar outra fonte com `get_stopwords()`. O exemplo a seguir utiliza a lista `stopwords-iso`, que não precisa coincidir com a lista Snowball:

```r
tidytext::get_stopwords(language = "pt", source = "stopwords-iso") %>%
  count(lexicon)
```

#### Acrescentando nossas próprias stopwords

Listas prontas não sabem qual é o assunto de nosso corpus. Como estamos estudando páginas sobre parques nacionais, talvez `parque`, `nacional` e `brasil` apareçam em tantos documentos que não ajudem a distinguir os temas específicos de cada página. Vamos **experimentar** incluí-las em uma lista personalizada, sem alterar as listas originais:

```r
termos_adicionais <- c("parque", "nacional", "brasil")

# Verifique quantas vezes esses termos aparecem antes da exclusão:
docs_token %>%
  filter(word %in% termos_adicionais) %>%
  count(word, sort = TRUE)
```

Se estivermos trabalhando com `tm::stopwords()`, basta reunir vetores com `c()` e retirar eventuais repetições com `unique()`:

```r
stopwords_tm_ampliadas <- unique(c(
  tm::stopwords("pt"),
  termos_adicionais
))

# O objeto continua sendo um vetor de caracteres.
tail(stopwords_tm_ampliadas, 10)
```

Com `tidytext::get_stopwords()`, podemos usar `bind_rows()`, exatamente como faremos no Tutorial 2. Acrescentamos uma coluna `lexicon` para deixar registrado que os novos termos foram definidos por nós:

```r
stopwords_tidy_ampliadas <- stopwords_tidy %>%
  bind_rows(
    tibble(
      word = termos_adicionais,
      lexicon = "dominio_acrescido"
    )
  ) %>%
  mutate(word = str_to_lower(remove_acentos(word))) %>%
  distinct(word, .keep_all = TRUE)

stopwords_tidy_ampliadas %>%
  filter(lexicon == "dominio_acrescido")
```

É importante distinguir **stopwords gerais da língua** das **stopwords escolhidas para a nossa pesquisa**. Remover `parque` pode fazer sentido se quisermos conhecer outras características da área protegida; seria inadequado se a pergunta fosse justamente como os textos empregam a expressão “parque nacional”.

#### Excluindo stopwords com `anti_join()`

Como no tutorial original, utilizaremos `anti_join()`: a função conserva as linhas do primeiro objeto que **não** têm correspondência no segundo. Vamos primeiro aplicar a lista convencional do `tm`, já preparada em `stopwords_df`:

```r
docs_token_sem_stop <- docs_token %>%
  anti_join(stopwords_df, by = "word")

tibble(
  etapa = c("Antes das stopwords", "Depois das stopwords"),
  tokens = c(nrow(docs_token), nrow(docs_token_sem_stop))
)
```

Agora vamos repetir a operação com a lista ampliada de `tidytext`. A coluna `lexicon` não interfere na correspondência porque informamos que a junção deve considerar apenas `word`:

```r
docs_token_sem_stop_extra <- docs_token %>%
  anti_join(stopwords_tidy_ampliadas, by = "word")

tibble(
  tratamento = c(
    "Todos os tokens",
    "Stopwords convencionais (tm)",
    "Stopwords ampliadas (tidytext)"
  ),
  ocorrencias = c(
    nrow(docs_token),
    nrow(docs_token_sem_stop),
    nrow(docs_token_sem_stop_extra)
  )
)
```

**Observe:** esta comparação depende de duas decisões: a lista de origem e os termos adicionais. Como examinamos as diferenças entre as listas anteriormente, podemos distinguir o que decorre da fonte e o que decorre de nossa ampliação. Para o restante do tutorial, vamos **manter a lista convencional do `tm`**, preservando os resultados dos exercícios originais. A lista personalizada fica disponível em `stopwords_tidy_ampliadas` para experimentarmos outras análises.

Em páginas da Wikipédia, também podemos encontrar tokens que são apenas letras isoladas, marcas de formatação ou fragmentos de referências. Vamos excluir os tokens com **um único caractere**, tal como no exemplo original, sem alterar os dados de origem:

```r
docs_token_limpo <- docs_token_sem_stop %>%
  filter(str_length(word) > 1)

docs_token_limpo %>%
  count(word, sort = TRUE) %>%
  slice_head(n = 25)
```

Importante: stopwords não são palavras "sem significado". Em uma pesquisa sobre negação, por exemplo, remover `não` seria um erro. A decisão deve ser guiada pela pergunta de pesquisa. Por isso, preservamos `docs_token`, `docs_token_sem_stop` e `docs_token_sem_stop_extra`, em vez de sobrescrever os tokens originais.

### Frequência de palavras

`count()` permite transformar as ocorrências de palavras em uma tabela de **frequências**:

```r
frequencia_palavras <- docs_token_limpo %>%
  count(word, sort = TRUE)

frequencia_palavras %>%
  slice_head(n = 30)
```

Podemos construir um gráfico de barras usando `ggplot2`. Para acompanhar o tutorial original, selecionaremos os termos com frequência **maior que 50**; dos selecionados, mostraremos os 30 mais frequentes para manter o gráfico legível. Isso **não restringe o corpus** usado no cálculo.

```r
frequencia_palavras %>%
  filter(n > 50) %>%
  slice_head(n = 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Palavras mais frequentes nos textos dos parques nacionais",
    x = NULL,
    y = "Número de ocorrências"
  )
```

Contagens brutas são um ótimo começo, mas textos mais longos oferecem mais oportunidades para qualquer palavra aparecer. Nos próximos tutoriais, estudaremos formas de comparar documentos de tamanhos diferentes, inclusive com medidas como **TF-IDF**.

## 4. Bigramas e *n-gramas*

**Tokens não precisam ser palavras isoladas.** Podemos analisar grupos de duas palavras adjacentes (**bigramas**) ou de três palavras adjacentes (**trigramas**). Isso permite observar algumas das combinações de palavras do corpus.

É importante formar os bigramas **antes de remover as stopwords**: se retirássemos palavras previamente, duas palavras que estavam separadas no texto poderiam tornar-se artificialmente adjacentes.

### Criando bigramas

```r
docs_bigram <- corpus_docs %>%
  select(id, nome, url_parque, texto) %>%
  unnest_tokens(bigram, texto, token = "ngrams", n = 2)

docs_bigram %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 25)
```

Os bigramas ainda contêm combinações com stopwords. Para poder examiná-las, primeiro separamos cada bigrama em duas colunas com `separate()`:

```r
bigrams_separados <- docs_bigram %>%
  separate(bigram, into = c("word1", "word2"), sep = " ")

bigrams_separados %>% slice_head(n = 15)
```

Agora retiramos os bigramas em que **a primeira ou a segunda palavra** pertence à lista de stopwords:

```r
bigrams_filtrados <- bigrams_separados %>%
  anti_join(stopwords_df, by = c("word1" = "word")) %>%
  anti_join(stopwords_df, by = c("word2" = "word"))

bigrams_filtrados %>%
  count(word1, word2, sort = TRUE) %>%
  slice_head(n = 25)
```

A função `unite()` permite reunir as duas colunas novamente em uma única string:

```r
bigrams_filtrados %>%
  unite(bigram, word1, word2, sep = " ") %>%
  count(bigram, sort = TRUE) %>%
  slice_head(n = 25)
```

### Quais palavras vêm antes ou depois de outra?

Como no tutorial original, podemos perguntar quais palavras aparecem **antes de `atlantica`**:

```r
bigrams_filtrados %>%
  filter(word2 == "atlantica") %>%
  count(word1, sort = TRUE)
```

Ou quais palavras aparecem **depois de `mata`**:

```r
bigrams_filtrados %>%
  filter(word1 == "mata") %>%
  count(word2, sort = TRUE)
```

Podemos reunir ambas as direções, mantendo uma coluna que informa a **posição** da palavra relacionada:

```r
bind_rows(
  bigrams_filtrados %>%
    filter(word2 == "mata") %>%
    count(word1, sort = TRUE) %>%
    rename(word = word1) %>%
    mutate(posicao = "antes de mata"),

  bigrams_filtrados %>%
    filter(word1 == "mata") %>%
    count(word2, sort = TRUE) %>%
    rename(word = word2) %>%
    mutate(posicao = "depois de mata")
) %>%
  arrange(desc(n)) %>%
  slice_head(n = 25)
```

A presença de duas palavras em um bigrama indica que elas aparecem **lado a lado** na versão preparada do texto. Isso é diferente de procurar palavras que aparecem em uma mesma frase ou em uma janela de texto, sem serem vizinhas. Voltaremos a essa distinção em aulas posteriores.

### E os trigramas?

Para analisar sequências de três palavras, mudamos `n = 2` para `n = 3`. O procedimento de separação e filtragem é semelhante:

```r
corpus_docs %>%
  select(id, nome, texto) %>%
  unnest_tokens(trigram, texto, token = "ngrams", n = 3) %>%
  separate(trigram, into = c("word1", "word2", "word3"), sep = " ") %>%
  anti_join(stopwords_df, by = c("word1" = "word")) %>%
  anti_join(stopwords_df, by = c("word2" = "word")) %>%
  anti_join(stopwords_df, by = c("word3" = "word")) %>%
  count(word1, word2, word3, sort = TRUE) %>%
  slice_head(n = 25)
```

## 5. Nuvem de palavras

Uma nuvem de palavras é outra maneira de representar as frequências do corpus. As palavras com mais ocorrências aparecem em tamanho maior. Ela é útil para uma visão inicial, embora um gráfico de barras seja mais fácil de ler e comparar com precisão.

Já calculamos as frequências em `frequencia_palavras`, portanto não precisamos repetir a tokenização nem a filtragem:

```r
set.seed(123)

wordcloud(
  words = frequencia_palavras$word,
  freq = frequencia_palavras$n,
  max.words = 100,
  random.order = FALSE
)
```

Experimente alterar `max.words` para 30 ou 200. A contagem de cada palavra não muda; apenas a quantidade de termos exibidos.

## Onde aprender mais?

- [Tutorial original de preparação e análise de textos sobre os parques nacionais](https://github.com/leobarone/aula-cci-webscraping-text/blob/master/tutoriais/textos-como-dado-parques-nacionais.md).
- [Expressões regulares e `stringr` — *Ciência de Dados em R*](https://livro.curso-r.com/7-4-o-pacote-stringr.html).
- [Documentação de `tm::stopwords()`](https://search.r-project.org/CRAN/refmans/tm/html/stopwords.html).
- [Documentação de `tidytext::get_stopwords()`](https://juliasilge.github.io/tidytext/reference/get_stopwords.html).
