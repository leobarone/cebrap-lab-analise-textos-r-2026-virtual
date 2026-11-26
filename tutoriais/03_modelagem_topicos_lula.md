
# Tutorial 3 — Dos pesos TF-IDF à modelagem de tópicos nos discursos de Lula

**Aula 2 · Modelagem não supervisionada de textos**

No Tutorial 2, usamos as frequências das palavras para construir
diferentes representações dos documentos. Vimos que a contagem de
palavras, a frequência relativa (TF) e o TF-IDF nos permitem representar
a relação entre os termos e os documentos de maneiras variadas.

Agora vamos utilizar essas ideias para explorar uma coleção de discursos
de Lula em seu terceiro mandato, coletados através de raspagem do site
oficial da Presidência da República. Primeiro, vamos identificar
palavras que distinguem cada discurso dos demais. Em seguida, vamos
construir um modelo de tópicos para investigar quais padrões de
vocabulário aparecem na coleção de textos.

O objetivo é descobrir quais são os assuntos sobre os quais os textos
versos sem precisar classificá-los manualmente e sem definir de antemão
quais são os assuntos presentes nos discursos. Trata-se, portanto, de
uma técnica exploratória. Vamos inicialmente agrupar os discursos pela
semelhança lexical e só depois de estimarmps o modelo vamos examinar as
palavras e os discursos para interpretar os tópicos encontrados, ou
seja, os assuntos representados em cada um dos grupos.

## O que vamos aprender

- Retomar a tokenização e a construção de uma tabela de contagens de
  palavras por documento.
- Usar o TF-IDF para identificar palavras distintivas de um documento.
- Construir uma matriz documento-termo e ajustar um modelo LDA (*Latent
  Dirichlet Allocation*.
- Interpretar os dois principais resultados do modelo: as palavras de
  cada tópico (beta) e a mistura de tópicos em cada discurso (gamma).

## 1. Pacotes e corpus

Vamos utilizar os pacotes abaixo. Execute a instalação apenas se ainda
não tiver algum deles.

``` r
# Execute apenas se precisar instalar os pacotes.
# install.packages(c("dplyr", "tidyr", "stringr", "tidytext", "stopwords",
#                    "ggplot2", "topicmodels", "reshape2"))
```

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.2.1     ✔ readr     2.2.0
    ## ✔ forcats   1.0.1     ✔ stringr   1.6.0
    ## ✔ ggplot2   4.0.3     ✔ tibble    3.3.1
    ## ✔ lubridate 1.9.5     ✔ tidyr     1.3.2
    ## ✔ purrr     1.2.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(stringr)
library(tidytext)
library(topicmodels)
```

Nosso corpus está armazenado no arquivo
[`data/corpus_discursos_lula.rds`](https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/corpus_discursos_lula.rds)
do repositório do curso. Vamos ler o RDS diretamente do GitHub, sem
precisar baixar uma cópia local ou ajustar o diretório de trabalho. Cada
linha da tabela corresponde a um discurso, identificado pela coluna
`id`. A coluna `texto` contém seu texto integral; `titulo` e
`data_publicacao` são metadados que poderemos utilizar para interpretar
os resultados.

Vamos definir alguns parâmetros para a nossa análise. Os parâmetros
abaixo controlam o número de tópicos, a inicialização do modelo e alguns
filtros de vocabulário. Vamos entender sua função mais adiante. Por
enquanto, mantenha os valores propostos.

``` r
k_topicos <- 6L
semente_lda <- 20261124L
df_min <- 5L              # Minimo de discursos em que o termo aparece.
df_max_prop <- 0.60       # Maxima proporcao de discursos com o termo.
ocorrencias_min <- 10L    # Minimo de ocorrencias no corpus.
tokens_min <- 50L         # Minimo de tokens retidos por discurso.
```

Agora vamos carregar o *corpus* a partir do arquivo de dados. Lembre-se
que não estamos trabalhando neste curso como construir um *corpus* e
partiremos sempre de uma coleção de documentos pronta e organizada como
*data frame*

``` r
corpus_original <- read_rds("https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/corpus_discursos_lula.rds") %>%
  mutate(id = as.character(id))

metadados <- corpus_original %>%
  select(id, titulo, data_publicacao)
```

Vamos rapidamente converster os ids em texto com `as.character()`, pois
isso facilita o uso dos identificadores nas junções posteriores.

**Uma observação sobre os dados:** neste exemplo introdutório vamos
utilizar o texto integral dos registros. A verificação de autoria, a
retirada de textos repetidos e a limpeza de notas editoriais ficam fora
do exercício. IDs únicos não garantem que todos os textos sejam
distintos nem que todos sejam falas de Lula. Para uma pesquisa
substantiva, precisaríamos examinar esses problemas.

## 2. Tokenização e limpeza

Como nos tutoriais anteriores, vamos transformar cada discurso em uma
sequência de tokens, mas primeiro precisamos definir quais palavras
desejamos retirar.

Usaremos a lista de stopwords em português do pacote `stopwords`,
acessada pela função `get_stopwords()` do `tidytext`. Vamos
complementá-la com algumas palavras muito frequentes na linguagem oral,
como “gente”, “pra” e “né”. Essa lista adicional é uma escolha do
analista, não uma lista universal de palavras sem importância.

No caso dos discursos de Lula, há muitos verbos, contração da preposição
para com artigos e outras marcas de linguagem que poderiam ser úteis
para distinguir os discursos de Lula do discurso de outras pessoas (por
exemplo, dos demais presidentes da República), mas não contribui para
distinguir entre discursos do Lula, pois são comuns a todos
independentemente do assunto.

De maneira mais geral, a remoção ou não de `stopwords` é um exercício
empírico e que deve ser feito para cada finalidade de análise.

``` r
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
```

A função `bind_rows()` reúne as duas listas. Para que isso funcione,
ambas precisam ter colunas com os mesmos nomes (`word` e `lexicon`). A
função `distinct()` evita repetições na lista final.

Agora podemos tokenizar o texto com `unnest_tokens()`. Cada linha da
nova tabela representa uma ocorrência de uma palavra em um discurso.
Logo depois, utilizamos `anti_join()` para retirar as stopwords e
`filter()` para excluir tokens compostos exclusivamente por dígitos.

``` r
tokens_lula <- corpus_original %>%
  select(id, texto) %>%
  unnest_tokens(palavra, texto) %>%
  anti_join(stopwords_extra, by = c("palavra" = "word")) %>%
  filter(!str_detect(palavra, "^\\d+$"))
```

A expressão regular `^\d+$` significa que o token inteiro deve ser
composto por um ou mais dígitos. Assim quantidades como `2023`, `100` e
`50`, bastante presentes neste *corpus* em formato numérico, são
excluídos, enquanto termos que combinam letras e números, como `g20`,
são preservados. No código em R, escrevemos `"^\\d+$"` porque a barra
invertida precisa ser escapada dentro de uma string.

## 3. Contagem de palavras por discurso

Agora que temos uma linha por ocorrência, vamos contar quantas vezes
cada palavra aparece em cada discurso.

``` r
contagens_lula <- tokens_lula %>%
  count(id, palavra, name = "n")

head(contagens_lula)
```

    ## # A tibble: 6 × 3
    ##   id                                       palavra       n
    ##   <chr>                                    <chr>     <int>
    ## 1 003f3b665a79df0df3c4c13410b69f7cf7919c83 abobrinha     1
    ## 2 003f3b665a79df0df3c4c13410b69f7cf7919c83 abraçam       1
    ## 3 003f3b665a79df0df3c4c13410b69f7cf7919c83 acabar        1
    ## 4 003f3b665a79df0df3c4c13410b69f7cf7919c83 acesso        2
    ## 5 003f3b665a79df0df3c4c13410b69f7cf7919c83 achando       1
    ## 6 003f3b665a79df0df3c4c13410b69f7cf7919c83 achava        2

Nossa tabela `contagens_lula` tem três colunas:

| Coluna    | Significado                                       |
|-----------|---------------------------------------------------|
| `id`      | Identificador do discurso                         |
| `palavra` | Palavra que aparece no discurso                   |
| `n`       | Número de ocorrências da palavra naquele discurso |

Essa é uma representação *bag-of-words*: preservamos as frequências, mas
não a ordem das palavras. A partir dessa mesma tabela podemos seguir por
dois caminhos: calcular pesos TF-IDF ou construir uma matriz de
contagens para um modelo de tópicos.

## 4. Retomando o TF-IDF

No Tutorial 2, vimos que uma palavra muito frequente em um documento não
é necessariamente uma palavra que o distingue dos demais. Por exemplo,
uma palavra pode aparecer muitas vezes em um discurso, mas também ser
comum a quase todos os outros.

O TF-IDF combina duas informações:

- **TF (term frequency):** a frequência relativa da palavra dentro do
  discurso.
- **IDF (inverse document frequency):** uma medida de quão pouco
  difundida a palavra é entre os discursos da coleção.

Se uma palavra aparece `n` vezes em um discurso que tem `total` tokens,
seu TF é `n / total`. Se ela está presente em `df` discursos de um
corpus com `N` discursos, seu IDF é `log(N / df)`. O peso TF-IDF é o
produto das duas medidas.

Vamos utilizar a mesma função `bind_tf_idf()` que conhecemos no tutorial
anterior.

``` r
tfidf_lula <- contagens_lula %>%
  bind_tf_idf(palavra, id, n)
```

**Atenção:** nesta análise retiramos as stopwords e os números antes das
contagens. O denominador do TF é, portanto, o total de tokens
**restantes** em cada discurso, e não o total de palavras do texto
original. O número `N` considera os discursos que ainda têm pelo menos
um token.

Para enxergar os elementos que entram no cálculo, vamos criar também uma
tabela com o total de tokens de cada discurso e uma tabela com o número
de discursos em que cada palavra está presente.

``` r
N <- n_distinct(contagens_lula$id)

totais <- contagens_lula %>%
  group_by(id) %>%
  summarise(total = sum(n), .groups = "drop")

difusao <- contagens_lula %>%
  count(palavra, name = "df")
```

### Quais palavras distinguem cada discurso?

Vamos selecionar as dez palavras com maior TF-IDF em cada discurso.

``` r
termos_distintivos <- tfidf_lula %>%
  group_by(id) %>%
  slice_max(tf_idf, n = 10, with_ties = FALSE) %>%
  ungroup()
```

Podemos escolher um discurso para examinar os resultados. Neste exemplo,
selecionamos o primeiro ID da tabela, sem escolher previamente um
assunto. Depois, recuperamos seu título e as informações necessárias
para conferir os pesos.

``` r
id_exemplo <- first(termos_distintivos$id)

metadados %>% 
  filter(id == id_exemplo) %>%
  pull(titulo)
```

    ## [1] "Pronunciamento do presidente Lula durante entrega das chaves dos ônibus do Programa Caminho da Escola, em Sete Lagoas/MG"

``` r
print(termos_distintivos %>%
  filter(id == id_exemplo) %>%
  left_join(totais, by = "id") %>%
  left_join(difusao, by = "palavra") %>%
  select(palavra, n, total, df, tf, idf, tf_idf) %>%
  arrange(desc(tf_idf)))
```

    ## # A tibble: 10 × 7
    ##    palavra         n total    df      tf   idf  tf_idf
    ##    <chr>       <int> <int> <int>   <dbl> <dbl>   <dbl>
    ##  1 iveco           7  1331     2 0.00526  6.07 0.0319 
    ##  2 bam             3  1331     3 0.00225  5.67 0.0128 
    ##  3 dentista        5  1331    37 0.00376  3.16 0.0119 
    ##  4 vans            3  1331     6 0.00225  4.98 0.0112 
    ##  5 escolher        7  1331   109 0.00526  2.08 0.0109 
    ##  6 consultório     3  1331     7 0.00225  4.82 0.0109 
    ##  7 ônibus          6  1331    86 0.00451  2.31 0.0104 
    ##  8 escolheriam     2  1331     1 0.00150  6.77 0.0102 
    ##  9 remédio         6  1331    94 0.00451  2.22 0.0100 
    ## 10 câncer          4  1331    39 0.00301  3.10 0.00933

Observe as colunas `n`, `total`, `df`, `tf`, `idf` e `tf_idf`. Uma
palavra pode receber um peso elevado porque é relativamente frequente
nesse discurso, porque é pouco difundida na coleção ou por uma
combinação desses fatores.

**Experimente:** substitua `id_exemplo` pelo ID de outro discurso e
repita o trecho acima. Os termos distintivos ajudam a compreender o
assunto ou a linguagem utilizada na fala?

O TF-IDF ajuda a caracterizar documentos individualmente. Mas agora
queremos identificar conjuntos de palavras que tendem a aparecer nos
mesmos documentos, mesmo quando não definimos previamente quais são os
assuntos.

## 5. Da representação ao modelo de tópicos

Um modelo de tópicos (*topic model*) procura identificar padrões
compartilhados de vocabulário em uma coleção de documentos. A ideia
fundamental é que, quando escrevemos ou falamos sobre determinados
assuntos, tendemos a utilizar certos conjuntos de palavras com maior
frequência do que outros.

Por exemplo, nos discursos de Lula sobre educação, poderíamos esperar
encontrar palavras como “escola”, “professores”, “universidade” e
“estudantes”. Em discursos sobre economia, poderíamos esperar encontrar
palavras como “emprego”, “investimento”, “crescimento” e “indústria”.
Entretanto, não precisamos estabelecer previamente quais são esses
conjuntos de palavras, nem identificar manualmente quais discursos
tratam de cada assunto. Queremos que o modelo nos ajude a descobrir
esses padrões a partir dos próprios textos.

Vamos utilizar o **LDA (Latent Dirichlet Allocation)**, um dos modelos
de tópicos mais conhecidos e utilizados na análise de textos. Para
compreender seu funcionamento, precisamos entender duas ideias
fundamentais.

### Um tópico é uma distribuição de probabilidades sobre as palavras

No LDA, um tópico não é simplesmente uma lista de palavras, mas uma
distribuição de probabilidades sobre todas as palavras do vocabulário.
Algumas palavras têm maior probabilidade de aparecer em determinado
tópico, enquanto outras têm uma probabilidade muito pequena.

Imagine que tivéssemos encontrado dois tópicos em nossa coleção de
discursos. As distribuições de probabilidades poderiam ser semelhantes
às seguintes (os valores são fictícios):

| Palavra                        | Tópico 1 | Tópico 2 |
|--------------------------------|---------:|---------:|
| escola                         |      12% |       1% |
| professores                    |      10% |       1% |
| universidade                   |       8% |       2% |
| estudantes                     |       7% |       1% |
| emprego                        |       2% |      12% |
| investimento                   |       3% |      10% |
| crescimento                    |       1% |       8% |
| indústria                      |       1% |       7% |
| Demais palavras do vocabulário |      56% |      58% |
| **Total**                      | **100%** | **100%** |

Observando essas distribuições, provavelmente interpretaríamos o tópico
1 como relacionado à educação e o tópico 2 como relacionado à economia.

Note, entretanto, que os nomes “educação” e “economia” não são
resultados produzidos pelo modelo. São interpretações que fazemos a
partir das palavras que apresentam maior probabilidade em cada tópico.

Observe também que uma mesma palavra pode aparecer em diferentes
tópicos, mas com probabilidades distintas. A palavra “investimento”, por
exemplo, pode aparecer tanto em discussões sobre economia quanto em
discussões sobre educação.

Essa distribuição de probabilidades das palavras em cada tópico é o que
vamos recuperar mais adiante com o nome **beta (β)**.

### Um documento é uma mistura de tópicos

A segunda ideia fundamental do LDA é que um documento não precisa
pertencer exclusivamente a um único tópico.

Imagine dois discursos de Lula: um pronunciamento sobre a expansão das
universidades federais e outro sobre a geração de empregos na indústria.
Mesmo que os discursos tenham assuntos principais diferentes, é
perfeitamente possível que Lula fale sobre a importância da educação
para a geração de empregos em ambos.

O modelo poderia representar esses dois discursos da seguinte maneira:

| Discurso | Tópico 1 (Educação) | Tópico 2 (Economia) |
|----|---:|---:|
| Expansão das universidades federais | 80% | 20% |
| Geração de empregos na indústria | 30% | 70% |

Em vez de atribuir cada discurso a uma única categoria, o LDA estima a
participação de cada tópico na composição de cada documento.

Essa é uma diferença importante em relação a técnicas de agrupamento que
atribuem cada documento a um único grupo. No LDA, um discurso pode
combinar vários tópicos em diferentes proporções.

Essa distribuição de tópicos em cada documento é o que vamos recuperar
mais adiante com o nome **gamma (γ)**.

### Como funciona o modelo?

Para compreender como o LDA encontra os tópicos, vamos inicialmente
imaginar o problema inverso: como poderíamos produzir um documento se já
conhecêssemos os tópicos e suas distribuições de palavras?

Imagine que quiséssemos construir um discurso artificial a partir dos
dois tópicos apresentados anteriormente. Poderíamos seguir os seguintes
passos:

1.  Definimos uma mistura de tópicos para o discurso: por exemplo, 80%
    de educação e 20% de economia.
2.  Sorteamos um tópico de acordo com essas probabilidades.
3.  Sorteamos uma palavra de acordo com a distribuição de probabilidades
    das palavras do tópico sorteado.
4.  Repetimos os passos 2 e 3 até obtermos a quantidade desejada de
    palavras.

Ao final desse processo, teríamos um documento com muitas palavras
relacionadas à educação e algumas palavras relacionadas à economia.

Evidentemente, esse procedimento não produziria um discurso coerente,
com frases, argumentos e relações gramaticais. Mas poderia produzir um
conjunto de palavras cujas frequências fossem semelhantes às encontradas
em um discurso real sobre esses assuntos.

Lembre-se de que estamos trabalhando com uma representação
*bag-of-words*, na qual a ordem das palavras não é considerada. O que
queremos explicar é a distribuição das palavras nos documentos, e não a
maneira como as frases são construídas.

É por essa razão que o LDA é chamado de **modelo probabilístico
generativo**: ele estabelece um mecanismo probabilístico hipotético
capaz de gerar os dados que observamos. Isso não significa que o modelo
descreva literalmente como uma pessoa pensa ou produz um discurso.

### Da geração de documentos à descoberta de tópicos

Na prática, estamos diante do problema inverso. Temos os discursos e
conhecemos as frequências das palavras, mas não sabemos quais são os
tópicos, quais palavras caracterizam cada tópico ou quanto cada tópico
está presente em cada discurso.

O modelo precisa estimar essas informações simultaneamente.

Intuitivamente, se determinadas palavras aparecem repetidamente nos
mesmos documentos, o modelo pode encontrar uma distribuição de
probabilidades que as reúna em um mesmo tópico. Ao mesmo tempo,
documentos que apresentam muitas dessas palavras tendem a receber uma
participação maior desse tópico.

Durante a estimação, o modelo ajusta sucessivamente suas estimativas das
distribuições de palavras e das misturas de tópicos, procurando explicar
as ocorrências de palavras que observamos na coleção de documentos.

Chamamos os tópicos de **latentes** porque eles não são diretamente
observados nos nossos dados. Observamos apenas as palavras e suas
ocorrências nos documentos. Os tópicos são estruturas que o modelo
procura inferir a partir dessas informações.

A distribuição de Dirichlet, que dá nome ao modelo, é uma distribuição
de probabilidades sobre conjuntos de proporções que somam 1. No LDA, ela
ajuda a representar as possíveis misturas de tópicos nos documentos e a
regularizar sua estimação, podendo favorecer, dependendo de seus
parâmetros, documentos mais concentrados em poucos tópicos ou
distribuídos entre vários deles.

### Quantos tópicos existem em uma coleção de documentos?

No LDA clássico, precisamos informar previamente o número de tópicos que
queremos estimar. Entretanto, não precisamos informar quais são esses
tópicos.

Neste exercício, vamos solicitar ao modelo que encontre seis tópicos nos
discursos de Lula. Poderíamos solicitar três, dez ou vinte tópicos e
obter diferentes representações da mesma coleção de documentos.

Com poucos tópicos, provavelmente encontraremos padrões de vocabulário
mais abrangentes. Com muitos tópicos, poderemos encontrar padrões mais
específicos, mas também poderemos fragmentar assuntos semelhantes em
vários componentes.

A escolha do número de tópicos faz parte do exercício de análise. Não
existe necessariamente uma única divisão correta de uma coleção de
textos em assuntos.

### Por que não utilizamos o TF-IDF no LDA?

Acabamos de aprender a utilizar o TF-IDF para representar a importância
relativa de uma palavra na distinção entre documentos. Por que, então,
não utilizar essa representação para estimar nosso modelo?

A resposta está no mecanismo probabilístico que acabamos de descrever. O
LDA procura explicar as ocorrências das palavras nos documentos, supondo
que cada ocorrência foi gerada a partir de um dos tópicos. Se uma
palavra aparece vinte vezes em um discurso, temos vinte ocorrências que
o modelo precisa explicar.

O TF-IDF, por sua vez, substitui essas contagens por pesos que dependem
tanto da frequência da palavra no documento quanto de sua presença nos
demais documentos da coleção. Esses pesos não representam mais
quantidades de ocorrências.

Isso não significa que o TF-IDF seja uma representação melhor ou pior do
que a contagem de palavras. Significa que diferentes modelos e técnicas
de análise pressupõem diferentes representações dos dados.

O LDA clássico utiliza uma matriz de **contagens inteiras de palavras**.
Por isso, voltamos ao objeto `contagens_lula`, que ainda contém as
frequências absolutas, para construir nossa matriz documento-termo e
estimar o modelo de tópicos.

### Seleção do vocabulário

Antes de construir a matriz, vamos usar alguns filtros simples.
Manteremos os termos que:

- contêm pelo menos uma letra;
- aparecem em pelo menos cinco discursos (`df_min`);
- aparecem em, no máximo, 60% dos discursos (`df_max_prop`);
- têm pelo menos dez ocorrências no corpus (`ocorrencias_min`).

Por fim, conservaremos os discursos com pelo menos 50 ocorrências
restantes (`tokens_min`). Isso reduz o vocabulário pouco informativo e
evita ajustar o modelo sobre textos que ficaram muito curtos. São
escolhas exploratórias, não valores obrigatórios.

``` r
contagens_lda <- contagens_lula %>%
  filter(str_detect(palavra, "\\p{L}")) %>%
  group_by(palavra) %>%
  filter(n() >= df_min,
         n() <= df_max_prop * N,
         sum(n) >= ocorrencias_min) %>%
  ungroup() %>%
  group_by(id) %>%
  filter(sum(n) >= tokens_min) %>%
  ungroup()
```

Como cada combinação de `id` e `palavra` aparece uma única vez na tabela
de contagens, `n()` dentro de `group_by(palavra)` corresponde ao número
de discursos em que aquele termo aparece. Já `sum(n)` corresponde ao
número de ocorrências. O filtro `str_detect(palavra, "\\p{L}")` mantém
tokens que contêm pelo menos uma letra.

### Construindo a matriz documento-termo

A função `cast_dtm()` transforma a tabela de contagens no formato
esperado pelo LDA. Temos uma linha por discurso, uma coluna por termo e,
em cada célula, o número de ocorrências daquela palavra naquele
discurso. Quando a palavra não aparece em um discurso, a contagem é
zero.

``` r
dtm_lula <- contagens_lda %>%
  cast_dtm(id, palavra, n)

print(dtm_lula)
```

    ## <<DocumentTermMatrix (documents: 867, terms: 7818)>>
    ## Non-/sparse entries: 400924/6377282
    ## Sparsity           : 94%
    ## Maximal term length: 19
    ## Weighting          : term frequency (tf)

``` r
stopifnot(nrow(dtm_lula) >= k_topicos, ncol(dtm_lula) >= k_topicos)
```

O comando `print(dtm_lula)` permite conhecer as dimensões da matriz sem
precisar imprimi-la inteira. A verificação seguinte garante que a
quantidade de discursos e de termos seja pelo menos igual à quantidade
de tópicos solicitada.

## 6. Estimando um modelo de seis tópicos

Vamos ajustar o LDA com a função `LDA()` do pacote `topicmodels`.
Indicamos o número de tópicos (`k_topicos = 6`) e uma semente (um numéro
aleatório que será usado como ponto de partida do modelo) para repetir o
ajuste em condições equivalentes.

``` r
set.seed(semente_lda)
modelo_lula <- LDA(dtm_lula, k = k_topicos,
                   control = list(seed = semente_lda))

print(modelo_lula)
```

    ## A LDA_VEM topic model with 6 topics.

O modelo recebe a matriz de contagens, e não os títulos dos discursos.
Também não informamos quais palavras devem pertencer a cada tópico. Por
isso, este é um exemplo de **aprendizado não supervisionado**.

O número seis foi escolhido para facilitar a exploração em aula. Isso
não significa que o corpus tenha exatamente seis assuntos. Com outra
quantidade de tópicos ou outra inicialização (semente), podemos
encontrar resultados diferentes. Experimentar o número de tópicos é uma
das tarefas na exploração de um *corpus* quando utilizamos este modelo.

## 7. Beta: quais palavras caracterizam cada tópico?

A primeira informação que vamos extrair do modelo é a distribuição de
palavras por tópico, chamada **beta** (β). Ela nos diz qual é a
probabilidade estimada de uma palavra dado um tópico. Para cada tópico,
as probabilidades de todas as palavras do vocabulário somam
aproximadamente 1.

Utilizamos `tidy()` para obter o resultado em uma tabela. Depois,
selecionamos as dez palavras mais prováveis de cada tópico.

``` r
beta_lula <- tidy(modelo_lula, matrix = "beta")

palavras_topicos <- beta_lula %>%
  group_by(topic) %>%
  slice_max(beta, n = 10, with_ties = FALSE) %>%
  ungroup()

print(palavras_topicos, n = Inf)
```

    ## # A tibble: 60 × 3
    ##    topic term             beta
    ##    <int> <chr>           <dbl>
    ##  1     1 casa          0.0137 
    ##  2     1 mulher        0.00817
    ##  3     1 mil           0.00568
    ##  4     1 cara          0.00562
    ##  5     1 dinheiro      0.00526
    ##  6     1 pobre         0.00525
    ##  7     1 mãe           0.00466
    ##  8     1 governador    0.00464
    ##  9     1 cidade        0.00456
    ## 10     1 prefeito      0.00450
    ## 11     2 países        0.0122 
    ## 12     2 global        0.00528
    ## 13     2 sul           0.00516
    ## 14     2 sobre         0.00482
    ## 15     2 fome          0.00441
    ## 16     2 cooperação    0.00408
    ## 17     2 amazônia      0.00384
    ## 18     2 internacional 0.00379
    ## 19     2 américa       0.00332
    ## 20     2 segurança     0.00313
    ## 21     3 educação      0.0157 
    ## 22     3 universidade  0.0113 
    ## 23     3 escola        0.00905
    ## 24     3 petrobras     0.00881
    ## 25     3 camilo        0.00517
    ## 26     3 estudar       0.00469
    ## 27     3 ensino        0.00436
    ## 28     3 mil           0.00422
    ## 29     3 crianças      0.00383
    ## 30     3 nunca         0.00363
    ## 31     4 países        0.00591
    ## 32     4 indústria     0.00581
    ## 33     4 dinheiro      0.00515
    ## 34     4 unidos        0.00494
    ## 35     4 estados       0.00489
    ## 36     4 empresários   0.00487
    ## 37     4 economia      0.00477
    ## 38     4 queremos      0.00460
    ## 39     4 questão       0.00396
    ## 40     4 bilhões       0.00391
    ## 41     5 dinheiro      0.00493
    ## 42     5 federal       0.00459
    ## 43     5 reunião       0.00442
    ## 44     5 bom           0.00383
    ## 45     5 trabalho      0.00371
    ## 46     5 congresso     0.00360
    ## 47     5 acho          0.00357
    ## 48     5 possível      0.00344
    ## 49     5 companheiros  0.00335
    ## 50     5 democracia    0.00332
    ## 51     6 mulheres      0.00822
    ## 52     6 cultura       0.00729
    ## 53     6 saúde         0.00585
    ## 54     6 ministério    0.00520
    ## 55     6 nacional      0.00469
    ## 56     6 ministra      0.00464
    ## 57     6 sociedade     0.00441
    ## 58     6 terra         0.00431
    ## 59     6 mulher        0.00427
    ## 60     6 lei           0.00426

A tabela contém `topic` (número do tópico), `term` (palavra) e `beta`
(probabilidade estimada). Podemos visualizar as palavras em um gráfico,
com um painel para cada tópico.

``` r
palavras_topicos %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col() +
  scale_x_reordered() +
  coord_flip() +
  facet_wrap(~ topic, scales = "free_y") +
  labs(title = "Os tópicos dos discursos",
       x = NULL,
       y = "Probabilidade da palavra (beta)") +
  theme_minimal() +
  theme(legend.position = "none")
```

![](03_modelagem_topicos_lula_files/figure-gfm/grafico-beta-1.png)<!-- -->

A função `reorder_within()` permite ordenar as palavras separadamente
dentro de cada tópico. O comprimento das barras indica o valor de beta,
ou seja, a probabilidade estimada da palavra naquele tópico.

**Exercício 1:** observe os seis painéis e proponha um nome para cada
tópico. Há palavras que dificultam a interpretação? Uma mesma palavra
pode ser provável em mais de um tópico, portanto essa tarefa não é uma
classificação automática.

## 8. Gamma: como os discursos combinam tópicos?

A segunda informação que podemos extrair é **gamma** (γ). Em vez de
perguntar quais palavras compõem um tópico, agora perguntamos quanto
cada tópico participa da mistura estimada de um discurso.

Cada discurso recebe seis valores de gamma, um por tópico, cuja soma é
aproximadamente 1. Podemos interpretar uma gamma elevada como uma
associação forte daquele discurso a um tópico, dentro do modelo
ajustado. Isso não significa literalmente uma porcentagem de frases ou
de tempo de fala sobre um assunto.

``` r
gamma_lula <- tidy(modelo_lula, matrix = "gamma") %>%
  rename(id = document) %>%
  mutate(id = as.character(id))
```

### Quais discursos representam melhor cada tópico?

Uma maneira de verificar nossa interpretação é recuperar os três
discursos com maior gamma em cada tópico. Como guardamos os metadados no
início, podemos reunir o resultado do modelo com os títulos e as datas
de publicação.

``` r
discursos_representativos <- gamma_lula %>%
  group_by(topic) %>%
  slice_max(gamma, n = 3, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(metadados, by = "id") %>%
  arrange(topic, desc(gamma)) %>%
  select(id, topic, gamma, titulo)

for (topico in unique(discursos_representativos$topic)) {

  cat("\nTópico", topico, "\n\n")

  titulos <- discursos_representativos %>%
    filter(topic == topico) %>%
    pull(titulo)

  cat(str_wrap(titulos, width = 50), sep = "\n\n")

  cat("\n")
}
```

    ## 
    ## Tópico 1 
    ## 
    ## Pronunciamento do presidente Lula durante visita
    ## ao Hospital Universitário de Rio Verde, em Goiás
    ## 
    ## Pronunciamento do presidente Lula durante entrega
    ## de moradias do programa Morar Carioca, no Rio de
    ## Janeiro
    ## 
    ## Pronunciamento do presidente Lula durante entrega
    ## de moradias do Minha Casa, Minha Vida em Fortaleza
    ## (CE)
    ## 
    ## 
    ## Tópico 2 
    ## 
    ## Declaração Conjunta dos Presidentes do Estado
    ## Plurinacional da Bolívia e da República Federativa
    ## do Brasil - Santa Cruz, 9 de julho de 2024
    ## 
    ## Discurso do presidente da República, Luiz Inácio
    ## Lula da Silva, em reunião com presidentes de
    ## países da América do Sul
    ## 
    ## Discurso do presidente Lula na abertura da 79ª
    ## Assembleia Geral da ONU, em Nova York
    ## 
    ## 
    ## Tópico 3 
    ## 
    ## Pronunciamento do presidente Lula durante
    ## premiação da 18ª Olimpíada Brasileira de
    ## Matemática das Escolas Públicas (OBMEP)
    ## 
    ## Pronunciamento do presidente Lula em durante
    ## cerimônia de entrega de medalhas da 20ª OBMEP
    ## 
    ## Pronunciamento do presidente Lula em durante
    ## cerimônia de entrega de medalhas da 20ª Olimpíada
    ## Brasileira de Matemática das Escolas Públicas
    ## 
    ## 
    ## Tópico 4 
    ## 
    ## Pronunciamento do presidente Lula na Reunião
    ## Empresarial Brasil-Malásia
    ## 
    ## Pronunciamento do presidente da República, Luiz
    ## Inácio Lula da Silva, em reunião com Presidência
    ## do NBD
    ## 
    ## DISCURSO do presidente da República, Luiz Inácio
    ## Lula da Silva, na abertura do Encontro Empresarial
    ## Brasil-Argentina
    ## 
    ## 
    ## Tópico 5 
    ## 
    ## Discurso do presidente Lula na cerimônia de sanção
    ## da regulamentação da reforma tributária
    ## 
    ## Pronunciamento do Presidente da República, Luiz
    ## Inácio Lula da Silva, em reunião com governadores
    ## 
    ## DISCURSO do presidente da República, Luiz
    ## Inácio Lula da Silva, na abertura da reunião com
    ## governadores e governadoras
    ## 
    ## 
    ## Tópico 6 
    ## 
    ## Discurso lido pelo presidente Lula na abertura da
    ## Bienal Internacional do Livro de São Paulo
    ## 
    ## Discurso do presidente Lula na abertura da 5ª
    ## Conferência Nacional de Políticas para as Mulheres
    ## 
    ## Pronunciamento do presidente Lula na cerimônia de
    ## entrega do Prêmio Mulheres das Águas

Observando os discursos com base nos seus títulos, parece que temos
tópicos suficientemente bem separados. Por exemplo, o tópico 3 junta 3
discursos em um mesmo tipo de evento – entrega de medalhas da OBMEP. O
tópico 2 trata de discursos de Lula em eventos internacionais e o tópico
5 de assuntos federativos.

**Exercício 2:** compare os títulos dos discursos representativos com as
palavras do gráfico de beta. Você manteria os nomes atribuídos
anteriormente aos tópicos? O mesmo discurso pode aparecer entre os mais
associados a tópicos diferentes.

### Um discurso como mistura de tópicos

Até aqui, olhamos para os discursos mais associados a cada tópico. Vamos
agora inverter a perspectiva e observar como os seis tópicos aparecem em
um único discurso.

Utilizaremos como exemplo o “DISCURSO do presidente Luiz Inácio Lula da
Silva em café da manhã com jornalistas de veículos independentes”, que,
pelo título, não conseguimos de antemão imaginar aqual tópico o discurso
pertencerá:

``` r
id_exemplo <- '4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630'

mistura_exemplo <- gamma_lula %>%
  filter(id == id_exemplo)

print(metadados %>% filter(id == id_exemplo))
```

    ## # A tibble: 1 × 3
    ##   id                                       titulo                data_publicacao
    ##   <chr>                                    <chr>                 <date>         
    ## 1 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630 DISCURSO do presiden… 2023-02-08

``` r
print(mistura_exemplo)
```

    ## # A tibble: 6 × 3
    ##   id                                       topic     gamma
    ##   <chr>                                    <int>     <dbl>
    ## 1 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630     1 0.00362  
    ## 2 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630     2 0.0837   
    ## 3 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630     3 0.0000200
    ## 4 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630     4 0.157    
    ## 5 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630     5 0.732    
    ## 6 4a8ab0a1a826ed643bbc93a5d0cc79cc24a00630     6 0.0244

``` r
print(mistura_exemplo %>%
  ggplot(aes(factor(topic), gamma)) +
  geom_col(fill = "steelblue") +
  scale_y_continuous(labels = scales::label_percent(), limits = c(0, 1)) +
  labs(title = "Mistura de tópicos em um discurso", x = "Tópico",
       y = "Participação estimada (gamma)") +
  theme_minimal())
```

![](03_modelagem_topicos_lula_files/figure-gfm/mistura-topicos-1.png)<!-- -->

Observe que o modelo pode atribuir valores diferentes de gamma a mais de
um tópico no mesmo discurso. Não é necessário que um único componente
concentre toda a distribuição.

## 10. O que aprendemos?

Partimos da mesma tabela de contagens para realizar duas operações
diferentes:

| Método | Representação utilizada | O que queremos descobrir |
|----|----|----|
| TF-IDF | Frequências relativas ponderadas pelo IDF | Palavras que distinguem um discurso dos demais |
| LDA | Matriz documento-termo de contagens | Padrões de vocabulário e suas misturas nos discursos |

Os tópicos não são categorias verdadeiras que o algoritmo simplesmente
recupera. Eles são distribuições de palavras estimadas a partir da
representação escolhida. A tarefa de interpretar essas distribuições
continua sendo nossa.

**Para experimentar:** altere `k_topicos`, `semente_lda` ou os filtros
de vocabulário no início do tutorial e execute novamente os blocos
correspondentes. Compare as palavras e os discursos associados aos
tópicos, não apenas seus números, pois os números dos componentes não
têm significado fixo.
