# Análise quantitativa de texto com R

**cebrap.lab · Curso virtual · 23, 24 e 26 de novembro de 2026**  
**Professor:** Leonardo Sangali Barone

Este repositório reúne os materiais do curso **Análise quantitativa de texto com R**. Os tutoriais e as bases de dados são disponibilizados conforme o andamento das aulas.

## Apresentação

Depois de coletar dados de páginas da internet, APIs, relatórios ou documentos, frequentemente terminamos com tabelas que contêm textos. Como transformar esses textos em dados para análise? O que conseguimos observar quando contamos palavras ou comparamos documentos? Como identificar sentimentos, relações entre palavras e temas recorrentes? E como utilizar o conteúdo textual para prever uma categoria de interesse?

O curso percorre essas perguntas utilizando **R** e ferramentas do ecossistema **`tidyverse`**. O curso dá continuidade à oficina [Captura de dados na internet com R](https://github.com/leobarone/cebrap-lab-raspagem-r-2026-virtual): partimos dos documentos já coletados para aprender a organizá-los, prepará-los, representá-los e analisá-los.

O percurso está dividido em três momentos. No primeiro, aprenderemos a tratar **texto como dado**, da preparação de um corpus às representações textuais utilizadas em análises e modelos. No segundo, utilizaremos a **modelagem de tópicos** como exemplo de aprendizado não supervisionado para explorar coleções de textos. Teremos também materiais complementares sobre análise de sentimentos e relações entre palavras. No terceiro, abordaremos **aprendizado supervisionado**, acompanhando as etapas que vão da definição de um problema à avaliação de previsões.

Mais do que conhecer funções ou algoritmos isolados, o objetivo é compreender as decisões que conectam uma pergunta de pesquisa, a organização dos textos, a escolha de uma representação e a interpretação dos resultados.

## Encontros e carga horária

| Encontro | Data | Tema | Carga horária |
|---|---|---|---:|
| **Aula 1** | **23/11/2026 (segunda-feira)** | Texto como dado: preparação, representações e perda de informação | 3 horas |
| **Aula 2** | **24/11/2026 (terça-feira)** | Aprendizado não supervisionado: do TF-IDF à modelagem de tópicos com LDA | 3 horas |
| **Aula 3** | **26/11/2026 (quinta-feira)** | Aprendizado supervisionado: preparação, treinamento e avaliação de modelos | 3 horas |

**Modalidade:** encontros virtuais síncronos. **Horários:** a confirmar, no horário de Brasília.

**Carga horária total:** 15 horas, sendo **9 horas síncronas** e **6 horas assíncronas**, distribuídas em duas atividades de três horas.

## Público e preparação

A oficina é destinada a estudantes, pesquisadores e profissionais que desejam analisar coleções de textos em R. Para acompanhar os exercícios, é recomendável conhecer operações básicas com objetos, vetores, *data frames* e funções do `dplyr`. Não é necessário ter experiência prévia com `tidytext` ou aprendizado de máquina.

Quem precisar rever esses conteúdos pode começar pelo **[Tutorial 0 — Preparação para trabalhar com R](tutoriais/00_tutorial_inicial_r.md)**. Essa revisão é opcional e apresenta os conceitos e operações de R utilizados nos exemplos.

## Programa das aulas

### Aula 1 — Texto como dado: preparação e representações

**Segunda-feira, 23/11 · 3 horas · Tutoriais 1 e 2**

Neste primeiro encontro, vamos reconhecer o que constitui um documento, inspecionar seus metadados e preparar o conteúdo textual. Depois, transformaremos textos em tokens, calcularemos frequências e estudaremos as formas de representar palavras e documentos numericamente.

A questão que orienta o encontro é: **o que cada transformação permite observar e que informações do texto deixam de ser preservadas?**

#### Tutorial 1 — Textos como dados: dos parques nacionais às palavras

Utilizaremos um *corpus* de textos de verbetes da Wikipédia sobre os parques nacionais brasileiros. Trabalharemos com `stringr`, expressões regulares, tokenização, *stopwords*, frequências, bigramas, trigramas e nuvens de palavras.

**[Ler o tutorial](tutoriais/01_textos_como_dados_parques.md)** · [Versão R Markdown](tutoriais/01_textos_como_dados_parques.Rmd)

#### Tutorial 2 — Como representar textos para análise e modelagem?

Com exemplos curtos e quatro romances de Machado de Assis, estudaremos *bag-of-words*, matrizes documento-termo (DTM/DFM), contagens, presença e ausência de palavras, TF, IDF e TF-IDF. Em seguida, examinaremos n-gramas, coocorrências, embeddings e sequências ordenadas para distinguir **organização dos dados**, **representação textual** e **modelo**.

**[Ler o tutorial](tutoriais/02_representacoes_textuais.md)** · [Versão R Markdown](tutoriais/02_representacoes_textuais.Rmd)

### Aula 2 — Exploração e aprendizado não supervisionado

**Terça-feira, 24/11 · 3 horas · Tutorial 3**

Na segunda aula, passaremos da representação dos textos à exploração de seus conteúdos. A referência principal será o livro *Text Mining with R*, de Julia Silge e David Robinson. Utilizaremos um *corpus* de discursos de Luiz Inácio Lula da Silva em seu terceiro mandato, coletados no site oficial da Presidência da República.

A aula será dedicada à **modelagem de tópicos**, retomando as representações estudadas no encontro anterior. A questão que orienta a análise é: **como identificar padrões de vocabulário e descobrir os assuntos presentes em uma coleção de documentos sem classificá-los previamente?**

#### Tutorial 3 — Dos pesos TF-IDF à modelagem de tópicos nos discursos de Lula

Retomaremos a tokenização, as contagens e o TF-IDF, identificando palavras que distinguem cada discurso dos demais. Em seguida, construiremos uma matriz documento-termo de contagens e ajustaremos um modelo LDA (*Latent Dirichlet Allocation*) com seis tópicos.

Vamos compreender a lógica probabilística do modelo, distinguindo **beta**, a distribuição de palavras em cada tópico, e **gamma**, a mistura de tópicos em cada documento. A partir das palavras e dos discursos mais associados a cada tópico, discutiremos como interpretar os resultados e atribuir significado aos padrões encontrados pelo modelo.

**[Ler o tutorial](tutoriais/03_modelagem_topicos_lula.md)** · [Versão R Markdown](tutoriais/03_modelagem_topicos_lula.Rmd)

**Leituras de referência:** [Silge e Robinson, cap. 3 — TF-IDF](https://www.tidytextmining.com/tfidf), [cap. 5 — Matrizes documento-termo](https://www.tidytextmining.com/dtm) e [cap. 6 — Modelagem de tópicos](https://www.tidytextmining.com/topicmodeling).

#### Materiais complementares (opcionais)

Os tutoriais 4 e 5 serão disponibilizados ao final da aula. Eles utilizam o mesmo *corpus* de discursos e podem ser realizados de maneira independente, como aprofundamento dos métodos de exploração textual. **Não fazem parte do conteúdo a ser trabalhado durante o encontro síncrono.**

**Tutorial 4 — Análise de sentimentos nos discursos de Lula**

Utilizaremos um léxico de sentimentos em português para identificar palavras positivas e negativas, calcular saldos de polaridade e observar como esses saldos variam ao longo dos discursos. Discutiremos os limites da análise baseada em dicionários, que atribui valores às palavras a partir de um conhecimento externo, e não constitui um modelo de aprendizado não supervisionado.

**[Ler o tutorial](tutoriais/04_sentimentos_discursos_lula.md)** · [Versão R Markdown](tutoriais/04_sentimentos_discursos_lula.Rmd) · [Leitura de referência: Silge e Robinson, cap. 2](https://www.tidytextmining.com/sentiment)

**Tutorial 5 — Relações entre termos nos discursos de Lula**

Exploraremos bigramas, trigramas, coocorrências e correlações entre palavras. A partir do vocabulário associado à fome, examinaremos a diferença entre palavras que aparecem em sequência, palavras que ocorrem nos mesmos parágrafos e palavras cuja associação é mais forte do que sugerem suas frequências individuais.

**[Ler o tutorial](tutoriais/05_relacoes_entre_termos_lula.md)** · [Versão R Markdown](tutoriais/05_relacoes_entre_termos_lula.Rmd) · [Leitura de referência: Silge e Robinson, cap. 4](https://www.tidytextmining.com/ngrams)

Os três tutoriais utilizam o arquivo [`corpus_discursos_lula.rds`](data/corpus_discursos_lula.rds), que será lido diretamente do repositório pelos códigos em R. Não é necessário realizar novamente a raspagem dos discursos. Informações sobre as bases utilizadas no curso estão em [`data/README.md`](data/README.md).

### Aula 3 — Aprendizado supervisionado: do problema à avaliação

**Quinta-feira, 26/11 · 3 horas · Tutoriais 6, 7 e 8**

No terceiro encontro, estudaremos situações nas quais, além dos textos, conhecemos uma variável que queremos prever. A referência será *Supervised Machine Learning for Text Analysis in R*, de Emil Hvitfeldt e Julia Silge.

Os tutoriais deste encontro serão disponibilizados conforme o andamento do curso.

## Materiais disponíveis

| Material | Conteúdo | Arquivos |
|---|---|---|
| **Tutorial 0 (opcional)** | Revisão de R, RStudio, `tidyverse` e operações com *data frames* | [Markdown](tutoriais/00_tutorial_inicial_r.md) · [R Markdown](tutoriais/00_tutorial_inicial_r.Rmd) |
| **Tutorial 1** | Preparação, tokenização e exploração dos textos dos parques nacionais | [Markdown](tutoriais/01_textos_como_dados_parques.md) · [R Markdown](tutoriais/01_textos_como_dados_parques.Rmd) |
| **Tutorial 2** | Representações textuais e exemplos com quatro romances de Machado de Assis | [Markdown](tutoriais/02_representacoes_textuais.md) · [R Markdown](tutoriais/02_representacoes_textuais.Rmd) |
| **Tutorial 3** | TF-IDF e modelagem de tópicos com LDA nos discursos de Lula — material principal da Aula 2 | [Markdown](tutoriais/03_modelagem_topicos_lula.md) · [R Markdown](tutoriais/03_modelagem_topicos_lula.Rmd) |
| **Tutorial 4 (opcional)** | Análise de sentimentos nos discursos de Lula — material complementar da Aula 2 | [Markdown](tutoriais/04_sentimentos_discursos_lula.md) · [R Markdown](tutoriais/04_sentimentos_discursos_lula.Rmd) |
| **Tutorial 5 (opcional)** | Bigramas, trigramas, coocorrências e associações entre termos — material complementar da Aula 2 | [Markdown](tutoriais/05_relacoes_entre_termos_lula.md) · [R Markdown](tutoriais/05_relacoes_entre_termos_lula.Rmd) |

Os **Tutoriais 6 a 8** integram a programação do terceiro encontro, mas ainda não estão publicados.

## Bibliografia e referências

**Livros recomendados**

- Julia Silge e David Robinson. [*Text Mining with R: A Tidy Approach*](https://www.tidytextmining.com/). Referência para a preparação e a exploração de textos, a análise lexical de sentimentos, as relações entre palavras e a modelagem de tópicos.
- Emil Hvitfeldt e Julia Silge. [*Supervised Machine Learning for Text Analysis in R*](https://smltar.com/). Referência para representações textuais, preparação de preditores e avaliação de modelos de aprendizado supervisionado.
