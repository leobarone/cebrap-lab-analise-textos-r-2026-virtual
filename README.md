# Análise quantitativa de texto com R

**cebrap.lab · Curso virtual · 23, 24 e 26 de novembro de 2026**  
**Professor:** Leonardo Sangali Barone

Este repositório reúne os materiais do curso **Análise quantitativa de texto com R**. Os tutoriais e as bases de dados são disponibilizados conforme o andamento das aulas.

## Apresentação

Depois de coletar dados de páginas da internet, APIs, relatórios ou documentos, frequentemente terminamos com tabelas que contêm textos. Como transformar esses textos em dados para análise? O que conseguimos observar quando contamos palavras ou comparamos documentos? Como identificar sentimentos, relações entre palavras e temas recorrentes? E como utilizar o conteúdo textual para prever uma categoria de interesse?

O curso percorre essas perguntas utilizando **R** e ferramentas do ecossistema **`tidyverse`**. O curso dá continuidade à oficina [Captura de dados na internet com R](https://github.com/leobarone/cebrap-lab-raspagem-r-2026-virtual): partimos dos documentos já coletados para aprender a organizá-los, prepará-los, representá-los e analisá-los.

O percurso está dividido em três momentos. No primeiro, aprenderemos a tratar **texto como dado**, da preparação de um corpus às representações de textos como dado utilizadas em análises e modelos. No segundo, utilizaremos modelos **aprendizado não supervisionado** para explorarmos as coleções de textos. No terceiro, abordaremos **aprendizado supervisionado**, acompanhando as etapas que vão da definição de um problema à avaliação de previsões.

Mais do que conhecer funções ou algoritmos isolados, o objetivo é compreender as decisões que conectam uma pergunta de pesquisa, a organização dos textos, a escolha de uma representação e a interpretação dos resultados.

## Encontros e carga horária

| Encontro | Data | Tema | Carga horária |
|---|---|---|---:|
| **Aula 1** | **23/11/2026 (segunda-feira)** | Texto como dado: preparação, representações e perda de informação | 3 horas |
| **Aula 2** | **24/11/2026 (terça-feira)** | Exploração de textos: sentimentos, relações entre palavras e modelagem de tópicos | 3 horas |
| **Aula 3** | **26/11/2026 (quinta-feira)** | Aprendizado supervisionado: preparação, treinamento e avaliação de modelos | 3 horas |

**Modalidade:** encontros virtuais síncronos. **Horários:** a confirmar, no horário de Brasília.

**Carga horária total:** 15 horas, sendo **9 horas síncronas** e **6 horas assíncronas**, distribuídas em duas atividades de três horas.

## Público e preparação

A oficina é destinada a estudantes, pesquisadores e profissionais que desejam analisar coleções de textos em R. Para acompanhar os exercícios, é recomendável conhecer operações básicas com objetos, vetores, *data frames* e funções do `dplyr`. Não é necessário ter experiência prévia com `tidytext` ou aprendizado de máquina.

Quem precisar rever esses conteúdos pode começar pelo **[Tutorial 0 — Preparação para trabalhar com R](tutoriais/00_tutorial_inicial_r.md)**. Essa revisão é opcional e apresenta os conceitos e operações de R utilizados nos exemplos.

## Programa das aulas

### Aula 1 — Texto como dado: preparação e representações

**Segunda-feira, 23/11 · 4 horas · Tutoriais 1 e 2**

Neste primeiro encontro reconhecer o que constitui um documento, inspecionar seus metadados e preparar o conteúdo textual. Depois, transformaremos textos em tokens, calcularemos frequências e estudaremos as formas de representar palavras e documentos numericamente.

A questão que orienta o encontro é: **o que cada transformação permite observar e que informações do texto deixam de ser preservadas?**

#### Tutorial 1 — Textos como dados: dos parques nacionais às palavras


Utilizaremos um _corpus_ de textos de verbetes da Wikipédia sobre os parques nacionais brasileiros. Trabalharemos com `stringr`, expressões regulares, tokenização, *stopwords*, frequências, bigramas, trigramas e nuvens de palavras.

**[Ler o tutorial](tutoriais/01_textos_como_dados_parques.md)** · [Versão R Markdown](tutoriais/01_textos_como_dados_parques.Rmd)

#### Tutorial 2 — Como representar textos para análise e modelagem?

Com exemplos curtos e quatro romances de Machado de Assis, estudaremos *bag-of-words*, matrizes documento-termo (DTM/DFM), contagens, presença e ausência de palavras, TF, IDF e TF-IDF. Em seguida, examinaremos n-gramas, coocorrências, embeddings e sequências ordenadas para distinguir **organização dos dados**, **representação textual** e **modelo**.

**[Ler o tutorial](tutoriais/02_representacoes_textuais.md)** · [Versão R Markdown](tutoriais/02_representacoes_textuais.Rmd)

### Aula 2 — Exploração e aprendizado não supervisionado

**Terça-feira, 24/11 · 4 horas · Tutoriais 3, 4 e 5**

Na segunda aula, passaremos da representação dos textos à exploração de seus conteúdos. A referência principal será o livro *Text Mining with R*, de Julia Silge e David Robinson. Entre os exemplos previstos estão discursos presidenciais em português, especialmente discursos de Luiz Inácio Lula da Silva, que permitem explorar vocabulário, associações entre termos e diferenças entre documentos.

Os tutoriais serão disponibilizados em breve.

### Aula 3 — Aprendizado supervisionado: do problema à avaliação

**Quinta-feira, 26/11 · 4 horas · Tutoriais 6, 7 e 8**

No terceiro encontro, estudaremos situações nas quais, além dos textos, conhecemos uma variável que queremos prever. A referência será *Supervised Machine Learning for Text Analysis in R*, de Emil Hvitfeldt e Julia Silge.

## Materiais disponíveis

| Material | Conteúdo | Arquivos |
|---|---|---|
| **Tutorial 0 (opcional)** | Revisão de R, RStudio, `tidyverse` e operações com *data frames* | [Markdown](tutoriais/00_tutorial_inicial_r.md) · [R Markdown](tutoriais/00_tutorial_inicial_r.Rmd) |
| **Tutorial 1** | Preparação, tokenização e exploração dos textos dos parques nacionais | [Markdown](tutoriais/01_textos_como_dados_parques.md) · [R Markdown](tutoriais/01_textos_como_dados_parques.Rmd) |
| **Tutorial 2** | Representações textuais e exemplos com quatro romances de Machado de Assis | [Markdown](tutoriais/02_representacoes_textuais.md) · [R Markdown](tutoriais/02_representacoes_textuais.Rmd) |

Os **Tutoriais 3 a 8** integram a programação dos encontros seguintes, mas ainda não estão publicados.

## Bibliografia e referências

**Livros recomebdadas**

- Julia Silge e David Robinson. [*Text Mining with R: A Tidy Approach*](https://www.tidytextmining.com/). Referência para a preparação e a exploração de textos, a análise lexical de sentimentos, as relações entre palavras e a modelagem de tópicos.
- Emil Hvitfeldt e Julia Silge. [*Supervised Machine Learning for Text Analysis in R*](https://smltar.com/). Referência para representações textuais, preparação de preditores e avaliação de modelos de aprendizado supervisionado.
