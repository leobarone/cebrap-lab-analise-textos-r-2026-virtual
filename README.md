# Análise de textos com R

**CEBRAP Lab · Oficina virtual · Novembro de 2026**  
**Instrutor:** Leonardo Sangali Barone

Este curso dá continuidade à oficina [Captura de dados na internet com R](https://github.com/leobarone/cebrap-lab-raspagem-r-2026-virtual). Partimos de coleções de textos já organizadas para aprender a preparar documentos, construir representações, explorar padrões e, finalmente, estimar e avaliar modelos de aprendizado de máquina.

A pergunta que orienta o curso é: **como transformar textos em dados para responder a perguntas de pesquisa?** Ao longo dos encontros, distinguiremos a organização dos dados, a representação dos documentos e os modelos utilizados na análise.

## Encontros

| Encontro | Data | Tema | Material | Carga |
|---|---|---|---|---:|
| 1 | 23/11/2026 (segunda-feira) | Texto como dado e representações textuais | Tutoriais 1 e 2 | 4h |
| **2** | **24/11/2026 (terça-feira)** | **Modelagem de tópicos: do TF-IDF ao LDA** | **Tutorial 3** | **4h** |
| 3 | 26/11/2026 (quinta-feira) | Aprendizado supervisionado de textos | Material a publicar | 4h |

**Carga horária:** 20 horas (12 horas síncronas e 8 horas assíncronas, em duas atividades de 4 horas).

## Aula 1 — Texto como dado e representações textuais

**23/11/2026 · 4 horas**

Começamos com textos dos verbetes da Wikipédia sobre parques nacionais e, em seguida, utilizamos quatro romances de Machado de Assis para comparar diferentes representações de um corpus. Discutimos o que cada representação preserva e o que se perde ao transformar textos em dados.

- **Tutorial 0 — Revisão de R e `dplyr` (opcional):** disponível na pasta [`tutoriais/`](tutoriais/), para quem precisar retomar a manipulação de tabelas.
- **Tutorial 1 — Textos como dados: dos parques nacionais às palavras:** [R Markdown](tutoriais/01_textos_como_dados_parques.Rmd). Organização do corpus, preparação dos textos, tokenização, stopwords e contagem de palavras.
- **Tutorial 2 — Como representar textos para análise e modelagem?:** [R Markdown](tutoriais/02_representacoes_textuais.Rmd). Bag-of-words, TF, DF, IDF, TF-IDF, DTM/DFM, n-gramas, coocorrências e embeddings. Usamos *Memórias Póstumas de Brás Cubas*, *Quincas Borba*, *Dom Casmurro* e *Esaú e Jacó*, considerando cada romance como um documento.

## Aula 2 — Do TF-IDF à modelagem de tópicos

**24/11/2026 · 4 horas**

Neste encontro, vamos trabalhar exclusivamente com o **Tutorial 3**, utilizando uma coleção de discursos de Lula em seu terceiro mandato, coletados no site oficial da Presidência da República. Retomaremos o que aprendemos no Tutorial 2 para passar das representações textuais a um modelo de aprendizado não supervisionado.

### Tutorial 3 — Dos pesos TF-IDF à modelagem de tópicos nos discursos de Lula

**[Abrir o Tutorial 3 (R Markdown)](tutoriais/03_modelagem_topicos_lula.Rmd)**

Vamos identificar palavras distintivas de cada discurso e, a partir das contagens, estimar um modelo LDA com seis tópicos. Não informaremos ao modelo quais são os assuntos dos discursos. Depois de estimá-lo, examinaremos os resultados para interpretar os padrões de vocabulário encontrados.

O percurso da aula será:

1. Carregar o corpus, tokenizar os discursos e retirar stopwords e tokens exclusivamente numéricos.
2. Retomar a contagem de palavras por documento e os pesos TF-IDF.
3. Compreender o LDA como modelo probabilístico generativo: tópicos como distribuições de palavras e documentos como misturas de tópicos.
4. Selecionar o vocabulário, construir a matriz documento-termo de contagens e ajustar o modelo de seis tópicos.
5. Interpretar **beta** (distribuição de palavras por tópico) e **gamma** (mistura de tópicos por discurso), examinando os termos e os títulos dos discursos mais representativos.
6. Experimentar o número de tópicos, a semente e os filtros de vocabulário, discutindo o papel das decisões do analista.

A ideia é compreender a distinção entre **representação textual e modelo**: o TF-IDF é utilizado para explorar o vocabulário distintivo, enquanto o LDA clássico recebe a matriz de contagens inteiras. Os tópicos não são categorias previamente rotuladas, e seus nomes dependem da nossa interpretação dos resultados.

**Leitura de referência:** Silge e Robinson, *Text Mining with R*, [capítulo 3 (TF-IDF)](https://www.tidytextmining.com/tfidf), [capítulo 5 (matrizes documento-termo)](https://www.tidytextmining.com/dtm) e [capítulo 6 (modelagem de tópicos)](https://www.tidytextmining.com/topicmodeling).

### Materiais complementares opcionais — ao final ou após a aula

Os dois tutoriais abaixo usam o mesmo corpus de discursos, mas **não fazem parte do percurso obrigatório do encontro de hoje**. Serão disponibilizados ao final da aula para quem quiser explorar outros procedimentos de análise de textos no próprio ritmo.

**[Tutorial 4 — Análise de sentimentos nos discursos de Lula (Rmd)](tutoriais/04_sentimentos_discursos_lula.Rmd)**

Usaremos o léxico em português OpLexicon para associar palavras a polaridades positivas e negativas, comparar discursos sobre o combate à fome e observar a variação do saldo lexical ao longo das falas. O exercício mostra também os limites de classificar palavras isoladas. A análise por dicionário é uma regra lexical, não um modelo de aprendizado não supervisionado.

*Referência:* [Silge e Robinson, capítulo 2](https://www.tidytextmining.com/sentiment.html).

**[Tutorial 5 — Relações entre palavras nos discursos de Lula (Rmd)](tutoriais/05_relacoes_entre_termos_lula.Rmd)**

Vamos construir bigramas e trigramas, contar coocorrências por parágrafo e calcular correlações phi entre palavras. A palavra *fome* será o ponto de partida para distinguir adjacência, frequência conjunta e associação, voltando aos parágrafos para interpretar os resultados.

*Referência:* [Silge e Robinson, capítulo 4](https://www.tidytextmining.com/ngrams.html).

## Aula 3 — Aprendizado supervisionado de textos

**26/11/2026 · 4 horas**

No terceiro encontro, passaremos da exploração de textos sem rótulos à construção de modelos para prever uma variável de resposta. Trabalharemos a formulação do problema, a preparação dos textos, a divisão em treino, validação e teste, a comparação de modelos e a avaliação de previsões. A classificação de reclamações financeiras será uma referência prática; a regressão e a previsão do ano de decisões judiciais servirão como contraponto. Discutiremos também o uso de embeddings como representação para modelos supervisionados.

Os materiais do terceiro encontro serão publicados posteriormente. A referência principal é Hvitfeldt e Silge, *Supervised Machine Learning for Text Analysis in R*, especialmente os capítulos sobre tokenização, embeddings, regressão e classificação.

## Atividades assíncronas

O curso inclui duas atividades de aproximadamente quatro horas, relacionadas à exploração de textos e à avaliação de modelos supervisionados. Os encaminhamentos serão apresentados nos encontros. Os tutoriais opcionais da Aula 2 são recursos para estudo complementar.

## Como acompanhar

Os tutoriais estão na pasta [`tutoriais/`](tutoriais/). Abra os arquivos `.Rmd` no RStudio e execute os blocos de código em ordem. Também é possível compilá-los em HTML ou, quando configurado, em Markdown para leitura no GitHub.

Para a Aula 2, instale os pacotes necessários ao Tutorial 3 (apenas se ainda não estiverem instalados):

```r
install.packages(c("tidyverse", "tidytext", "stopwords", "topicmodels", "reshape2"))
```

Para os tutoriais opcionais 4 e 5, instale também:

```r
install.packages(c("lexiconPT", "stringi", "widyr"))
```

Para compilar os Rmd, é necessário ter o pacote `rmarkdown` e o Pandoc (normalmente disponível com o RStudio).

Os Tutoriais 3, 4 e 5 leem o mesmo RDS diretamente do GitHub. Não é preciso executar novamente a raspagem, baixar o corpus manualmente ou ajustar o diretório de trabalho para esses tutoriais. Cada tutorial é independente. É necessária conexão à internet para a leitura remota.

## Dados

A pasta [`data/`](data/) contém os dados utilizados no curso. Consulte o [README dos dados](data/README.md) para conhecer os arquivos, formatos e endereços públicos.

- Aula 1: corpus dos parques nacionais, amostra didática e quatro romances de Machado de Assis.
- Aula 2: [`corpus_discursos_lula.rds`](data/corpus_discursos_lula.rds), utilizado nos Tutoriais 3, 4 e 5.

## Bibliografia

- Julia Silge e David Robinson. [*Text Mining with R: A Tidy Approach*](https://www.tidytextmining.com/).
- Emil Hvitfeldt e Julia Silge. [*Supervised Machine Learning for Text Analysis in R*](https://smltar.com/).
