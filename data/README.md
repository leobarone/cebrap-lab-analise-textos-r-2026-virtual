# Dados da Aula 1

Os tutoriais utilizam três arquivos. As leituras são feitas sem precisar realizar novamente a raspagem ou a conversão dos romances.

## Parques nacionais — corpus completo

**Arquivo:** `tabelas-parques-utf8.csv`  
**Uso:** Tutorial 1, preparando, tokenizando e contando os textos dos parques.  
**Formato:** CSV UTF-8, separado por ponto e vírgula.  
**Endereço público neste repositório:**

`https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/tabelas-parques-utf8.csv`

O arquivo completo foi produzido a partir de páginas da Wikipédia no curso de raspagem. A cópia original está em:

`https://raw.githubusercontent.com/leobarone/aula-cci-webscraping-text/master/data/tabelas-parques-utf8.csv`

Caso o CSV completo ainda não tenha sido publicado neste repositório, o Tutorial 1 recorre automaticamente ao endereço de origem. Para armazenar uma cópia local na pasta `data/` e incluí-la no repositório, execute a partir da pasta do projeto:

```r
url_origem <- paste0(
  "https://raw.githubusercontent.com/leobarone/",
  "aula-cci-webscraping-text/master/data/tabelas-parques-utf8.csv"
)

utils::download.file(
  url_origem,
  destfile = "data/tabelas-parques-utf8.csv",
  mode = "wb"
)
```

A publicação desse arquivo no GitHub tornará disponível também o novo endereço público indicado acima.

## Parques nacionais — amostra para revisão

**Arquivo:** `parques_exemplo_didatico.csv`  
**Uso:** Tutorial 0.  
**Formato:** CSV UTF-8, separado por vírgulas.  
**Link:** https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/parques_exemplo_didatico.csv

Esta é uma amostra didática, com textos curtos. **Ela não substitui o corpus completo utilizado no Tutorial 1.**

## Machado de Assis — quatro romances completos

**Arquivo:** `corpus_machado_4_romances.rds`  
**Uso:** Tutorial 2.  
**Formato:** objeto RDS, com as colunas `book` e `text`; cada linha corresponde ao texto completo de um romance.  
**Link:** https://raw.githubusercontent.com/leobarone/cebrap-lab-analise-textos-r-2026-virtual/main/data/corpus_machado_4_romances.rds

```r
livros <- readr::read_rds("data/corpus_machado_4_romances.rds")
```

Os quatro romances são *Memórias Póstumas de Brás Cubas*, *Quincas Borba*, *Dom Casmurro* e *Esaú e Jacó*.
