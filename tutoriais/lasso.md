### Um exemplo simples de classificação com regressão logística LASSO

A regressão logística funciona de maneira diferente do Naive Bayes. Em vez de combinar probabilidades de ocorrência das palavras, ela aprende um conjunto de **coeficientes (pesos)** que indica como cada característica textual contribui para a classificação de um documento.

Vamos utilizar as mesmas quatro palavras do exemplo anterior: desigualdade, pobreza, criminalidade e armamento.

Para simplificar, vamos representar cada documento pela presença ou ausência dessas palavras. Se uma palavra aparece no documento, atribuímos o valor 1. Caso contrário, atribuímos o valor 0.

Imagine que, depois de estimar uma regressão logística com os documentos do conjunto de treinamento, encontramos os seguintes coeficientes (valores fictícios):

| Palavra | Coeficiente |
|---|---:|
| desigualdade | 2,0 |
| pobreza | 1,5 |
| criminalidade | -1,4 |
| armamento | -0,5 |

Neste exemplo, estamos modelando a probabilidade de o discurso ser de um parlamentar do PT. Os coeficientes positivos contribuem para aumentar essa probabilidade, enquanto os negativos contribuem para diminuí-la, favorecendo a classificação como PL.

```{r exemplo-coeficientes, fig.width=8, fig.height=4}
library(tidyverse)

coeficientes_exemplo <- tibble(
  palavra = c("desigualdade", "pobreza",
              "criminalidade", "armamento"),
  coeficiente = c(2.0, 1.5, -1.4, -0.5)
)

coeficientes_exemplo %>%
  ggplot(aes(reorder(palavra, coeficiente), coeficiente,
             fill = coeficiente > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick",
                               "FALSE" = "steelblue")) +
  labs(
    x = NULL,
    y = "Coeficiente",
    title = "Coeficientes da regressão logística",
    subtitle = "Positivos favorecem PT; negativos favorecem PL"
  ) +
  theme_minimal()
```

Diferentemente do Naive Bayes, a regressão logística não precisa supor que as palavras sejam independentes entre si dentro de cada categoria. Os coeficientes são estimados conjuntamente, procurando produzir boas previsões para os documentos do treinamento.

### O que é a regularização LASSO?

Imagine que, em vez de quatro palavras, tivéssemos mil ou dez mil palavras como preditores. Algumas seriam muito úteis para distinguir os partidos, outras seriam pouco informativas e outras poderiam estar associadas aos partidos apenas por acaso.

Um modelo com muitos preditores pode aprender características muito particulares dos documentos do treinamento, que não estarão presentes em documentos novos. Chamamos esse problema de **sobreajuste** (*overfitting*).

O LASSO é uma técnica de regularização que procura evitar esse problema. Sua ideia é penalizar a magnitude dos coeficientes durante a estimação do modelo.

Assim, o modelo precisa conciliar dois objetivos:

- Produzir boas previsões para os documentos do treinamento.
- Evitar coeficientes desnecessariamente grandes.

Uma característica particularmente interessante do LASSO é que alguns coeficientes podem se tornar exatamente iguais a zero. Quando isso acontece, a palavra deixa de contribuir diretamente para a classificação.

Imagine que, após estimarmos a regressão logística com regularização LASSO, obtenhamos os seguintes coeficientes:

| Palavra | Sem regularização | Com LASSO |
|---|---:|---:|
| desigualdade | 2,0 | 1,2 |
| pobreza | 1,5 | 0,8 |
| criminalidade | -1,4 | -1,0 |
| armamento | -0,5 | 0,0 |

```{r exemplo-regularizacao, fig.width=8, fig.height=5}
coeficientes_lasso_exemplo <- tibble(
  palavra = c("desigualdade", "pobreza",
              "criminalidade", "armamento"),
  `Sem regularização` = c(2.0, 1.5, -1.4, -0.5),
  `Com LASSO` = c(1.2, 0.8, -1.0, 0.0)
)

coeficientes_lasso_exemplo %>%
  pivot_longer(
    cols = -palavra,
    names_to = "modelo",
    values_to = "coeficiente"
  ) %>%
  ggplot(aes(palavra, coeficiente, fill = modelo)) +
  geom_col(position = "dodge") +
  geom_hline(yintercept = 0) +
  coord_flip() +
  labs(
    x = NULL,
    y = "Coeficiente",
    fill = "Modelo",
    title = "O efeito da regularização LASSO"
  ) +
  theme_minimal()
```

Observe que, neste exemplo, os coeficientes ficaram menores em magnitude e que o coeficiente da palavra "armamento" passou a ser igual a zero.

O modelo passou a utilizar apenas três das quatro palavras disponíveis. Dizemos que o LASSO realiza também uma **seleção de variáveis** durante a estimação.

Os valores apresentados são ilustrativos. Na prática, os novos coeficientes são aprendidos a partir dos dados, durante a estimação do modelo com a penalização.

### Classificando um novo documento

Agora recebemos um novo discurso contendo as quatro palavras, mas não sabemos a qual partido pertence o parlamentar que o proferiu.

| Palavra | Presença no documento | Coeficiente LASSO |
|---|---:|---:|
| desigualdade | 1 | 1,2 |
| pobreza | 1 | 0,8 |
| criminalidade | 1 | -1,0 |
| armamento | 1 | 0,0 |

Para classificar esse documento, o modelo multiplica o valor de cada preditor pelo seu respectivo coeficiente e soma os resultados, juntamente com uma constante (o intercepto), que neste exemplo vamos supor ser igual a zero.

O resultado é um escore:

$$z = b_0 + b_1x_1 + b_2x_2 + b_3x_3 + b_4x_4$$

Substituindo pelos valores do nosso exemplo:

$$z = 0 + (1 \times 1{,}2) + (1 \times 0{,}8) + (1 \times -1{,}0) + (1 \times 0)$$

$$z = 1{,}0$$

Observe que a palavra "armamento", embora esteja presente no documento, não contribui para a previsão, pois seu coeficiente é igual a zero.

```{r contribuicao-palavras, fig.width=8, fig.height=4}
documento_novo <- tibble(
  palavra = c("desigualdade", "pobreza",
              "criminalidade", "armamento"),
  presenca = c(1, 1, 1, 1),
  coeficiente = c(1.2, 0.8, -1.0, 0.0)
) %>%
  mutate(contribuicao = presenca * coeficiente)

documento_novo
```

Podemos representar graficamente a contribuição de cada palavra para a classificação do documento:

```{r grafico-contribuicoes, fig.width=8, fig.height=4}
documento_novo %>%
  ggplot(aes(reorder(palavra, contribuicao), contribuicao,
             fill = contribuicao > 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_fill_manual(values = c("TRUE" = "firebrick",
                               "FALSE" = "steelblue")) +
  labs(
    x = NULL,
    y = "Contribuição para o escore",
    title = "Como cada palavra contribui para a classificação"
  ) +
  theme_minimal()
```

### Transformando o escore em uma probabilidade

O escore calculado pela regressão pode assumir qualquer valor, positivo ou negativo. Mas queremos obter uma probabilidade, ou seja, um número entre zero e um.

Para isso, a regressão logística utiliza a **função logística**:

$$P(PT \mid texto) = \frac{1}{1+\exp(-z)}$$

Essa função transforma qualquer número em uma probabilidade entre zero e um.

```{r funcao-logistica, fig.width=8, fig.height=4}
tibble(z = seq(-6, 6, by = 0.1)) %>%
  mutate(probabilidade = plogis(z)) %>%
  ggplot(aes(z, probabilidade)) +
  geom_line(linewidth = 1, colour = "steelblue") +
  geom_hline(yintercept = 0.5, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(
    data = tibble(z = 1, probabilidade = plogis(1)),
    colour = "firebrick",
    size = 4
  ) +
  scale_y_continuous(labels = scales::label_percent()) +
  labs(
    x = "Escore (z)",
    y = "Probabilidade estimada de PT",
    title = "A função logística",
    subtitle = "O ponto vermelho representa o nosso documento"
  ) +
  theme_minimal()
```

Quando o escore é igual a zero, a probabilidade é de 50%. Quanto maior o escore, mais a probabilidade se aproxima de 100%. Quanto menor o escore, mais a probabilidade se aproxima de zero.

No nosso exemplo, encontramos um escore igual a 1. Portanto:

$$P(PT \mid texto) = \frac{1}{1+\exp(-1)} \approx 0{,}731$$

Como temos apenas duas categorias, a probabilidade de o documento ser do PL é:

$$P(PL \mid texto) = 1 - 0{,}731 = 0{,}269$$

Vamos realizar essas operações em R:

```{r calculo-probabilidade-lasso}
z <- sum(documento_novo$contribuicao)

prob_pt <- plogis(z)
prob_pl <- 1 - prob_pt

resultado_lasso <- tibble(
  partido = c("PT", "PL"),
  probabilidade = c(prob_pt, prob_pl)
)

resultado_lasso
```

```{r grafico-probabilidades-lasso, fig.width=7, fig.height=4}
resultado_lasso %>%
  ggplot(aes(partido, probabilidade, fill = partido)) +
  geom_col(width = 0.5, show.legend = FALSE) +
  geom_text(
    aes(label = scales::percent(probabilidade, accuracy = 0.1)),
    vjust = -0.5,
    size = 5
  ) +
  scale_y_continuous(
    limits = c(0, 1),
    labels = scales::label_percent()
  ) +
  scale_fill_manual(values = c("PT" = "firebrick",
                               "PL" = "steelblue")) +
  labs(
    x = "Partido",
    y = "Probabilidade estimada",
    title = "Classificação de um novo discurso com LASSO"
  ) +
  theme_minimal()
```

Como a probabilidade estimada de PT é maior (73,1%), classificamos o documento como pertencente a um parlamentar desse partido.

### Controlando a intensidade da regularização

A intensidade da penalização do LASSO é controlada por um hiperparâmetro chamado **lambda** ($\lambda$).

Quando lambda é igual a zero, não há penalização. À medida que aumentamos seu valor, o modelo sofre uma penalização maior pela magnitude de seus coeficientes. Com uma penalização suficientemente forte, todos os coeficientes das palavras podem se tornar iguais a zero.

Não queremos necessariamente um modelo com o menor número possível de palavras. Queremos um modelo que seja capaz de produzir boas previsões para documentos novos, evitando aprender relações que existem apenas por acaso no treinamento.

### Comparando o Naive Bayes e a regressão logística LASSO

| Característica | Naive Bayes | Regressão logística LASSO |
|---|---|---|
| O que o modelo aprende? | Distribuições das características textuais em cada categoria | Coeficientes associados às características textuais |
| Como combina as informações? | Multiplica probabilidades ou densidades | Soma os valores dos preditores ponderados pelos coeficientes |
| Supõe independência condicional dos preditores? | Sim | Não |
| Pode zerar coeficientes e selecionar palavras? | Não é uma característica do modelo convencional | Sim |
| Resultado | Probabilidades estimadas das categorias | Probabilidades estimadas das categorias |

Neste exemplo, utilizamos apenas quatro palavras, representadas por sua presença ou ausência nos documentos. Em nossa aplicação aos discursos parlamentares, utilizaremos mil palavras, representadas por seus pesos TF-IDF.

A mecânica do modelo, entretanto, permanece a mesma: cada palavra possui um coeficiente aprendido durante o treinamento, e o modelo combina os valores dos preditores com esses coeficientes para estimar a probabilidade de pertencimento a cada partido.

É importante observar que o sinal dos coeficientes depende de qual categoria está sendo modelada. Neste exemplo didático, os coeficientes positivos favorecem PT. Na aplicação com o `glmnet`, devemos verificar a ordem das categorias antes de interpretar os sinais dos coeficientes.