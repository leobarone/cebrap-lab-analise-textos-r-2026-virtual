### Um exemplo simples de classificação com Naive Bayes

Imagine que, nos documentos do conjunto de treinamento, encontramos as seguintes probabilidades de ocorrência de quatro palavras (valores fictícios):

| Palavra | Discursos do PT | Discursos do PL |
|---|---:|---:|
| desigualdade | 30% | 10% |
| pobreza | 20% | 5% |
| criminalidade | 10% | 30% |
| armamento | 10% | 20% |

Neste exemplo, as duas primeiras palavras são proporcionalmente mais presentes nos discursos do PT, enquanto as duas últimas são mais presentes nos discursos do PL. Essas associações são hipotéticas e não significam necessariamente que os parlamentares defendam ou rejeitem aquilo a que as palavras se referem.

```{r exemplo-probabilidades, fig.width=8, fig.height=4}
library(tidyverse)
probabilidades <- tibble(
  partido = c("PT", "PL"),
  prob_inicial = c(0.5, 0.5),
  desigualdade = c(0.30, 0.10),
  pobreza = c(0.20, 0.05),
  criminalidade = c(0.10, 0.30),
  armamento = c(0.10, 0.20)
)

probabilidades %>%
  select(-prob_inicial) %>%
  pivot_longer(
    cols = -partido,
    names_to = "palavra",
    values_to = "probabilidade"
  ) %>%
  ggplot(aes(palavra, probabilidade, fill = partido)) +
  geom_col(position = "dodge") +
  scale_y_continuous(labels = scales::label_percent()) +
  scale_fill_manual(values = c("PT" = "firebrick",
                               "PL" = "steelblue")) +
  labs(
    x = NULL,
    y = "Probabilidade de ocorrência",
    fill = "Partido",
    title = "Probabilidade de encontrar cada palavra nos discursos"
  ) +
  theme_minimal()
```

Agora recebemos um novo discurso contendo essas quatro palavras, mas não sabemos a qual partido pertence o parlamentar que o proferiu.

Vamos supor que, antes de observarmos o texto, os dois partidos sejam igualmente prováveis (50% de probabilidade para cada um).

Cada palavra fornece uma evidência a favor de um partido ou do outro. O Naive Bayes assume que, dentro de cada categoria, essas evidências são independentes entre si. Podemos, portanto, combinar as probabilidades das quatro palavras multiplicando-as pela probabilidade inicial de cada partido:

- **PT:** 0,5 × 0,30 × 0,20 × 0,10 × 0,10 = 0,0003
- **PL:** 0,5 × 0,10 × 0,05 × 0,30 × 0,20 = 0,00015

Para transformar esses valores em probabilidades de pertencimento a cada partido, precisamos **normalizá-los**, ou seja, dividir cada valor pela soma dos dois resultados:

$$0{,}0003 + 0{,}00015 = 0{,}00045$$

A probabilidade estimada de que o discurso seja de um parlamentar do PT é:

$$P(PT \mid texto) = \frac{0{,}0003}{0{,}00045} \approx 0{,}667$$

A probabilidade estimada de que o discurso seja de um parlamentar do PL é:

$$P(PL \mid texto) = \frac{0{,}00015}{0{,}00045} \approx 0{,}333$$

Vamos realizar essas operações em R:

```{r calculo-naive-bayes}
resultado <- probabilidades %>%
  mutate(
    escore = prob_inicial * desigualdade * pobreza *
      criminalidade * armamento,
    probabilidade = escore / sum(escore)
  )

resultado %>%
  select(partido, escore, probabilidade)
```

Podemos representar graficamente as probabilidades estimadas:

```{r grafico-naive-bayes, fig.width=7, fig.height=4}
resultado %>%
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
    title = "Classificação de um novo discurso com Naive Bayes"
  ) +
  theme_minimal()
```

Apesar de o documento conter duas palavras mais características dos discursos do PT e duas palavras mais características dos discursos do PL, o modelo atribui uma probabilidade maior ao PT (66,7%).

Isso ocorre porque o Naive Bayes não faz simplesmente uma contagem de quantas palavras estão associadas a cada partido. Algumas palavras fornecem evidências mais fortes do que outras. O modelo leva em consideração a intensidade dessas diferenças ao combinar as probabilidades.

Podemos observar a contribuição de cada palavra:

- A palavra "desigualdade" é três vezes mais provável nos discursos do PT.
- A palavra "pobreza" é quatro vezes mais provável nos discursos do PT.
- A palavra "criminalidade" é três vezes mais provável nos discursos do PL.
- A palavra "armamento" é duas vezes mais provável nos discursos do PL.

Ao combinar essas evidências, o modelo considera que a ocorrência conjunta das quatro palavras é duas vezes mais provável nos discursos do PT do que nos discursos do PL.

Em um problema real de classificação de textos, realizamos uma operação semelhante, mas utilizando centenas ou milhares de características textuais.

A principal simplificação do Naive Bayes está na hipótese de independência condicional dessas características. Sabemos, entretanto, que as palavras de um texto frequentemente estão relacionadas. As palavras "pobreza" e "desigualdade", por exemplo, podem aparecer juntas com frequência. Ao tratá-las como evidências independentes, o modelo pode contabilizar informações redundantes e produzir probabilidades excessivamente próximas de zero ou de um.

Neste exemplo simplificado, consideramos apenas a presença das quatro palavras. Existem diferentes versões do Naive Bayes, que podem trabalhar com a presença e a ausência de palavras, suas contagens ou mesmo variáveis contínuas, como os pesos TF-IDF.