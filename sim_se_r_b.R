library(tidyverse)

k <- 10000
cors <- numeric(k)
cor_ci_width <- numeric(k)
bs <- numeric(k)
b_ci_width <- numeric(k)

n <- 100
for(i in seq_along(1:k)) {
  cor <- tibble(
    x = rnorm(n),
    y = 0.3 * x + 0.953 * rnorm(n)
  ) %>% filter(x > -1)
  corci <- broom::tidy(cor.test(cor$x,cor$y))
  cors[i] <- corci$estimate
  cor_ci_width[i] <- corci$conf.high - corci$conf.low
  slopeci <- broom::tidy(lm(y~x,cor), conf.int = TRUE)[2,]
  bs[i] <- slopeci$estimate
  b_ci_width[i] <- slopeci$conf.high - slopeci$conf.low
}
psych::describe(cors - 0.3)
psych::describe(bs - 0.3)
psych::describe(cor_ci_width)
psych::describe(b_ci_width)
