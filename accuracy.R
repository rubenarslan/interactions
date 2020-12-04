library(tidyverse)

n <- 1000

accuracy <- tibble(
  close_friend = if_else(runif(n) > 0.8, 1, 0),
  months_known = rpois(n, 5 + 3*close_friend),
  trait = rnorm(n),
  perception = trait + (5 - (close_friend))/log(months_known/3 + 2) * rnorm(1000)
)
round(cor(accuracy),2)

cor.test(accuracy$trait, accuracy$perception)
accuracy %>% group_by(close_friend) %>%
  summarise(cor(trait, perception))
accuracy %>% group_by(months_known) %>%
  summarise(cor(trait, perception), n())

ggplot(accuracy, aes(trait, perception)) +
  geom_point() +
  geom_smooth(method = "lm")

accuracy$close_friend_f <- factor(accuracy$close_friend, 0:1, c("acquaintance", "close friend"))

cors <- accuracy %>% group_by(close_friend_f) %>%
  do(broom::tidy(cor.test(.$trait, .$perception)))

ggplot(accuracy, aes(trait, perception, color = close_friend_f)) +
  facet_wrap(~ close_friend_f) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_color_discrete(guide = F)+
  geom_text(aes(label = sprintf("%.2f [%.2f;%.2f]", estimate, conf.low, conf.high)), x= 0, y=-13, data = cors) +
  theme_bw()

ggplot(accuracy, aes(trait, perception)) +
  facet_wrap(~ months_known) +
  geom_point()# +
  # geom_smooth(method = "lm")

summary(lm(perception ~ trait * (months_known + close_friend), data = accuracy))

library(brms)
m_simple <- brm(perception ~ trait, data = accuracy, cores = 4, iter = 2000)
m_simple

m_sig <- brm(bf(perception ~ trait,
                sigma ~ close_friend), data = accuracy, cores = 4, iter = 2000)

m_sig_main <- brm(bf(perception ~ trait + close_friend,
                sigma ~ close_friend), data = accuracy, cores = 4, iter = 2000)

m_int <- brm(bf(perception ~ trait * close_friend), data = accuracy, cores = 4, iter = 2000)

m_int <- brm(bf(perception ~ trait * (close_friend + months_known)), data = accuracy, cores = 4, iter = 2000)
knitr::kable(fixef(m_int))

m_sig_int_main <- brm(bf(perception ~ trait * close_friend,
                     sigma ~ close_friend), data = accuracy, cores = 4, iter = 2000)



n <- 1000

accuracy <- tibble(
  close_friend = if_else(runif(n) > 0.8, 1, 0),
  months_known = rpois(n, 5 + 3*close_friend),
  trait = rnorm(n),
  perception = trait + (5 - (close_friend))/log(months_known/3 + 2) * rnorm(1000)
)
round(cor(accuracy),2)

cor.test(accuracy$trait, accuracy$perception)
accuracy %>% group_by(close_friend) %>%
  summarise(cor(trait, perception))
accuracy %>% group_by(months_known) %>%
  summarise(cor(trait, perception), n())

ggplot(accuracy, aes(trait, perception)) +
  geom_point() +
  geom_smooth(method = "lm")

accuracy$close_friend_f <- factor(accuracy$close_friend, 0:1, c("acquaintance", "close friend"))

cors <- accuracy %>% group_by(close_friend_f) %>%
  do(broom::tidy(cor.test(.$trait, .$perception)))

ggplot(accuracy, aes(trait, perception, color = close_friend_f)) +
  facet_wrap(~ close_friend_f) +
  geom_point(alpha = 0.3) +
  geom_smooth(method = "lm") +
  scale_color_discrete(guide = F)+
  geom_text(aes(label = sprintf("%.2f [%.2f;%.2f]", estimate, conf.low, conf.high)), x= 0, y=-13, data = cors) +
  theme_bw()
