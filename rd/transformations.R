devtools::load_all()

x <- rnorm(1000000, 4.5, 1.2) + rnorm(2000000, .8, 1.4)

# Cap
summary(x)
cap(x) %>% summary()

# Analyse
analyse(x)

# Conform
y <- c(
  rep(NaN, 10),
  rnorm(10, 1, 1),
  rep(NA, 8),
  Inf,
  -Inf
)

analyse(y)
conform(y) %>% analyse()
