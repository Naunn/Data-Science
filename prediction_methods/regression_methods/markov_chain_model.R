# MARKOV CHAIN MODEL ===================================================================================================
# https://www.hindawi.com/journals/jam/2022/2863302/

# Libraries ============================================================================================================
library(dplyr)

# https://stephens999.github.io/fiveMinuteStats/stationary_distribution.html
stationary <- function(transition) {
  stopifnot(
    is.matrix(transition) &&
      nrow(transition) == ncol(transition) &&
      all(transition >= 0 & transition <= 1)
  )
  p <- diag(nrow(transition)) - transition
  A <- rbind(t(p),
             rep(1, ncol(transition)))
  b <- c(rep(0, nrow(transition)),
         1)
  res <- qr.solve(A, b)
  names(res) <- paste0("state.", 1:nrow(transition) - 1)
  return(res)
}

# Data =================================================================================================================
df <-
  readr::read_csv("./data/stan_uk_d.csv") %>% #wig_nrchom_d; stan_uk_d
  as.data.frame() %>%
  transmute(date = Data, close = Zamkniecie)

plot(x = df$date,
     y = df$close,
     type = 'l')

# Markov Chain =========================================================================================================
# calculate X_t
tol = 1
df <-
  df %>% mutate(X_t = case_when(sqrt((lag(
    close
  ) - close) ^ 2) < tol  ~ 1,
  lag(close) > close ~ 0,
  lag(close) < close ~ 2))

df$X_t %>% table()

# # initial distribution
# # by hand
# n <-
#   (df %>% nrow() - 1) # we do not know the change of the first row, i.e. NA value
# n_0 <- df$X_t %>% table() %>% .[[1]]
# n_1 <- df$X_t %>% table() %>% .[[2]]
# n_2 <- df$X_t %>% table() %>% .[[3]]
# p_0 <- n_0 / n
# p_1 <- n_1 / n
# p_2 <- n_2 / n
#
# p_0 + p_1 + p_2 == 1

# more ellegant approach
initial_states <-
  df %>%
  group_by(X_t) %>%
  count() %>%
  ungroup() %>%
  filter(!is.na(X_t)) %>%
  transmute(X_t, p = n / sum(n))
sum(initial_states$p)

initial_states

# state matrix
df_states <-
  df %>%
  mutate(
    state_0 = case_when(
      lag(X_t) == 0 & X_t == 0 ~ 0,
      lag(X_t) == 0 & X_t == 1 ~ 1,
      lag(X_t) == 0 & X_t == 2 ~ 2
    ),
    state_1 = case_when(
      lag(X_t) == 1 & X_t == 0 ~ 0,
      lag(X_t) == 1 & X_t == 1 ~ 1,
      lag(X_t) == 1 & X_t == 2 ~ 2
    ),
    state_2 = case_when(
      lag(X_t) == 2 & X_t == 0 ~ 0,
      lag(X_t) == 2 & X_t == 1 ~ 1,
      lag(X_t) == 2 & X_t == 2 ~ 2
    )
  )

p_00 <- df_states$state_0 %>% table() %>% .[[1]] / n_0
p_01 <- df_states$state_0 %>% table() %>% .[[2]] / n_0
p_02 <- df_states$state_0 %>% table() %>% .[[3]] / n_0
round(p_00 + p_01 + p_02, 3) == 1

p_10 <- df_states$state_1 %>% table() %>% .[[1]] / n_1
p_11 <- df_states$state_1 %>% table() %>% .[[2]] / n_1
p_12 <- df_states$state_1 %>% table() %>% .[[3]] / n_1
round(p_10 + p_11 + p_12, 3) == 1

p_20 <- df_states$state_2 %>% table() %>% .[[1]] / n_2
p_21 <- df_states$state_2 %>% table() %>% .[[2]] / n_2
p_22 <- df_states$state_2 %>% table() %>% .[[3]] / n_2
round(p_20 + p_21 + p_22, 3) == 1

p_states <-
  matrix(
    c(p_00, p_01, p_02, p_10, p_11, p_12, p_20, p_21, p_22),
    nrow = 3,
    byrow = TRUE
  )
p_states

# solve for stationary distribution
stationary(p_states)
