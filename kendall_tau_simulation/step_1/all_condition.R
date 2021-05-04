# parameters setting


# indicator = 8 -----------------------------------------------------------


# ind_n = 8, loading_pattern: same, loading = 0.9
obs_n <- 5000
ind_n <- 8
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <- matrix(rep(first_lam, ind_n),
                 nrow = ind_n,
                 ncol = 1,
                 byrow = TRUE)
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 8, loading_pattern: same, loading = 0.6
obs_n <- 5000
ind_n <- 8
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.6
lambda <- matrix(rep(first_lam, ind_n),
                 nrow = ind_n,
                 ncol = 1,
                 byrow = TRUE)
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 8, loading_pattern: uni-difference, loading = 0.9
obs_n <- 5000
ind_n <- 8
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <-
  matrix(
    c(rep(first_lam, 2), rep((first_lam*2/3), 2), rep(first_lam, 2), rep((first_lam*2/3), 2)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 8, loading_pattern: uni-difference, loading = 0.6
obs_n <- 5000
ind_n <- 8
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.6
lambda <-
  matrix(
    c(rep(first_lam, 2), rep((first_lam*2/3), 2), rep(first_lam, 2), rep((first_lam*2/3), 2)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 8, loading_pattern: multi-difference, loading = 0.9
obs_n <- 5000
ind_n <- 8
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <-
  matrix(
    c(rep(first_lam, 2), rep((first_lam*2/3), 2), rep(first_lam, 2), rep((first_lam*1/3), 2)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 8, loading_pattern: multi-difference, loading = 0.6
obs_n <- 5000
ind_n <- 8
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.6
lambda <-
  matrix(
    c(rep(first_lam, 2), rep((first_lam*2/3), 2), rep(first_lam, 2), rep((first_lam*1/3), 2)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1


# indicator = 16 ----------------------------------------------------------


# ind_n = 16, loading_pattern: same, loading = 0.9
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <- matrix(rep(first_lam, ind_n),
                 nrow = ind_n,
                 ncol = 1,
                 byrow = TRUE)
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 16, loading_pattern: same, loading = 0.6
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.6
lambda <- matrix(rep(first_lam, ind_n),
                 nrow = ind_n,
                 ncol = 1,
                 byrow = TRUE)
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 16, loading_pattern: uni-difference, loading = 0.9
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <-
  matrix(
    c(rep(first_lam, 4), rep((first_lam*2/3), 4), rep(first_lam, 4), rep((first_lam*2/3), 4)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 16, loading_pattern: uni-difference, loading = 0.6
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.6
lambda <-
  matrix(
    c(rep(first_lam, 4), rep((first_lam*2/3), 4), rep(first_lam, 4), rep((first_lam*2/3), 4)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 16, loading_pattern: multi-difference, loading = 0.9
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.9
lambda <-
  matrix(
    c(rep(first_lam, 4), rep((first_lam*2/3), 4), rep(first_lam, 4), rep((first_lam*1/3), 4)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1

# ind_n = 16, loading_pattern: multi-difference, loading = 0.6
obs_n <- 5000
ind_n <- 16
eta_n <- 1
seed <- 123
nu <- matrix(rep(1, ind_n),
             nrow = ind_n,
             ncol = 1,
             byrow = TRUE)
first_lam <- 0.6
lambda <-
  matrix(
    c(rep(first_lam, 4), rep((first_lam*2/3), 4), rep(first_lam, 4), rep((first_lam*1/3), 4)),
    nrow = ind_n,
    ncol = 1,
    byrow = TRUE
  )
epsilon_mean <- 0
epsilon_sd <- 0.6
eta_mean <- 0
eta_sd <- 1