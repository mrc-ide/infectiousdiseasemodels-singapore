beta <- user(0.5) # contact rate
sigma <- user(0.3) # recovery
N <- user(1000)  # total population.

initialise_at_steady_state <- user(TRUE)

I_init <- user(1)

## Steady-state prevelance
I_star <- if (beta > sigma) N * (beta - sigma) / beta else 0

dt <- 0.01
time <- step * dt

## Deterministic solution
I_det <- if (initialise_at_steady_state) 
  I_star / (1 + (I_star / I_det_init - 1) * exp(-(beta - sigma) * time)) else 0

## Stochastic solution
initial(I) <- if (initialise_at_steady_state) round(I_star) else I_init

FOI <- beta * I / N
S <- N - I

n_infections <- rbinom(S, FOI * dt)
n_recoveries <- rbinom(I, sigma * dt)

update(I) <- I + n_infections - n_recoveries
output(S) <- TRUE
output(time) <- TRUE
output(I_det) <- TRUE
output(extinct) <- I == 0
