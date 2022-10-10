beta <- user(0.5) # contact rate
sigma <- user(0.3) # recovery
N <- user(1000)  # total population.

I_init_at_steady_state <- user(1)

I_init <- user(1)

## Steady-state prevelance
I_star <- if (beta > sigma) N * (beta - sigma) / beta else 0

dt <- 0.01
time <- step * dt

## Deterministic solution
I_det_init <- if (I_init_at_steady_state > 0) I_star else I_init
output(I_det) <- I_star / (1 + (I_star / I_det_init - 1) * exp(-(beta - sigma) * time))

## Stochastic solution
initial(I) <- if (I_init_at_steady_state > 0) round(I_star) else I_init

FOI <- beta * I / N
S <- N - I

n_infections <- rbinom(S, FOI * dt)
n_recoveries <- rbinom(I, sigma * dt)

update(I) <- I + n_infections - n_recoveries
output(S) <- TRUE
output(time) <- TRUE

output(extinct) <- I == 0
