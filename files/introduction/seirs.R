# variables
deriv(S) <- Births - b * S - beta * S * I / N + delta * R 
deriv(E) <- beta * S * I / N - (b + gamma) * E
deriv(I) <- gamma * E - (b + sigma + alpha) * I
deriv(R) <- sigma * I - (b + delta) * R

# initial conditions of the variables
initial(S) <- N_init - I_init
initial(E) <- 0
initial(I) <- I_init
initial(R) <- 0

N <- S + E + I + R
output(pop) <- N

# parameter values
N_init <- user(1e7)       # initial population size
I_init <- user(1)         # num infectious cases at start of epidemic
gamma <- user(1)          # 1 / duration latent = 1 week
sigma <- user(1)          # recovery rate (1/mean duration infectiousness = 1 week)
alpha <- user(0)          # rate of death from infection
delta <- user(0)          # waning antibody rate
b <- user(2.6e-4)         # death rate, average life expectancy of 75y i.e 1/(75*52)
R0 <- user(5)

Births <- b * N_init      # number of births (for a constant population size)
beta <- R0 * ((b + gamma) / gamma) * (b + alpha + sigma)
