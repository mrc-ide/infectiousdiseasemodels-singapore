# variables
deriv(S) <- Births - mu * S - beta * S * I / N + delta * R 
deriv(E) <- beta * S * I / N - (mu + gamma) * E
deriv(I) <- gamma * E - (mu + sigma + alpha) * I
deriv(R) <- sigma * I - (mu + delta) * R

# initial conditions of the variables
initial(S) <- N_init - I_init
initial(E) <- 0
initial(I) <- I_init
initial(R) <- 0

N <- S + E + I + R
output(pop) <- N

# parameter values
N_init <- user(1e7) # initial population size
I_init <- user(1)   # num infectious cases at start of epidemic
gamma <- user(1)    # 1/duration latent = 1 week
sigma <- user(1)    # recovery rate (1/mean duration infectiousness = 1 week)
alpha <- user(0)    # death rate from infection
delta <- user(0)    # waning antibody rate
mu <- user(2.6e-4)  # population death rate, average life expectancy of 75y
R0 <- user(5)       # basic reproduction number

Births <- mu * N_init      # number of births (for a constant population size)
beta <- R0 * ((mu + gamma) / gamma) * (mu + alpha + sigma)
