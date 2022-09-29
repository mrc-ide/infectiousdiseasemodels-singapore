# variables
deriv(S) <- Births - mu * S - beta * S * I / N 
deriv(I) <- beta * S * I / N - (mu + sigma) * I
deriv(R) <- sigma * I - mu * R 

# initial conditions of the variables
initial(S) <- N - I_init
initial(I) <- I_init
initial(R) <- 0

output(pop) <- S + I + R   
output(R0) <- beta / (sigma + mu)

# parameter values
N <- 1e6                    # total population size
I_init <- user(1)           # num infectious cases at start of epidemic
# beta <- R0 * (sigma + mu)  # transmission parameter
beta <- user(5)
sigma <- user(2)            # recovery rate (1 / mean duration infectiousness)
# R0 <- user(5)             # av num new cases caused by single infectious case
mu <- 1 / 52                # death rate (mean life expectancy of 1 year or 52 weeks)
Births <- N * mu            # number of births (for a constant population size)


