# variables
deriv(N) <- a * N

# initial conditions of the variables
initial(N) <- N_init

# parameter values
N_init <- user(1)             # total population size
a <- user(0.5)            # growth rate

# additional output
output(N_analytical) <- N_init * exp(a * t)