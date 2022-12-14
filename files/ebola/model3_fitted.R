################################################################################
# Model 3 elaborates on Model 2 by including an acute death infectious stage 
# representing the infectiousness associated with unsafe burials procedures.
# A given proportion of cases who dies and doesn't go to hospital and are buried 
# unsafely. For infectious cases who will die, we assume that a proportion 
# 'p_funeral' of onward transmission potential would occur during the acute 
# death stage mentioned above.
# 
# S = # susceptible 
# E = # latent (split in two for more realistic distributed latency)
# I_d = # early infectious - not yet hospitalised - will die
# I_r = # early infectious - not yet hospitalised - will recover
# H_d = # hospitalised - will die
# H_r = # community (non-hospitalised) - will recover
# C_d = # community (non-hospitalised) - will die
# C_r = # hospitalised - will recover
# R = # recovered
# A_d = # between death and buried (accute transmission during dead stage)
################################################################################

################################################################################
### model dynamics
################################################################################

deriv (S) <- -S / N * (beta_r * (I_r + C_r) + 
                         beta_d * (I_d + C_d + beta_acute_d * A_d))

deriv (E_1) <- S / N * (beta_r * (I_r + C_r) + 
                         beta_d * (I_d + C_d + beta_acute_d * A_d)) - 
                         gamma_1 * E_1
deriv (E_2) <- gamma_1 * E_1 - gamma_2 * E_2
deriv (I_d) <- cfr * gamma_2 * E_2 - sigma_h * I_d 
deriv (I_r) <- (1 - cfr) * gamma_2 * E_2 - sigma_h * I_r
deriv(H_d) <- p_hosp * sigma_h * I_d - sigma_d * H_d
deriv(C_d) <- (1 - p_hosp) * sigma_h * I_d - sigma_d * C_d
deriv(H_r) <- p_hosp * sigma_h * I_r - sigma_r * H_r
deriv(C_r) <- (1 - p_hosp) * sigma_h * I_r - sigma_r * C_r
deriv(A_d) <- (1 - p_safe) * sigma_d * C_d - sigma_acute_d * A_d
deriv(R) <- sigma_r * (H_r + C_r)
deriv(Dead) <- sigma_d * (H_d + C_d)

deriv(cumul_onset) <-  p_hosp * gamma_2 * E_2 
deriv(cumul_death_h) <-  sigma_d * H_d

### create delayed variables in order to compute non cumulative (weekly) 
### incidence and deaths variables
cumul_onset_delayed <- delay(cumul_onset, 7) # 7 for weekly delay
cumul_death_h_delayed <- delay(cumul_death_h, 7) # 7 for weekly delay
output(weekly_onset) <- cumul_onset - cumul_onset_delayed
output(weekly_death_h) <- cumul_death_h - cumul_death_h_delayed

### useful variables to output

Exposed <- E_1 + E_2
Infectious_Community <- I_d + I_r + C_d + C_r
Infectious_isolated <- H_d + H_r
unsafe_community_burial <- (1 - p_safe) * sigma_d * C_d
safe_community_burial <- p_safe * sigma_d * C_d

################################################################################
### initial numbuer of individuals in each compartment
################################################################################

initial(S) <- N
initial(E_1) <- 0
initial(E_2) <- 0
initial(I_d) <- I_init / 2
initial(I_r) <- I_init / 2
initial(H_d) <- 0
initial(C_d) <- 0
initial(A_d) <- 0
initial(H_r) <- 0
initial(C_r) <- 0
initial(R) <- 0 
initial(Dead) <- 0
initial(cumul_onset) <- 0
initial(cumul_death_h)  <- 0

################################################################################
### user defined parameters
################################################################################

N <- user(5e+5, min = 0) # population size with default value
I_init <- user(25.61, min = 0) # initial number of infected individuals ### default value was fitted to data
L <- user(9.92, min = 0) # mean latency
L_frac_1 <- user(0.212, min = 0, max = 1) # fraction of latency spent in first latent compartment
mu_d <- user(8.0, min = 0) # time from admission to death in days
sigma_acute_d <- user(1, min = 0) # 1 / mean time of acute death stage
mu_r <- user(16.98, min = 0) # mean time from admission to recovery in days
cfr <- user(0.5656, min = 0, max = 1) # case fatality ratio
p_hosp <- user(0.7, min = 0, max = 1) # proportion hospitalised
R0 <- user(2.69, min = 0) ### default value was fitted to data
    # R0 (assumed the same for those who stay in community and
    # (1) recover or (2) died with unsafe burial AND pFuneral transmission event 
    # occur after death for those dying
p_funeral <- user(0.5, min = 0, max = 1) # proportion of transmission potential occurring 
  # during funeral for those who die
t_intervention <- user(170, min = 0) # time of interventions
p_safe_before <- user(0.1, min = 0, max = 1) # proportion of safe burials before interventions
p_safe_after <- user(0.7, min = 0, max = 1) # proportion of safe burials after interventions
mu_h_before <- user(5.79, min = 0) # mean onset to hosp. delay before interventions ### default value was fitted to data
mu_h_after <- user(1.43, min = 0) # mean onset to hosp. delay after interventions ### default value was fitted to data

### compute other parameters from the ones above
gamma_1 <- 1 / (L * L_frac_1)
gamma_2 <- 1 / (L * (1 - L_frac_1))
sigma_h <- 1 / mu_h
sigma_d <- 1 / (mu_d - mu_h)
sigma_r <- 1 / (mu_r - mu_h)
beta_r <- R0 / mu_r
beta_d <- R0 * p_funeral / mu_d
beta_acute_d <- mu_d
# get an Rt (assuming no running out of suceptible, i.e. S~N)
Rt <- cfr * beta_d * (p_hosp * mu_h + (1 - p_hosp) * mu_d + 
     (1 - p_hosp) * (1 - p_safe) * beta_acute_d) + 
  (1 - cfr) * beta_r * (p_hosp * mu_h + (1 - p_hosp) * mu_r)

p_safe <- if (t <= t_intervention) p_safe_before else p_safe_after 
mu_h <- if (t <= t_intervention) mu_h_before else mu_h_after 

### additional things to output
output(mu_h) <- TRUE
output(p_safe) <- TRUE
