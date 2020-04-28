# Stochastic model for generating epidemic curves.

library(tidyverse)
library(magrittr)

set.seed(as.numeric(Sys.time()))

# Starting population

Pop_Setup <- function(n, seed) {
  Popn <- data.frame(Status = rep('S', n), Time = 0, Time_Infected = NA, Time_Recovered = NA)
  levels(Popn$Status) <- c('S', 'I', 'R')
  Popn$ID <- 1:nrow(Popn)
  Popn[1:seed,]$Status = 'I'
  Popn %<>% mutate(
    Time_Infected = ifelse(Status == 'I', 0, NA)
  )
  return(Popn)
}

# pairing function - simulate paired contacts and transmissions - returns a new data frame of infection status

Transmission <- function(Popn, Prob, time, Duration) {
  Trans <- bind_cols(
    Popn %>% sample_frac(size = 1), Popn
  ) %>%
    mutate(
      Thresh = runif(length(ID)),
      Time_Infected = ifelse(
        Status1 == 'I' & Status =='S' & Thresh <= Prob,
        time, Time_Infected
      ),
      Status = ifelse(
        Status == 'S' & Status1 == 'I' & Thresh <= Prob,
        'I', as.character(Status)
      ),
      Time_Recovered = ifelse(
        Status == 'I' & time - Time_Infected >= Duration,
        time, Time_Recovered),
      Status = ifelse(
        is.na(Time_Recovered),
        as.character(Status),
        'R'
      )
      ) %>%
    select(Status, ID, Time_Infected, Time_Recovered)
  return(Trans %>% arrange(ID))
}

# Population update function

Pupdate <- function(Popn) {
  Popn_Orig_Sum <- Popn %>%
    
    group_by(Status) %>% summarise(n = n())
  return(Popn_Orig_Sum)
}

# Individual simulation function

Sim <- function(n, seed, prob, duration, iterations) {
  
  Popn <- Pop_Setup(n, seed)
  a <- list()
  for (i in 1:iterations){
    data <- Pupdate(Popn)
    data$time <- i
    a[[i]] <- data
    Popn <- Transmission(Popn, prob, i, duration)
  }
  return(do.call(rbind, a))
}

# Ensemble SIM function

Ensemble_Sim <- function(n, seed_low, seed_high, prob_low, prob_high, duration, iterations) {
  a <- list()
  for (seed in seed_low:seed_high) {
    data = Sim(n, seed, prob_low, duration, iterations)
    data$seed = seed
    a[[seed]] = data
  }
  return(do.call(rbind, a))
}

# Run a few sims with preciselt the same parameters and plot

a <- list()
for (i in 1:50) {
  data = Sim(1000, 10, 0.25, 5, 50)
  data$run = i
  a[[i]] = data
}
b <- do.call(rbind, a)

ggplot(b, aes(x = time, y = n, col = Status)) + geom_point(alpha=0.3) + stat_smooth() +
  ggtitle('50 Individual Simulations: Stochastic, Susceptible:Infected:Recovered') + xlab('time') + ylab('population')
