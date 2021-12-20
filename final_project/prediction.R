## MODEL
library(here)
library(dplyr)
library(ggplot2)
library(lubridate)
library(deSolve)

# DATA FOR REGION: Alabama
data_with_mandates <- read.csv(here('data', 
                                    'processed_data',
                                    'weekly_cases_data_plus_mandates-3.csv'))
alabama <- data_with_mandates %>% 
  filter(state =="Alabama") %>%
  mutate(date=as.Date(date))

startdate <- alabama[1,]$date

N <- alabama[1,]$population
# SIR FUNCTION
SIR <- function(time, state, parameters) {
  par <- as.list(c(state, parameters))
  with(par, {
    dS <- -beta * I * S/N
    dI <- beta * I * S/N - gamma * I
    dR <- gamma * I
    list(c(dS, dI, dR))
  })
}

# CREATE A VECTOR OF DAILY CUMULATIVE INCIDENCE NUMBERS OF ALABAMA FROM START DATE
infected <- alabama %>% 
  filter(new_cases>0) %>% 
  pull(cases)

# Create an incrementing Day vector the same length as our cases vector
day <- 1:(length(infected))
# now specify initial values for S, I and R
init <- c(S = N - infected[1], I = infected[1], R = 0)

RSS <- function(parameters) {
  names(parameters) <- c("beta", "gamma")
  out <- ode(y = init, times = day, func = SIR, parms = parameters)
  fit <- out[, 3]
  sum((infected - fit)^2)
}

# now find the values of beta and gamma that give the
# smallest RSS, which represents the best fit to the data.
# Start with values of 0.5 for each, and constrain them to
# the interval 0 to 1.0
optimization <- optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0,0), upper = c(1, 1))

# check for convergence
optimization$message

# Optimization Parameters
opt_par <- setNames(optimization$par, c("beta", "gamma"))
opt_par

# Reproduction Number
R0 <- opt_par[1]/opt_par[2]
R0

# PREDICTION
# time in days for predictions
t <- 1:150
# get the fitted values from our SIR model
fittedcum <- data.frame(ode(y = init, times = t, func = SIR, parms = opt_par))
# add a Date column and join the observed incidence data
fittedcum <- fittedcum %>%
  mutate(date = as.Date(startdate) + (t * 7) - 7, state = "Alabama") %>%
  left_join(alabama %>% select(date, cases))

# plot the data
ggplot(fittedcum, aes(x = date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_point(aes(y = cases), colour = "orange")+
labs(y = "Cumulative incidence", x="Date",
     title = "COVID-19 fitted vs observed cumulative incidence, alabama",
     subtitle = "(red=fitted incidence from SIR model, orange=observed incidence)")

# plot the data
ggplot(fittedcum, aes(x = date)) +
  geom_line(aes(y = I), colour = "red") +
  geom_line(aes(y = S), colour = "black") +
  geom_line(aes(y = R), colour = "green") +
  geom_point(aes(y = cases), colour = "orange") +
  scale_y_continuous(labels = scales::comma) +
  labs(y = "Persons", title = "COVID-19 fitted vs observed cumulative incidence, alabama province") +
  scale_colour_manual(name = "",
                      values = c(red = "red", black = "black", green = "green", orange = "orange"),
                      labels = c("Susceptible", "Recovered", "Observed incidence", "Infectious")) +
  scale_y_continuous(trans="log10")




alabama <- data_with_mandates %>% filter(state == "Illinois")
day_2 <- lubridate::ymd(alabama[1,]$mandate_start)
alabama <- alabama %>% mutate(mandate_start = as.Date(mandate_start))
alabama <- alabama %>% mutate(date = as.Date(date))

alabama <- alabama %>% filter(date >= mandate_start-days(30))
alabama <- alabama %>% filter(date <= mandate_start+days(60))
ggplot(alabama, aes(x = date, y=new_cases, group=1)) +
  +     geom_line(aes(y = new_cases_proyected), colour = "red") + 
  +     geom_line(aes(y = new_cases/1000), colour = "green") +
  +     annotate(geom = "vline",
                 +              x = c(day_2),
                 +              xintercept = c(day_2),
                 +              linetype = c("dashed"))
