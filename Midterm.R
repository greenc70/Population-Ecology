library (deSolve)
log.growth <- function(t, y, p){
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1-(N / K)^theta)
    return(list(dN.dt))
  })
}


p <- c('r' = 0.2, 'K' = 1.05, 'theta' = 1.05)
y0 <- c('N' = 0.01)
t <- 1:100

sim <- ode(y = y0, times = t, func = log.growth, parms = p, method = 'lsoda')
sim <- as.data.frame(sim)

## plot sim
?points
plot(N ~ time, dat = sim, type = 'l', col = 'red')

p.gra <- c('r' = 0.28, 'K' = 0.75, 'theta' = 1.25)
sim.2 <- ode (y = y0, times = t, func = log.growth, parms = p.gra, method = 'lsoda')
sim.2 <- as.data.frame(sim.2)

## plot sim.2
plot (N ~ time, dat = sim.2, type = 'p', col = 'blue')

p.pea <- c('r' = 0.15, 'K' = 1, 'theta' = 1)
sim.3 <- ode (y = y0, times = t, func = log.growth, parms = p.pea, method = 'lsoda')
sim.3 <- as.data.frame(sim.3)

##plot sim. 3
plot (N ~ time, dat = sim.3, type = 'p', col = 'green')

## Plotting
?points
points(x = sim, y = NULL, type = 'l', col = 'red')
points(x = sim.2, y = NULL, type = 'l', col = 'blue')
points(x = sim.3, y = NULL, type = 'l', col = 'green')

?diff

##Taking the derivatives


sim$deriv <- c(diff(sim$N), NA)
sim$N[which(sim$deriv == max(sim$deriv, na.rm = TRUE))]
plot(deriv ~ N, data = sim, type = 'l', col = 'blue', bty = 'l')

sim.2$deriv <- c(diff(sim.2$N), NA)


plot(deriv ~ N, data = sim.2, type = 'l', col = 'green', bty = 'l')


sim.2$N[which(sim.2$deriv == max(sim.2$deriv, na.rm = TRUE))]

sim.3$deriv <- c(diff(sim.3$N), NA)
plot(deriv ~ N, data = sim.3, type = 'p', col = 'purple')
points (deriv ~ N, data = sim, type = 'l', col = 'red')
points (deriv ~ N, data = sim.2, type = 'l', col = 'green')
