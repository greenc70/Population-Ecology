library (deSolve)
## Function for pred-prey-parasite
pred.prey <- function (t, y, p) {
  H <- y [1]
  Z <- y [2]
  P <- y [3]
  with(as.list(p), {
    dH.dt <- r * H * (1 - H / K) - b * H * Z
    dZ.dt <- c * H * Z - m * Z - d * Z * P
    dP.dt <- e * Z * P - n * P
    return(list(c(dH.dt, dZ.dt, dP.dt)))
  })
}
## Specify the parameters and initial conditions
p <- c('r' = 1,
       'b' = 1,
       'c' = 1,
       'K' = 1,
       'm' = 0.1,
       'd' = 1,
       'e' = 1,
       'n' = 0.1)
y0 <- c('H' = 1, 'Z' = 0.1, 'P' = 0.1)
t <- 1:100

## Simulate
sim <- ode(y = y0, times = t, func = pred.prey, parms = p, method = 'lsoda')
sim <- as.data.frame(sim)

## Plot Time Series
plot(H ~ time, data = sim, type = 'l', col = 'green', bty = 'l', lwd = 2)
points(Z ~ time, data = sim, type = 'l', col = 'purple', lty = 2, lwd = 2)
points(P ~ time, data = sim, type = 'l', col = 'darkblue', lty = 3, lwd = 1)

## Plot Phase Space and Attractor
plot(Z ~ H, data = sim, type = 'p', bty = 'l', pch = 20)
