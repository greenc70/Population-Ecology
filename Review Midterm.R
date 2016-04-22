## Vectors
x <- c(1,2,3,4)
Y <- c(1:4)
Z <- seq(from = 1, to = 4, by = 1)
## Which if any x, Y, Z are different?
x == Y
?all.equal
w <- 3 - 2.9
w == 0.1
## This shows up as false because of the number of zeroes that the calculation is made to.
q <- round(3 - 2.9, 1)
q == 0.1
## Now it is true that q is 0.1. 
## Looking at functions. Functions have name, arguments, and values
## Creating a function for a mean
## We want our function to take the mean of a vector and then add 10% to that average.
adjusted.mean <- function(x, cheat) {
  sum.of.x <- sum(x)
  n <- length(x)
  true.mean <- sum.of.x / n
  (1 + cheat) * true.mean
}
real.sales <- c(1:10)
adjust <- 0.1
adjusted.mean(x = real.sales, cheat = adjust)

## To study continous pop dynamics, often need to integrate non-linear functions.
## We need small but finite steps of dt. So hence deSolve
## ODE (ordinary differential equation) is a wrapper for suite of functions and gives us access to deSolve.

## Using ODE to access exponential functions
library(deSolve)
## now we should write out the ODE
exponential.growth <- function( t, y, p) {
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N
    return(list(dN.dt))
  })
}
## Note that the N is the name given to our y-value. 'with' is used to allow us to use
## the names given to the parameters, but only works if p is within the parameters listed
## as a named vector.
## ODE requires parameters and initial conditions to be specified.
p <- c('r' = 0.1)
y0 <- c('N' = 0.1)
t <- 1:25
## Now we need to input the parameters and initial conditions into the ODE
?ode
sim <- ode(y = y0, times = t, func = exponential.growth, parms = p, method = 'lsoda')
## We want an output which means putting it into a matrix thing. With columns and things.
head(sim)
class(sim)
sim.frame <- as.data.frame(sim)
## A data frame is used for storing tables. Header contains column names.
names(sim.frame)
names(sim.frame) <- c('t', 'abundance')
sim.frame$t
sim.frame$abundance
?points
## Now we want to graph the table we have created.
plot(abundance ~ t, data = sim.frame, type = 'l', lwd = 2, col = 'purple', bty = 'l')
## So now we have plotted abundance over time.

##Let's look at HW 2 Companion
##We should load the deSolve thing since we will be using an ODE to solve stuff.
library(deSolve)
##Now we need to write out our theta log function.
##First name it and then determine the parameters. Then assign a more world name to y.
##And then use that with command with p since p is a named vector.
log.growth <- function(t, y, p){
  N <- y[1]
  with(as.list(p), {
    dN.dt <- r * N * (1-(N / K)^theta)
  })}