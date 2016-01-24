# Implementation of Ackley Function.
ackley <- function (x)
{
    n <- length(x)
    a <- 20
    b <- 0.2
    c <- (2 * pi)
    return(-a * exp(-b * sqrt((1/n) * sum(x^2))) - exp((1/n) *
          sum(cos(c * x))) + a + exp(1))
}

# x^2 function
x2func <- function (x)
{
    return (x * x)
}

# Implementation of Multimodal Function.
multimodal <- function(x)
{
    n <- length(x)
    beta <- 15
    m <- 5
    return(
        ( exp( -sum( (x / beta) ^ (2*m) )) - 2*exp( -sum( (x - pi)^2 )) ) *
            prod( (cos(x))^2 )
    )
}
