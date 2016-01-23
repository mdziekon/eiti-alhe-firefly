# Implementation of Ackley Function.
ackley <- function (x)
{
    n <- length(x)
    return(-20 * exp(-0.2 * sqrt((1/n) * sum(x^2))) - exp((1/n) *
          sum(cos(2 * pi * x))) + 20 + exp(1))
}
