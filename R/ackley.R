# Implementation of Ackley Function.
ackley <- function (x)
{
    n <- length(x)
    return(-20 * exp(-0.2 * sqrt((1/n) * sum(x^2))) - exp((1/n) *
          sum(cos(2 * pi * x))) + 20 + exp(1))
}

# Converts values of Ackley Function to matrix.
ackleyF <- function(min, max, res) {
    dist <- (max - min)
    sizes <- (dist * (1 / res)) + 1

    result <- matrix(, ncol = sizes, nrow = sizes)
    axis <- numeric(sizes)

    i <- min
    a <- 1
    while(i <= max) {
        j <- min
        b <- 1

        while(j <= max) {
            result[a, b] =  ackleyValue(c(i, j))

            j <- j + res
            b <- b + 1
        }

        axis[a] = i

        i <- i + res
        a <- a + 1
    }

    list(result = result, axis = axis)
}



