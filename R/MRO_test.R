system.time({ x <- replicate(5e3, rnorm(5e3)); tcrossprod(x) })

