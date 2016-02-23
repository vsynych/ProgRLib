set.seed(7637)
x <- runif(100, -3, 3)
curve(dnorm (x, mean (x), sd (x)), 
        col =" darkred", lwd = 3, ann = F)
par(new = T)
plot(density(x), col =" forestgreen", 
        lty = 2, lwd = 3, 
        main =" Normal Curve and\ n Kernel Density Plot", 
        sub =" 3/16/14", xlab ="", axes = F)
legend("topright", lty = 1: 2, lwd = 3, 
        col = c("darkred", "forestgreen"), 
        legend = c("normal curve", "Kernel density curve"))
par(xpd = T)
set.seed(4957)
salary <- data.frame(gender = 
        sample(c(" male"," female"), 200, replace = T), 
        x = rep(1:10, each = 20), y = runif(200, 1, 100))

with(salary, plot(x, y, pch = 19, cex =.9, 
        col = gender, xlab = expression(paste(
        "Monthly Salary in ", italic("Thousands"))), ylab = 
        expression(bolditalic("population in Millions"))))

legend(-0.5, -8, pch = 19, col = 1:2, legend = c(
        "men","women"), bg ="bisque1", ncol = 2)
title(main = expression(paste(underline(italic(
  "Salary Distribution")))), cex.main = 1.5)
        