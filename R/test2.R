Sys.setenv (R_GSCMD ="d:/program files/gs/gs9.18/bin/gswin64.exe")
library (extrafont)
loadfonts ()
pdf ("test2.pdf", family ="Comic Sans MS")
plot(runif(100), main ="Random Numbers", xlab ="x-axis label", ylab ="y-axis label", cex.main = 1.5, cex.lab = 1.5)
dev.off()
embed_fonts("test2.pdf")

## ggplot (Orange, aes (x = Tree, y = age, colour = circumference, size = circumference)) + geom_point()
Orange$Tree <- factor (Orange$Tree, levels = unique (Orange$Tree), ordered = T)
ggplot(Orange, aes (x = Tree, y = age, colour = circumference, size = circumference)) + geom_point()

##

with(data.frame(u = c(5,10,15,20,30,40,60,80,100),
                lot1 = c(118,58,42,35,27,25,21,19,18),
                lot2 = c(69,35,26,21,18,16,13,12,12)),
     list(summary(glm(lot1 ~ log(u), family = Gamma)),
          summary(glm(lot2 ~ log(u), family = Gamma))))

##

x <- 1:10
y <- sort(10*runif(10))
z <- runif(10)
z3 <- cbind(z, 2*runif(10), runif(10))
symbols(x, y, thermometers = cbind(.5, 1, z), inches = .5, fg = 1:10)
symbols(x, y, thermometers = z3, inches = FALSE)
text(x, y, apply(format(round(z3, digits = 2)), 1, paste, collapse = ","),
     adj = c(-.2,0), cex = .75, col = "purple", xpd = NA)

##

N <- nrow(trees)
with(trees, {
  ## Girth is diameter in inches
  symbols(Height, Volume, circles = Girth/24, inches = FALSE,
          main = "Trees' Girth") # xlab and ylab automatically
  ## Colours too:
  op <- palette(rainbow(N, end = 0.9))
  symbols(Height, Volume, circles = Girth/16, inches = FALSE, bg = 1:N,
          fg = "gray30", main = "symbols(*, circles = Girth/16, bg = 1:N)")
  palette(op)
})