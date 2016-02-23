
---
title: "test"
author: "me"
date: "November 30, 2015"
output: html_document
---

Example text outside R code here; we know the value of pi is \Sexpr{pi}.

<<my-label, eval=TRUE, dev='png'>>=
  set.seed(1213)  # for reproducibility
x = cumsum(rnorm(100))
mean(x)  # mean of x
plot(x, type = 'l')  # Brownian motion
@
  
  Other text outside R code here.