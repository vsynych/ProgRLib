my.summary <-
function (x) print (c (" mean" = round (mean (x), 2), "median" = median (x), "mode" = names (sort (-table (x))) [1], "std" = round (sd (x), 2)))
