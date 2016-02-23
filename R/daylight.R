library(XML2R)
cen <- "http://www.nyooztrend.com/sitemap.xml"
census <- XML2R(cen)

web <- readLines('http://aa.usno.navy.mil/cgi-bin/aa_durtablew.pl?
                 form=1&year=2016&task=-1&state=MI&place=detroit')
dur <- web[20:50]
# df <- as.data.frame(dur)
