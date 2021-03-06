% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/theme.R
\name{theme_eem_pdf}
\alias{theme_eem_pdf}
\title{ggplot color theme for enelmargen.org}
\usage{
theme_eem_pdf(font_size = 10, base_family = "Verdana",
  legend_right = TRUE, no_legendname = TRUE)
}
\arguments{
\item{font_size}{font size. Default = 10.}

\item{base_family}{base family. Default = Verdana.}

\item{no_legendname}{will the legend have no title? Default = TRUE}

\item{legend_bottom}{will the legend be at the bottom? Default = TRUE}
}
\description{
Style plots used in \emph{www.enelmargen.org} as of july-2015.
}
\details{
\code{theme_eem} implements the standard redish-brown
background theme.
\code{theme_eem_white} implements a variant with a white
panel and background.
\code{theme_eem_pdf} implements standard coloring theme optimized for use in rmarkdown to pdf documents.
}
\examples{
library(ggplot2)
library(eem)
dsamp <- diamonds[sample(nrow(diamonds), 1000), ]
q <- (qplot(carat, price, data=dsamp, colour=clarity)
     + labs(title = "Diamonds Are Forever"))
q + theme_eem_pdf()
}

