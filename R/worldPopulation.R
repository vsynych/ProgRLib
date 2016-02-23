require(XML)

theURL <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"
table <- readHTMLTable(theURL, which = 1, header = TRUE)

library("rvest")
# theURL <- "http://en.wikipedia.org/wiki/List_of_U.S._states_and_territories_by_population"
theURL <- "https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_population"

population <- theURL %>%
  read_html() %>%
  html_nodes(xpath='//*[@id="mw-content-text"]/table[1]') %>%
  html_table()
population <- population[[1]]

head(population)