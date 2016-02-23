#What the candidates say, analyzing republican debates using R


#As most people realize, this is probably one of the most data-rich primary campaigns in history, with hundreds of professional pollsters poring over every data-point trying to understand voter's intention.

#So here is another data-rich post to that end.

#I was glad to discover the University of California at Santa Barbara's webpage with tons of high-quality data related to the elections.

#Amongst these are the transcripts of presidential debates going back to 1960, which I will pore over a bit further.

#Because the Republican race is arguably more fun to watch, i'll be concentrating on these debates.

#Getting and cleaning the data

#A few things to set-up before downloading the data:
  
#  Update (1/4/2015) - Thanks to Alan Jordan who nicely corrected my regex with a more robust version and a more sucinct function for cleaning the data below..

# some packages for scraping and cleaning the data
library(rvest)
library(plyr)
library(dplyr)
library(stringi)
library(magrittr)

#++++++++++++++++++++++++++++++++++
# rquery.wordcloud() : Word cloud generator
# - http://www.sthda.com
#+++++++++++++++++++++++++++++++++++
# x : character string (plain text, web url, txt file path)
# type : specify whether x is a plain text, a web page url or a file path
# lang : the language of the text
# excludeWords : a vector of words to exclude from the text
# textStemming : reduces words to their root form
# colorPalette : the name of color palette taken from RColorBrewer package, 
# or a color name, or a color code
# min.freq : words with frequency below min.freq will not be plotted
# max.words : Maximum number of words to be plotted. least frequent terms dropped

# value returned by the function : a list(tdm, freqTable)
rquery.wordcloud <- function(x, type=c("text", "url", "file"), 
                             lang="english", excludeWords=NULL, 
                             textStemming=FALSE,  colorPalette="Dark2",
                             min.freq=3, max.words=200)
{ 
  library("tm")
  library("SnowballC")
  library("wordcloud")
  library("RColorBrewer") 
  
  if(type[1]=="file") text <- readLines(x)
  else if(type[1]=="url") text <- html_to_text(x)
  else if(type[1]=="text") text <- x
  
  # Load the text as a corpus
  docs <- Corpus(VectorSource(text))
  # Convert the text to lower case
  docs <- tm_map(docs, content_transformer(tolower))
  # Remove numbers
  docs <- tm_map(docs, removeNumbers)
  # Remove stopwords for the language 
  docs <- tm_map(docs, removeWords, stopwords(lang))
  # Remove punctuations
  docs <- tm_map(docs, removePunctuation)
  # Eliminate extra white spaces
  docs <- tm_map(docs, stripWhitespace)
  # Remove your own stopwords
  if(!is.null(excludeWords)) 
    docs <- tm_map(docs, removeWords, excludeWords) 
  # Text stemming
  if(textStemming) docs <- tm_map(docs, stemDocument)
  # Create term-document matrix
  tdm <- TermDocumentMatrix(docs)
  m <- as.matrix(tdm)
  v <- sort(rowSums(m),decreasing=TRUE)
  d <- data.frame(word = names(v),freq=v)
  # check the color palette name 
  if(!colorPalette %in% rownames(brewer.pal.info)) colors = colorPalette
  else colors = brewer.pal(8, colorPalette) 
  # Plot the word cloud
  set.seed(1234)
  wordcloud(d$word,d$freq, min.freq=min.freq, max.words=max.words,
            random.order=FALSE, rot.per=0.35, 
            use.r.layout=FALSE, colors=colors)
  
  invisible(list(tdm=tdm, freqTable = d))
}

#++++++++++++++++++++++
# Helper function
#++++++++++++++++++++++
# Download and parse webpage
html_to_text<-function(url){
  library(RCurl)
  library(XML)
  # download html
  html.doc <- getURL(url)  
  #convert to plain text
  doc = htmlParse(html.doc, asText=TRUE)
  # "//text()" returns all text outside of HTML tags.
  # We also don't want text such as style and script codes
  text <- xpathSApply(doc, "//text()[not(ancestor::script)][not(ancestor::style)][not(ancestor::noscript)][not(ancestor::form)]", xmlValue)
  # Format text vector into one character string
  return(paste(text, collapse = " "))
}
# function to partially separate and clean into a data.frame a debate from the presidency project

# Function to fill data frame with previous name 
# (a run-on speech in the debate is recorded as a set of paragraphs)
FillEmptyDF <- function(df, col){
  vector.of.nas <- is.na(df[,col])
  vector.of.names <- df[,col]
  #dataframe
  vector.df <- data.frame(change = vector.of.nas, 
                          names = vector.of.names)
  vector.of.rows <- as.numeric(rownames(vector.df[vector.of.nas,]))
  vector.of.realnames <- vector.of.rows-1 
  # clean if has smaller than 1 
  vector.of.realnames[vector.of.realnames <1] <- 1
  vector.df[vector.of.rows,] <- vector.df[vector.of.realnames,]
  
  df[, col] <- vector.df['names']
  return(df)
}

# function to see if there are still NA's in data.frame 
CheckDF <- function (df, errors = FALSE) 
{
  row.has.na <- apply(df, 1, function(x) {
    any(is.na(x))
  })
  if (sum(row.has.na) > 0) {
    r <- TRUE
  }
  else {
    r <- FALSE
  }
  if (errors) {
    return(row.has.na)
    return(r)
  }
  else {
    return(r)
  }
}

MakeDebateDF<-function(df){
  newdf <- data.frame(
    person = apply(df, 
                   MARGIN = 1, 
                   function(x){
                     stri_extract_first_regex(x, 
                                              "[A-Z'-]+(?=(:\\s))")
                   }),
    message = apply(df, 
                    MARGIN = 1, 
                    function(x){
                      stri_replace_first_regex(x,
                                               "[A-Z'-]+:\\s+", 
                                               "")
                    }),
    stringsAsFactors=FALSE
  )
  for (j in 2:nrow(newdf)) { 
    if (is.na(newdf[j,'person'])) 
    {newdf[j,'person'] <-  newdf[(j-1),'person'] }
  }
  
  return(newdf)
}

#Now, to download the last 4 debates (i'm only going to analyze the "big-boy" debates between top contenders and omit New Hampshire because it is not in the database). I'll use the main webpage for the presidency project:
  
  # Importing debates --- 
  # url for all debates
  url <- "http://www.presidency.ucsb.edu/ws/index.php?pid="

#And download and fix each debate:
  
  ### -------- debate in Wisconsin (fourth debate)
  wisconsin <- "110908"

debate_w <- read_html(paste0(url, wisconsin)) %>% 
  html_nodes("p") %>%
  html_text()

debate_w <- ldply(debate_w, rbind)
debate_w <- MakeDebateDF(debate_w)

### -------- debate in Boulder, Col. (third debate)
boulder <- "110906"

debate_b <- read_html(paste0(url, boulder)) %>% 
  html_nodes("p") %>%
  html_text()

debate_b <- ldply(debate_b, rbind)
debate_b <- MakeDebateDF(debate_b)

### -------- debate in Simi Valley, California (second debate)
california <- "110756"

debate_c <- read_html(paste0(url, california)) %>% 
  html_nodes("p") %>%
  html_text()

debate_c <- ldply(debate_c, rbind)
debate_c <- MakeDebateDF(debate_c)

### -------- debate in Cleveland, Ohio (first debate)
ohio <- "110489"

debate_h <- read_html(paste0(url, ohio)) %>% 
  html_nodes("p") %>%
  html_text()

debate_h <- ldply(debate_h, rbind)
debate_h <- MakeDebateDF(debate_h)

#The way the webpage is structured means that there are paragraph breaks even if it's the same candidate speaking. Using one of the previous functions, i'll fix this:
  
  # loop until clean is finished... 
  # wisconsin
  while(CheckDF(debate_w[2:length(debate_w[,1]),])){
    debate_w <- FillEmptyDF(debate_w, 1)
  }
# boulder
while(CheckDF(debate_b[2:length(debate_b[,1]),])){
  debate_b <- FillEmptyDF(debate_b, 1)
}
# california
while(CheckDF(debate_c[2:length(debate_c[,1]),])){
  debate_c <- FillEmptyDF(debate_c, 1)
}
# ohio
while(CheckDF(debate_h[2:length(debate_h[,1]),])){
  debate_h <- FillEmptyDF(debate_h, 1)
}


#Analyzing

#Now, for the fun part. First, let's start with some simply word-clouds (using this excellent example as a starting point)

#I'm going to use the rquery.wordcloud the function, taken shamelessly from sthda.com in the previous link to see what contenders like to talk about the most.

# Join into large d.f.
all_debates <- rbind(debate_w, 
                     debate_b,
                     debate_c,
                     debate_h)

# these are necesary for plots 
library(ggplot2)
# this is for order_axis and theme_eem
# it can be downloaded using 
# devtools::install_github("eflores/eem")
library(eem)
#library(EEM)

trump_words <- apply(subset(all_debates, person == "TRUMP")['message'],
                     1,
                     paste)
# cloud
library(RCurl)
trump_cloud <- rquery.wordcloud(trump_words, 
                                "text", 
                                max.words = 300,
                                excludeWords = c("going","and",
                                                 "applause","get",
                                                 "got","let"))

trump_freq <- trump_cloud$freqTable

# top 10
trump_top10 <- ggplot(order_axis(
  trump_freq[1:10,],
  word, freq), 
  aes(x = word_o, 
      y = freq))+
  geom_bar(stat="identity",
           fill = eem_colors[1]) +
  theme_eem() + 
  labs(title = "Top 10 words in Debates \n Donald Trump", 
       x = "Word",
       y = "Frequency")


#Donald really likes "great" more than the other candidates.

trump_wordcloud

trump_top10

#Using the same process. What about Jeb Bush? He prefers to mention Hillary.

bush_wordcloud

#bush_top10
bush_top10
#Marco Rubio likes to present his tax plan.

rubio_wordcloud

rubio_top10

#And the notoriously outspoken Cruz omits "people", talks tax and prefers to confront "washington" more than his colleagues:
  
cruz_wordcloud

cruz_top10

#More stats!
  
#  The former stats are all about the total participation in debates, but more interesting is probably the way these candidates have (if they have), shifted views over the course of these debates.

#It would be interesting to do some simple arithmetic on the corpus of words.

UnlistAndExtractInfo <- function(candidate){
  # this function is not general - it only applies to these particular debates...
  # all the debates must be named the same in the parent env.
  # for example: debate_h ...
  
  allwords_1 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_h, person == candidate)['message'],
        1,
        paste))))
  allwords_2 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_c, person == candidate)['message'],
        1,
        paste))))
  allwords_3 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_b, person == candidate)['message'],
        1,
        paste))))
  allwords_4 <- tolower(unlist(
    stri_extract_all_words(
      apply(
        subset(debate_w, person == candidate)['message'],
        1,
        paste))))
  df_insights <- data.frame(
    debate = c("Ohio", "California", "Colorado", "Wisconsin"),
    average_intervention = c(mean(stri_count_words(
      apply(
        subset(debate_h, person == candidate)['message'],
        1,
        paste))),
      mean(stri_count_words(
        apply(
          subset(debate_c, person == candidate)['message'],
          1,
          paste))),
      mean(stri_count_words(
        apply(
          subset(debate_b, person == candidate)['message'],
          1,
          paste))),
      mean(stri_count_words(
        apply(
          subset(debate_w, person == candidate)['message'],
          1,
          paste)))
    ),
    words_total = c(length(allwords_1),length(allwords_2),length(allwords_3),length(allwords_4)),
    words_unique = c(length(unique(allwords_1)),
                     length(unique(allwords_2)),
                     length(unique(allwords_3)),
                     length(unique(allwords_4))),
    words_repeated_fromfirst = c(0, sum(allwords_2 %in% allwords_1), 
                                 sum(allwords_3 %in% allwords_1),
                                 sum(allwords_4 %in% allwords_1)),
    unique_words_repeated_fromfirst = c(0,
                                        length(unique(allwords_2[allwords_2 %in% allwords_1])),
                                        length(unique(allwords_3[allwords_3 %in% allwords_1])),
                                        length(unique(allwords_4[allwords_4 %in% allwords_1]))
    ),
    words_repeated_fromsecond = c(0, 0, 
                                  sum(allwords_3 %in% allwords_2),
                                  sum(allwords_4 %in% allwords_2)),
    unique_words_repeated_fromsecond = c(0, 0,
                                         length(unique(allwords_3[allwords_3 %in% allwords_2])),
                                         length(unique(allwords_4[allwords_4 %in% allwords_2]))
    ),
    words_repeated_fromthird = c(0, 0, 0,
                                 sum(allwords_4 %in% allwords_3)),
    unique_words_repeated_fromthird = c(0, 0, 0,
                                        length(unique(allwords_4[allwords_4 %in% allwords_3]))
    )   
    , stringsAsFactors = FALSE)
  return(df_insights)
}

# going to create a data frame with all the counts from the top candidates...
candidates <- c("TRUMP","CARSON","RUBIO",
                "KASICH","CRUZ","BUSH",
                "FIORINA","PAUL","CHRISTIE")
info <- NULL
info_all <- NULL
for(i in 1:9){
  info <- UnlistAndExtractInfo(candidates[i])
  info$CANDIDATE <- candidates[i]
  info_all <- rbind(info_all, info)
}

# i'm going to add a few more columns...
info_all %<>% mutate(carry_over_p1 = unique_words_repeated_fromfirst/words_unique,
                     word_repeat = words_total/words_unique)


#Let's make some nice graphs with this information.

# graph of most words spoken by debate
ggplot(order_axis(
  subset(info_all, debate != "Ohio" & CANDIDATE != "CHRISTIE"), # christie didn't go to wisconsin
  CANDIDATE, carry_over_p1), 
  aes(x = CANDIDATE_o, 
      y = carry_over_p1)) + 
  geom_bar(stat = "identity", 
           aes(fill = CANDIDATE_o)) + 
  facet_grid(debate ~.) + 
  theme_eem() +
  scale_fill_eem(20) + 
  labs(title = "Repetition of words by candidate", 
       x = "Candidate", 
       y = "% of unique words repeated from first debate")

#If we take the full amount of unique words from the first debate, it's clear the candidates haven't been saying very different things. For example in California (the second debate), 41% of the words Trump said were the same he said in Ohio. The Donald is arguably the most repetitive, increasing this to 49% and 48% in Colorado and Wisconsin.

#On the other hand, Fiorina always seems to have a surprise for viewers.

#repetitions

#Although this could simply be due to the fact that she got very few words in the first debate (the outlier at the bottom is Fiorina) .

ggplot(subset(info_all,CANDIDATE != "CHRISTIE"), 
       aes(x = words_total, 
           y = words_unique)) + 
  geom_point(aes(colour = CANDIDATE), size = 3, shape = 2) +
  theme_eem()+ # uses "eflores/eem"
  scale_colour_eem(20) + # uses "eflores/eem"
  labs(title = "Words per Debate",
       x = "Total Words", 
       y = "Unique Words")

#repetitions

# average length of interventions

ggplot(info_all, 
       aes(x = factor(CANDIDATE), 
           y = average_intervention, 
           fill = eem_colors[1])) + # the eem colors are from "eflores/eem"
  geom_boxplot() +
  theme_eem()+
  labs(title = "Average words per intervention",
       x = "Candidate", 
       y = "Words") + theme(legend.position = "none")
#When it comes to the average length of "interventions" (I define one as the slot a candidate is speaking in without being interrupted), Fiorina and Trump like to keep it short and simple while Rubio takes his time.

#words_int

# average times unique word is repeated...

ggplot(info_all, 
       aes(x = factor(CANDIDATE), 
           y = word_repeat, fill = eem_colors[1])) +
  geom_boxplot() +
  theme_eem()+
  labs(title = "Average repetition of unique words",
       x = "Candidate", 
       y = "Repetitions") + theme(legend.position = "none")


#Another interesting tid-bit is the average repetitions of words. Again, Trump seems like an outlier, he repeated an average of 5 times every unique word in the California debate and has been repetitive since then.

#repetition_avg

#A trend seems to be emerging: Trump repeats the same thing every debate. But there should be much more proof after a few more debates.
