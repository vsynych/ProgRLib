### https://gist.githubusercontent.com/programmingr/a0f3fc3a7ee7b56b3266/raw/

### Standard Library of Commonly Used Functions
### Author: Bryan Shepherd
### Email [reversed for spam protection]: moc.rgnimmargorp@nayrb
### Version: 0.2
### Note: This Gist may change at any time. If you want to use it, you should fork your own version.

# Display some basic descriptives
descs <- function (x) {
  if(!hidetables) {
    if(length(unique(x))>30) {
      print("Summary results:")
      print(summary(x))
      print("")
      print("Number of categories is greater than 30, table not produced")
    } else {
      print("Summary results:")
      print(summary(x))
      print("")
      print("Table results:")
      print(table(x, useNA="always"))
    }

  } else {
    print("Tables are hidden")
  }
}

# Create dummies for each level of a categorical variable
# Returns dataframe
createDummies <- function(x, df, keepNAs = TRUE) {
  uniq.cats <- unique(df[, x])
  # Sanitize variable names
  # TODO: possibly set up to keep only a certain number of characters
  uniq.cats <- gsub("[[:space:][:punct:]]", "", uniq.cats)

  for (i in seq(1, length(uniq.cats))) {
    if(keepNAs) {
      df[, paste(x,".", uniq.cats[i], sep = "")] <- ifelse(df[, x] != uniq.cats[i], 0, 1)
    } else {
      df[, paste(x,".", uniq.cats[i], sep = "")] <- ifelse(df[, x] != uniq.cats[i] | is.na(df[, x]) , 0, 1)
    }
  }
  return(df)
}

# Recode values below zero to NA
# Adds variable and returns dataframe
recodeNegs <- function(x, df) {
  df[, paste(x,".noneg", sep = "")] <- ifelse(df[, x] < 0, NA, df[, x])
  return(df)
}

# Check if package is installed. If it is, load it. If it isn't, install then load it.
instalib <- function(x) {
  if(!suppressWarnings(require(x, character.only=TRUE))) {
    install.packages(x)
    library(x, character.only=TRUE)
  }
}
