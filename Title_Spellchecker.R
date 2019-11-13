#!/usr/bin/Rscript

# This will create a simple spellcheck for Craigslist titles
# we'll use a perceptron to train the hyperparameters of the scoring function.

# Outline: 1. Select the words that have very low frequency.
#          2. For each word calculate the distance function to all other words based
#             on number of different letters, nearness on keyboard and phonetically.
#             Normalize by word length.
#          3. Check what the "nearest" words are with the highest frequency (frequency
#             as weights)
#          3. Then for "near" matches above a threshold report as "suspected" spelling
#             errors. 
#          4. Optimize this model's threshold and weighting formula to max F1 score.

# Ideally we could just use setdiff, but we care about not just unique letters but also their positioning.  The below returns only unique letters across both strings..
#difflettersOnlyUnique <- function(word1, word2){
#  return(setdiff(strsplit(word1, "")[[1]], strsplit(word2, "")[[1]]))
#}

# Instead, we'll use the below for string comparisons where the input is a list of characters used in a word.
diffletter <- function(list1, list2){
  #For every character we will compare them and if there is no match in list2 we will add it to our difference return vector.
  differences <- sapply(1:length(list1), function(i){
                                                      if(i>length(list2)){
                                                        list1[i]
                                                      }else if(list1[i]!=list2[i]){
                                                        list1[i]
                                                      }
                                                    })
  #This vector has Nulls wherever there are matches.  We will remove matches
  differences[sapply(differences, is.null)] <- NULL
  return(differences)
}

distance <- function(word1, word2, a, b, c){
  discrepancy <- 0
  word1 <- c(strsplit(word1, "")[[1]], "&") # "&" is added to pad the word length for later when we shift letters to check for missing letters
  word2 <- c(strsplit(word2, "")[[1]], "@") # "@" is added to pad the word length for later when we shift letters to check for missing letters
  n1 <- length(word1)-1
  n2 <- length(word2)-1
  #Increase score by a for each letter in the same position in word1 and word2.
  #Also increase score by b if 2 consecutive letters are swapped
  #Penalty c if a single letter is missing
  diff_1 <- diffletter(word1, word2)
  if(length(diff_1)>0){
    discrepancy <- a * length(diff_1)
  }
  if(n1>1){
    #check for switched letters
    diff_2 <- c()
    for(i in 1:min(c(n1-1,n2-1))){
      if(i==0){break} #necessary in the case n2 = 1
      wordalt1 <- word1
      wordalt1[i] <- word1[i+1]
      wordalt1[i+1] <- word1[i]
      diff_2 <- c(diff_2, length(diffletter(wordalt1,word2)))
    }
    diff_2 <- min(diff_2)
    if(diff_2<length(diff_1)){discrepancy <- a*diff_2 + 2*b}
    # check for missing letters
    diff_3 <- c()
    for(i in 0:min(c(n1, n2))){
      if(i==0){
        wordalt2 <- c(" ", word2)
      } else {
        wordalt2 <- c(word2[1:i], " ", word2[(i+1):(n2+1)])
      }
      diff_3 <- c(diff_3, length(diffletter(word1, wordalt2)))
    }
    for(i in 0:min(c(n1, n2))){
      if(i==0){
        wordalt1 <- c(" ", word1)
      } else {
        wordalt1 <- c(word1[1:i], " ", word1[(i+1):(n1+1)])
      }
      diff_3 <- c(diff_3, length(diffletter(wordalt1, word2)))      
    }
    diff_3 <- min(diff_3)
    if(diff_3<min(c(diff_2, length(diff_1))) & diff_2<length(diff_1)){
      discrepancy <- a*diff_3 +c
    } else if (diff_3<min(c(diff_2, length(diff_1))) & diff_2>=length(diff_1)) {
      discrepancy <- a*diff_3 + c
    }
  }
  #We will reduce discrepancy by 1 to account for the non-matching character added to the end of each string.
  normscore <- (discrepancy-1)/n1 
  return(c(normscore, paste("discrepancy: ", discrepancy-1)))
}

vectorizeddistance <- function(word, vocab){
  dist <- sapply(1:length(vocab), function(x) distance(word, vocab[x], 1, 0.5, 1)[1])
  dist <- sapply(1:length(vocab), function(x){if(word==vocab[x]){2}else{dist[x]}}) #This ensures when minimizing we do not match to self.
  return(dist)
}

vocabdata[,1] <- sapply(vocabdata[,1], as.character)

#nearest match will take a string and a dataframe with column "word" and "freq" and output the nearest match (chooses higher frequency) if ties exist (besides itself)
nearestmatch <- function(string, vocab){
  vocab$distance <- vectorizeddistance(string, vocab$word)
  index <- grep(paste("^", min(vocab$distance), "$", sep=""), vocab$distance) #This is identifying the rows of all min distance words.
  bestmatch <- index[grep(max(vocab$freq[index]), vocab$freq[index])] #finds highest frequency used of minimum distance words and return that row num.
  vocab$distance <- NULL
  return(c(vocab$word[bestmatch], vocab$freq[bestmatch]))
}

manualselect <- function(string, vocab){
  frequencyofstring <- vocab$freq[grep(paste("^", string, "$", sep=""), vocab$word)]
  print(paste("Frequency of word: ", frequencyofstring))
  nmatch <- nearestmatch(string, vocab)
  print(paste("Alternative spelling: ", nmatch[1], ".  Frequency of alt. spelling: ", nmatch[2]))
  response <- readline(prompt="Use alternate spelling (Y/N)?")
  if(tolower(str_trim(response))=="y" | tolower(str_trim(response))=="yes"){
    return(nmatch[1])
  } else if (tolower(str_trim(response))=="n" | tolower(str_trim(response))=="no"){
    return(string)
  } else {
    warning('Not a "Y" or "N"')
  }
}

autoselect <- function(string,vocab){
  frequencyofstring <- vocab$freq[grep(paste("^", string, "$", sep=""), vocab$word)]
  nmatch <- nearestmatch(string, vocab)
  if(frequencyofstring>0){
    freqratio <- as.integer(nmatch[2])/frequencyofstring
  } else {
    return(nmatch[1])
  }
  if(freqratio>2 & as.integer(nmatch[2])>round(mean(vocab$freq)/3)){
    return(nmatch[1])
  } else {
    return(string)
  }
}

#Let's test it!
if (sys.nframe() == 0){
  vocabdata <- read_csv("./NaturalLanguageProcessing-Spellchecker/title_vocab_sampler.csv")
  manualselect(vocabdata$word[1], vocabdata)
  vocabdata$suggest <- NA
  for(row in 1:nrow(vocabdata)){
    vocabdata$suggest[row] <- autoselect(vocabdata$word[row], vocabdata)
  }
}