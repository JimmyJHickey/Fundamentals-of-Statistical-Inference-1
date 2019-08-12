##############################################
##Text analysis of Jane Austen books
##Justin Post 2017
##############################################

#much of the code below is taken from 
#https://cran.r-project.org/web/packages/tidytext/vignettes/tidytext.html

#load in packages to read in data and manipulate it
library(janeaustenr)
library(dplyr)
library(stringr)

#load in book text
original_books <- austen_books()
#check a few lines of the book

#separate lines with chapter and line number
original_books <- austen_books() %>%
  group_by(book) %>%
  mutate(linenumber = row_number(),
         chapter = cumsum(str_detect(text, regex("^chapter [\\divxlc]",
                                                 ignore_case = TRUE)))) %>%
  ungroup()

#now read in tidytext library to enable easy parsing of the text
library(tidytext)
#create one entry per word
tidy_books <- original_books %>%
  unnest_tokens(word, text)

#remove "stop words" such as the, a, etc.
data("stop_words")
cleaned_books <- tidy_books %>%
  anti_join(stop_words)

#check how often words appear
cleaned_books %>%
  count(word, sort = TRUE) 

#do it by book, only keep the top 5 overall words (that aren't names)
popular_words<-cleaned_books %>%
  group_by(book) %>%
  count(word) %>%
  filter(word=="miss"|word=="time"|word=="dear"|word=="lady"|word=="sir")

#create observed joint distribution of book and word to look at relationship
matrix(popular_words$n,ncol=5,nrow=6,dimnames=list(levels(popular_words$book),c("dear","lady","miss","sir","time")))

#as percentages instead
round(matrix(popular_words$n,ncol=5,nrow=6,dimnames=list(levels(popular_words$book),c("dear","lady","miss","sir","time")))/sum(popular_words$n),3)

#just for the fun of it
library(wordcloud)

#make sure your plotting window is pretty large
cleaned_books %>%
  count(word) %>%
  with(wordcloud(word, n, max.words = 100))
