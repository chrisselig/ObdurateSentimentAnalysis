
#Load libraries
library(XML); library(tidyverse);library(stringr);library(lubridate);library(tidytext)
library(ggplot2);library(wordcloud);library(reshape2)

#Load data
rawdata <- xmlToDataFrame("//CARNAGE-MOBILE/Personal/R coding/Data/deepdoop_movies.xml")
con <- file("//CARNAGE-MOBILE/Personal/R coding/Data/profane_list.txt", open = "r")
profanedata <- readLines(con)
profanedata <- as.data.frame(profanedata)

#Begin tidying data
tidydata <- rawdata %>%
  select(filmname,score,quote,reviewdate) %>%
  filter(!is.na(reviewdate)) 

#Clean up names
names(tidydata) <- c("filmName","rating","reviewText","reviewDate") 

#Convert ratings to integer
tidydata$rating <- as.numeric(tidydata$rating)

#Tidy the handwritten reviews by obdurate, to do some analysis
reviews <- data_frame(tidydata$reviewText)
reviews <- reviews[!reviews$`tidydata$reviewText`=="",]
names(reviews) <- "word"

reviews <- reviews %>%
unnest_tokens(word,word)

data(stop_words)

#remove certain words from being seen as negative (funny, plot, silly)
custom_stop_words <- bind_rows(data_frame(word = c("funny","plot","silly"),
lexicon = c("custom")),
stop_words)

#Remove stop words ("the","of", "to", etc)
reviews <- reviews %>%
anti_join(custom_stop_words)

#Remove Profanity
reviews <- reviews[!reviews$word%in%profanedata$profanedata,]
#reviews <- reviews %>%
#  anti_join(profanedata, by = c("word" = "profanedata"))

#Plot word counts
reviews %>%
count(word, sort = TRUE) %>%
filter(n > 150) %>%
mutate(word = reorder(word,n)) %>%
ggplot(aes(word,n)) +
geom_col() + 
coord_flip() + 
labs (x = "Word", y = "Frequency", title = "Word Frequencies With Greater than 150 Occurences")+
theme_classic()

sentMovies <- tidydata %>%
group_by(filmName) %>%
mutate(
movie = filmName) %>% #added so I can keep track of the movie the word is from
ungroup() %>%
unnest_tokens(word,reviewText)

#Get the words labelled as "positive" from the nrc sentiments package
nrcPositive <- get_sentiments("nrc") %>%
filter(sentiment == "positive")

#Join review text on positive words
sentMovies %>%
inner_join(nrcPositive) %>%
count(word, sort = TRUE)

#Compare positive and negative words
bing_word_counts <- sentMovies %>%
inner_join(get_sentiments("bing")) %>%
count(word, sentiment, sort = TRUE) %>%
ungroup()

reviews %>%
inner_join(get_sentiments("bing")) %>%
count(word,sentiment, sort = TRUE) %>%
acast(word ~ sentiment, value.var = "n", fill = 0) %>%
comparison.cloud(colors = c("#F8766D", "#00BFC4"),
max.words =300, scale = c(4,0.4))

bing_word_counts %>%
group_by(sentiment) %>%
top_n(10) %>%
mutate(word = reorder(word,n)) %>%
ggplot(aes(word,n,fill = sentiment)) + 
geom_col(show.legend = FALSE)+
facet_wrap(~sentiment,scales = "free_y") + 
labs(y="Contribution to sentiment",
x = NULL, title = "Top 10 Negative and Positive Words")+
coord_flip() +
theme_classic()

#Most frequent ratings
ratingsdist <- tidydata %>%
  group_by(rating) %>%
  summarize(
    ratingCount = n()) %>%
  ggplot(mapping = aes(x = rating, y = ratingCount)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label = ratingCount), vjust = -1.0, colour = "black", size = 2)+
  labs(x = "Rating", y = "Rating Frequency", title = "Rating Frequencies")+
  scale_x_continuous(breaks = seq(0,100,by = 5))+
  theme_classic()
ratingsdist
medianscore <- median(tidydata$rating)
meanscore <- round(mean(tidydata$rating))

