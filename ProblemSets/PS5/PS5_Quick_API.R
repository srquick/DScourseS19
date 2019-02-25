library(twitteR)
library(tm)
library(tidytext)
library(dplyr)

request.URL = "https://api.twitter.com/oauth/request_token"
access.URL = "https://api.twitter.com/oauth/access_token"
author.URL = "https://api.twitter.com/oauth/authorize"
con.Key = "oSHhzey2ljAi3GeVoBkVBqyZj"
con.Secret = "AsmDgWyATC6pJ3a2UvK76U1YHjpMMMe48ZW0ncQdh0cJZLaOvd"
acc.Token = "766121358-zYExxt5yHQ6wKLOnANqAwuv7Byr9cPYjye4ax2yO"
acc.Secret = "iB3z1ZNrJVNABwgiPu9ifNp8XfmJiLm0yUleiebcnvT2S"

setup_twitter_oauth(con.Key, con.Secret, acc.Token, acc.Secret)

tweets <- searchTwitter('Energy', geocode='35.2093206,-97.4441789,50mi', n=300, retryOnRateLimit=1)

t.df <- twListToDF(tweets) 
View(t.df)

head(t.df$text)

t.df <- gsub("http.*","",t.df$text)
t.df <- gsub("#.*","",t.df)
t.df <- gsub("https.*","",t.df)
t.df <- gsub("RT","",t.df)
t.df <- gsub("@.*","",t.df)


data <- data.frame(tweets = as.character(t.df), stringsAsFactors = FALSE)
data <- data %>% unnest_tokens(word, tweets)
nrc <- get_sentiments("nrc")
data <- inner_join(data, nrc, by = "word")