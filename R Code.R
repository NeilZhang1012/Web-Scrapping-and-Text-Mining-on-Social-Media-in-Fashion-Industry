library(twitteR)
library(streamR)
library(RCurl)
library(RJSONIO)
library(stringr)
library(ROAuth)
library(tm)
library(SnowballC)
library(textstem)
library(plyr)
library(dplyr)
library(ggplot2)
library(wordcloud)
library(grid)

api_key <- "Your Twitter API Key"
api_secret <- "Your Twitter API Secret"
access_token <- "Your Twitter Access Token"
access_token_secret <- "Your Twitter Access Token Secret"
setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)

brands <- list("ASOS", "AnnTaylor", "ModCloth", "jcrew", "Uniqlo", "madewell", "Forever21", "cosstores", "rag_bone", "toryburch", "RebeccaMinkoff", "katespadeny", "MichaelKors", "CalvinKlein", "Coach")
getTweets <- function(brand, n = 10000) {
  search_string <- paste0("@", brand, " OR ", "#", brand, " OR ", brand)
  # get tweets
  tweets <- searchTwitter(search_string, since = "2017-05-01", n = n)
  # strip retweets
  tweets <- strip_retweets(tweets, strip_manual = TRUE, strip_mt = TRUE)
  # convert to data.frame
  tweetdf <- twListToDF(tweets)
  # add brand and return
  out <- cbind(brand, tweetdf)
  return(out)
}
tweets_by_brand <- ldply(brands, function(brand) getTweets(brand, n = 10000))

dim(tweets_by_brand)
head(tweets_by_brand, 5)

tweets_count <- data.frame(tweets_by_brand %>% group_by(brand) %>% summarise(tweets_count=n()) %>% arrange(desc(tweets_count)))
library(ggplot2)
ggplot(tweets_count, aes(brand, tweets_count)) + geom_bar(stat="identity", fill="blue") + xlab("Brand") + ylab("Tweets Count") + ggtitle("Clothing Brand Popularity") + theme(axis.text.x=element_text(angle=45, hjust=1), plot.title = element_text(hjust = 0.5))

tweets_text <- data.frame(text = tweets_by_brand$text, stringsAsFactors = F)
tweets_text$text <- gsub("http\\S+\\s*", "", tweets_text$text)
tweets_text$text <- gsub("w/", "", tweets_text$text)
tweets_text$text <- gsub("&amp", "", tweets_text$text)
tweets_text$text <- iconv(tweets_text$text, "latin1", "ASCII", sub="")
text_corpus <- Corpus(DataframeSource(tweets_text))
text_corpus <- tm_map(text_corpus, content_transformer(tolower))
my_stopwords <- c(stopwords(kind = "en"), "im", "youre", "hes", "shes", "its", "were", "theyre", "ive", "youve", "weve", "theyve", "id", "youd", "hed", "shed", "wed", "theyd", "ill", "youll", "hell", "shell", "well", "theyll", "isnt", "arent", "wasnt", "werent", "hasnt", "havent", "hadnt", "doesnt", "dont", "didnt", "wont", "wouldnt", "shant", "shouldnt", "can", "cant", "couldnt", "mustnt", "lets", "thats", "whos", "whats", "heres", "theres", "whens", "wheres", "whys", "hows", "RT")
text_corpus <- tm_map(text_corpus, removeWords, my_stopwords)
text_corpus <- tm_map(text_corpus, removePunctuation, preserve_intra_word_dashes = T)
text_corpus <- tm_map(text_corpus, stripWhitespace)
text_corpus <- tm_map(text_corpus, content_transformer(lemmatize_strings))

dtm <- DocumentTermMatrix(text_corpus)
dtm
dim(dtm)

freq <- sort(colSums(as.matrix(dtm)), decreasing = T)
head(freq, 100)
findFreqTerms(dtm, lowfreq = 350)
word_freq = data.frame(Term=names(freq), Count=freq)
ggplot(subset(word_freq, freq > 800), aes(Term, Count)) + geom_bar(stat="identity") + xlab("Term") + ylab("Frequency") + ggtitle("Word Frequency") + theme(axis.text.x=element_text(angle=45, hjust=1), plot.title = element_text(hjust = 0.5))
set.seed(1234)
wordcloud(names(freq), freq, min.freq = 550, random.order = FALSE, colors=brewer.pal(8, "Accent"), rot.per = 0)

findAssocs(dtm, term = "ragnbone", 0.2)
plot(dtm, terms = names(findAssocs(dtm,term="ragnbone",0.2)[["ragnbone"]]), corThreshold = 0.2, attrs=list(node=list(label="foo", fillcolor="lightgreen", fontsize="20", shape="ellipse")), edge=list(color="cyan"))

requestURL <- "https://api.twitter.com/oauth/request_token"
accessURL <- "https://api.twitter.com/oauth/access_token"
authURL <- "https://api.twitter.com/oauth/authorize"
consumerKey <- "t9CMDimaZVL9jVzWZPfpwHxoo"
consumerSecret <- "fPYPpmAOtK5NxHLDfOvTwwTHzcQUO0yriaNRniAJKnligCRFUH"
my_oauth <- OAuthFactory$new(consumerKey = consumerKey,
                             consumerSecret = consumerSecret,
                             requestURL = requestURL,
                             accessURL = accessURL,
                             authURL = authURL)
my_oauth$handshake(cainfo = system.file("CurlSSL", "cacert.pem", package = "RCurl"))
save(my_oauth, file = "my_oauth.Rdata")

load("my_oauth.Rdata")
filterStream(file.name = "tweets_fashion.json",
             track = c("fashion", "clothing", "clothes"),
             locations = c(-180, -90, 180, 90),
             timeout = 60*5,
             oauth = my_oauth)
tweets_stream_fashion <- parseTweets("tweets_fashion.json", simplify = FALSE)
dim(tweets_stream_fashion)
colnames(tweets_stream_fashion)

tweets_stream_fashion %>% group_by(country_code) %>% summarise(tweets_count=n()) %>% arrange(desc(tweets_count)) %>% top_n(20)

map.data <- map_data("state")
points <- data.frame(x = as.numeric(tweets_stream_fashion$place_lon), y = as.numeric(tweets_stream_fashion$place_lat))
points <- points[points$y < 50 & points$y > 25 & points$x < -66 & points$x > -125, ]
ggplot(map.data) + geom_map(aes(map_id = region), map = map.data, fill = "white", color = "grey20", size = 0.25) + expand_limits(x = map.data$long, y = map.data$lat) + theme(axis.line = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank(), panel.background = element_blank(), panel.border = element_blank(), panel.grid.major = element_blank(), plot.background = element_blank(), plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines")) + geom_point(data = points, aes(x = x, y = y), size = 1, alpha = 1/5, color = "darkblue")

