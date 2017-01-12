# library tm for text mining R library 
library("tm")
library("SnowballC")
sms_raw <- read.csv("spam.csv", stringsAsFactors = FALSE)
sms_raw <- sms_raw[c('v1','v2')]
names(sms_raw) <- c('type','text')
sms_raw$type <- factor(sms_raw$type)

## creating corpus/bag of words
sms_corpus <- VCorpus(VectorSource(sms_raw$text))


### two ways of doing
sms_corpus_clean <- tm_map(sms_corpus,content_transformer(tolower))
sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)
sms_corpus_clean <- tm_map(sms_corpus_clean,removeWords, stopwords())
sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation)
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace)

sms_dtm <- DocumentTermMatrix(sms_corpus_clean)
### corpus cleaning 
# sms_dtm <- DocumentTermMatrix(sms_corpus, control = list(
#   tolower = TRUE,
#   removeNumbers = TRUE,
#   stopwords = TRUE,
#   removePunctuation = TRUE,
#   stemming = TRUE
# ))


### training data and labels
sms_dtm_train <- sms_dtm[1:4169, ]
sms_dtm_test  <- sms_dtm[4170:5559, ]

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

####spam messages
spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

#######
wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)
wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5))

