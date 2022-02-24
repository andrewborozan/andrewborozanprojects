library(RedditExtractoR)
Politics <- get_thread_content("https://www.reddit.com/r/politics/comments/syn5ca/us_house_candidate_apologizes_for_behavior_at/")

# extracts the df we want to look at with comments. Extracts the second object in the list and stores it in an object.
comments <- Politics[[2]]


# what does the other df look like?
otherdf <- Politics[[1]]
# metadata for the thread. 



# trying it with two threads at same time.
Politics2 <- get_thread_content(c(
  "https://www.reddit.com/r/politics/comments/sz2ffs/psaki_confirms_bidenputin_meeting_off_the_table/",
  "https://www.reddit.com/r/politics/comments/sz6fs9/trump_refers_to_russian_aggression_toward_ukraine/"
))
str(Politics2)
comments2 <- Politics2[[2]]
otherdf2 <- Politics2[[1]]


# This worked. You can put in multiple Reddit threads into the get_thread_content. 


# Here is searching for keywords and pulling a bunch of comments based on the keyword.
Cubbies <- find_thread_urls(keywords = "Chicago Cubs", sort_by = "top")


# Sentiment analysis. Install and load package. There are two: SentimentAnalysis and sentimentr. Let's look at both.

library(SentimentAnalysis)
library(sentimentr)

comment_sentiment <- sentiment(comments2$comment)
comment_sentiment %>% group_by(element_id) %>% summarize(sentiment = mean(sentiment), word_count = sum(word_count))


my_comments <- c(
  "What do you think we should do?",
  "Black hole sun.",
  "In my life, words, words, words, I don't know the words to this or any song.", 
  "I really suck at this. How was I ever musical?", 
  "Wash away the rain. Black hole sun, won't you come.", 
  "Is this enough sentences?"
)
my_sentences <- get_sentences(my_comments) #Breaks up sentences and puts them in a list. 
my_sentences

sentiment(my_sentences)
sentiment_by(my_sentences) %>% plot()
extract_emotion_terms(my_sentences)
