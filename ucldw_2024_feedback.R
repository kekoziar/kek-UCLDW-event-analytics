# https://www.tidytextmining.com/tidytext
library(tidytext)
library(textdata)

my.data<-read.csv("input/UCLDW2024_PresenterFeedback.csv")
my.data<-read.csv("input/UCLDW2024_AttendeeFeedback.csv")
df<-my.data

##########################################################
##########################################################
# CLEANING
##########################################################
# remove punctuation in column names
colnames(df)<-str_trim(colnames(df))
colnames(df)<-str_replace_all(colnames(df), "[:punct:]", "") # removes the ?'s
colnames(df)<-str_replace_all(colnames(df), "\\s+", ".") # the + tackles if there are one or more spaces to trim
colnames(df)<- str_to_lower(colnames(df))
colnames(df)

# changing column names to be more sensible; need to specify that rename is for dplyr if both dplyr and plyr are being used
### presenter
df<-dplyr::rename(df, role=wereyouaucldw2024organizerorpresenter, 
                  firstYear=ifthisyourfirstyearparticipatinginucldw, 
                  plus = fromyourexperienceasapresenterororganizerpleaseshareoneortwothingsyoulikedaboutucldw, 
                  delta = fromyourexperienceasapresenterororganizerpleaseshareoneortwothingswecanimproverelatedtoucldw, 
                  nextYear = wouldyouconsiderparticipatinginucldw2025)

### attendee
df<-dplyr::rename(df, workshops=whichsessionsdidyouattendcheckallthatapply, 
                  rating=overallhowwouldyourateyourexperienceofuclovedataweek2024, 
                  feedback = pleasetellusmoreaboutyourexperiencewhatdidyoulikewhatcanweimprovefornextyearwevalueyourfeedback, 
                  topics = whattopicsoreventswouldyouliketoseeatuclovedataweek2025, 
                  comments = doyouhaveanyothercommentsorfeedbackfortheuclovedataweekorganizers,
                  campus = whatisyourcampusaffiliation, 
                  domain = withwhatdomaindoyoumostidentify, 
                  role = areyoua
                  )
colnames(df)
df[is.na(df)] <- "emptyFeedback"
df[df == ""] <- "emptyFeedback"
df$response <- 1:length(df[,1])

domains <- c("Health sciences",
             "Humanities",
             "Life sciences",
             "Mathematical and computational sciences",
             "Physical sciences",
             "Social sciences")
df$domain[grep(paste(domains, collapse="|"), df$domain, invert=TRUE)] <- "Other"


topic <- "feedback" #because I want to change as little text as possible when looking at different columns

text_df <- tibble(response = 1:length(df[,topic]), text = df[,topic])
text_df %>%
  unnest_tokens(word, text)

ungroup() %>%
  unnest_tokens(word, text)

get_sentiments(lexicon = c("bing", "afinn", "loughran", "nrc"))

nrc_joy <- get_sentiments("nrc") %>% 
  filter(sentiment == "joy")

text_df %>%
  unnest_tokens(word, text) %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)


df$rating <- as.factor(df$rating)
df %>%
  unnest_tokens(word, feedback) %>%
  inner_join(nrc_joy) %>%
  count(word, sort = TRUE)
  

attendee_sentiment_full <- df %>%
  unnest_tokens(word, feedback)  %>%
  inner_join(get_sentiments("bing")) 

attendee_sentiment <- df %>%
  unnest_tokens(word, feedback)  %>%
  inner_join(get_sentiments("bing")) %>%
  count(rating, index = response, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

attendee_sentiment_domain <- df %>%
  unnest_tokens(word, feedback)  %>%
  inner_join(get_sentiments("bing")) %>%
  count(domain, index = response, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)

attendee_sentiment_role <- df %>%
  unnest_tokens(word, feedback)  %>%
  inner_join(get_sentiments("bing")) %>%
  count(role, index = response, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(sentiment = positive - negative)


library(ggplot2)

ggplot(attendee_sentiment, aes(index, sentiment, fill = rating)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~rating, ncol = 2, scales = "free_x")

ggplot(attendee_sentiment_domain, aes(index, sentiment, fill = domain)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~domain, ncol = 2, scales = "free_x")

ggplot(attendee_sentiment_role, aes(index, sentiment, fill = role)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~role, ncol = 2, scales = "free_x")


## Term frequency

rating_words <- df %>%
  unnest_tokens(word, feedback) %>%
  count(rating, word, sort = TRUE)

total_words <- rating_words %>% 
  group_by(rating) %>% 
  summarize(total = sum(n))

rating_words <- left_join(rating_words, total_words)

rating_tf_idf <- rating_words %>%
  bind_tf_idf(word, rating, n)

rating_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
##################
domain_words <- df %>%
  unnest_tokens(word, feedback) %>%
  count(domain, word, sort = TRUE)

total_words_domain <- domain_words %>% 
  group_by(domain) %>% 
  summarize(total = sum(n))

domain_words <- left_join(domain_words, total_words_domain)

domain_tf_idf <- domain_words %>%
  bind_tf_idf(word, domain, n)

domain_tf_idf %>%
  select(-total) %>%
  arrange(desc(tf_idf))
########################################
feedback_bigrams <- df %>%
  unnest_tokens(bigram, feedback, token = "ngrams", n=2) %>%
  filter(!is.na(bigram))

feedback_bigrams %>%
  count(bigram, sort=TRUE)

install.packages("igraph")
library(igraph)
bigram_graph <- feedback_bigrams %>%
  count(bigram, sort=TRUE) %>%
  filter( n> 3) %>%
  graph_from_data_frame()

install.packages("ggraph")
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)