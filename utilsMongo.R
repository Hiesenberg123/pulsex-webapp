library(shinythemes)
library(wordcloud2)
library(knitr)
library(DT)
library(shinydashboard)
library(shinyWidgets)

library(rtweet) 
library(RSQLite)
library(ggplot2)
library(dplyr)
# text mining library
library(tidytext)
library(igraph)
library(ggraph)
library(lattice)   # word-cloud generator 
library(RColorBrewer) # color palettes
library(stopwords) 

library(rtweet)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(utf8)
library(latexpdf)
library(stringr)
library(twitteR)
library(qdapRegex)
library(DT)
library(rvest)
library(xml2)
library(chron)

library(keras)
library(dplyr)
library(ggplot2)
library(purrr)
library(quanteda)

library(gutenbergr)
gutenberg_metadata

library(dplyr)
library(janeaustenr)
library(tidytext)

library(tidytext)

get_sentiments("bing")

library(janeaustenr)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)

library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
library(igraph)
library(ggraph)

library(widyr)
library(stringr)
library(magrittr)
library(shiny.semantic)
library(tidyr)
library(mongolite)

# # load("~/Desktop/Social_Media_Sentiment_Analysis/pulsex_DB/data_tweet.Rda")
# load("../../data_tweet.Rda")
# # load("~/Desktop/Social_Media_Sentiment_Analysis/pulsex_DB/Final_DB.Rda")
# load("Final_DB.Rda")
# djt<- New
# djt$date<- as.Date(djt$created_at, format = "yyyy-mm-dd")

connection_string = "mongodb://127.0.0.1:27017"
New = mongo(collection="data_tweets", db="dummy", url=connection_string)
Final_DB = mongo(collection="tweets", db="dummy", url=connection_string)

choice2<- matrix(NA, nrow = 6, ncol = 2)%>% as.data.frame()
colnames(choice2)<- c("topic", "sub")
choice2$topic<- c("Leader", "Leader", "Leader", "Leader", "List", "Issue")
choice2$sub<- c("Yogi Adityanath", "Akhilesh Yadav", "Smriti Irani", "Gaurav Bhatia", "UP Media", "Law & Order")

myGrid <- grid_template(
  default = list(
    # Here we define the data.frame describing our layout
    # The easiest way is to use rbind so that the layout can be 'visualized' in code
    areas = rbind(
      c("header", "input1", "input2", "input3", "input4", "submit"),
      c("info1", "info1", "info2", "info2", "info3", "info3"),
      c("freq",   "freq",   "freq", "senti", "senti", "senti"),
      c("active",   "active",   "active",   "ht", "ht", "ht"),
      c("cor",   "cor",   "cor", "wd_fq", "wd_fq", "wd_fq"),
      c("tweet_table",   "tweet_table",   "tweet_table", "wcloud", "wcloud", "wcloud"),
      c("note", "note", "note", "note", "note", "note")
    ),
    # Then we define the dimensions of the different elements of the layout
    # We can use any valid css units to make the layout behave exactly as desired
    rows_height = c("100px", "150px", "2fr", "2fr", "2fr", "2fr", "100px"),
    cols_width = c("30%", "10%", "20%", "10%", "20%", "10%")
  ),
  # This is optional, but we can define a specific layout for mobile (screen width below 768px)
  mobile = list(
    areas = rbind(
      c("header", "input1", "input2", "input3", "input4", "submit"),
      c("info1", "info1", "info2", "info2", "info3", "info3"),
      c("freq",   "freq",   "freq", "senti", "senti", "senti"),
      c("active",   "active",   "active",   "ht", "ht", "ht"),
      c("cor",   "cor",   "cor", "wd_fq", "wd_fq", "wd_fq"),
      c("tweet_table",   "tweet_table",   "tweet_table", "wcloud", "wcloud", "wcloud"),
      c("note", "note", "note", "note", "note", "note")
    ),
    rows_height = c("100px", "150px", "2fr", "2fr", "2fr", "2fr", "100px"),
    cols_width = c("30%", "10%", "20%", "10%", "20%", "10%")
  )
)


# twitter_sub <- function(twitter_db, sub, date_start, date_end, lang){
#   # twitter_1<- twitter_db%>% subset(date> date_start & date< date_end)
#   # result <- New$find('{ "created_at": {"$gte": { "$date": "2021-09-15T00:00:00.000Z"}, "$lte": { "$date": "2021-09-20T00:00:00.000Z"}}}')
#   date_start <- paste0(as.character(date_start), "T00:00:00.000Z")
#   date_end <- paste0(as.character(date_end), "T00:00:00.000Z")
#   
#   twitter_1 <- New$find(paste0('{ "created_at": {"$gte": { "$date":"', date_start,'"}, "$lte": { "$date":"', date_end, '"}}}'))
#   twitter_2<- twitter_1[twitter_1$sub==sub,]
#   if(lang== "Both"){
#     twitter_3<- twitter_2 
#   }else{
#     twitter_3<- twitter_2[twitter_2$lang== lang,]
#   }
#   return(twitter_3)
# }

twitter_sub <- function(twitter_db, sub, date_start, date_end, lang){
  # twitter_1<- twitter_db%>% subset(date> date_start & date< date_end)
  # result <- New$find('{ "created_at": {"$gte": { "$date": "2021-09-15T00:00:00.000Z"}, "$lte": { "$date": "2021-09-20T00:00:00.000Z"}}}')
  date_start <- paste0(as.character(date_start), "T00:00:00.000Z")
  date_end <- paste0(as.character(date_end), "T00:00:00.000Z")
  
  query <- paste0('{ "created_at": {"$lte": { "$date":"', date_start,'"}, "$gte": { "$date":"', date_end, '"}}')
  query <- paste0(query, ' ,', '"sub": ', '"', sub, '"')
  
  if(lang!="Both")
  {
    query <- paste0(query, ' ,', '"lang": ', '"', lang, '"')
  }
  
  query <- paste0(query, ' }')
  print(query)
  
  twitter_3 <- New$find(query)
  print(colnames(twitter_3))
  twitter_3 <- twitter_3[, -1]
  
  
  
  twitter_3$created_at <- as.Date(twitter_3$created_at, format = "yyyy-mm-dd")
  
  return(twitter_3)
}

part<- function(DB){
  test8<- DB
  part2<- count(DB, screen_name)
  colnames(part2)<- c("part2", "count")
  #part2$score<- 0
  for (i in 1:dim(part2)[1]) {
    part2$name[i]<- test8$name[test8$screen_name== part2$part2[i]][1]
    part2$location[i]<- test8$location[test8$screen_name== part2$part2[i]][1]
    part2$description[i]<- test8$description[test8$screen_name== part2$part2[i]][1]
    part2$followers[i]<- test8$followers_count[test8$screen_name== part2$part2[i]][1]
    #part2$score[i]<- sum(test8$final_sc[test8$screen_name== part2$part2[i]])
    part2$retweet[i]<- sum(test8$retweet_count[test8$screen_name== part2$part2[i]])
    part2$profile[i]<- test8$profile_image_url[test8$screen_name== part2$part2[i]][1]
  }
  part2<- part2[order(-part2$retweet),]
  #row.names(part2) <- NULL
  return(part2)
}

Score<- function(Final, para){
  tidy_books <- Final %>%
    unnest_tokens(word, para)
  
  Content_Sentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) 
  
  for (i in 1:dim(Final)[1]) {
    X<- Final$text[i]%>% as.data.frame()
    
    word_set<- X %>% unnest_tokens(word, X) %>%
      count(word, sort = TRUE) %>%
      inner_join(get_sentiments("bing"))
    
    
    Final$positive[i]<- sum(word_set$n[word_set$sentiment=="positive"])
    Final$negative[i]<- sum(word_set$n[word_set$sentiment=="negative"])
    Final$senti_bing[i]<- Final$positive[i]- Final$negative[i]
    
  }
  return(Final) 
}

Hashtg<- function(Final){
  # print(colnames(Final))
  Final<- Final[Final$is_retweet=="FALSE",]
  part2<- as.data.frame(unique(Final$screen_name))
  colnames(part2)<- "part2"
  for (i in 1:dim(part2)[1]) {
    id<- filter(Final, screen_name == part2$part2[i])
    Fi<- id$hashtags[[1]]
    if (length(id$hashtags)>1){
      for (j in 1:(length(id$hashtags)-1)) {
        Fi<- paste(Fi, id$hashtags[[j+1]], sep = ", ")
      }
    }
    part2$hashtag[i]<- Fi
  }
  
  ht_token<- tokens(part2$hashtag, what = "word", 
                    remove_numbers = TRUE, remove_punct = TRUE,
                    remove_symbols = TRUE, split_hyphens = TRUE, remove_url = TRUE, remove_separators = TRUE, padding = FALSE)
  ht_token<- Reduce(c, ht_token)
  ht_unique<- unique(ht_token) %>% as.data.frame()
  
  for (i in 1:dim(ht_unique)[1]) {
    ht_unique$count[i]<- sum(ht_token== ht_unique$.[i])
  }
  ht_unique<- ht_unique[order(-ht_unique$count),]
  colnames(ht_unique)<- c("hashtag", "count") 
  ht_unique<- filter(ht_unique, hashtag != "NA") %>% mutate(perc<- round(100*count/ sum(count),2))
  colnames(ht_unique)<- c("hashtag", "count", "perc")
  return(ht_unique)
}


WC<- function(Final, para){
  # continue cleaning the text
  Final<- Final[Final$is_retweet=="FALSE",]
  
  Final3<- paste(unlist(Final[, colnames(Final)== para]), collapse =" ")
  
  Final3<- gsub('#\\S+', '', Final3)
  Final3<- gsub('@\\S+', '', Final3)
  # Tokenize SMS text messages.
  
  Final3.tokens <- tokens(Final3, what = "word", 
                          remove_numbers = TRUE, remove_punct = TRUE,
                          remove_symbols = TRUE, split_hyphens = TRUE, remove_url = TRUE, remove_separators = TRUE, padding = FALSE)
  
  
  # Lower case the tokens.
  Final3.tokens <- tokens_tolower(Final3.tokens)
  
  # Stopword removal.
  Final3.tokens <- tokens_select(Final3.tokens, stopwords(), 
                                 selection = "remove")
  
  # Stemming.
  Final3.tokens <- tokens_wordstem(Final3.tokens, language = "english")
  
  text <- str_c(Final3.tokens, collapse = " ")
  
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(text)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
  
  # build wordcloud 
  wordcloud <- wordcloud2(data = textCorpus, minSize = 5, minRotation = 0, gridSize = 5, maxRotation = 0, ellipticity = 0.8)
  return(wordcloud)
}


count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    
    count(word1, word2, sort = TRUE)
}

count_bigrams2 <- function(dataset) {
  # print(colnames(dataset))
  # print("printed")
  # print(typeof(dataset))
  # print(dataset[1:5,])
  dataset <- dataset[,-1]
  dataset %>%
    unnest_tokens(bigram, trans, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)

}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}


word_cor<- function(Final, para){  
  
  austen_section_words <- Final %>%
    mutate(section = row_number() %/% 1) %>%
    filter(section > 0) %>%
    unnest_tokens(word, para) %>%
    filter(!word %in% stop_words$word) 
  
  # count words co-occuring within sections
  word_pairs <- austen_section_words %>%
    pairwise_count(word, section, sort = TRUE)
  
  word_cors <- austen_section_words %>%
    group_by(word) %>%
    filter(n() >= 10) %>%
    pairwise_cor(word, section, sort = TRUE)
} 

wd_corr<- function(wd_cor, vector){
  wd_cor %>%
    filter(item1 %in% vector) %>%
    group_by(item1) %>%
    slice_max(correlation, n= 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()
}


wd_corr2<- function(djt2){  
  
  kjv_bigrams <- count_bigrams(djt2)
  
  # filter out rare combinations, as well as digits
  kjv_bigrams %>%
    filter(n > 30,
           !str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    visualize_bigrams()
}

wd_corr22<- function(djt2){  
  
  kjv_bigrams <- count_bigrams2(djt2)
  
  # print("counting")
  # print(kjv_bigrams)
  
  # filter out rare combinations, as well as digits
  kjv_bigrams %>%
    filter(n > 30,
           !str_detect(word1, "\\d"),
           !str_detect(word2, "\\d")) %>%
    visualize_bigrams()
}

trans<- function(djt3){
  ##Translating tweets
  
  uni<- unique(djt3$text[djt3$lang!="en"]) %>% as.data.frame()
  uni$trans<- uni$.
  
  for (i in 1:dim(uni)[1]) {
    uni$trans[i]<- gl_translate(uni$.[i], target = "en")[1]
  }
  unlist.col2 <- unlist(uni$trans)
  unlist.col2<- as.data.frame(unlist.col2)
  uni$trans<- unlist.col2$unlist.col2
  
  
  
  djt3$trans<- djt3$text
  for (i in 1:dim(uni)[1]) {
    djt3$trans[djt3$text==uni$.[i]]<-uni$trans[i]
  }
  return(djt3)
} 

Polarity<- function(djt3, column){
  Final<- unique(djt3[,colnames(djt3)== column]) %>% as.data.frame()
  colnames(Final)<- "para"
  djt3$polarity<- 0
  
  for (i in 1:dim(Final)[1]) {
    djt3$polarity[djt3$text== Final$para[i]]<- sentiment(Final$para[i])[1,4]
  }
  
  unlist.col2 <- unlist(djt3$polarity)
  unlist.col2<- as.data.frame(unlist.col2)
  djt3$polarity<- unlist.col2$unlist.col2
  return(djt3)
}  

Polarity_plot<- function(djt3, column){
  
  X<- djt3[,which(colnames(djt3)== column)] %>% as.data.frame()
  djt3<- mutate(djt3, score= log(djt3$followers_count)*X[,1])
  djt3$hour<- round(difftime(djt3$created_at, Sys.time(), units = "hours"),0)%>% as.data.frame()
  
  
  hour<- unique(djt3$hour)%>% as.data.frame() 
  Target<- c("NaN", Inf, -Inf)
  
  for (i in 1:dim(hour)[1]) {
    X<- djt3$score[djt3$hour==as.numeric(hour$.[i])]
    hour$score[i]<- sum(X[!X %in% Target], na.rm = TRUE)
  }
  
  hour$time <- Sys.time()+hour$.
  plot<- ggplot(hour, aes(x=time, y=score))+geom_line() + ggtitle("Plot of Sentiment Score") +
    xlab("Time Period") + ylab("Sentiment Score")+ theme(legend.position="bottom",
                                                         plot.title = element_text(size = 15, face = "bold"))
  
  return(plot)
}


wd_freq<- function(Final){
  vaccine_words <- Final %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word, 
           !word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark", "measles", "thackrey")) %>%
    count(date, word, sort = TRUE)
  
  
  plot_physics <- vaccine_words %>%
    bind_tf_idf(word, date, n) %>%
    mutate(word = str_remove_all(word, "_")) %>%
    group_by(date) %>% 
    slice_max(tf_idf, n = 20) %>%
    ungroup() %>%
    mutate(word = reorder_within(word, tf_idf, date)) %>%
    mutate(author = factor(date))
  
  ggplot(plot_physics, aes(word, tf_idf, fill = date)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~date, ncol = 2, scales = "free") +
    coord_flip() +
    scale_x_reordered()+ ggtitle("Important Words Day Wise") +
    xlab("Words") + ylab("Day")
}


wd_promi<- function(djt2, vector){
  
  djt2<- djt2[djt2$is_retweet== "FALSE",]
  austen_section_words <- djt2 %>%
    mutate(section = row_number() %/% 1) %>%
    filter(section > 0) %>%
    unnest_tokens(word, text) %>%
    filter(!word %in% stop_words$word)
  
  ps_dtm <- VectorSource(djt2$text) %>%
    VCorpus() %>%
    DocumentTermMatrix(control = list(removePunctuation = TRUE,
                                      removeNumbers = TRUE,
                                      stopwords = TRUE))
  
  inaug_td <- tidy(ps_dtm)
  
  
  year_term_counts <- inaug_td %>%
    left_join(austen_section_words, na_matches = "never", by = c("term" = "word")) %>%
    group_by(week2) %>%
    mutate(year_total = sum(count)) %>%
    filter(!is.na(week2))
  
  
  year_term_counts %>%
    filter(word %in% c("uttar", "Police", "Mafia")) %>%
    ggplot(aes(week2, count / year_total)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ word, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "% frequency of word in articles") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}


### Media

myGrid2 <- grid_template(
  default = list(
    # Here we define the data.frame describing our layout
    # The easiest way is to use rbind so that the layout can be 'visualized' in code
    areas = rbind(
      c("header1", "input11", "input12", "input12", "input13","submit1"),
      c("header1", "input14", "input14", "input15", "input15", "submit1"),
      c("info11", "info11", "info11", "info12", "info12", "info12"),
      c("src_tbl1",   "src_tbl1",   "src_tbl1", "senti_src1", "senti_src1", "senti_src1"),
      c("senti_hd1",   "senti_hd1",   "senti_hd1",   "senti_bd1", "senti_bd1", "senti_bd1"),
      c("cor1",   "cor1",   "cor1", "wd_fq1", "wd_fq1", "wd_fq1"),
      c("wd_freq12",   "wd_freq12",   "wd_freq12", "correlation", "correlation", "correlation"),
      c("article1",   "article1",   "article1", "wcloud1", "wcloud1", "wcloud1"),
      c("note1", "note1", "note1", "note1", "note1", "note1")
    ),
    # Then we define the dimensions of the different elements of the layout
    # We can use any valid css units to make the layout behave exactly as desired
    rows_height = c("100px", "100px", "150px", "2fr", "2fr", "2fr", "2fr", "2fr", "100px"),
    cols_width = c("30%", "10%", "20%", "10%", "20%", "10%")
  ),
  # This is optional, but we can define a specific layout for mobile (screen width below 768px)
  mobile = list(
    areas = rbind(
      c("header1", "input11", "input12", "input12", "input13","submit1"),
      c("header1", "input14", "input14", "input15", "input15", "submit1"),
      c("info11", "info11", "info11", "info12", "info12", "info12"),
      c("src_tbl1",   "src_tbl1",   "src_tbl1", "senti_hd1", "senti_hd1", "senti_hd1"),
      c("senti_ct1",   "senti_ct1",   "senti_ct1",   "ht1", "ht1", "ht1"),
      c("cor1",   "cor1",   "cor1", "wd_fq1", "wd_fq1", "wd_fq1"),
      c("wd_freq12",   "wd_freq12",   "wd_freq12", "correlation", "correlation", "correlation"),
      c("article1",   "article1",   "article1", "wcloud1", "wcloud1", "wcloud1"),
      c("note1", "note1", "note1", "note1", "note1", "note1")
    ),
    rows_height = c("100px", "100px", "150px", "2fr", "2fr", "2fr", "2fr", "2fr", "100px"),
    cols_width = c("30%", "10%", "20%", "10%", "20%", "10%")
  )
)

# src<- Final_DB%>% count(Source)
src<-Final_DB$aggregate('[{
   "$group": {
       "_id": "$Source",
       "count": {
           "$sum": 1
       }
   }

                        }]')

colnames(src) <- c('Source', 'n')
src<- src[order(-src$n),]

# news_sub <- function(Final_DB, sub, week= "All", lang, src= "All"){
#   if(week== "All"){
#     # news_1<- Final_DB
#     news_1<-Final_DB$find()
#   }else{
#     news_1<- Final_DB[Final_DB$week2== week]
#     # news1 <- Final_DB$find(paste('{"$week2": ', week, '}', sep=''))
#   }
#   
#   news_2<- news_1[news_1$sub==sub,]
#   if(lang== "Both"){
#     news_3<- news_2 
#   }else{
#     news_3<- news_2[news_2$lang== lang,]
#   }
#   
#   if(src== "All"){
#     news_4<- news_3
#   }else{
#     news_4<- news_3[news_3$Source== src,]
#   }
#   
#   return(news_4)
# }


news_sub <- function(Final_DB, sub, week= "All", lang, src= "All"){
  
  query <- paste0('{"sub": ', '"', sub, '"')
  
  if(week!="All")
  {
    query <- paste0(query, ', ', '"week2": ', '"', week, '"')
  }
  if(lang!="Both")
  {
    query <- paste0(query, ', ', '"lang": ', '"', lang, '"') 
  }
  if(src!="All")
  {
    query <- paste0(query, ', ', '"src": ', '"', src, '"') 
  }
  
  
  query <- paste0(query, ' }')
  
  news4 <- Final_DB$find(query)
  news4 <- news4[, -1]
  
  return(news4)
}


senti<- function(Final_DB){
  tidy_books <- Final_DB %>%
    unnest_tokens(word, trans)
  
  nrc_joy <- get_sentiments("afinn") %>% 
    filter(value == 2 |value == 3 | value == 4 | value == 5)
  
  tidy_books %>%
    inner_join(nrc_joy) %>%
    count(word, sort = TRUE)
  
  Content_Sentiment <- tidy_books %>%
    inner_join(get_sentiments("bing")) 
  
  for (i in 1:dim(Final_DB)[1]) {
    X<- Final_DB$trans[i]
    X<- as.data.frame(X)
    Y<- paste(Final_DB$Header[i], Final_DB$Detail[i], sep = ". ")
    Y<- as.data.frame(Y)
    word_set<- X %>% unnest_tokens(word, X) %>%
      count(word, sort = TRUE) %>%
      inner_join(get_sentiments("bing"))
    
    word_set_head<- Y %>% unnest_tokens(word, Y) %>%
      count(word, sort = TRUE) %>%
      inner_join(get_sentiments("bing"))
    
    Final_DB$positive[i]<- sum(word_set$n[word_set$sentiment=="positive"])
    Final_DB$negative[i]<- sum(word_set$n[word_set$sentiment=="negative"])
    Final_DB$senti_bing[i]<- Final_DB$positive[i]- Final_DB$negative[i]
    
    Final_DB$posi_Head[i]<- sum(word_set_head$n[word_set_head$sentiment=="positive"])
    Final_DB$neg_Head[i]<- sum(word_set_head$n[word_set_head$sentiment=="negative"])
    Final_DB$senti_Head[i]<- Final_DB$posi_Head[i]- Final_DB$neg_Head[i]
    
    return(Final_DB)
  }
}

wd_promi2<- function(djt2, vector){
  
  austen_section_words <- djt2 %>%
    mutate(section = row_number() %/% 1) %>%
    filter(section > 0) %>%
    unnest_tokens(word, trans) %>%
    filter(!word %in% stop_words$word)
  
  ps_dtm <- VectorSource(djt2$trans) %>%
    VCorpus() %>%
    DocumentTermMatrix(control = list(removePunctuation = TRUE,
                                      removeNumbers = TRUE,
                                      stopwords = TRUE))
  
  inaug_td <- tidy(ps_dtm)
  
  
  year_term_counts <- inaug_td %>%
    left_join(austen_section_words, na_matches = "never") %>%
    group_by(week2) %>%
    mutate(year_total = sum(count)) %>%
    filter(!is.na(week2))
  
  
  year_term_counts %>%
    filter(word %in% vector) %>%
    ggplot(aes(week2, count / year_total)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ word, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "% frequency of word in articles") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
  
}


wd_freq2<- function(Final){
  print(colnames(Final))
  vaccine_words <- Final %>%
    unnest_tokens(word, trans) %>%
    filter(!word %in% stop_words$word, 
           !word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark", "measles", "thackrey")) %>%
    count(week2, word, sort = TRUE)
  
  
  plot_physics <- vaccine_words %>%
    bind_tf_idf(word, week2, n) %>%
    mutate(word = str_remove_all(word, "_")) %>%
    group_by(week2) %>% 
    slice_max(tf_idf, n = 20) %>%
    ungroup() %>%
    mutate(word = reorder_within(word, tf_idf, week2)) %>%
    mutate(author = factor(week2))
  
  ggplot(plot_physics, aes(word, tf_idf, fill = week2)) +
    geom_col(show.legend = FALSE) +
    labs(x = NULL, y = "tf-idf") +
    facet_wrap(~week2, ncol = 2, scales = "free") +
    coord_flip() +
    scale_x_reordered()+ ggtitle("Important Words Week Wise") +
    xlab("Words") + ylab("Week")
}

austen_words <- function(Final){
  Final %>%
    mutate(section = row_number() %/% 1) %>%
    filter(section > 0) %>%
    unnest_tokens(word, para) %>%
    filter(!word %in% stop_words$word) %>%
    filter(!word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark"))
}

correlation<- function(Final, vector){
  austen_section_words<- austen_words(Final)
  
  word_cors <- austen_section_words %>%
    group_by(word) %>%
    filter(n() >= 15) %>%
    pairwise_cor(word, section, sort = TRUE)
  
  word_cors %>%
    filter(item1 %in% vector) %>%
    group_by(item1) %>%
    slice_max(correlation, n= 6) %>%
    ungroup() %>%
    mutate(item2 = reorder(item2, correlation)) %>%
    ggplot(aes(item2, correlation)) +
    geom_bar(stat = "identity") +
    facet_wrap(~ item1, scales = "free") +
    coord_flip()
}


wd_freq22<- function(Final, vector){
  austen_section_words<- austen_words(Final)
  
  ps_dtm <- VectorSource(Final$trans) %>%
    VCorpus() %>%
    DocumentTermMatrix(control = list(removePunctuation = TRUE,
                                      removeNumbers = TRUE,
                                      stopwords = TRUE))
  
  inaug_td <- tidy(ps_dtm)
  
  inaug_tf_idf <- inaug_td %>%
    bind_tf_idf(term, document, count) %>%
    arrange(desc(tf_idf))
  
  colnames(inaug_td)<- c("document", "word", "count" )
  
  
  
  year_term_counts <- inaug_td %>%
    left_join(austen_section_words, na_matches = "never") %>%
    group_by(week2) %>%
    mutate(year_total = sum(count)) %>%
    filter(!is.na(week2))
  
  
  year_term_counts %>%
    filter(word %in% vector) %>%
    ggplot(aes(week2, count / year_total)) +
    geom_point() +
    geom_smooth() +
    facet_wrap(~ word, scales = "free_y") +
    scale_y_continuous(labels = scales::percent_format()) +
    labs(y = "% frequency of word in articles") +theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
}


Wcloud<- function(Final){
  Final3<- Final
  # continue cleaning the text
  Final3$para<- gsub('#\\S+', '', Final3$para)
  Final3$para<- gsub('@\\S+', '', Final3$para)
  
  
  # Tokenize SMS text messages.
  
  Final.tokens <- tokens(Final3$trans, what = "word", 
                         remove_numbers = TRUE, remove_punct = TRUE,
                         remove_symbols = TRUE, split_hyphens = TRUE, remove_url = TRUE, remove_separators = TRUE, padding = FALSE)
  
  
  # Take a look at a specific SMS message and see how it transforms.
  Final.tokens[[57]]
  
  # Lower case the tokens.
  Final.tokens <- tokens_tolower(Final.tokens)
  
  # Stopword removal.
  Final.tokens <- tokens_select(Final.tokens, stopwords(), 
                                selection = "remove")
  
  # Stemming.
  Final.tokens <- tokens_wordstem(Final.tokens, language = "english")
  
  text <- str_c(Final.tokens, collapse = " ")
  
  # Convert the data into a summary table
  textCorpus <- 
    Corpus(VectorSource(text)) %>%
    TermDocumentMatrix() %>%
    as.matrix()
  
  textCorpus <- sort(rowSums(textCorpus), decreasing=TRUE)
  textCorpus <- data.frame(word = names(textCorpus), freq=textCorpus, row.names = NULL)
  
  # build wordcloud 
  wordcloud <- wordcloud2(data = textCorpus, minSize = 5, minRotation = 0, gridSize = 5, maxRotation = 0, ellipticity = 0.8)
  wordcloud
}

