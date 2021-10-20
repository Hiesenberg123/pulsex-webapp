server <- function(input, output) {
  ##Twitter Part
  
  part<- reactive({
    test8<- subset(test2, date> input$date[1] & date < input$date[2])
    part2<- unique(test8$screen_name) %>% as.data.frame() 
    colnames(part2)<- c("part2")
    part2$score<- 0
    for (i in 1:dim(part2)[1]) {
      part2$name[i]<- test8$name[test8$screen_name== part2$part2[i]][1]
      part2$location[i]<- test8$location[test8$screen_name== part2$part2[i]][1]
      part2$description[i]<- test8$description[test8$screen_name== part2$part2[i]][1]
      part2$followers[i]<- test8$followers_count[test8$screen_name== part2$part2[i]][1]
      part2$score[i]<- sum(test8$final_sc[test8$screen_name== part2$part2[i]])
      part2$retweet[i]<- sum(test8$retweet_count[test8$screen_name== part2$part2[i]])
      part2$profile[i]<- test8$profile_image_url[test8$screen_name== part2$part2[i]][1]
    }
    part2<- part2[order(part2$score),]
    #row.names(part2) <- NULL
    return(part2[,c(1,2,3,4)])
  })
  
  # output$progressBox2 <- renderInfoBox({
  #     infoBox(
  #         "Progress", as.data.frame(part()[1,1]), icon = icon("list"),
  #         color = "purple", fill = TRUE
  #     )
  # })
  # output$approvalBox2 <- renderInfoBox({
  #     infoBox(
  #         "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
  #         color = "yellow", fill = TRUE
  #     )
  # })
  # output$approvalBox3 <- renderInfoBox({
  #         infoBox(
  #             "Approval", "80%", icon = icon("thumbs-up", lib = "glyphicon"),
  #             color = "yellow", fill = TRUE
  #         )
  # })
  
  
  Score<- reactive({
    
    test8<- filter(test2, between(created_at ,input$date[2], input$date[1]))
    date1<- unique(test8$created_at)
    #Score<- matrix(0, nrow = 5, ncol = 2)
    Score<- matrix(NA, nrow = length(date1), ncol = 4)
    Score<- as.data.frame(Score)
    
    colnames(Score)<- c("Date", "Overall Sentiment Score", "Tweet_count", "Ave Sentiment Score")
    Score$Date<- date1
    
    for (i in 1:length(date1)) {
      
      Date_test<- test2[test2$created_at==date1[i],]
      Score$`Overall Sentiment Score`[i]<- sum(Date_test$final_sc)
      Score$Tweet_count[i]<- dim(Date_test)[1]
      Score$`Ave Sentiment Score`[i]<- Score$`Overall Sentiment Score`[i]/Score$Tweet_count[i]
    }
    return(Score)
  })
  
  
  output$hist<- renderPlot({
    ggplot(data=Score(), aes(x=Date, y=`Ave Sentiment Score`, group=1)) + geom_line()+  geom_point()+ labs(title="Sentiment around #farmlaws",x="Date", y = "Sentiment_Score")+    theme_bw()
  })
  
  
  output$hist2<- renderPlot({
    ggplot(data=Score(), aes(x=Date, y= Tweet_count, group=1)) + geom_line()+  geom_point()+ labs(title="Count of Tweets around #farmlaws",x="Date", y = "Tweet_Count")+    theme_bw()
  })
  
  wordcloud<- reactive({
    test8<- filter(test2, between(created_at ,input$date[2], input$date[1]))
    
    
    # continue cleaning the text
    test8$text<- gsub('#\\S+', '', test8$text)
    test8$text<- gsub('@\\S+', '', test8$text)
    
    
    # Tokenize SMS text messages.
    
    test8.tokens <- tokens(test8$text, what = "word", 
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, split_hyphens = TRUE, remove_url = TRUE, remove_separators = TRUE, padding = FALSE)
    
    
    # Take a look at a specific SMS message and see how it transforms.
    test8.tokens[[57]]
    
    # Lower case the tokens.
    test8.tokens <- tokens_tolower(test8.tokens)
    
    # Stopword removal.
    test8.tokens <- tokens_select(test8.tokens, stopwords(), 
                                  selection = "remove")
    
    # Stemming.
    test8.tokens <- tokens_wordstem(test8.tokens, language = "english")
    
    text <- str_c(test8.tokens, collapse = " ")
    
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
  })
  
  output$cloud <- renderWordcloud2({
    # When input$n is 1, filename is ./images/image1.jpeg
    wordcloud() 
    
  })
  
  output$table1 <- DT::renderDataTable({
    part()
  }, rownames=FALSE,
  options = list(pageLength = 5, autoWidth = TRUE,
                 columnDefs = list(list( targets = c(0,1), width = '400px')),
                 scrollX = TRUE))
  
  
  ## Media Part
  Final11<- reactive({
    Final2<- subset(Final, Duration> input$date15[2] & Duration < input$date15[1])
    if(input$smoothMethod14 !="All"){
      Final2<- Final2[Final2$Source==input$smoothMethod14,]
    }
    
    return(Final2)
    #Final2<- Final
  })
  
  plot_vaccine<- reactive({    
    words_senti <- Final11() %>%
      unnest_tokens(word, para) %>%
      filter(!word %in% stop_words$word, 
             !word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark")) %>%
      count(Duration, word, sort = TRUE)
    
    total_words <- words_senti %>% 
      group_by(Duration) %>% 
      summarize(total = sum(n))
    
    words_senti <- left_join(words_senti, total_words)
    
    book_tf_idf <- words_senti %>%
      bind_tf_idf(word, Duration, n)
    
    book_tf_idf %>%
      group_by(Duration) %>%
      slice_max(tf_idf, n = 15) %>%
      ungroup() %>%
      ggplot(aes(tf_idf, fct_reorder(word, tf_idf), fill = Duration)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~Duration, ncol = 2, scales = "free") +
      labs(x = "tf-idf", y = NULL)
    
    vaccine_words <- Final11() %>%
      unnest_tokens(word, para) %>%
      filter(!word %in% stop_words$word, 
             !word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark", "measles", "thackrey")) %>%
      count(Duration, word, sort = TRUE)
    
    plot_vaccine <- vaccine_words %>%
      bind_tf_idf(word, Duration, n) %>%
      mutate(Duration = factor(Duration))
    
    return(plot_vaccine)
    
  }) 
  
  word_cors<- reactive({
    austen_section_words <- Final11() %>%
      mutate(section = row_number() %/% 1) %>%
      filter(section > 0) %>%
      unnest_tokens(word, para) %>%
      filter(!word %in% stop_words$word) %>%
      filter(!word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark"))
    
    #austen_section_words <- Austen %>%
    #  unnest_tokens(word, para) %>%
    #  filter(!word %in% stop_words$word)
    
    # count words co-occuring within sections
    word_pairs <- austen_section_words %>%
      pairwise_count(word, section, sort = TRUE)
    
    word_pairs %>%
      filter(item1 == "government")
    
    word_cors <- austen_section_words %>%
      group_by(word) %>%
      filter(n() >= 20) %>%
      pairwise_cor(word, section, sort = TRUE)
    
    return(word_cors)
  })
  
  count_bigrams <- function(dataset) {
    dataset %>%
      unnest_tokens(bigram, para, token = "ngrams", n = 2) %>%
      separate(bigram, c("word1", "word2"), sep = " ") %>%
      filter(!word1 %in% stop_words$word,
             !word2 %in% stop_words$word,
             !word2 %in% c("weight", "happy", "doggo's", "newsletter", "alerts",  "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark", "express")) %>%
      
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
  
  
  kjv_bigrams <- reactive({count_bigrams(Final11())}) 
  
  # filter out rare combinations, as well as digits
  
  austen_section_words<- reactive({
    austen_section_words <- Final11() %>%
      mutate(section = row_number() %/% 1) %>%
      filter(section > 0) %>%
      unnest_tokens(word, para) %>%
      filter(!word %in% stop_words$word) %>%
      filter(!word %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark"))
    
    return(austen_section_words)
  })
  
  inaug_td<- reactive({
    inaug_td<- VectorSource(Final11()$para) %>%
      VCorpus() %>%
      DocumentTermMatrix(control = list(removePunctuation = TRUE,
                                        removeNumbers = TRUE,
                                        stopwords = TRUE)) %>%
      tidy()
    colnames(inaug_td) <- c("document", "word", "count" )
    return(inaug_td)
  }) 
  
  wordcloud4<- reactive({
    Final3<- Final11()
    # continue cleaning the text
    Final3$para<- gsub('#\\S+', '', Final3$para)
    Final3$para<- gsub('@\\S+', '', Final3$para)
    
    
    # Tokenize SMS text messages.
    
    Final.tokens <- tokens(Final3$para, what = "word", 
                           remove_numbers = TRUE, remove_punct = TRUE,
                           remove_symbols = TRUE, split_hyphens = TRUE, remove_url = TRUE, remove_separators = TRUE, padding = FALSE)
    
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
    
    return(wordcloud)
  })
  
  output$hist11<- renderPlot({
    ggplot(Final11(), aes(Duration, senti_Head, fill = Source)) +
      geom_col(show.legend = TRUE)
  })
  
  output$hist12<- renderPlot({
    ggplot(Final11(), aes(Duration, senti_bing, fill = Source)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~Source, ncol = 2, scales = "free_x")
  })
  
  output$hist13<- renderPlot({
    plot_vaccine() %>% 
      group_by(Duration) %>% 
      slice_max(tf_idf, n = 15) %>% 
      ungroup() %>%
      mutate(word = reorder(word, tf_idf)) %>%
      ggplot(aes(tf_idf, word, fill = Duration)) +
      geom_col(show.legend = FALSE) +
      labs(x = "tf-idf", y = NULL) +
      facet_wrap(~Duration, ncol = 2, scales = "free")
  })
  
  output$hist14<- renderPlot({
    kjv_bigrams() %>%
      filter(n > 10,
             !str_detect(word1, "\\d"),
             !str_detect(word2, "\\d")) %>%
      visualize_bigrams()
  })
  
  output$hist15<- renderPlot({
    word_cors() %>%
      filter(item1 %in% c("stock", "inflation", "gdp", "nirmala", "modi")) %>%
      group_by(item1) %>%
      slice_max(correlation, n= 6) %>%
      ungroup() %>%
      mutate(item2 = reorder(item2, correlation)) %>%
      ggplot(aes(item2, correlation)) +
      geom_bar(stat = "identity") +
      facet_wrap(~ item1, scales = "free") +
      coord_flip()
  })
  
  output$hist16<- renderPlot({
    VectorSource(Final11()$para) %>%
      VCorpus() %>%
      DocumentTermMatrix(control = list(removePunctuation = TRUE,
                                        removeNumbers = TRUE,
                                        stopwords = TRUE)) %>% 
      tidy() %>% 
      filter(!term %in% c("weight", "fat", "workout", "kilos", "wellbeing", "wishes", "happy", "tips", "quotes", "messages", "father's", "app", "bookmark", "lose")) %>%
      inner_join(get_sentiments("bing"), by = c(term = "word")) %>%
      count(sentiment, term, wt = count) %>%
      ungroup() %>%
      filter(n >= 20) %>%
      mutate(n = ifelse(sentiment == "negative", -n, n)) %>%
      mutate(term = reorder(term, n)) %>%
      ggplot(aes(n, term, fill = sentiment)) +
      geom_col() +
      labs(x = "Contribution to sentiment", y = NULL)
    
  })
  
  output$hist17<- renderPlot({
    inaug_td() %>%
      left_join(austen_section_words(), na_matches = "never") %>%
      group_by(Duration) %>%
      mutate(year_total = sum(count)) %>%
      filter(!is.na(Duration)) %>%
      filter(word %in% c("stock", "inflation", "gdp", "nirmala", "modi")) %>%
      ggplot(aes(Duration, count / year_total)) +
      geom_point() +
      geom_smooth() +
      facet_wrap(~ word, scales = "free_y") +
      scale_y_continuous(labels = scales::percent_format()) +
      labs(y = "% frequency of word in articles")
  })
  
  output$cloud11 <- renderWordcloud2({
    # When input$n is 1, filename is ./images/image1.jpeg
    wordcloud4() 
    
  })
  
  Art_list<- reactive({
    Table<- Final11()
    Table<- Table[,c(4,5,6,3)]
    colnames(Table)<- c("Headline", "Source", "Date", "Link")
    return(Table)
  })
  
  output$table11 <- DT::renderDataTable({
    Art_list()
  }, rownames=FALSE,
  options = list(pageLength = 5, autoWidth = TRUE,
                 columnDefs = list(list( targets = c(0,3), width = '400px')),
                 scrollX = TRUE)
  ) 
  
}