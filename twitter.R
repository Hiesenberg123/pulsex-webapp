source("utils.R")
# source("utilsMongo.R")

uitwitter <- function(id, label = "Counter") {
  ns <- NS(id)
  semanticPage(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
    ),
    
    
    title = h2(class = "ui header", icon("twitter square"), div(class = "content", "Twitter Analysis")), 
    includeCSS("main.css"),
    shiny.semantic::grid(myGrid,
                         # We can define the css style of the grid using container_style
                         container_style = "",
                         # We can define the css style of each of the grid elements using area_styles
                         area_styles = list(header = "",
                                            input1 ="color: black",
                                            input2 ="color: black",
                                            input3 ="color: black",
                                            input4 ="color: black",
                                            submit ="margin-top: 40px; margin-left: 40px;",
                                            
                                            info1 = "margin-top: 10px",
                                            info2 = "margin-top: 10px",
                                            info3 = "margin-top: 10px",
                                          
                                            freq = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            senti = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            
                                            active = "margin-bottom: 5px; border: 1px solid #0099f9; background-color: white",
                                            ht = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            
                                            cor = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            wd_fq = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            
                                            tweet_table = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            wcloud = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            
                                            note = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white"
                                            ),
                                            
                         # Finally, we define the ui content we would like to have inside each element
                         header = h1(class="ui header", icon("twitter square"), theme = "cosmo", div(style = "margin-left: 50px", class="content", "Tweet Analysis")),
                         input1 = selectInput(
                           inputId = ns("Topic"),
                           label = div(shiny::tags$h3("Select Head" , style="color:black")),
                           choices = unique(choice2$topic),
                           selected = unique(choice2$topic)[1],
                           width = 200
                         ),
                         
                         input2 = uiOutput(ns("choice")),
                         
                         input3= selectInput(
                           inputId = ns("lang"),
                           label = div(shiny::tags$h3("Language" , style="color:black")),
                           choices = c("Both", "hi", "en"),
                           selected = "en",
                           width = 200
                         ),
                         
                         input4= dateRangeInput(ns('dateRange'), label = div(shiny::tags$h3("Date" , style="color:black")), start = Sys.Date() - 2, end = Sys.Date() + 2, min = NULL,
                                                               max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                                               language = "en", separator = " to ", width = 300),
                         
                         submit= actionButton(ns("button"), "Submit"),
                         
                         info1 = card(class = "red",
                                      div(class = "content",
                                          div(class = "description", verbatimTextOutput(ns("tweetcount")))
                                      )
                         ),
                         
                         info2 = card(class = "red",
                                      div(class = "content",
                                          div(class = "description", verbatimTextOutput(ns("uniquetweets")))
                                      )
                         ),
                         
                         info3 = card(class = "red",
                                      div(class = "content",
                                          div(class = "description", verbatimTextOutput(ns("uniqueusers")))
                                      )
                         ),
                         
                         freq= plotOutput(ns("freq")),
                         senti= plotOutput(ns("senti")),

                         active= tabset(
                           tabs = list(
                             list(menu = "active", class="ui blue ribbon label", content = plotOutput(ns("active")), id = "first_tab"),
                             list(menu = "table", class = "basic", content = semantic_DTOutput(ns("table")), id = "second_tab")
                           ),
                          #active = "first_tab",
                           #id = "exampletabset"
                         ),
                        # active= plotOutput(ns("active")),
                         ht= tabset(
                           tabs = list(
                             list(menu = "Hashtag", class="ui blue ribbon label", content = plotOutput(ns("ht")), id = "first_httab"),
                             list(menu = "HT_table", class = "basic", content = semantic_DTOutput(ns("ht_table")), id = "second_httab")
                           ),
                           #active = "first_tab",
                           #id = "exampletabset"
                         ),

                         cor= plotOutput(ns("cor")),
                         wd_fq= plotOutput(ns("wd_fq")),

                         tweet_table= semantic_DTOutput(ns("tweet_table")),
                         wcloud= wordcloud2Output(ns("wcloud")),

                         note = textOutput(ns("text"))
    )
  )
}

twitterServer <- function(id) {
  moduleServer(id, function(input, output, session) {
  get_choice<- reactive(choice2)
  data<- eventReactive(input$button,{twitter_sub(djt, input$choice, input$dateRange[2], input$dateRange[1], input$lang)})
  print(">>>>>>>>>>..")
  # print(data())
  part2<- eventReactive(input$button,{
    part(data())})
  hashtag<- eventReactive(input$button,{
    Hashtg(data())})
  wdc<- eventReactive(input$button,{
    word_cor(data(), "text")})

  output$choice<- renderUI({
    ns <- session$ns
    selectInput(
      inputId = ns("choice"),
      label = div(shiny::tags$h3("Select Input" , style="color:black")),
      choices = get_choice()$sub[get_choice()$topic== input$Topic],
      selected = get_choice()$sub[get_choice()$topic== input$Topic][1],
      width = 200
    )
  })

  output$tweetcount <- renderText({
    str1<- paste("Total tweets: ", dim(data())[1], " ")
    str2<- paste("Hindi tweets: ", dim(data()[data()$lang== "hi",])[1], " ")
    str3<- paste("English tweets: ", dim(data()[data()$lang== "en",])[1], " ")

    paste(str1, str2, str3, sep = "\n")
  })
  output$uniquetweets <- renderText({
    str4<- paste("Original tweets: ", dim(data()[data()$is_retweet=="FALSE",])[1])
    str5<- paste("Retweets: ", dim(data()[data()$is_retweet=="TRUE",])[1])

    paste(str4, str5, sep = "\n")

  })
  output$uniqueusers<- renderText({
    str6<- paste("Total unique handles:", length(unique(data()$screen_name)))
    str7<- paste("Original content handles:", dim(unique(data()[data()$is_retweet=="FALSE", colnames(data())== "screen_name"]))[1])
    str8<- paste("Active handles:", dim(part2()[part2()$count>10,])[1])

    paste(str6, str7, str8, sep = "\n")
  })

  output$freq<- renderPlot({
     ts_plot(data(), by= "hours")
   })

  output$senti<- renderPlot({
    Polarity_plot(data(), "polarity")
  })

  output$active<- renderPlot({
    Grouped<- matrix("NA", nrow = 20, ncol = 3)%>% as.data.frame()
    colnames(Grouped)<- c("Participant", "Count", "Type")
    Grouped$Participant<- c(part2()$name[1:10], part2()$name[1:10])
    Grouped$Count<- c(60*part2()$count[1:10], part2()$retweet[1:10])
    Grouped$Type<- c(rep("count" , 10), rep("retweet" , 10))

    ggplot(Grouped,
           aes(fill= Type, x= Count, y = Participant))  +
      geom_bar(position = "dodge", stat = "identity") +
      theme(legend.position="bottom",
                              plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Active and Popular Handles") + labs(fill = "Region") +
      scale_x_continuous(name = "Retweets",
                         sec.axis = sec_axis(~./60, name = "Count of Tweets Posted",
                                             ))
  })

  output$table<- DT::renderDataTable(
    semantic_DT(part2()[,c(3,4,5,6,2,7)],
    options = list(
      pageLength = 2))

  )

  output$ht<- renderPlot({

    ggplot(hashtag()[1:10,],
           aes(x= perc, y = hashtag))  +
      geom_bar(position = "dodge", stat = "identity") + ylab("Hashtags") +
      xlab("Product") + theme(legend.position="bottom",
                              plot.title = element_text(size = 15, face = "bold")) +
      ggtitle("Important Hashtags") + labs(fill = "Region")

  })

  output$ht_table<- DT::renderDataTable(
    semantic_DT(hashtag(),
                options = list(
                  pageLength = 5))

  )

  output$cor<- renderPlot({
    wd_corr2(data())
  })

  output$wd_fq<- renderPlot({
    wd_freq(data())
  })

  output$tweet_table<- DT::renderDataTable({
    twt_tbl<- data()[data()$is_retweet=="FALSE",]
    twt_tbl<- twt_tbl[order(-twt_tbl$retweet_count),]

    semantic_DT(twt_tbl[c(1:100),c(5,3,4,13,14)],
                options = list(
                  pageLength = 2))
  })

  output$wcloud<- renderWordcloud2({
    WC(data(), "text")
  })
    
    output$text <- renderText({
      str9<- paste("Above analysis was conducted on tweets scraped on search topic-  ", input$choice, ".")
      str10<- paste("Total ", dim(data())[1], " tweets analysed in the exercise. ", part2()$name[1], " came out as most popular influencer regarding topic.")

      paste(str9, str10, sep = "\n")
    })
  
  })
}

  

