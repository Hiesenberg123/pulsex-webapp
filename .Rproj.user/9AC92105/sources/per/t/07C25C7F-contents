# source("utils.R")
source("utilsMongo.R")

uimedia <- function(id, label = "Counter") {
  ns <- NS(id)
  semanticPage(
    useShinyjs(),
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "main.css")
    ),


    title = h2(class = "ui header", icon("newspaper outline"), div(class = "content", "Media Insight")),
    includeCSS("main.css"),
    shiny.semantic::grid(myGrid2,
                         # We can define the css style of the grid using container_style
                         container_style = "",
                         # We can define the css style of each of the grid elements using area_styles
                         area_styles = list(header1 = "",
                                            input11 ="color: black",
                                            input12 ="color: black; margin-left: 20px",
                                            input13 ="color: black",
                                            input14 ="color: black",
                                            input15 ="color: black",
                                            submit1 ="margin-top: 40px; margin-left: 40px;",

                                            info11= "margin-top: 10px",
                                            info12= "margin-top: 10px",

                                            src_tbl1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            senti_src1= "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",

                                            senti_hd1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            senti_bd1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",

                                            cor1 = "margin-bottom: 5px; border: 1px solid #0099f9; background-color: white",
                                            wd_fq1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",

                                            cor1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            wd_fq1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",

                                            wd_freq12 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            correlation = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",

                                            article1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",
                                            wcloud1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white",

                                            note1 = "margin-bottom: 5px; margin-left: 5px; border: 1px solid #0099f9; background-color: white"
                          ),

                         ## Finally, we define the ui content we would like to have inside each element
                         header1 = h1(class="ui header", icon("newspaper"), theme = "cosmo", div(style = "margin-left: 50px", class="content", "Media Insight")),
                         input11 = selectInput(
                           inputId = ns("Topic1"),
                           label = div(shiny::tags$h3("Select Head" , style="color:black")),
                           choices = unique(choice2$topic),
                           selected = unique(choice2$topic)[1],
                           width = 200
                         ),

                         input12 = uiOutput(ns("choice1")),

                         input13= selectInput(
                           inputId = ns("lang1"),
                           label = div(shiny::tags$h3("Language" , style="color:black")),
                           choices = c("Both", "hi", "en"),
                           selected = "en",
                           width = 200
                         ),

                         input14= selectInput(
                           inputId = ns("week1"),
                           label = div(shiny::tags$h3("Select Week" , style="color:black")),
                           # choices = c("All", unique(Final_DB$week2)),
                           choices = c("All", Final_DB$distinct("week2")),
                           selected = "All",
                           width = 200
                         ),



                         input15= selectInput(
                           inputId = ns("source1"),
                           label = div(shiny::tags$h3("Select Source" , style="color:black")),
                           choices = c("All", src$Source[1:15]),
                           selected = "All",
                           width = 200
                         ),

                         submit1= actionButton(ns("button1"), "Submit"),

                         info11 = card(class = "red",
                                       div(class = "content",
                                           div(class = "description", verbatimTextOutput(ns("artcnt1")))
                                       )
                         ),

                         info12 = card(class = "red",
                                      div(class = "content",
                                          div(class = "description", verbatimTextOutput(ns("src1")))
                                      )
                         ),



                         src_tbl1= plotOutput(ns("src_tbl1")),
                         senti_src1= plotOutput(ns("senti_src1")),

                         senti_hd1= plotOutput(ns("senti_hd1")),

                         # active= plotOutput(ns("active")),
                         senti_bd1= plotOutput(ns("senti_bd1")),

                         cor1= plotOutput(ns("cor1")),
                         wd_fq1= plotOutput(ns("wd_fq1")),

                         wd_freq12= plotOutput(ns("wd_freq12")),
                         correlation= plotOutput(ns("correlation")),

                         article1= semantic_DTOutput(ns("article1")),
                         wcloud1= wordcloud2Output(ns("wcloud1")),

                         note1 = textOutput(ns("text1"))
    )
  )
}

mediaServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    get_choice2<- reactive(choice2)
    Final<- eventReactive(input$button1,{news_sub(Final_DB, input$choice1, input$week1, input$lang1, input$source1)})
    Final2<- reactive({
      Final2<- Final()
      get_src<- Final2%>% count(Source)
      get_src<- get_src[order(-get_src$n),]

      Final2$Source2<- "Other"
      for (i in 1:6) {
        Final2$Source2[Final2$Source== get_src$Source[i]]<- get_src$Source[i]
      }
      return(Final2)
    })

    output$artcnt1 <- renderText({
        str11<- paste("Total Articles: ", dim(count(Final(), Header))[1], " ")
        str12<- paste("Hindi Articles: ", dim(count(Final()[Final()$lang== "hi",], Header))[1], " ")
        str13<- paste("English Articles: ", dim(count(Final()[Final()$lang== "en",], Header))[1], " ")

        paste(str11, str12, str13, sep = "\n")
      })

    output$src1 <- renderText({
      str14<- paste("Unique Sources: ", dim(count(Final(), Source))[1], " ")
      str15<- paste("Hindi Sources: ", dim(count(Final()[Final()$lang== "hi",], Source))[1], " ")
      str16<- paste("English Sources: ", dim(count(Final()[Final()$lang== "en",], Source))[1], " ")

      paste(str14, str15, str16, sep = "\n")
    })

    output$choice1<- renderUI({
      ns <- session$ns
      selectInput(
        inputId = ns("choice1"),
        label = div(shiny::tags$h3("Select Input" , style="color:black")),
        choices = get_choice2()$sub[get_choice2()$topic== input$Topic1],
        selected = get_choice2()$sub[get_choice2()$topic== input$Topic1][1],
        width = 200
      )
    })

    output$src_tbl1<- renderPlot({
      get_src<- Final()%>% count(Source)
      get_src<- get_src[order(-get_src$n),]
      # lock in factor level order
      get_src$Source <- factor(get_src$Source, levels = get_src$Source)
      ggplot(get_src[1:20,],
            aes(x= n, y = Source))  +
            geom_bar(position = "dodge", stat = "identity") +
            theme(legend.position="bottom",
                      plot.title = element_text(size = 15, face = "bold")) +
            ggtitle("Important Digital Media Sources")

    })

    output$senti_src1<- renderPlot({

      ggplot(Final2(), aes(week2, senti_bing, fill = Source2)) +
        geom_col(show.legend = TRUE) +
        facet_wrap(~Source2, ncol = 2, scales = "free_x")+
        theme(legend.position="bottom",
              plot.title = element_text(size = 15, face = "bold")) +
        ggtitle(" Sentiment, Source Wise")

    })

    output$senti_hd1<- renderPlot({
      ggplot(Final2(), aes(week2, senti_Head, fill = Source2)) +
        geom_col(show.legend = TRUE) +theme(legend.position="bottom",
                                            plot.title = element_text(size = 15, face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle(" Sentiment, Header")
    })

    output$senti_bd1<- renderPlot({
      ggplot(Final2(), aes(week2, senti_bing, fill = Source2)) +
        geom_col(show.legend = TRUE) +theme(legend.position="bottom",
                                            plot.title = element_text(size = 15, face = "bold"), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
        ggtitle(" Sentiment, Content")
    })

     output$cor1<- renderPlot({
       wd_corr22(Final())
     })

     output$wd_fq1<- renderPlot({
       wd_freq2(Final())
     })

     output$wd_freq12<- renderPlot({
       wd_freq22(Final(), c("covid", "growth", "vaccination"))
     })

     output$correlation<- renderPlot({
       correlation(Final(), c("growth", "police", "women"))
     })
    
      output$article1<- DT::renderDataTable({
         semantic_DT(Final()[c(1:100),c(1, 2, 5,7)],
                     options = list(
                      pageLength = 2))
       })
    
      output$wcloud1<- renderWordcloud2({
         Wcloud(Final())
        })
    
      output$text1 <- renderText({
        str17<- paste("Above analysis was conducted on top news articles of every week, taken from Google News on search topic- ", input$choice1, ".")
        str18<- paste( "Total ", dim(Final())[1], " articles analysed in the exercise. ", Final()$Source[1], " came out as most popular source of information regarding topic.")
        paste(str17, str18, sep = "\n")
      })

  })
}

