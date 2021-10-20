fluidPage(theme = shinytheme("cosmo"),
          navbarPage(
           collapsible = TRUE, inverse = TRUE, theme = shinytheme("spacelab"),
            
            tabPanel("Comprehesive Sentiment Analysis", "This panel is intentionally left blank"),
            tabPanel("Twitter Insights",
                     sidebarLayout(
                       sidebarPanel(
                         tags$h3(strong("Input")),
                         radioButtons("Choices", "Choices", c("Party", "Issues", "Leaders"),
                                      selected= "Issues"), 
                         conditionalPanel(
                           condition = "input.Choices == 'Party'",
                           selectInput("smoothMethod", "Party",
                                       list("BJP", "INC", "RJD", "RSP", "BSP"))),
                         conditionalPanel(
                           condition = "input.Choices == 'Issues'",
                           selectInput("smoothMethod2", "Issues",
                                       list("Farm Law", "CAA/NRC", "Covid", "Foreign Policy", "Employment"))),
                         conditionalPanel(
                           condition = "input.Choices == 'Leaders'",
                           selectInput("smoothMethod3", "Leaders",
                                       list("NaMo", "RaGa", "AS", "Yogi", "Akhilesh"))),
                         dateRangeInput("date", "Date", start = "2021-07-26", end = "2021-07-19", min = NULL,
                                        max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                        language = "en", separator = " to ", width = NULL)),
                       
                       # sidebarPanel
                       
                       mainPanel("",
                                 fluidRow(box(width = 12,
                                              infoBoxOutput("progressBox2"),
                                              infoBoxOutput("approvalBox2"),
                                              infoBoxOutput("approvalBox3")
                                 )),
                                 
                                 
                                 fluidRow(box(width = 12, h1(strong("Brand Performance")),
                                              
                                              h4("Time Series and overall analysis of search item"),
                                              plotOutput(outputId = "hist"))),
                                 
                                 
                       )),
                     
                     mainPanel("",  
                               
                               # fluidRow(h1(strong("Brand Performance")),
                               # 
                               #          h4("Time Series and overall analysis of search item"),
                               #          plotOutput(outputId = "hist2")),
                               
                               
                               fluidRow(box(width = 16, h1(strong("Brand Impact")),
                                            h4("Comparable Performance Assessment of search item with comparable items"),
                                            splitLayout(cellWidths = c("100%", "50%"),
                                                        plotOutput(outputId = "hist2"), wordcloud2Output("cloud")))),
                               
                               fluidRow(box(width = 30, h1(strong("Influencers and Narrative Setters")),
                                            h4("Important Impact Makers"),
                                            DT::dataTableOutput(outputId = "table1")),
                               )
                     )
                     
                     # mainPanel
                     
            ), # Navbar 1, tabPanel
            
            
            tabPanel("Digital Media Insights", 
                     sidebarLayout(
                       sidebarPanel(
                         tags$h3(strong("Input")),
                         radioButtons("Choices11", "Choices", c("Party", "Issues", "Leaders"),
                                      selected= "Issues"), 
                         conditionalPanel(
                           condition = "input.Choices == 'Party'",
                           selectInput("smoothMethod11", "Party",
                                       list("BJP", "INC", "RJD", "RSP", "BSP"))),
                         conditionalPanel(
                           condition = "input.Choices == 'Issues'",
                           selectInput("smoothMethod12", "Issues",
                                       list("Covid Vaccination", "Farm Law", "CAA/NRC", "Covid", "Foreign Policy", "Employment"))),
                         conditionalPanel(
                           condition = "input.Choices == 'Leaders'",
                           selectInput("smoothMethod13", "Leaders",
                                       list("NaMo", "RaGa", "AS", "Yogi", "Akhilesh"))),
                         
                         selectInput("smoothMethod14", "Media",
                                     list("All", "Times of India", "Mint", "Hindustan Times", "Economic Times", "Reuters India", "India Today", "The Indian Express", "Livemint",              
                                          "Business Standard", "Scientific American", "The New Indian Express", "The Better India", "The News Minute", "India TV News", "Times Now", "Al Jazeera English",    
                                          "Scroll.in", "BBC News", "OpIndia", "USA TODAY",             
                                          "Moneycontrol"  )),
                         
                         dateRangeInput("date15", "Date", start = "2021-07-17", end = "2021-07-26", min = NULL,
                                        max = NULL, format = "yyyy-mm-dd", startview = "month", weekstart = 0,
                                        language = "en", separator = " to ", width = NULL)),
                       
                       # sidebarPanel
                       
                       mainPanel("",
                                 fluidRow(box(width = 12,
                                              infoBoxOutput("progressBox11"),
                                              infoBoxOutput("approvalBox12"),
                                              infoBoxOutput("approvalBox13")
                                 )),
                                 
                                 
                                 fluidRow(box(width = 12, h1(strong("Sentiments around Headers")),
                                              
                                              h4("Day wise Positive and Negative Sentiments in Header and Desription around the issue covered across major new portals"),
                                              plotOutput(outputId = "hist11"))),
                                 
                                 
                       )),
                     
                     mainPanel("",  
                               
                               # fluidRow(h1(strong("Brand Performance")),
                               # 
                               #          h4("Time Series and overall analysis of search item"),
                               #          plotOutput(outputId = "hist2")),
                               
                               
                               fluidRow(box(width = 16, h1(strong("Sentiment in contents")),
                                            
                                            h4("Day wise Positive and Negative Sentiments around the issue covered across major new portals"),
                                            plotOutput(outputId = "hist12"))),
                               
                               fluidRow(box(width = 16, h1(strong("Popular Terms and Correlation")),
                                            
                                            h4("Most popular words used in covered articles and their popular correlated bigrams"),
                                            splitLayout(cellWidths = c("75%", "75%"),
                                                        plotOutput(outputId = "hist13"), plotOutput(outputId = "hist14")))),
                               
                               fluidRow(box(width = 16, h1(strong("Conversation around Topics of Interest")),
                                            
                                            h4("Belows plots cover the important correlated terms of our topic of interest and frequent positive and negative sentiment generating words"),
                                            splitLayout(cellWidths = c("75%", "75%"),
                                                        plotOutput(outputId = "hist15"), plotOutput(outputId = "hist16")))),
                               
                               fluidRow(box(width = 16, h1(strong("Coverage of Topics of Interest")),
                                            
                                            h4("Below plots portrays the frequency of use of topics of interest in the covered articles"),
                                            splitLayout(cellWidths = c("100%", "50%"),
                                                        plotOutput(outputId = "hist17"), wordcloud2Output("cloud11")))),
                               
                               fluidRow(box(width = 30, h1(strong("Covered Articles")),
                                            h4("List of articles, sources and links covered under our study"),
                                            DT::dataTableOutput(outputId = "table11")),
                               )
                     )
                     
                     # mainPanel
            ),
            tabPanel("Online forums Insights", "This panel is intentionally left blank"),
            tabPanel("Youtube/fb Insights", "This panel is intentionally left blank")
            
          ) # navbarPage
) # fluidPage
