# Concepts about Reactive programming used by Shiny, 
# https://shiny.rstudio.com/articles/reactivity-overview.html

# Load R packages
library(glue)
library(DT)
library(ggpubr)
library(tibble)
library(shiny)
library(shiny.router)
library(shiny.semantic)
library(shinyjs)
library(leaflet)
library(dplyr)


source("twitter.R")
source("media.R")
source("forums.R")
source("ytfb.R")
source("comp.R")


info_page <- div(
  div(class = "ui two column stackable grid container",
      div(class = "six wide column",
          img(height = 520, width = 325, src="https://bloximages.newyork1.vip.townnews.com/mcdowellnews.com/content/tncms/assets/v3/editorial/9/40/940339fe-a378-11ea-b389-d3b63880f481/5ed40ba081618.image.jpg?resize=500%2C667")),
      div(class = "ten wide column",
          div(class="ui center aligned big header", "Public Sentiment Barometer"),
          p("This app was created for the purposes of demonstrating Public Sentiment Barometer
             features in creating an interactive data visualization. This dashboard uses ",
             "data and has been inspired by ",
            "an amazing ", "."),
          p("Feel free to explore the various features of this application and ",
            "analyze your favourite topics"),
          div(class="ui center aligned", style = "text-align: center;",
              action_button("go_modal", "Learn more", class = "teal"),
              br(),br(),
              HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/n4L5hHFcGVk" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
      )
  ))

router <- make_router(
  route("index", info_page),
  route("media", uimedia("p1")),
  route("twitter", uitwitter("p2")),
  route("forums", uitwitter("p3")),
  route("ytfb", uiytfb("p4")),
  route("comp", uicomp("p5"))
)

server <- function(input, output, session) {
  # router pages
  router$server(input, output, session)#router(input, output) #
  mediaServer("p1")
  twitterServer("p2")
  twitterServer("p3")
  ytfbServer("p4")
  compServer("p5")
}


ui <- semanticPage(
  title = "Public Sentiment Barometer",
  tags$head(
    tags$link(rel="stylesheet", href="main.css", type="text/css")
  ),
  horizontal_menu(
    list(
      list(name = "Home", link = route_link("index"), icon = "world"),
      list(name = "Digital Media", link = route_link("media"), icon = "newspaper"),
      list(name = "Twitter", link = route_link("twitter"), icon = "twitter"),
      list(name = "Youtube/ Facebook", link = route_link("ytfb"), icon = "youtube"),
      list(name = "Forums", link = route_link("forums"), icon = "reddit"),
      list(name = "Comprehensive", link = route_link("comp"), icon = "eject")
    ), logo = "https://www.pulsex.co.in/images/logo.png"
  ),
  router$ui,#router_ui(),
  tags$footer("Created by PulseX for X", align = "center", style = "position:fixed; bottom:0; right:0; left:0; background:black; color: white; padding:10px; box-sizing:border-box; z-index: 1000; text-align: center")
)


shinyApp(ui, server)