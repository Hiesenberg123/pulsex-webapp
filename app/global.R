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
library(lattice)
library(googlesheets4)

library(wordcloud)    # word-cloud generator 
library(RColorBrewer) # color palettes
library(stopwords) 
library(udpipe)

library(twitteR)
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


library(keras)
library(dplyr)
library(ggplot2)
library(purrr)


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

library(DT)
library(leaflet)
library(readr)
library(geosphere)
library(shinyjs, warn.conflicts = FALSE)
library(testthat, warn.conflicts = FALSE)
library(utils, warn.conflicts = FALSE)

#ships <- readr::read_csv(unzip("ships_04112020.zip", "ships.csv"))
#ships<- ships[c(1:1000000),]
#ships <- read_csv("ships.csv")
#ships$Time <- format(ships$DATETIME,"%H:%M:%S")




myGrid <- grid_template(
  default = list(
    # Here we define the data.frame describing our layout
    # The easiest way is to use rbind so that the layout can be 'visualized' in code
    areas = rbind(
      c("header", "info1", "info2"),
      c("map",   "map",   "map"),
      c("note",   "note",   "note"),
      c("input1",   "input2",   "general")
    ),
    # Then we define the dimensions of the different elements of the layout
    # We can use any valid css units to make the layout behave exactly as desired
    rows_height = c("100px", "2fr", "1fr", "100px"),
    cols_width = c("34%", "33%", "33%")
  ),
  # This is optional, but we can define a specific layout for mobile (screen width below 768px)
  mobile = list(
    areas = rbind(
      c("header", "info1", "info2"),
      c("map",   "map",   "map"),
      c("note",   "note",   "note"),
      c("input1",   "input2",   "general")
    ),
    rows_height = c("100px", "2fr", "1fr", "100px"), # Notice how we changed the rows heights here
    cols_width = c("34%", "33%", "33%")
  )
)
