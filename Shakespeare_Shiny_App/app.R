library(shiny)
library(tidytext)
library(ggplot2)
library(tidyverse)
library(wordcloud)
library(shinythemes)
library(RColorBrewer)

books <- list("A Mid Summer Night's Dream" = "summer",
              "The Merchant of Venice" = "merchant",
              "Romeo and Juliet" = "romeo")

# task4: add in getFreq function for pre-processing
getFreq <- function(book, stopwords = TRUE) {
  # check that only one of three books is selected
  if (!(book %in% books))
    stop("Unknown book")
  
  text <-  tibble(text = readLines(sprintf("data/%s.txt", book), encoding="UTF-8"))
  
  # could also pass column of text/character instead
  text <- text %>%
    unnest_tokens(word, text) %>%
    count(word, sort = TRUE) 
  
  if(stopwords){
    text <- text %>%
      anti_join(stop_words)
  }
  return(text)
}

ui <- fluidPage(
  # task6: add in shinythemes function
  theme = shinytheme("cerulean"),
  
  titlePanel("Shakespeare's Plays Word Frequencies"), # Application title
  
  # task1: add in the sidebarLayout with sidebarPanel and mainPanel
  sidebarLayout(
    
    sidebarPanel(
      # task2: add in the inputs in the sidebarPanel
      selectInput(inputId = "book", label = "Choose a book:",
                  choices = books,
                  selected = "summer"),
      
      checkboxInput(inputId="stop_words", label = "Stop words:",
                    value=TRUE),
      
      actionButton(inputId = "rerun", 
                   label = "Rerun"),
      
      hr(),
      
      h3("Word Cloud Settings"),
      
      sliderInput(inputId = "max", label = "Max # of Words:",
                  min = 10, max = 200, value = 100, step = 10),
      
      sliderInput(inputId = "size_large", label = "Size of largest words:",
                  min = 1, max = 8, value = 4),
      
      sliderInput(inputId = "size_small", label = "Size of smallest words:",
                  min = 0.1, max = 4, value = 0.5),
      
      hr(),
      
      h3("Word Count Settings"),
      
      sliderInput(inputId = "min_words", label = "Minimum words for Counts Chart:",
                  min = 10, max = 100, value = 25),
      
      sliderInput(inputId = "word_size", label = "Word size for Counts Chart:",
                  min = 8, max = 30, value = 14)
      
    ),
    
    mainPanel(
      tabsetPanel(
        # task1: within the mainPanel, create two tabs (Word Cloud and Frequency)
        # task3: add in the outputs in the sidebarPanel
        # task6: and modify your figure heights
        tabPanel("Word Cloud", plotOutput("cloud", height='600px')),
        tabPanel("Word Counts", plotOutput("freq", height='600px'))
      )
    )
  )
  
)

server <- function(input, output) {
  
  # task5: add in reactivity for getFreq function based on inputs
  freq <- eventReactive(
    input$rerun,
    {
    withProgress({
      setProgress(message = "Processing corpus...")
      getFreq(input$book,input$stop_words) # ... = replace with the two inputs from Task 2
    })
  })
  
  output$cloud <- renderPlot(
    {
    v <- freq()
    pal <- brewer.pal(8,"Dark2")
    v %>% 
      with(
        wordcloud(
          word, 
          n, 
          scale = c(input$size_large, input$size_small),
          random.order = FALSE, 
          max.words = input$max, 
          colors=pal))
  })
  
  output$freq <- renderPlot({
    v <- freq()
    v %>% filter(n > input$min_words) %>%
      ggplot(aes(x=reorder(word,n), y=n)) +
      geom_bar(stat='identity', fill='#f68060', alpha=0.8) +
      coord_flip() +
      theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(size=input$word_size)) 
  })
}

shinyApp(ui = ui, server = server)
