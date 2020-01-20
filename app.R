#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#Load all relevant libraries

library(shiny)
library(wordcloud)
library(genius)
library(tm)
library(ggwordcloud)
library(wordcloud2)
library(tidyverse)
library(tidytext)

# Define user interface

ui <- fluidPage(
   
   # Application title
   titlePanel("Text Analysis on Lyrics"),
   h1("Welcome to my app !"), 
   h2("Here, you will be able to perform text analysis on your favorite songs. Please follow the instructions."),
   
   # Sidebar where users can input artist and song name 
   sidebarLayout(
      sidebarPanel(
        textInput(inputId = "artist", label = "Please insert artist name", value = "PNL"),
        textInput(inputId = "song", label = "Please insert song name", value = "Jusqu'au dernier gramme") 
        
         
      ),
      
      # A main panel to display the wordcloud, based on the user's input
      mainPanel(
        
        wordcloud2Output("wordcloud")
         
      )
   )
)

# Define server logic 

server <- function(input, output) {
  
  #Create the stopwords dataframe. 
   
  stopwords_french <- as.data.frame(stopwords(kind = "french"))
  names(stopwords_french)[1] <- "word"  
  my_stopvector = c("comme", "g", "c'est", "j'les", "j'ai", "j'dois", "plus", "ça", "si", "d'mon", "j'suis")
  my_stopwords <- data.frame(word = my_stopvector)
  
  #Import text data from genius.com, using the genius package
  
  lyrics <- reactive({ 
    genius_lyrics(input$artist, input$song)
  })
  
  
  #Plot the wordcloud in relation to the user's input
 
  output$wordcloud <- renderWordcloud2({
    
    lyrics() %>% 
      unnest_tokens(word, lyric) %>%  #unnest to have one row per word
      anti_join(stopwords_french) %>% 
      anti_join(my_stopwords) %>% #use anti_join to remove stopwords from lyrics dataframe
      count(word) %>% #count the occurence of each word
      arrange(desc(n)) %>% #arrange words from the most frequent to the less frequent
      mutate(frequence = n / nrow(lyrics())) %>% #create a new column "frequence"
      arrange(desc(frequence)) %>% #arrange words by descending frequence
      top_n(30, frequence) %>% #keep only the 30 most frequent words (by frequence)
      wordcloud2(fontWeight = "normal", size = 0.5) #plot the wordcloud using the wordcloud2 package
  
    


      
  } 
    
    ) 
  
  
    

  
}

# Run the application 
shinyApp(ui = ui, server = server) 

