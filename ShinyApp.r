
#------------------------------------------------------------------------
# Topic: Shiny App to demonstrate UDPipe NLP workflow
#
# Authors: Shyla Kumar (11910070), Vinayak , Rakesh (11910046),
#
# Program / Component: ui.R - It is user interface part of Shiny App
#
# Date: 19-May-2018
#
# ------------------------------------------------------------------------

# ------------------------------------------------------------------------
# Install & Load the required Libraries
# ------------------------------------------------------------------------

if (!require(shiny)){install.packages("shiny")}
if (!require(udpipe)){install.packages("udpipe")}
if (!require(textrank)){install.packages("textrank")}
if (!require(lattice)){install.packages("lattice")}
if (!require(igraph)){install.packages("igraph")}
if (!require(ggraph)){install.packages("ggraph")}
if (!require(wordcloud)){install.packages("wordcloud")}
if (!require(rsconnect)){install.packages("rsconnect")}
if (!require(gridExtra)){install.packages("gridExtra")}
if (!require(tidyverse)) {install.packages("tidyverse")}
if (!require(tidytext)) {install.packages("tidytext")}
if (!require(tm)) {install.packages("tm")}

library("shiny")
library(tidyverse)
library(tidytext)
library(udpipe)
library(NLP)
library(textrank)
library(lattice)
library(igraph)
library(ggraph)
library(ggplot2)
library(wordcloud)
library(stringr)
library('shiny')
library(readr)
library(rsconnect)
library('gridExtra')
library('tm')
library(textstem)



#----------------------------------------------------------------------------------------------------------
# Define UI for application that draws a histogram
#----------------------------------------------------------------------------------------------------------

ui <- shinyUI( 
      
  fluidPage(
   
      titlePanel("UDPipe NLP Workflow"),
      
      sidebarLayout( 
        
        sidebarPanel(  
          
          fileInput("file", "Upload text data"), #Input field to load the TXT file 
          
          radioButtons(inputId = "language", label = "Language Model", choices = c("english")), # Radio button to select the language
          
          checkboxGroupInput(inputId = "speech", label = "Universal Parts-of-Speech Tags", choices = c("ADJ", "NOUN", "PROPN", "ADV", "VERB"),selected=c("ADJ","NOUN","PROPN")),
          downloadButton("download", "Download Annotation") # Checkbob to select the Parts of Speech
          
        ),   # end of sidebar panel
        
        
        mainPanel(
          
          tabsetPanel(type = "tabs",
                      
                      #Pane to display the instructions to use this Shiny Application
                      
                      tabPanel("About App",
                               h4(p(strong("About this Shiny App"))),
                               p("This app is all about a Shiny App to showcase UDPipe NLP Workflow",align="justify"),
                               br(),
                               h4('How to use this App'),
                               p("* This app supports only TEXT files.",align="justify"),
                               p("* Before click on any TAB, upload the TEXT file",align="justify"),
                               p("* This APP has three Tabs i.e, Annotation, Word Cloud, Network Plot",align="justify"),
                               p("   * Annotaton - All about Tokens and catagorize them with Part of Speech"),
                               p("   * Word Cloud - Two Word Clouds, one with VERBs and one with NOUNs",align="justify"),
                               p("   * Network Plot - Top 30 co-occurances at document level based on UPOS selection (sidebar)",align="justify"),
                               p("* Also download Annotation option available",align="justify")
                      ),
                      
                      
                      # Pane to display the Annotation information
                      
                      tabPanel("Annotation", 
                               dataTableOutput('plot1')),
                      
                      # Pane to display the wordcloud(s)
                                            
                      tabPanel("Word Cloud",
                               plotOutput('cloud')),

                      # Pane to display the Co-occurance
                      
                      tabPanel("Network Plot_Co_occurance",
                               plotOutput('co_occ'))
                      
          ) # end of tabsetPanel
        )# end of main panel
      ) # end of sidebarLayout
    )  # end if fluidPage
)

#--------------------------------------------------------------------------------------------------------------
# Define server logic required to draw a histogram
#--------------------------------------------------------------------------------------------------------------

server <- shinyServer(function(input, output) {
  
  model <- udpipe_download_model(language = "english")

  # ------------------------------------------------------------------------------------------------------------  
  # Retrieve the dataset selected in ui.R coponent & do basic cleaning of the same  
  # ------------------------------------------------------------------------------------------------------------
  
  Dataset <- reactive({
    
    Data <- read_file(input$file$datapath)
    Data0 <- gsub("[^A-Za-z\\s]"," ",Data)
    Data1 <- str_replace_all(Data0, "[\\s]+", " ")
    Data1_1 <- data.frame(Data1,stringsAsFactors = FALSE) 
    Data1_2 <- Data1_1 %>% unnest_tokens(word,Data1) %>% anti_join(stop_words) 
    Data1_2 <- lemmatize_words(Data1_2$word)
    Data1_3 <- paste0((c(Data1_2)),collapse = " ")
    
  })
  
  Dataset_raw <- reactive({
    
    Data_raw <- read_file(input$file$datapath)
    Data_raw_0 <- gsub("[^A-Za-z\\s]"," ",Data_raw)
    Data_raw_1 <- str_replace_all(Data_raw_0, "[\\s]+", " ")
    
  })
  
  
  # ------------------------------------------------------------------------------------------------------------
  # Retrieve the parts of speech selection (from group check box) from ui.R side panel    
  # ------------------------------------------------------------------------------------------------------------    
  
  speech <- reactive({
    speech <- input$speech
    
  })
  
  # ------------------------------------------------------------------------------------------------------------
  # Retrieve language selection from ui.R side panel  
  # ------------------------------------------------------------------------------------------------------------
  
  language <- reactive({
    language <- input$language
  })
  
  # ------------------------------------------------------------------------------------------------------------  
  # Populate the first TAB of the main panel i.e., Annotation output with 100 rows excluding statement column
  # ------------------------------------------------------------------------------------------------------------
  
  output$plot1 = renderDataTable({ 
    plot1 <- Dataset()
    language_list <- language()
    #model <- udpipe_download_model(language = language_list)
    udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
    s <- udpipe_annotate(udmodel_english, plot1)
    x <- data.frame(s)
    y <- filter(x,x$upos != 'PUNCT')
    y$sentence <- NULL 
    head(y,100)
  }) # end of output$plot1 section
  
  
  # ------------------------------------------------------------------------------------------------------------
  # Build the NOUN word cloud from the input TEXT file word corpus
  # ------------------------------------------------------------------------------------------------------------  
  
  cloud1 <- reactive({
    cloud_1 <- Dataset()
    #model <- udpipe_download_model(language = "english")
    udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
    s <- udpipe_annotate(udmodel_english, cloud_1)
    x <- data.frame(s)
    stats <- subset(x, upos %in% c("NOUN"))
    stats <- txt_freq(stats$token)
    stats$key <- factor(stats$key, levels = rev(stats$key))
    wordcloud(words = stats$key, freq = stats$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  # ------------------------------------------------------------------------------------------------------------
  # Build the VERB word cloud from the input TEXT file word corpus
  # ------------------------------------------------------------------------------------------------------------
  
  cloud2 <- reactive({
    cloud_2 <- Dataset()
    #model <- udpipe_download_model(language = "english")
    udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
    s <- udpipe_annotate(udmodel_english, cloud_2)
    x <- data.frame(s)
    stats1 <- subset(x, upos %in% c("VERB")) 
    stats1 <- txt_freq(stats1$token)
    stats1$key <- factor(stats1$key, levels = rev(stats1$key))
    wordcloud(words = stats1$key, freq = stats1$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.35, 
              colors=brewer.pal(8, "Dark2"))
  })
  
  # ------------------------------------------------------------------------------------------------------------
  # Populate the Word Cloud TAB of the main panel with VERB & NOUN words from the input TEXT file word corpus
  # ------------------------------------------------------------------------------------------------------------  
  
  output$cloud = renderPlot({ 
    
    par(mfrow=c(1,2), cex = .75)
    cloud1()
    cloud2()
  })  
  
  # ------------------------------------------------------------------------------------------------------------
  # Populate the co-ocuurance TAB of the main panel with selected UPOS from side panel  
  # ------------------------------------------------------------------------------------------------------------
  
  output$co_occ = renderPlot({ 
    
    speech_list  <- speech()
    
    
    co_occ <- Dataset_raw()
    #model <- udpipe_download_model(language = "english")
    udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
    s <- udpipe_annotate(udmodel_english, co_occ)
    x <- data.frame(s)  
    stats2 <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
                            relevant = x$upos %in% speech_list, ngram_max=3)
    
    #stats2 <- keywords_rake(x = x, term = "lemma", group = "doc_id", 
    #                        relevant = x$upos %in% c("VERB", "NOUN", "PROPN", "ADJ"))
    
    stats2$key <- factor(stats2$keyword, levels = rev(stats2$keyword))
    barchart(key ~ rake, data = head(subset(stats2, freq > 1), 30), col = "red", main = "Co-occurances of selected UPOS", 
             xlab = "Occurances")
    
  })
  
  
  # ------------------------------------------------------------------------------------------------------------
  # Code block to handle the download option for Annotation
  # ------------------------------------------------------------------------------------------------------------
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste("Test", ".csv", sep = "")
    },
    
    
    content = function(file) {
      download <- Dataset()
      #model <- udpipe_download_model(language = "english")
      udmodel_english <- udpipe_load_model(file = 'english-ud-2.0-170801.udpipe')
      s <- udpipe_annotate(udmodel_english, download)
      x <- data.frame(s)  
      y <- x
      y$sentence <- NULL 
      y
      write.csv(y, file)
    })
  
})

# ------------------------------------------------------------------------------------------------------------
# Run the application 
# ------------------------------------------------------------------------------------------------------------

shinyApp(ui = ui, server = server)

