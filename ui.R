library(DT)
library(timevis)
library(visNetwork)
library(tidyverse)
library(shiny)



## Define UI for dataset viewer application -----------------------------------
fluidPage(
  
  ## App title -----------------------------------------------------------------
  titlePanel("Suspect Network Visualization"),
  
  ## Sidebar layout with input and output definitions --------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      h2('Suspect Information'),
      
      # Input: Text for first name
      textInput(inputId = "id",
                label = "ID Number",
                value = ""),
      
      br(),
      
      br(),
      
      selectInput(inputId = "centrality", 
                  label = "Choose a centrality measure:", 
                  choices = c("None", 
                              "Betweenness", 
                              "Closeness", 
                              "Degree",
                              "Eigen")),
      
      selectInput(inputId = "crime_type", 
                  label = "Choose a crime of interest:", 
                  choices = c("None", 
                              "Aggravated Violence",
                              "Auto Burglary",
                              "Domestic Violence",
                              "Gun Crime", 
                              "Property Crime",
                              "Violent Crime",
                              "Violent Gun Crime")),
      
      numericInput(inputId = "degree_separation", 
                   label = "Degrees from suspect to view:", 
                   value = 1,
                   min = 1,
                   max = 5)
      
    ),
    
    ## Main panel that shows captions and determines outputs -------------------
    mainPanel(
      h3(textOutput("Network_Title")), 
      
      h5(textOutput("Network_Test")),
      
      # Output: Tabset w/ four tabs -------------------------------------------
      tabsetPanel(type = "tabs",
                  tabPanel("Introduction", textOutput("intro_text")), #Not done
                  tabPanel("Criminal History Summary", DTOutput("crime_hist_table")),
                  tabPanel("Violence Timeline", timevisOutput('timeline'), tableOutput('test')),
                  tabPanel("Suspects", visNetworkOutput("network_suspect", height = '750px'), downloadButton('download_network', 'export as html'))
      )
      
    )
  )
)
