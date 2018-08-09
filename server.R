###############################################################################
#########                    SHINY SERVER SCRIPT                     ##########
###############################################################################
# This script is the servever side of the Shiny app for suspect network
# visualization.


## Loading Packages -----------------------------------------------------------
library(network)
library(sna)
library(igraph)
library(stringr)
library(GGally)
library(intergraph)
library(RecordLinkage)
library(timevis)
library(visNetwork)
library(DT)
library(htmlwidgets)
library(tidyverse)
library(shiny)
library(shinythemes)


# Loading in source
source('data_cleaning.R')


## Define server logic to summarize and view data -----------------------------
shinyServer(function(input, output, session) {
  
  ## Central node to test if name is in data ----------------------------------
  central_node <- reactive({
    # Ensuring that the input$UID is upper case
    test_UID <- input$id
    
    # Calculate nearest strings
    distance = levenshteinSim(test_UID, suspect_attr$UID)
    
    if (test_UID %in% suspect_attr$UID){
      name_used <- test_UID 
    }
    else if (max(distance) > 0.7){
      # Taking the best matched string given the user entry
      name_used <- suspect_attr$UID[distance == max(distance)][1]
    }
    else {
      name_used <- test_UID
    }
    
    return(name_used)
  })
  
  # The output$Network_Title is computed based on a reactive expression that
  # returns input$UID. When the user changes first name, last name, or DOB:
  #
  #  1) This expression is automatically called to recompute the output 
  #  2) The new caption is pushed back to the browser for re-display
  # 
  output$Network_Title <- renderText({
    paste0(input$id, "'s network.")
  })
  
  output$Network_Test <- renderText({
    if (central_node() %in% suspect_attr$UID){
      paste0(input$id, " appears in the data. This is the name closest to what you
             entered.")
    }
    else{
      paste0(input$id, " does not appear in the data. Be sure that you've entered the correct unique identifier.")
    }
    })
  
  #############################################################################
  ##########                  Criminal History Summary               ##########
  #############################################################################
  crime_hist <- all_data %>%
    ungroup(UID) %>%
    select(DATE, INCIDENT_NO, UID, DESCRIPTION, CRIME_TYPE, CITY)
  
  output$crime_hist_table <- renderDT({
    datatable(crime_hist %>%
      filter(UID == central_node()),
      colnames = c('Date', 'Incident #', 'ID', 'Description', 'Category', 'City'),
      extensions = c('Buttons'), options = list(
        dom = 'Brtip',
        buttons = 
          list('colvis', 'copy', 'print', list(
            extend = 'collection',
            buttons = c('csv', 'excel', 'pdf'),
            text = 'Download'))
      )) %>%
      formatDate(1, method = 'toLocaleString')
  })
  
  
  #############################################################################
  ##########                    VIOLENCE TIMELINE                    ##########
  #############################################################################
  timeline_info <- reactive({
    data.frame(filter(timeline_data, UID == central_node()))
  })
  
  output$timeline <- renderTimevis({
    timevis(timeline_info())
  })

  
  #############################################################################
  ##########                Graphing Suspect Network                 ##########
  #############################################################################
  # Reactive induced subgraph that is based on central_node() and input$centrality
  
  # Grabbing the subnetwork for the suspects
  G_suspect_subnetwork <- reactive({
    induced.subgraph(suspect_network, 
                     vids = unlist(neighborhood(suspect_network, 
                                                order = input$degree_separation, 
                                                nodes = central_node())))
  })
  
  
  ## Setting node size --------------------------------------------------------
  node_suspect_size <- reactive({
    
    if (input$centrality == 'Betweenness'){
      node_size <- data.frame('betweenness' = betweenness(G_suspect_subnetwork(), 
                                                                  v = V(G_suspect_subnetwork()),
                                                                  directed = FALSE,
                                                                  normalized = TRUE)*10,
                              'UID' = V(G_suspect_subnetwork())$name) %>%
        mutate(value = (betweenness / (max(betweenness) - min(betweenness)))*10) %>% 
        select(UID, value)
    }
    else if (input$centrality == 'Closeness'){
      node_size <- data.frame('closeness' = closeness(G_suspect_subnetwork(),
                                                              vids = V(G_suspect_subnetwork()),
                                                              normalized = TRUE),
                              'UID' = V(G_suspect_subnetwork())$name) %>%
        mutate(value = (closeness / (max(closeness) - min(closeness)))^4) %>%
        select(UID, value)
    }
    else if (input$centrality == 'Degree'){
      node_size <- data.frame('degree' = degree(G_suspect_subnetwork(),
                                                        v = V(G_suspect_subnetwork()),
                                                        loops = FALSE,
                                                        normalized = TRUE),
                              'UID' = V(G_suspect_subnetwork())$name) %>%
        mutate(value = ((degree / (max(degree) - min(degree)))*10)^2) %>%
        select(UID, value)
    }
    else if (input$centrality == 'Eigen'){
      node_size <- data.frame(eigen_centrality(G_suspect_subnetwork(),
                                                       directed = FALSE)) %>%
        mutate('UID' = V(G_suspect_subnetwork())$name,
               value = (vector / (max(vector) - min(vector)))*20) %>%
        select(UID, value)
    }
    else {
      node_size <- data.frame('UID' = V(G_suspect_subnetwork())$name,
                              'value' = 10)
    }
    
    node_size$UID <- as.character(node_size$UID)
    node_size$value <- as.integer(node_size$value)
    return(node_size)
  })
  
    
  ## Creating node and edge list ----------------------------------------------
  
  # Creating the node list
  node_suspect_list <- reactive({
    ids <- as_ids(V(G_suspect_subnetwork()))
                 
  node_list <- data.frame(id = ids,
                          label = ids)
  
  
  # Merging attributes that impact node shape to node list
  node_list <- left_join(node_list, suspect_attr, by = c('id' = 'UID'))
  
  # Joining afftributes that impact node size to node list
  node_list <- left_join(node_list, node_suspect_size(), by = c('id' = 'UID'))
  
  # Joining afftributes that impact node shape to node list
  if (input$crime_type == "Aggravated Violence") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_AGG_VIOLENCE == 1, 'triangle', 'dot'))
  }
  else if (input$crime_type == "Auto Burglary") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_AUTOBURG == 1, 'triangle', 'dot'))
  }
  else if (input$crime_type == "Domestic Violence") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_DV == 1, 'triangle', 'dot'))
  }
  else if (input$crime_type == "Gun Crime") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_GUN == 1, 'triangle', 'dot'))
  }
  else if (input$crime_type == "Property Crime") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_PROPERTY == 1, 'triangle', 'dot'))
  }
  else if (input$crime_type == "Violent Crime") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_VIOLENT == 1, 'triangle', 'dot'))
  }
  else if (input$crime_type == "Violent Gun Crime") {
    node_list <- node_list %>%
      mutate(shape = ifelse(COMMIT_GUN_VIOLENT == 1, 'triangle', 'dot'))
  }
  else {
    node_list <- node_list %>%
      mutate(shape = 'dot')
  }
  
  # Return the node list
  return(node_list)
  
  })

  
  # Creating the edge list
  edge_suspect_list <- reactive ({
    
    # Creating data frame of edges from graph
    edge_list <- G_suspect_subnetwork() %>%
      as_data_frame() %>%
      select(from, to) %>%
      distinct()
    
    # Return edge list
    return(edge_list)
  })
  
  ## Graphing Network ---------------------------------------------------------
  # The output$network_suspect visuazlies subgraphs with the following qualities:
  # 1) centrality = size of node
  # 2) crime type = shape of node (triangle if have committed; circle if not)
  
  my_network <- reactive({
    nodes <- node_suspect_list()
    edges <- edge_suspect_list()
    visNetwork(nodes, edges)
  })
  
  output$network_suspect <- renderVisNetwork({
    my_network() %>%
      visLegend(width = 0.1, position = 'right', main = 'Committed Crime of Interest?',useGroups = FALSE, addNodes = data.frame(label = c('Yes', 'No'), shape = c('triangle', 'dot'))) %>%
      visExport(type = 'pdf', float = 'left', name = paste0(central_node(), '_network')) %>%
      visOptions(nodesIdSelection = TRUE,
                 highlightNearest = TRUE)
    
  })
  
  output$download_network <- downloadHandler(
    filename = function() {
      paste('network-', Sys.Date(), '.html', sep='')
    },
    content = function(con) {
      my_network() %>% visSave(con)
    }
  )
  
  
})
  
  
