library(glue)
library(shiny)
library(tidyverse)
library(ggplot2)
library(spotifyr)
library(dplyr)
library(knitr)
library(httr)
library(genius)
library(shinydashboard)


#setup spotify

id <- '5cb5123230cf45479f2ff67d9e2b25a9'
secret <- 'b61ae71af8bd41e48fbfa45b076c87c3'
Sys.setenv(SPOTIFY_CLIENT_ID = id)
Sys.setenv(SPOTIFY_CLIENT_SECRET = secret)
access_token <- get_spotify_access_token()

#function i call later
plug_info <- function(dataset) {
  track_ids <- dataset %>%
    pull(id)
  full_join(dataset, get_track_audio_features(track_ids), by = c("uri", "id"))  
}

# UI
ui <- shinyUI( fluidPage(
  
  # Application title
  titlePanel("Discover How Spotify Categorizes your Favorite Albums"),
  # Sidebar with a text input for album; and checkboxes for variables
  sidebarLayout(
    sidebarPanel(
      
      textInput(inputId = "a_input", 
                label = "What album would you like to look at?", 
                value =  "funk wav bounce"),
      checkboxGroupInput(inputId = "variables", 
                         label = "What factors would you like to look at?", 
                         choices = c("instrumentalness", "liveness", "speechiness", "acousticness", 
                                     "energy", "danceability", "valence"),
                         selected = c("acousticness", "energy", "danceability", "valence")),
      "Behind Spotify's recommendation algorithm is a set of specific values attached to every song in its database.
      Here, you can look at those values and how they vary (or stay the same) through an album, as well as what they actually mean.
      If you can't think of any albums to look at, try clicking some of the links in the value description!"
    ),
    
    # plots and the text below
    mainPanel(
      plotOutput("plot"), htmlOutput("instrumentalness"), htmlOutput("liveness"), 
      htmlOutput("speechiness"), htmlOutput("acousticness"), htmlOutput("energy"),
      htmlOutput("danceability"), htmlOutput("valence"), dataTableOutput("table")
    )
  )
))

# SERVER
server <- function(input, output, session) {
  
  #this is just the name of the album
  a_name <- reactive({
    res <- GET("https://api.spotify.com/v1/search", query = list(q = input$a_input, 
                                                                 type = "album", 
                                                                 access_token = access_token)) %>% content
    res[["albums"]][["items"]][[1]][["name"]]
  })
  
  #artist of the album
  a_artist <- reactive({
    res <- GET("https://api.spotify.com/v1/search", query = list(q = input$a_input, 
                                                                 type = "album", 
                                                                 access_token = access_token)) %>% content
    res[["albums"]][["items"]][[1]][["artists"]][[1]][["name"]]
  })
  
  #this is a dataset with the each song on the album and the tracks on it, modded for interesting stuff
  a_tracks <- reactive({
    res <- GET("https://api.spotify.com/v1/search", query = list(q = input$a_input, 
                                                                 type = "album", 
                                                                 access_token = access_token)) %>% content
    
    get_album_tracks(res[["albums"]][["items"]][[1]][["id"]]) %>%
      mutate(album_name = a_name()) %>%
      plug_info() %>%
      select(album_name, track_number, name, disc_number, tempo, valence, liveness, instrumentalness, acousticness, 
             speechiness, loudness, energy, danceability, mode, duration_ms.x)  %>%
      rename(duration.ms =duration_ms.x) %>%
      mutate(mode = case_when(mode == 1 ~ "Major", mode == 0 ~ "Minor")) %>%
      filter(!is.na(track_number)) %>%
      arrange(track_number)
  })
  
  #evil code that does bad things. also gets the length of the album
  a_length <- reactive({
    aaa <- a_tracks() 
    tail(aaa$track_number, n = 1) %>%
      str_remove("L") %>%
      as.numeric()
  }) 
  
  
  #make a table
  #output$table <- renderDataTable({ a_tracks() %>% select(!album_name) })
  
  
  #make the graph
  output$plot <- renderPlot({
    a_tracks() %>%
      pivot_longer(cols = input$variables, names_to = "variable", values_to = "value") %>%
      mutate(disc_number = case_when(disc_number == 1 ~ "Disc 1", 
                                     disc_number == 2 ~ "Disc 2")) %>%
      ggplot(aes(x = track_number, y = value)) + 
      #geom_line()+ #remember to take out thhis line
      geom_smooth(method = "lm", formula = y ~ poly(x, (a_length()-3)), se = FALSE) + 
      geom_hline(aes(yintercept = 0.5))+
      facet_wrap(variable~disc_number) +
      ylim(0,1) +
      labs(title = glue(a_name(), " by ", a_artist()), 
           y = " ", 
           x = "track number")
  })
  
  
  #make the explanatory & example text for each variable
  output$liveness <- renderUI({
    if("liveness" %in% input$variables) {
      HTML("<p>Liveness describes the presence of an audience, audio grain, or other factors associated with 
           live recording. A score above the line is cataloged to be performed live, and below the line is 
           cataloged as being performed in a studio. A high liveness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=cheap%20trick%20at%20budokan&variables=liveness'>Cheap Trick at Budokan</a>,
           and a low liveness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=be%the%cowboy&variables=liveness'>Be The Cowboy</a>.</p>")
      
    }
  })
  
  output$acousticness <- renderUI({
    if("acousticness" %in% input$variables) {
      HTML("<p>Acousticness describes the presence of acoustic instruments. A score above the line is categorized as 
           largely acoustic, and a score below the line is categorized as largely electronic. A high acousticness 
           album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=remember%20that%20i%20love%20you&variables=acousticness'>Remember That I Love You</a>,
           and a low acousticness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=1000%20gecs&variables=acousticness'>1000 Gecs</a>.</p>")
    }
  })
  
  output$valence <- renderUI({
    if("valence" %in% input$variables) {
      HTML("<p>Valence describes how 'positive' a song is. A score above the line is cataloged as happy, and below 
           the line is catalogued as sad. A high valence album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=bonito%20generation&variables=valence'>Bonito Generation</a>,
           and a low valence album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=a%20crow%20looked%20at%20me&variables=valence'>A Crow Looked At Me</a>.</p>")
    }
  })
  
  output$instrumentalness <- renderUI({
    if("instrumentalness" %in% input$variables) {
      HTML("<p>Instrumentalness describes the presence of a vocal track in a song. A score above the line is cataloged 
           as a purely instrumental track, and below the line is cataloged as having a spoken or sung aspect. A high 
           instrumentalness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=off%20peak&variables=instrumentalness'>Off-Peak</a>, 
           and a low instrumentalness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=21&variables=instrumentalness'>21</a>.</p>")
    }
  })
  
  output$speechiness <- renderUI({
    if("speechiness" %in% input$variables) {
      HTML("<p>Speechiness describes the presence of spoken word. A score above the line is categorized as a spoken word recording, 
           and a score below the line is categorized as a melodic song. A high speechiness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=new%20in%20town&variables=speechiness'>New in Town</a>, 
           and a low speechiness album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=titanic%20rising&variables=speechiness'>Titanic Rising</a>.</p>")
    }
  })
  
  output$energy <- renderUI({
    if("energy" %in% input$variables) {
      HTML("<p>Energy describes how energetic an album is. A score above the line would categorize a song as energetic, and a score below the line
            would be categorized as calm. A high energy album would be
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=good%20faith&variables=energy'>Good Faith</a>, 
           and a low energy album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=carrie%20and%20lowell&variables=energy'>Carrie & Lowell</a>.</p>")
    }
  })
  
  output$danceability <- renderUI({
    if("danceability" %in% input$variables) {
      HTML("<p>Danceability describes how danceable an album is. A score above the line would categorize a song as danceable, and a score below
           the line would categorize a song as not. A high danceability album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=discovery&variables=danceabiliity'>Discovery</a>, 
           and a low danceability album would be 
           <a href='https://shiny.reed.edu/s/users/stevensw/spotify/?a_input=loveless&variables=danceabiliity'>Loveless</a>.</p>")
    }
  })
  
  #lets the hyperlinks work    
  observe({
    query <- parseQueryString(session$clientData$url_search)
    if (!is.null(query[['a_input']])) {
      updateTextInput(session, "a_input", value = query[['a_input']])
    }
    if (!is.null(query[['variables']])) {
      updateCheckboxGroupInput(session, "variables", selected = query[['variables']])
    }
  })
}


# Run the application 
shinyApp(ui = ui, server = server)
