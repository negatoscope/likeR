# Written by Luis Eudave (@negatoscope)
# You may share and modify this code as you see fit.
#
# Modified from @AmandaRP. You can find the original code here: 
# https://github.com/AmandaRP/TwitterBrowser/blob/master/TwitterBrowser/app.R

library(shiny)
library(rtweet)
library(tidyverse)
library(tidytext)
library(bslib)

# Define UI 
ui <- fluidPage(
    
    # You might want to add a theme, but some elements end out of place
    #theme = bs_theme(version = 4, bootswatch = "minty"),
    
    # Application title
    titlePanel("likeR - Browse your Likes"),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            h4(tags$b("Welcome!")),
            h5("If you, like me, favorites or likes a tweet just save it, but now you have some many 
               it's become unbrowsable, this is for you. You can browse and search your favorite likes 
               with this Shiny App!"),
            textInput("user", "Your Twitter handle without an @:", value = ""),
            actionButton("update", "Go"),
            h5(tags$b("DISCLAIMER: "), "By default it will load the maximum of 3,000 tweets that Twitter allows. You might need 
               to give permission to allow this app to retrieve your Tweets. Some users might be 
               unreachable."),
            h5("Code for this app can be found ",a("here.", href='https://www.github.com/negatoscope/likeR')),
            width = 3,
        ),
        
        mainPanel(
            dataTableOutput("favoritesTable")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    username <- eventReactive(input$update, {
        runif(input$user)
    })
   
    output$favoritesTable <- renderDataTable({
        
        get_favorites(input$user, n = 3000)  %>% 
             select(status_id, created_at, text, name, screen_name) %>%
             mutate(Link = str_c("<a href='https://twitter.com/", screen_name, "/status/", status_id, "'>link</a>")) %>%
             select(-status_id) %>%
             arrange(desc(created_at)) %>%
             mutate(created_at = as.character(created_at)) %>%
             rename(Date = created_at, Text = text, Name = name, Handle = screen_name)
      },
      options = list(
          autoWidth = TRUE), escape = FALSE)
    
}

# Run the application 
shinyApp(ui = ui, server = server)