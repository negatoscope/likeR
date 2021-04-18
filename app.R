# Written by Luis Eudave (@negatoscope)
# You may share and modify this code as you see fit.
#
# Modified from @AmandaRP. You can find the original code here: 
# https://github.com/AmandaRP/TwitterBrowser/blob/master/TwitterBrowser/app.R

library(shiny)
library(rtweet)
library(tidyverse)
library(tidytext)
library(httpuv)
library(shinybusy)
#library(bslib)

## API keys and authentication  are necessary if you plan load the app online. 
## You need a developer Twitter account and follow this instructions:
## https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html

## Store api keys (these are fake example values; replace with your own keys)
api_key <- ""
api_secret_key <- ""
access_token <- ""
access_token_secret <- ""

## Authenticate via web browser
token <- create_token(
    app = "negatoscope_liker",
    consumer_key = api_key,
    consumer_secret = api_secret_key,
    access_token = access_token,
    access_secret = access_token_secret)

# Define UI 
ui <- fluidPage(title = "likeR",
  
    # Adds browser window icon
    tags$head(
        tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/icon.png")),
    
    # You might want to add a theme, but some elements end out of place
    #theme = bs_theme(version = 4, bootswatch = "minty"),
    
    # Just a loader
    add_busy_gif(src = "https://media0.giphy.com/media/l31p1SkNXGz3l1nwwu/giphy.gif?cid=ecf05e476m3k585mg9216s5a8athfuk7ehmafik0uzemtyc7&rid=giphy.gif", height = 70, width = 70),
    
    # Application title
    headerPanel(h3("likeR - Browse your Likes", style = "color: #47D6E3")),
    
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
        
        get_favorites(input$user, n = 3000, token = token)  %>% 
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
