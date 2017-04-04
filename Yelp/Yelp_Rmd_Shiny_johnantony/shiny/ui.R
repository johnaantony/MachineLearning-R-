library(shiny)
library(leaflet)
library(plotly)


# Choices for drop-downs
color_vars <- c(
  "Stars mark" = "stars",
  "Review count" = "review_count",
  "Starts difference" = "stars_diff"
)

size_vars <- c(
  "Review count" = "review_count",
  "Stars mark" = "stars"
)

category_vars <- c(
   "All" = "All",
   "Restaurant" = "Restaurant",
   "Shopping" = "Shopping",
   "Health & Medical" = "Health & Medical",
   "Hotels" = "Hotels",
   "Home Services" = "Home Services",
   "Food"="Food"
)

shinyUI(navbarPage("Yelp Data Analysis", id="nav",

  tabPanel("Interactive map",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("libraries/styles.css"),
        includeScript("libraries/gomap.js")
      ),

      leafletOutput("map", width="100%", height="100%" ),

      # Shiny versions prior to 0.11 should use class="modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = TRUE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = 330, height = "auto",
        h4("Business explorer"),

        
         br(),
        h4("Filter options"),
        
        selectInput("business_category", "Category", category_vars, selected = "adultpop"),
  
        sliderInput("reviews", "Minimum number of reviews:",min = 0, max = 500, value = 10, step = 5), 
        sliderInput("stars", "Minimum number of stars:",min = 0, max = 5, value = 0, step = 1),
        #sliderInput("stars",label= h3("Number of Stars"),min=1,max=5,value=c(1,5))
        checkboxInput("takeout",label="Take-out",value=TRUE),
        checkboxInput("reserve", label="Takes Reservations",value=TRUE),
        checkboxInput("wifi",label="Free Wi-Fi",value=FALSE),
        checkboxInput("caters",label="Caters",value=TRUE)
        
        ),

      
      
      tags$div(id="cite",
         tags$em('Machine Learning with R : Abhishek,John,Naveen,Vidhyasree')
      )
    )
  ),

  tabPanel("Data explorer",
    fluidRow(
      column(3,
        selectInput("states", "States", c("All states"="", structure(state.abb, names=state.name), "Washington, DC"="DC"), multiple=TRUE)
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("cities", "Cities", c("All cities"=""), multiple=TRUE)
        )
      ),
      column(3,
        conditionalPanel("input.states",
          selectInput("zipcodes", "Zipcodes", c("All zipcodes"=""), multiple=TRUE)
        )
      )
    ),
    
    hr(),
    DT::dataTableOutput("ziptable")
  ),

  conditionalPanel("false", icon("crosshair"))
))
