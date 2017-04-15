library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(magrittr)
library(plotly)

# Leaflet bindings are a bit slow; for now we'll just sample to compensate
set.seed(100)
business_data <- business.df[sample.int(nrow(business.df), 60000),]
# By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
# will be drawn last and thus be easier to see
business_data <- business_data[order(business.df$stars),]

business_data <- business_data %>%filter(str_detect(categories,"Restaurant" ))

cleantable <- business_data %>%
  dplyr::select(
    City = city,
    State = state,
    Zipcode = postal_code,
    Stars = stars,
    Lat = latitude,
    Long = longitude
  )

shinyServer(function(input, output, session) {
  
  ## Interactive Map ###########################################
  
  
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      
      mapOptions(zoomToLimits="always") %>%             
      
      addMarkers(lat=business_data$latitude,lng=business_data$longitude,clusterOptions = markerClusterOptions(),popup=business_data$name)   
    #setView(lng = -93.85, lat = 37.45, zoom = 4)
  })
  
  
  
  # Filter the business, returning a data frame
  zipsIFiltered<- reactive({
    reviews <- input$reviews
    stars_input <- input$stars
    
    # Apply filters
    m <- subset(zipsInBounds(),
                review_count >= reviews &
                  stars >= stars_input)
    
    # filter on stars
    lower<-as.numeric(input$stars[1])
    wo <- sbizdata$stars >= lower
    sbizdata<-sbizdata[wo,]
    
    
    upper<-as.numeric(input$stars[2])
    wo <- sbizdata$stars <= upper
    sbizdata<-sbizdata[wo,]
   
    
    
    
  })
  
  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
    if (is.null(input$map_bounds))
      return(business_data[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)
    
    subset(business_data,
           latitude >= latRng[1] & latitude <= latRng[2] &
             longitude >= lngRng[1] & longitude <= lngRng[2])
  })
  
  

  
  
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showZipcodePopup(event$id, event$lat, event$lng)
    })
  })
  
  # Show a popup at the given location
  showZipcodePopup <- function(zipcode, lat, lng) {
    selectedZip <- business_data[business_data$postal_code == zipcode,]
    content <- as.character(tagList(
      tags$h4(as.character(selectedZip$name)),
      tags$h5("Categories:", as.character(selectedZip$categories)),
      sprintf("Number of reviews: %s", as.integer(selectedZip$review_count)), tags$br(),
      sprintf("Stars score: %s", as.integer(selectedZip$stars))
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zipcode)
  }
  
  # Calculate the color palette
  colorPalette <- function(colorData, colorBy) {
    if(colorBy=="stars" || colorBy=="review_count"){
      pal <- colorBin("Spectral", colorData, 4, pretty=TRUE,alpha = FALSE)
    }else{
      pal <- colorBin("Spectral", colorData, 4, pretty=TRUE,alpha = FALSE)
    }
    return (pal)
  }
  
  ## Data Explorer ###########################################
  
  observe({
    cities <- if (is.null(input$states)) character(0) else {
      filter(cleantable, State %in% input$states) %>%
        `$`('City') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$cities[input$cities %in% cities])
    updateSelectInput(session, "cities", choices = cities,
                      selected = stillSelected)
  })
  
  
  
  observe({
    zipcodes <- if (is.null(input$states)) character(0) else {
      cleantable %>%
        filter(State %in% input$states,
               is.null(input$cities) | City %in% input$cities) %>%
        `$`('Zipcode') %>%
        unique() %>%
        sort()
    }
    stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
    updateSelectInput(session, "zipcodes", choices = zipcodes,
                      selected = stillSelected)
  })
  
  observe({
    if (is.null(input$goto))
      return()
    isolate({
      map <- leafletProxy("map")
      map %>% clearPopups()
      dist <- 0.5
      zip <- input$goto$zip
      lat <- input$goto$lat
      lng <- input$goto$lng
      showZipcodePopup(zip, lat, lng)
      map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    })
  })
  
  output$ziptable <- DT::renderDataTable({
    df <- cleantable %>%
      filter(
        Stars >= 0,
        Stars <= 5,
        is.null(input$states) | State %in% input$states,
        is.null(input$cities) | City %in% input$cities,
        is.null(input$zipcodes) | Zipcode %in% input$zipcodes
       
      ) %>%
      mutate(Action = paste('<a class="go-map" href="" data-lat="', Lat, '" data-long="', Long, '" data-zip="', Zipcode, '"><i class="fa fa-crosshairs"></i></a>', sep=""))
    action <- DT::dataTableAjax(session, df)
    
    DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
  })
})
