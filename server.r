library(leaflet)
library(RColorBrewer)
library(scales)
library(lattice)
library(dplyr)
library(ggplot2)
library(data.table)
library(DT)

function(input, output, session) {
    
    
    # Leaflet bindings are a bit slow; for now we'll just sample to compensate
    set.seed(100)
    
    
    zipdataInput <- reactive({
  
        hold <- allzips
    
    
    hold
    
    })
    
    
    # By ordering by centile, we ensure that the (comparatively rare) SuperZIPs
    # will be drawn last and thus be easier to see
    zipdataToo <- reactive({
    zipdata <- zipdataInput()
    zipdata[order(zipdata$amount),]
    })
    


  ## Interactive Map ###########################################

  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles(
        urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
        attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>'
      ) %>%
      setView(lng = -93.85, lat = 37.45, zoom = 4)
  })

  # A reactive expression that returns the set of zips that are
  # in bounds right now
  zipsInBounds <- reactive({
      zipdata <- zipdataToo()
    if (is.null(input$map_bounds))
      return(zipdata[FALSE,])
    bounds <- input$map_bounds
    latRng <- range(bounds$north, bounds$south)
    lngRng <- range(bounds$east, bounds$west)

    subset(zipdata,
      latitude >= latRng[1] & latitude <= latRng[2] &
        longitude >= lngRng[1] & longitude <= lngRng[2])
  })

  # Precalculate the breaks we'll need for the two histograms
  centileBreaks <- hist(plot = FALSE, as.numeric(as.vector(allzips$amount)), breaks = 20)$breaks

  output$histCentile <- renderPlot({
    # If no zipcodes are in view, don't plot
    if (nrow(zipsInBounds()) == 0)
      return(NULL)

    hist(zipsInBounds()$amount,
      main = "Donation Amount (visible zips)",
      xlab = "Amount",
      col = '#00DD00',
      border = 'white')
  })


      

  # This observer is responsible for maintaining the circles and legend,
  # according to the variables the user has chosen to map to color and size.
  observe({
     zipdata <- zipdataToo()
    colorBy <- as.numeric(as.vector(allzips$amount))
    sizeBy <- as.numeric(as.vector(allzips$amount))


      pal <- colorBin("Spectral", colorBy, 7, pretty = FALSE)
    

        radius <- sizeBy /25
    

    leafletProxy("map", data = zipdata) %>%
      clearShapes() %>%
      addCircles(~longitude, ~latitude, radius=radius, layerId=~zip,
        stroke=FALSE, fillOpacity=0.4, fillColor=pal(colorBy)) %>%
      addLegend("bottomleft", pal=pal, values=dollar(colorBy*1000), title="Donation Amount",
        layerId="colorLegend")
  })

  # Show a popup at the given location
  showZipcodePopup <- function(zip, lat, lng) {
    selectedZip <- allzips[allzips$zip == zip,]
    content <- as.character(tagList(
      tags$strong("Donation"),tags$br(),
     
      sprintf("Amount %s", dollar(selectedZip$amount * 1000)), tags$br(),
      sprintf("Name %s", selectedZip$name_clean), tags$br(),
      sprintf("Address %s", selectedZip$google_address_normalized), tags$br()

      )
    )
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = zip)
  }

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
  
  ####When zipcode is selected, show popup with city info
  
  observe({
      leafletProxy("map") %>% clearPopups()
      event <- as.numeric(paste(input$yourzipcode))
      zipframe <- subset(allzips, allzips$zip==event)
      
      
      
      if (is.null(event))
      return()
      
      isolate({
          showZipcodePopup(event, zipframe$latitude, zipframe$longitude)
      })
  })



reactiveZip <- reactive({
    
    smalls <- zipsInBounds()
    
    smalls$latitude <- jitter(smalls$latitude)
    smalls$longitude <- jitter(smalls$longitude)
    smalls$zipcode <- smalls$zip
    smalls$money <- dollar(smalls$amount*1000)
    
    
    zoomtable <- smalls %>%
    select(
    Name = name_clean,
    Address = google_address_normalized,
    Zipcode = zipcode,
    Amount = money,
    Lat = latitude,
    Long = longitude
    )
    
    zoomtable
    
})


output$ziptable <- DT::renderDataTable({

 df <- reactiveZip()
 
 action <- DT::dataTableAjax(session, df)


 DT::datatable(df, options = list(ajax = list(url = action)), escape = FALSE)
 
})


}
