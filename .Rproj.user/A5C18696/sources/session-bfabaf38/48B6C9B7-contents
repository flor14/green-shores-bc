#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(leaflet)
library(leaflet.extras)
library(sf)
library(DT)

sf::st_layers("data/bc.gpkg")
bc <- st_read("data/bc.gpkg") |> 
  dplyr::filter(NAME_1 == "British Columbia")

# Links
link_database <- tags$a(
  shiny::icon("leaf"),
  "Green shores BC",
  href = "https://www.gbif.org/occurrence/search?dataset_key=8a863029-f435-446a-821e-275f4f641165",
  target = "_blank"
)
link_appsilon <- tags$a(shiny::icon("bolt"),
                        "Appsilon",
                        href = "https://appsilon.com/",
                        target = "_blank")

ui <- bslib::page_navbar(
   title = tags$span(tags$img(src = 'logo.png', height = 35),
                    ""),
  theme = bslib::bs_theme(
    version = 5,
    navbar_bg = "#658FA2",
    primary = "#658FA2",
    bootswatch = "cosmo",
    base_font = bslib::font_google("Raleway",
                                   wght = "400"),
    heading_font = bslib::font_google("Raleway",
                                      wght = "200")
  ),
bslib::nav_panel(
  title = "Permit",
  bslib::layout_sidebar(
    class = "p-0",
    leafletOutput('map'),
    sidebar = sidebar(
    position = 'right',
    width = 600,
    DTOutput('table'),
    br(),
    textAreaInput('area', 
                  paste("Briefly describe the main goals or desired outcomes",
                        "of your shoreline restoration project:")
),
    downloadButton("report", "Download")
  ))),
bslib::nav_panel(
  title = "About",
  tags$style(type = "text/css", "body {margin-top: 50px; margin-left: 200px}"),
  tags$img(
    src = "logo.png",
    width = '10%',
    height = 'auto',
    style = "float: right; margin-left: 10px;"
  ),
  shiny::br(),
  # div(
  #   includeMarkdown('www/about.md'),
  #   class = "markdown-container",
  #   style = "width: 100%; height: 500px; overflow: auto;" # Scrolling
  # )
),
bslib::nav_spacer(),
bslib::nav_menu(
  title = "Links",
  align = "right",
  bslib::nav_item(link_database),
 # bslib::nav_item(link_appsilon)
)
)

  
# Define server logic required to draw a histogram
server <- function(input, output) {

  

    output$map <- renderLeaflet({
#  print(input$map_shape_click)
      # plot a map, any map... (blank in this case)
      leaflet::leaflet() |> 
        leaflet::addTiles(group = "OpenStreetMap") |>
        # leaflet::addProviderTiles(
        #   "Esri.WorldImagery",
        #   group = "Esri.WorldImagery",
        #   options = leaflet::providerTileOptions(
        #     attribution = paste(
        #       'Tiles',
        #       '&copy; Esri &mdash; Source: Esri, i-cubed, USDA, USGS,',
        #       'AEX, GeoEye, Getmapping, Aerogrid, IGN, IGP, UPR-EGP,',
        #       'and the GIS User Community - Powered by Esri'
        #     )
        #   )
        # ) |>
        # leaflet::addLayersControl(
        #   baseGroups = c("OpenStreetMap",
        #                  "EsriWorldImagery"),
        #   options = leaflet::layersControlOptions(collapsed = FALSE)
        # ) |>  
        leaflet::addPolygons(data = bc,
                             color = "black",
                             fillColor = "blue",
                             weight = 1,
                             stroke = TRUE,
                             fillOpacity = 0.1,
                             smoothFactor = 0.5,
                             popup = bc$NAME_3) |> 
        leaflet::setView(lng = -127.65,
                         lat = 53.73, zoom = 6)
      
    })
    
    # Select HYDROlakeDB polygon when clicking the map
    click <- eventReactive(input$map_shape_click, {
      
      
      # convert hover coordinates in a sfc point
      p  <-
        sf::st_sfc(sf::st_point(
          x = c(
            input$map_shape_click$lng,
            input$map_shape_click$lat
          ),
          dim = "XY"
        ),
        crs = 4326)
      print(p)
      
      # detect detect polygon hovered by the user
      bc[sf::st_intersects(bc,
                           p,
                           sparse = FALSE), ]
      
    }
    )
    
    
    output$table <- DT::renderDT({
      
      shiny::validate(
        shiny::need(!is.null(input$map_shape_click),
                    "Please, click your location in the map"))
      
      
        if (!is.null(input$map_shape_click)) {
          datalk <- t(data.frame(
            c('Province',
              click()$NAME_1),
            c('District',
              click()$NAME_2),
            c('',
              click()$NAME_3),
            c('Area type',
              click()$ENGTYPE_3),
            c('First Nations Territory',
              "?"),
            c('Guiding agency that operates in the area',
              "?"),
            c('Adjacent to a National Wildlife Area',
              "?"),
            c('Presence of species at risk or sensitive habitats',
              "?")
          ))
          
          colnames(datalk) <- NULL
          
          DT::datatable(
            datalk,
            rownames = FALSE,
            options = list(ordering = FALSE,
                           dom = 't'),
            # remove table interactive default options
            colnames = rep("", ncol(datalk))
          ) # remove column names
    }
  })
    
    
    output$report <- downloadHandler(
      # https://community.rstudio.com/t/retain-formatting-on-a-pdf-output-from-shiny-downloadhandler/36410
      filename = "GreenShoresBC_Permit.html",
      content = function(file) {

        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)
        
        params <- list(province = click()$NAME_1,
                       district1 = click()$NAME_2,
                       district2 = click()$NAME_3,
                       areatype = click()$ENGTYPE_3,
                       firstnations = "?",
                       agency = "?",
                       spsrisk = "?",
                       text = input$area)
        
        id <- showNotification(
          "Creating report...",
          duration = 5,
          closeButton = FALSE
        )
        on.exit(removeNotification(id), add = TRUE)
        
        rmarkdown::render(tempReport,
                          output_file = file,
                          #  output_format = word_document(reference_docx = tempTemplate),
                          params = params,
                          envir = new.env(parent = globalenv())
        )})
}

# Run the application 
shinyApp(ui = ui, server = server)
