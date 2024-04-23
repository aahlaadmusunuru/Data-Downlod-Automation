## app.R ##
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(sp)
library(sf)
library(leaflet)
library(DT)
library(leaflet.extras)
library(dplyr)
library(data.table)
library(rgdal)
library(ggplot2)
library(reshape2)
library(plotly)
library(raster)
library(rAmCharts)
library(proj4)
library(shinyjs)
library(leaflet.minicharts)
library(manipulateWidget)
library(htmltools)
library(mapview)
library(devtools)
library(rapidjsonr)
library(magrittr)
library(spatialEco)
library(rgeos)
library(maptools)
library(effects)    
library(markdown)
library(leafpop)
library(shinythemes)
library(janitor)
library(openxlsx)
library(writexl)
library(terra)
library(highcharter)
library(shinycssloaders)
library(splitstackshape)

# List of color ramps
# Define the color ramps
List_colors<-list("Red", "Orange", "Yellow", "Green", "Blue", "Indigo", "Violet")

# Color ramps
color_ramps <- list(
  "Blues" = c("#F7FBFF", "#DEEBF7", "#C6DBEF", "#9ECAE1", "#6BAED6", "#4292C6", "#2171B5", "#08519C", "#08306B"),
  "Greens" = c("#F7FCF5", "#E5F5E0", "#C7E9C0", "#A1D99B", "#74C476", "#41AB5D", "#238B45", "#006D2C", "#00441B"),
  "Oranges" = c("#FFF5EB", "#FEE6CE", "#FDD0A2", "#FDAE6B", "#FD8D3C", "#F16913", "#D94801", "#A63603", "#7F2704"),
  "Reds" = c("#FFF5F0", "#FEE0D2", "#FCBBA1", "#FC9272", "#FB6A4A", "#EF3B2C", "#CB181D", "#A50F15", "#67000D"),
  "Purples" = c("#FCFBFD", "#EFEDF5", "#DADAEB", "#BCBDDC", "#9E9AC8", "#807DBA", "#6A51A3", "#54278F", "#3F007D"),
  "YlOrBr" = c("#FFFFD4", "#FEE391", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#993404", "#662506", "#401604"),
  "YlGnBu" = c("#FFFFCC", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238B45", "#006D2C", "#00441B", "#003D1C"),
  "YlOrRd" = c("#FFFFB2", "#FED976", "#FEB24C", "#FD8D3C", "#FC4E2A", "#E31A1C", "#BD0026", "#800026", "#800026"),
  "BuPu" = c("#F7FCFD", "#E0ECF4", "#BFD3E6", "#9EBCDA", "#8C96C6", "#8C6BB1", "#88419D", "#810F7C", "#4D004B"),
  "GnBu" = c("#F7FCF0", "#E0F3DB", "#CCEBC5", "#A8DDB5", "#7BCCC4", "#4EB3D3", "#2B8CBE", "#0868AC", "#084081"),
  "PuBu" = c("#F1EEF6", "#D0D1E6", "#A6BDDB", "#74A9CF", "#3690C0", "#0570B0", "#045A8D", "#023858", "#00234B"),
  "PuRd" = c("#F1EEF6", "#E7D4E8", "#D4B9DA", "#C994C7", "#DF65B0", "#E7298A", "#CE1256", "#980043", "#67001F"),
  "RdPu" = c("#FFF7F3", "#FDE0DD", "#FCC5C0", "#FA9FB5", "#F768A1", "#DD3497", "#AE017E", "#7A0177", "#49006A"),
  "YlGn" = c("#FFFFE5", "#F7FCB9", "#D9F0A3", "#ADDD8E", "#78C679", "#41AB5D", "#238443", "#006837", "#004529"),
  "Greys" = c("#FFFFFF", "#F0F0F0", "#D9D9D9", "#BDBDBD", "#969696", "#737373", "#525252", "#252525", "#000000")
)
Stasticies<-list("Min","Mean","Max","SD","Sum")

# Define UI for application that draws a histogram
ui <-  
  
  navbarPage("Disaster Related Stasticies Tool",id = "inTabset",collapsible = TRUE,inverse = TRUE,theme = shinytheme("united"),
             
             ###### Here : insert shinydashboard dependencies ######
             
             tabPanel("Home",
                      includeHTML("index.html")
                      ),
             
             
             header = tagList(
               useShinydashboard()
             ),
             # tabPanel(
             #  "Global Habitat Accounting",
             # value = "panel1",
             # tags$style(type ="text/css", "leaflet {height: calc(100vh - 80px) !important;}"),
             # leafletOutput("Habitat_Accounting"  ,height = '90vh') 
             #  ),
             
             tabPanel(
               "Step-1 Upload Admin Boundaries",    value = "panel1", 
               fluidPage(
                 br(),               
                 sidebarLayout(
                   sidebarPanel(
                     width = 3, # adjust the width here
                     # Upload Political Boundaries 
                     tags$head(
                       tags$style(
                         HTML("
        /* Custom CSS for side navigation bar */
        .sidebar {
          font-family: 'Arial', sans-serif;
          font-size: 16px;
          /* Add any other custom styles for the sidebar here */
        }
.gray-heading {
          color: #555555; /* Darker gray color */
          font-size: 24px;
          font-weight: bold;
          margin: 10px 0;
        }
      ")
                       )
                     ),                     
                     tags$div(class = "sidebar",
                              # Add your side navigation bar content here
                              # This could include menu items, links, icons, etc.
                              # Example:
                              tags$h2(class = "gray-heading", "Upload the total geographical extent.", tags$img(src = "area.png", height = "30px", width = "30px")),
                              
                     ),
                     
                     #  p("Upload the shapefile data, including the shapefile (shp), database file (dbf), spatial index files (sbn, sbx, shx), and projection file (prj), in the WGS-1984 coordinate system."),
                     # Upload Political Boundaries 
                     fileInput(
                       inputId = "filemap",
                       label = "Upload the shapefile data layers.",
                       multiple = TRUE,
                       accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
                     ), 
                     
                     # Political Boundaries Attribute Header Selector
                     uiOutput("Politecal_boundaries"),
                     # Attribute  Selector
                     uiOutput("Politecal_boundaries_Selection"),
                     tags$div(class = "sidebar",
                              # Add your side navigation bar content here
                              # This could include menu items, links, icons, etc.
                              # Example:
                              tags$h2(class = "gray-heading", "Upload the hazard extent layer.", tags$img(src = "flood.png", height = "30px", width = "30px")),
                              
                     ),
                     # Upload Political Boundaries 
                     fileInput(
                       inputId = "Hazard",
                       label = "",
                       multiple = TRUE,
                       accept = c(".shp", ".dbf", ".sbn", ".sbx", ".shx", ".prj")
                     ),
                     # Political Boundaries Attribute Header Selector
                     uiOutput("Hazard_layer"),
                     # Attribute  Selector
                     uiOutput("Select_Natural_Hazard_layer"),
                     selectInput(
                       inputId = "color_Data",
                       label = "Select a color ramp:",
                       choices = List_colors,
                       selected = "Blue"
                     ),
                     fileInput("csvFile6", "Meta Data"),
                     
                     actionButton('jumpToP2', 'Next Step-2')
                   ),
                   
                   # Show a plot of the generated distribution
                   mainPanel(
                     tabBox(
                       title = tags$img(
                         src = "disaster.png",
                         height = "30px",
                         width = "30px"
                       ),
                       width = "100%",
                       tabPanel('Adming Boundaries',
                     
                 
                       div(class = 'main_menu_theme',style="height: 760px; ",
                           
                           
                           
                     
                     
                           leafletOutput("adming", height = '80vh')                
                       )))
                     
                     
                     
                     
                   )
                 )
               )
             ),
             
             
             tabPanel("Step-2 Land Cover Data", value = "panel2", 
                      
                      sidebarLayout(
                        sidebarPanel(
                          width = 3, # # adjust the width here
                          tags$div(class = "sidebar",

                            tags$h2(class = "gray-heading", "Upload LandCover Data-set.", tags$img(src = "land.png", height = "30px", width = "30px") ),
                          ),
                          
                          p("The Land cover raster datasets should be in the WGS 1984 geographic coordinate system."),
                          
                          # opening Extent Raster data sets  
                          
                          fileInput('layer', 'Opening Extent', multiple=FALSE, accept='asc',), 

                          tags$div(class = "sidebar",
                                   tags$h2(class = "gray-heading", "Upload the parameters.",tags$img(src = "adjust.png", height = "30px", width = "30px") ),
                          ),
                          p("The parameters to style the raster datasets should be in CSV file format."),
                          
                          fileInput("csvFile1", "Habitats  Change Class Colors"),
                         
                   tags$h2(class = "gray-heading", "Click on the run button to produce accounts maps and reports.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
                   tags$br(),
                                
                   
                   actionButton("RefreshPlotHabitate",  "  Run-Data", icon("refresh")),
                   tags$br(),
                   
                   tags$br(),
                   
                   
                          actionButton('jumpToP3', 'Next Step-3')
               
                          
                          
                         
                          
                          
                          
                          
                          
                        ),
                        
                       
                          
                        mainPanel(
                          tabBox(
                            title = tags$img(
                              src = "cityscape.png",
                              height = "30px",
                              width = "30px"
                            ),
                            tabPanel('Total Land Cover Area',
                                     
                                     
                                     div(class = 'main_menu_theme',style="height: 760px; ",
                                         
                                   
                                     
                                             title = tags$img(src = "cityscape.png", height = "30px", width = "30px"),
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      tags$div(
                                                        style = "position:relative;",
                                                        leafletOutput("Habituate_Opening_Extent" ,height = '80vh')%>% withSpinner(color="#0dc5c1"),
                                                   
                                                        tags$div(
                                                          style = "position:absolute; top:105px; right:10px;",
                                                          downloadButton("Total_Land_Cover",  "", icon("download"),
                                                                         
                                                                         
                                                                         style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                                        ),
                                          
                                                      ),
                                                      
                                                      
                                                      
                                                      
                                                      tags$style(
                                                        HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                                      )
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
                                                      
                                             )
                                             
                                             
                                           ),
                       
                            
                            tabPanel("Statistics",
                                     br(),
            tags$h2(class = "gray-heading", "Land use or land cover area Statistics.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
            br(),
            
            p("In this section users can see the total land cover area and the percentage Statistics information.
              click on download button to download the statistic information."),
            
            downloadButton("Total_Land_Cover_Data",  "", icon("download"),
                           
                           
                           style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")  ,
                                     dataTableOutput("Habitent_Table"),
                                     
                                     
                            )
                    
                                         
                                     ),
                          
                          tabBox(
                            title = tags$img(
                              src = "earthquake.png",
                              height = "30px",
                              width = "30px"
                            ),
                            tabPanel('Total Land Cover Area',
                                     
                                     
                                     div(class = 'main_menu_theme',style="height: 760px; ",
                                         
                                         
                                         
                                         title = tags$img(src = "cityscape.png", height = "30px", width = "30px"),
                                         
                                         
                                         
                                         
                                         
                                         tags$div(
                                           style = "position:relative;",
                                           leafletOutput("Land_Cover_Area_Affected" ,height = '80vh')%>% withSpinner(color="#0dc5c1"),
                                           
                                           tags$div(
                                             style = "position:absolute; top:105px; right:10px;",
                                             downloadButton("Land_Cover_Affected",  "", icon("download"),
                                                            
                                                            
                                                            style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;")                                           
                                           ),
                                           
                                         ),
                                         
                                         
                                         
                                         
                                         tags$style(
                                           HTML("
      .btn-download:hover {
        color: black;
        background-color: white;
      }
    ")
                                         )
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                         
                                     )
                                     
                                     
                            ),
                            
                            tabPanel("Statistics",
                                     br(),
                                     tags$h2(class = "gray-heading", "Impact of Hazard on the land use or land cover areas Statistics.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
                                     
                                     p("In this section users can see the total land cover area affected and the percentage Statistics information.
              click on download button to download the statistic information."),
                                     
                                     downloadButton("Total_Land_Cover_Data_Affected",  "", icon("download"),
                                                    
                                                    
                                                    style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                                     
                                     dataTableOutput("Habitent_affected_Statistics"),
                                     
                                     
                            )
                            
                          )
                          
                          
                          )
                        )
                      
             
),
tabPanel("Step-3 Socio Economic Indicators", value = "panel3",
         fluidPage(
           br(),               
           sidebarLayout(
             shinyjs::hidden(
               sidebarPanel(
                 width = 3,
                 tags$h2(class = "gray-heading", "Socio Economic Indicators.", `data-toggle` = "your-value", tags$img(src = "diversity.png", height = "30px", width = "30px")),
                 fileInput("layer3", "Upload Socio Economic Indicators (Raster File format)", multiple = TRUE),
                 fileInput("csvFile2", "Parameters of the Indicators should be in  (.csv File format)"),
                 uiOutput("Opening_stock_Condition"),
                 
                 selectInput(
                   inputId = "color_ramp",
                   label = "Select a color ramp:",
                   choices = names(color_ramps),
                   selected = "PuRd"
                 ),
                 
                 selectInput(
                   inputId = "Stasticies_Data",
                   label = "Stasticies Data",
                   choices = Stasticies,
                   selected = "Min",
                   multiple = TRUE
                   
                 ),
                              
                 tags$br(),
                 tags$br(),
                 
                 actionButton("RefreshPlotHabitateOpeningYear", "  Run-Data",  icon("refresh")),
                 tags$br(),
                 tags$br(),
                 
                 actionButton('jumpToP4', ' Next Step-4')
                              
                 
               )),
             mainPanel(
               
               tabBox(
                 title = tags$img(
                   src = "people.png",
                   height = "30px",
                   width = "30px"
                 ),
                 # Title can include an icon
                 tabPanel("Socio Economic Indicators",
                          
                          
                          leafletOutput("Habitent_condition",height = '80vh')%>% withSpinner(color="#0dc5c1"),
                          tags$div(
                            style = "position:absolute; top:180px; right:30px;",
                            downloadButton("OpeningStastics_Condition", "", icon("download"),
                                           
                                           style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                            
                          ),
                          
                          
                 ),
                 tabPanel("Statistics",
                          tags$h2(class = "gray-heading", "Socioeconomics Indicators Statistics.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
                          tags$p("In this section users can see the total socieconomics statistics information. click on download button to download the statistic information."),
                  
                            downloadButton("Population_Statistics_Total", "", icon("download"),
                                           
                                           style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                            
                         
                          dataTableOutput("Population_Statistics")
                          
                 )
                 
                 
               ) ,
               
               tabBox(
                 title = tags$img(
                   src = "people_flood.png",
                   height = "30px",
                   width = "30px"
                 ),
                   tabPanel("Socio Economic Indicators Affected to Hazard",
                                       
                          
                                       leafletOutput("Closing_stock_Habitent_condition", height = '80vh')%>% withSpinner(color="#0dc5c1"),
       
                                       
                            tags$div(
                              style = "position:absolute; top:180px; right:30px;",
                              downloadButton("Opening_affected_Stastics", "", icon("download"),
                                             
                                             style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                              
                            )  
                              ),
                 tabPanel("Statistics",

                          tags$h2(class = "gray-heading", "Socioeconomics Indicators affected to hazard  Statistics.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
                          tags$p("In this section, users can view population and socioeconomics indicators affected information. Click on the download button to access and download the statistical information."),
                          downloadButton("Population_indicators_Affected_Statistics", "", icon("download"),
                                         
                                         style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                          dataTableOutput("Population_Affected_Statistics")
                          
                          
                          )
           
                 
               )
               
             )))),

tabPanel("Step-4 POI Data", value = "panel4",
         fluidPage(
           br(),               
           sidebarLayout(
             
             sidebarPanel(
               fileInput("layer5", "Upload line Spacial data as Shapfile Data", multiple = TRUE),
               # Political Boundaries Attribute Header Selector
               uiOutput("POI_layer"),
               # Attribute  Selector
               uiOutput("Select_Natural_POI_layer"), 
               width = 3, # adjust the width here
             ),
             
               # Upload Political Boundaries 
               mainPanel(
                 leafletOutput("Point_data", height = '80vh')%>% withSpinner(color="#0dc5c1"),
                 
                 
               )
  
      
      
    
  )
  
  )),

tabPanel("Step-5 Infrastructure Data", value = "panel5",
         fluidPage(
           br(),               
           sidebarLayout(
             sidebarPanel(
               fileInput("layer6", "Upload Point of intrest Shapfile Data", multiple = TRUE),
               # Political Boundaries Attribute Header Selector
               uiOutput("TR_layer"),
               # Attribute  Selector
               uiOutput("Select_Natural_TR_layer"), 
               
               tags$br(),
               tags$br(),
               
               actionButton("RefreshInfrastruvture", "  Run-Data",  icon("refresh")),
               width = 3, # adjust the width here
             ),
             
             # Upload Political Boundaries 
             mainPanel(
               tabBox(
                 title = tags$img(
                   src = "people_flood.png",
                   height = "30px",
                   width = "30px"
                 ),
               tabPanel("Total Infrastructure Data",
                        tags$div(
                          style = "position:absolute; top:180px; right:30px;",
                          downloadButton("Transportation_total_data", "", icon("download"),
                                         
                                         style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                          
                        ),
                        
               leafletOutput("Transportation_data", height = '80vh')%>% withSpinner(color="#0dc5c1"),
               ),
               tabPanel("Statistics",
                        tags$h2(class = "gray-heading", "Total Infrastruture Stasticies.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
                        tags$p("In this section users can see the total Infrastructure statistics information. Users can click on the below download button to download the statistic information."),
                        downloadButton("Transportation_total_Stats_data", "", icon("download"),
                                      
                                      style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"), 
                        dataTableOutput("Transportation_data_Statistics")
                      
                        
                        
               )
               ),
               
               tabBox(
                 title = tags$img(
                   src = "people_flood.png",
                   height = "30px",
                   width = "30px"
                 ),
                 tabPanel("Total Infrastructure Data Affected",
                          tags$div(
                            style = "position:absolute; top:180px; right:30px;",
                          downloadButton("Transportation_effected_To_hazrd", "", icon("download"),
                                         
                                         style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                          ),
                          leafletOutput("Transportation_data_Affected", height = '80vh')%>% withSpinner(color="#0dc5c1"),
                 ),
                 tabPanel("Statistics",
                          tags$h2(class = "gray-heading", "Total Infrastruture Stasticies Data Affected.", tags$img(src = "statistics.png", height = "30px", width = "30px") ),
                          tags$p("In this section users can see the total Infrastruture Stasticies Data Affected. Users can click on the below download button to download the statistic information."),
                          downloadButton("Transportation_total_data_stats_Affected", "", icon("download"),
                                         
                                         style = "background-color:black;color:white;
                             height:40px;width:40px;border:none;
                             padding:0;display:flex;justify-content:center;
                             align-items:center;"),
                          dataTableOutput("Transportation_Affected_Statistics")
                          
                          
                 )
               )    
               
             )
             
             #    Statistics 
             
             
           )
           
         ))


)
# Define server logic required to draw a histogram
server <- function(input, output,session) {
  
  
  # upload the shape file or image file data in R shiny  wiht maximum my of data 
  
  options(shiny.maxRequestSize=300000*1024^2) 
  
  
  
  observeEvent(input$jumpToP2, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel2")
  })
  
  observeEvent(input$jumpToP3, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel3")
  })
  
  
  observeEvent(input$jumpToP4, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel4")
  })
  
  
  observeEvent(input$jumpToP5, {
    updateTabsetPanel(session, "inTabset",
                      selected = "panel5")
  })

  # Upload Admin Boundaries  data and the Hazard Area Data Sets.
  
  map <- reactive({
    req(input$filemap)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$filemap
    tempdirname <- dirname(shpdf$datapath[1])
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/" ))
    map
  })
  
  # Extract Admin Boundaries Data by Attribute Header
  Admin_Boundaries <- reactive({
    map() %>%
      subset(select = input$variable_mapAdminshp)
  }) %>% bindCache(map(), input$variable_mapAdminshp)
  
  # Extract Admin Boundaries Data by Attribute Name
  Admin_boundaries_Data <- reactive({
    as.data.frame(Admin_Boundaries()) -> Admin_extent
    split(Admin_extent, Admin_extent)
  }) %>% bindCache(Admin_Boundaries())
  
  
  
  
  # Upload Admin Boundaries 
  map_2 <- reactive({
    req(input$Hazard)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$Hazard
    tempdirname <- dirname(shpdf$datapath[1])
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/" ))
    map
  })
  
  
  
  
  
  # Extract Admin Boundaries Data by Attribute Header
  Hazard_Boundaries <- reactive({
    map_2() %>%
      subset(select = input$Hazard_Admin_layer)
  }) %>% bindCache(map_2(), input$Hazard_Admin_layer)
  
  # Extract Admin Boundaries Data by Attribute Name
  Hazard_Boundaries_Data <- reactive({
    as.data.frame(Hazard_Boundaries()) -> Admin_extent
    split(Admin_extent, Admin_extent)
  }) %>% bindCache(Hazard_Boundaries())
  
  filtered_polygons <- reactive({
    adminBoundaries <- Admin_Boundaries()[Admin_Boundaries()[[1]] %in% input$variable_PoliticalBoundaries_Data, ]
    
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
   # clipped_polygon<-gIntersection(adminBoundaries, Hazard_Boundaries_data)
    x<-Hazard_Boundaries_data@bbox[[1]]
    y<-Hazard_Boundaries_data@bbox[[2]]
    
    leaflet() %>%
      addTiles(group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addPolygons(data = adminBoundaries, fillColor = "#d5b43c", stroke = TRUE, color = "black", weight = 1, fillOpacity = 1, group = "Admin") %>%
      addPolygons(data = Hazard_Boundaries_data, fillColor = input$color_Data, fillOpacity = 0.5, group = "Hazard") %>% setView(lng = x, lat = y, zoom = 12)%>%
      addLayersControl(
        baseGroups = c("Esri", "OSM (default)"),
        overlayGroups = c("Admin","Hazard")
      ) %>%
      addScaleBar() %>%
      addLegend(position = "bottomright", colors = "#d5b43c", labels = 'Habitat', title = "Legend")
    
  })
  

  output$adming <- renderLeaflet({
    filtered_polygons()
  })
  
  
  
  
  metardata<-reactive({
    file2 <- input$csvFile6
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    data.frame(data)
    
  })
  
  
  
  
  
  
  # Step-2 Land Cover Data 
  
  
  Total_Land_Cover_Data <-reactive({
    inFile1 <- input$layer
    if (is.null(inFile1))
      return(NULL)
    Hb_1 <- raster(inFile1$datapath)
    
  })
  
  
  Hazared_Affected<-reactive({
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
    masked <- mask(x = Total_Land_Cover_Data (), mask = Hazard_Boundaries_data)    
    cropped <- crop(x = masked , y = extent(Hazard_Boundaries_data))
    return(cropped)
  })
  
  habitates_data <- eventReactive(input$RefreshPlotHabitate, {
    Hazared_Affected() %>% na.omit() %>% as.data.frame(xy = TRUE) -> habitentes
  })
  
  
  
  habitates_plote<-eventReactive(input$RefreshPlotHabitate,{
    habitates_data()[[1]][[1]]->latitude
    habitates_data()[[2]][[1]]->longitude
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
    
    clipped_polygon<-gIntersection(adminBoundaries, Hazard_Boundaries_data)
    
    map<-leaflet()%>%setView(lng =latitude, lat = longitude, zoom = 14)%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%addRasterImage(Total_Land_Cover_Data (),group="Extent"  
                                                                                                                         
                                                                                                                         ,colors=c(unique(data$Colors)))%>%addRasterImage(Hazared_Affected(),group="Affected"  
                                                                                                                                                                             
                                                                                                                                                                             ,colors=c(unique(data$Colors)))%>% addPolygons(data =adminBoundaries,fill = F, weight = 2, color = "#FFFFCC", group = "Admin" )%>%addPolygons(data = Hazard_Boundaries_data, fillColor = input$color_Data, fillOpacity = 0.5,group = "Hazard") %>%addLayersControl(
                                                                                                                           baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Affected","Hazard", "Admin","Extent"),
                                                                                                                           options = layersControlOptions(collapsed = TRUE))%>% addScaleBar()%>% addLegend("bottomright", colors=unique(data$Colors), labels=unique(data$Classes), title="Legend")
    
    

    
    map%>%hideGroup(c("Hazard", "Admin","Extent"))%>% syncWith("maps")
      })
  
  
  
  
  Land_cover_Total<-eventReactive(input$RefreshPlotHabitate,{
    habitates_data()[[1]][[1]]->latitude
    habitates_data()[[2]][[1]]->longitude
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file=file1$datapath)
    
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
    
    clipped_polygon<-gIntersection(adminBoundaries, Hazard_Boundaries_data)
    
    map<-leaflet()%>%setView(lng =latitude, lat = longitude, zoom = 14)%>%
      addTiles(group = "OSM (default)") %>%addProviderTiles(providers$Esri.WorldImagery,group = "Esri")%>%addRasterImage(Total_Land_Cover_Data (),group="Extent"  
                                                                                                                         
                                                                                                                         ,colors=c(unique(data$Colors)))%>%addPolygons(data =adminBoundaries,fill = F, weight = 2, color = "#FFFFCC", group = "Admin" )%>%addPolygons(data = Hazard_Boundaries_data, fillColor = input$color_Data, fillOpacity = 0.5,group = "Hazard") %>%addLayersControl(
                                                                                                                                                                            baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Hazard", "Admin","Extent"),
                                                                                                                                                                            options = layersControlOptions(collapsed = TRUE))%>% addScaleBar()%>% addLegend("bottomright", colors=unique(data$Colors), labels=unique(data$Classes), title="Legend")
    
    
    
    
    map%>%hideGroup(c("Hazard", "Admin"))%>% syncWith("maps")
    
    
    
    
    
  })
  output$Habituate_Opening_Extent<-renderLeaflet({
    Land_cover_Total()  
  })
  
  adminBoundaries_Total<-reactive({
    adminBoundaries <- Admin_Boundaries()[Admin_Boundaries()[[1]] %in% input$variable_PoliticalBoundaries_Data, ]
    masked <- mask(x = Total_Land_Cover_Data (), mask = adminBoundaries)    
    cropped <- crop(x = masked , y = extent(adminBoundaries))
    return(cropped)
    
  })
  
  
  
  Total_Land_Cover_Area_Dataset_admin <- eventReactive(input$RefreshPlotHabitate, {
    
    na.omit(as.data.frame(adminBoundaries_Total(),xy=TRUE))-> habitentes
  })
  
  

  
  Land_Cover_exposed_To_Hazard_Areas_data_total<-eventReactive(input$RefreshPlotHabitate, {
    # Extract the latitude values
    
    # Extract the latitude values
    min_lat <- adminBoundaries_Total()@extent@ymin
    max_lat <- adminBoundaries_Total()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(adminBoundaries_Total())[2]
    resolution_meters <- resolution_degrees * conversion_factor
    print(resolution_meters)
    # Select the required columns from habitentes data
    Total_Land_Cover_Area_Dataset_admin()%>%dplyr::select(-x,-y)->Total_Land_Cover_Area_Dataset_admin
    
    file1 <- input$csvFile1
    if (is.null(file1)) {
      return()
    }
    data <- read.csv(file = file1$datapath)
    
    # Perform left join on habitat and data
    R1 <- left_join(Total_Land_Cover_Area_Dataset_admin, data, by = c("X0" = "ID"))
    
    # Extract specific column
    habitat <- R1[2]
    
    # Calculate area and percentage
    result <- data.frame(table(habitat))
    result$Freq <- result$Freq * resolution_meters * resolution_meters / 10000
    result$Percentage <- result$Freq / sum(result$Freq) * 100
    names(result) <- c("Classes", "Area ha", "Percentage")
    
    # Round area and percentage values
    Area_ha <- round(result[2], 0)
    Percentage <- round(result[3], 0)
    
    data.frame(result[1], Area_ha, Percentage) -> Total_Land_Cover_Area_Dataset_admin
    

  })
  

  output$Habitent_Table<- renderDataTable({
    DT<-data.frame(Land_Cover_exposed_To_Hazard_Areas_data_total())
    DT
    # amPie(data = DT, show_values = FALSE ,legend = FALSE,backgroundColor = "transparent")
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,  # Specify the height after which scrolling will be enabled
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  

  
  
  
  output$Land_Cover_Area_Affected<-renderLeaflet({
    
    habitates_plote()
    
  })
  
  output$maps <- renderUI({
    combineWidgets("Habituate_Opening_Extent", "Land_Cover_Area_Affected")
  })
  

  
  Opening_Condition_statis_Total<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    as.data.frame(Opening_Year())%>%na.omit()->stats_Opening
    
    data.frame(lapply(stats_Opening, min))->minCondition
    data.frame(lapply(stats_Opening, mean))->mannCondition
    data.frame(lapply(stats_Opening, max))->maxCondition
    data.frame(lapply(stats_Opening, sd))->SDCondition
    data.frame(lapply(stats_Opening, sum))->Sum
    
    rbind(minCondition,mannCondition,maxCondition,SDCondition,Sum)->Result_Closing_Condition
    
    
    row.names(Result_Closing_Condition)<-c("Min","Mean","Max","SD","Sum")
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    names(data) -> dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    as.data.frame(data) -> df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    data
    Re<-round(as.data.frame(Result_Closing_Condition),3)
    
    names(Re)<-data$habitents
    subset(Re, row.names(Re) == c(input$Stasticies_Data))
    
  })
  
  
  output$Population_Statistics<-renderDataTable({
    as.data.frame(Opening_Condition_statis_Total())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,  # Specify the height after which scrolling will be enabled
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))%>% bindEvent(Opening_Condition_statis_Total())
  
  

  
  

  
  
  Land_Cover_exposed_To_Hazard_Areas<-eventReactive(input$RefreshPlotHabitate, {
    # Extract the latitude values
    
    # Extract the latitude values
    min_lat <- Hazared_Affected()@extent@ymin
    max_lat <- Hazared_Affected()@extent@ymax
    average_lat <- (min_lat + max_lat) / 2
    
    # Calculate the conversion factor
    conversion_factor <- 111320.0 * cos(average_lat * pi / 180)
    
    # Calculate the resolution in meters
    resolution_degrees <- res(Hazared_Affected())[2]
    resolution_meters <- resolution_degrees * conversion_factor
    print(resolution_meters)
    # Select the required columns from habitentes data
    habitates_data()%>%dplyr::select(-x,-y)->habitantes
    
    file1 <- input$csvFile1
    if (is.null(file1)) {
      return()
    }
    data <- read.csv(file = file1$datapath)
    
    # Perform left join on habitat and data
    R1 <- left_join(habitantes, data, by = c("X0" = "ID"))
    
    # Extract specific column
    habitat <- R1[2]
    
    # Calculate area and percentage
    result <- data.frame(table(habitat))
    result$Freq <- result$Freq * resolution_meters * resolution_meters / 10000
    result$Percentage <- result$Freq / sum(result$Freq) * 100
    names(result) <- c("Classes", "Area ha", "Percentage")
    
    # Round area and percentage values
    Area_ha <- round(result[2], 0)
    Percentage <- round(result[3], 0)
    
    data.frame(result[1], Area_ha, Percentage) -> Habitat_Area
  })
  
  output$Habitent_affected_Statistics <- renderDataTable({
    DT<-data.frame(Land_Cover_exposed_To_Hazard_Areas())
    DT
    # amPie(data = DT, show_values = FALSE ,legend = FALSE,backgroundColor = "transparent")
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,  # Specify the height after which scrolling will be enabled
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  
  
  
  
  
  
  output$Total_Land_Cover_Data <- downloadHandler(
    filename = function(){"Natural Hazard Statistics.csv"}, 
    content = function(fname){
      
      
      write.csv(Land_Cover_exposed_To_Hazard_Areas_data_total(), fname)
    }
  )
  
  Total_Land_Cover_Data_Areas<-eventReactive(input$RefreshPlotHabitate,{
    Land_Cover_exposed_To_Hazard_Areas_data_total()%>%dplyr::select(Classes,Area.ha)->result
    names(result)<-c("label","value")
    amBarplot(main = "Total Land Cover Area",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
  })
  
  
  Total_Land_Cover_Data_Percentage<-eventReactive(input$RefreshPlotHabitate,{
    
    Land_Cover_exposed_To_Hazard_Areas_data_total()%>%dplyr::select(Classes,Percentage)->result
    names(result)<-c("label","value")
    amBarplot(main = "Percentage of land Cover Affected.",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
    
  })
  
  output$Total_Land_Cover_Data_Affected <- downloadHandler(
    filename = function(){"Natural Hazard Statistics.csv"}, 
    content = function(fname){
      
      
      write.csv(Land_Cover_exposed_To_Hazard_Areas(), fname)
    }
  )
  
  
  habitates_Percentage<-eventReactive(input$RefreshPlotHabitate,{
    
    Land_Cover_exposed_To_Hazard_Areas()%>%dplyr::select(Classes,Percentage)->result
    names(result)<-c("label","value")
    amBarplot(main = "Percentage of land Cover Affected.",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
    
  })
  
  
  
  habitates_Aea_hectarea<-eventReactive(input$RefreshPlotHabitate,{
    
    Land_Cover_exposed_To_Hazard_Areas()%>%dplyr::select(Classes,Area.ha)->result
    
    
    names(result)<-c("label","value")
    
    
    
    amBarplot(main = "Total Land Cover Area",x = "label", y = "value", data = result, labelRotation = -45, export = TRUE,legend = TRUE, legendPosition = "right")
    
  })
  
  
  
  
  # habituate Condition data Opening_Year  
  Opening_Year<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    inFile3<-input$layer3
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    masked <- mask(x = data1, mask = adminBoundaries)    
    cropped <- crop(x = masked , y = extent(adminBoundaries))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    as.data.frame(data)->reselt_data
    
    names(cropped)<-reselt_data[[1]]
    data1
    return(cropped)
    
  })
  Opening_Year_names<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    inFile3<-input$layer3
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    Admin_Boundaries()[Admin_Boundaries()[[1]]%in% input$variable_PoliticalBoundaries_Data,]->adminBoundaries
    masked <- mask(x = data1, mask = adminBoundaries)    
    cropped <- crop(x = masked , y = extent(adminBoundaries))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    as.data.frame(data)->reselt_data
    
    names(cropped)<-reselt_data[[1]]
    data1
    layer_names <- c(reselt_data[[1]])
    mapview(cropped, layer.name = layer_names)
    
  })

  output$Habitent_condition <- renderLeaflet({
    
    pal <- colorNumeric(input$color_ramp, values(Opening_Year()[[input$Opening_stock]]),
                        na.color = "transparent")
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addScaleBar() %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values(Opening_Year()[[input$Opening_stock]]),
        title = "Legend"
      ) %>%
      addRasterImage(
        Opening_Year()[[input$Opening_stock]], 
        colors = pal, 
        opacity = 0.8, 
        group = "Population & Socio Economic Indicators"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Population & Socio Economic Indicators"),
        options = layersControlOptions(collapsed = TRUE))%>% syncWith("maps_2")
  }) %>% 
    bindEvent(Opening_Year(), input$Opening_stock)
  
  
  
  
  
  
  
  # habituate Condition data Opening_Year  
  Hazard_Affected<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    inFile3<-input$layer3
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
    masked <- mask(x = data1, mask = Hazard_Boundaries_data)    
    cropped <- crop(x = masked , y = extent(Hazard_Boundaries_data))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    as.data.frame(data)->reselt_data
    
    names(cropped)<-reselt_data[[1]]
    data1
    return(cropped)
    
  })
  
  
  
  
  Hazard_Affected_names<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    inFile3<-input$layer3
    if(is.null(inFile3))
      return(NULL)
    data1 <-stack( lapply(inFile3$datapath,raster))
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
    masked <- mask(x = data1, mask = Hazard_Boundaries_data)    
    cropped <- crop(x = masked , y = extent(Hazard_Boundaries_data))
    
    
    file2 <- input$csvFile2
    if (is.null(file2)) { 
      return() 
    } 
    data = read.csv(file=file2$datapath)
    
    as.data.frame(data)->reselt_data
    
    names(cropped)<-reselt_data[[1]]
    data1
    layer_names <- c(reselt_data[[1]])
    mapview(cropped, layer.name = layer_names)
    
  })
  
  
  
  
  
  
  Hazard_Affected_Condition_statis_Total<-eventReactive(input$RefreshPlotHabitateOpeningYear,{
    as.data.frame(Hazard_Affected())%>%na.omit()->stats_Opening
    
    data.frame(lapply(stats_Opening, min))->minCondition
    data.frame(lapply(stats_Opening, mean))->mannCondition
    data.frame(lapply(stats_Opening, max))->maxCondition
    data.frame(lapply(stats_Opening, sd))->SDCondition
    data.frame(lapply(stats_Opening, sum))->Sum
    
    rbind(minCondition,mannCondition,maxCondition,SDCondition,Sum)->Result_Closing_Condition
    
    
    row.names(Result_Closing_Condition)<-c("Min","Mean","Max","SD","Sum")
    
    
    file1 <- input$csvFile1
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    names(data) -> dt
    file1 <- input$csvFile2
    if (is.null(file1)) { 
      return() 
    } 
    data = read.csv(file = file1$datapath)
    as.data.frame(data) -> df
    data.frame(df[1], df[2],df[3]) -> dd
    data$habitents <- paste(dd$Time.Intervel,'/',dd$Condition.Indicator,'/',"Units", '/',df$Measurement.Unit)
    data
    Re<-round(as.data.frame(Result_Closing_Condition),3)
    
    names(Re)<-data$habitents
    subset(Re, row.names(Re) == c(input$Stasticies_Data))
    
  })  
  
  
  output$Population_Affected_Statistics<-renderDataTable({
    as.data.frame(Hazard_Affected_Condition_statis_Total())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,  # Specify the height after which scrolling will be enabled
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))%>% bindEvent(Hazard_Affected_Condition_statis_Total())
  
  
  
  
  
  
  
  
  
  

  output$Closing_stock_Habitent_condition <- renderLeaflet({
    
    pal <- colorNumeric(input$color_ramp, values(Hazard_Affected()[[input$Opening_stock]]),
                        na.color = "transparent")
    
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%
      addScaleBar() %>%
      addLegend(
        "bottomright",
        pal = pal,
        values = values(Hazard_Affected()[[input$Opening_stock]]),
        title = "Legend"
      ) %>%
      addRasterImage(
        Hazard_Affected()[[input$Opening_stock]], 
        colors = pal, 
        opacity = 0.8, 
        group = "Population & Socio Economic Indicators"
      ) %>%
      
      addLayersControl(
        baseGroups = c("Esri","OSM (default)"),overlayGroups = c("Population & Socio Economic Indicators"),
        options = layersControlOptions(collapsed = TRUE))%>% syncWith("maps_2")
  }) %>% 
    bindEvent(Hazard_Affected(), input$Opening_stock)
  # habituate Condition data Closing_Year
  
  output$maps <- renderUI({
    combineWidgets("Habitent_condition", "Closing_stock_Habitent_condition")
  })
  
  
  
  
  
  

  # Upload Admin Boundaries  data and the Hazard Area Data Sets.
  
  Infrastructure <- reactive({
    req(input$layer5)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$layer5
    tempdirname <- dirname(shpdf$datapath[1])
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/" ))
    map
  })
  
  
  # Extract Admin Boundaries Data by Attribute Header
  POI <- reactive({
    Infrastructure() %>%
      subset(select = input$Select_POI_Attribute_Heading)
  }) %>% bindCache(Infrastructure(), input$Select_POI_Attribute_Heading)
  
  # Extract Admin Boundaries Data by Attribute Name
  POI_Data <- reactive({
    as.data.frame(POI()) -> POI_Dt
    split(POI_Dt, POI_Dt)
  }) %>% bindCache(POI())
  
  
  filtered_polygons_DDD <- reactive({
    adminBoundaries <- POI()[POI()[[1]] %in% input$Select_POI_AttributeName, ]
    leaflet()%>%addTiles()%>%addPolygons(data = adminBoundaries)
    
  })
  
  # input$Select_Natural_Hazard_PoliticalBoundaries_Data
  
  output$Point_data<-renderLeaflet({
    filtered_polygons_DDD()
  })
  
  # Upload Admin Boundaries  data and the Hazard Area Data Sets.
  
  Transportationdata <- reactive({
    req(input$layer6)
    
    # shpdf is a data.frame with the name, size, type and
    # datapath of the uploaded files
    shpdf <- input$layer6
    tempdirname <- dirname(shpdf$datapath[1])
    # Rename files
    for (i in 1:nrow(shpdf)) {
      file.rename(
        shpdf$datapath[i],
        paste0(tempdirname, "/", shpdf$name[i])
      )
    }
    map <- readOGR(paste(tempdirname,
                         shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                         sep = "/" ))
    map
  })

  
  # Extract Admin Boundaries Data by Attribute Header
  TR_Infrastructure <- reactive({
    Transportationdata() %>%
      subset(select = input$Select_TR_Attribute_Heading)
  }) %>% bindCache(Transportationdata(), input$Select_TR_Attribute_Heading)
  
  # Extract Admin Boundaries Data by Attribute Name
  TR_Infrastructure_Data <- reactive({
    as.data.frame(TR_Infrastructure()) -> POI_Dt
    split(POI_Dt, POI_Dt)
  }) %>% bindCache(TR_Infrastructure()
                   
                   )
  Clipped_Lines_Data<-eventReactive(input$RefreshInfrastruvture,{
    adminBoundaries <- Admin_Boundaries()[Admin_Boundaries()[[1]] %in% input$variable_PoliticalBoundaries_Data, ]
    
    TR_Infrastructure_Data_sets <- TR_Infrastructure()[TR_Infrastructure()[[1]] %in% c(input$Select_TR_AttributeName), ]
    line_data_sf <- st_as_sf(TR_Infrastructure_Data_sets)
    clip_area_sf <- st_as_sf(adminBoundaries)
    
    TR_Infrastructure_Data_sets_Admin<-st_intersection(line_data_sf ,clip_area_sf)
    clipped_data_sp <- as(TR_Infrastructure_Data_sets_Admin, "Spatial")
    
  })
  
  Total_Line_stats<-eventReactive(input$RefreshInfrastruvture,{
 
    DP<-Clipped_Lines_Data() %>%
      subset(select = input$Select_TR_Attribute_Heading)    
    DP$LengtLine<-lengthLine(DP)
    
    split(DP,DP[[1]])->DP_Dtata
    lapply(DP_Dtata,function(x){x@data})->Data
    melt(Data)->Result
    Result_DT<-Result%>%dplyr::select("L1","value")
    split(Result_DT,Result_DT[[1]])->Result_DT_Dtata
    
    lapply(Result_DT_Dtata,function(x){
     dd<- x%>%group_by(unique(x[1]))%>%summarize(sum(value))
    })->Res_DT_Re
    melt(Res_DT_Re)->Result_Fi
    Result_DaTa<-Result_Fi%>%dplyr::select("L1","value")
    names(Result_DaTa)<-c("Classes","Length.Km")
    Result_DaTa$Length.Km<-round(Result_DaTa$Length.Km/1000,2)
    Result_DaTa
    

    

  })
  
  output$Transportation_data_Statistics<-renderDataTable({
    
    data.frame(Total_Line_stats())
    
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,  # Specify the height after which scrolling will be enabled
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))

  
  filtered_polygons_Total_TR <- reactive({
   
   
    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%addPolylines (data = Clipped_Lines_Data(),weight = 2,opacity = 1,fill = FALSE,fillOpacity = 0.7,smoothFactor = 0.2,color="#06283D",group = "Infrastructure")
      
    
  })
  
  # input$Select_Natural_Hazard_PoliticalBoundaries_Data
  
  output$Transportation_data<-renderLeaflet({
    
    filtered_polygons_Total_TR()
  })
  Clipped_Lines_Data_Affected<-reactive({
    adminBoundaries <- Admin_Boundaries()[Admin_Boundaries()[[1]] %in% input$variable_PoliticalBoundaries_Data, ]
    Hazard_Boundaries_data <- Hazard_Boundaries()[Hazard_Boundaries()[[1]] %in% input$Select_Natural_Hazard_PoliticalBoundaries_Data, ]
    
    clip_area_sf <- st_as_sf(Hazard_Boundaries_data)
    
    
    TR_Infrastructure_Data_sets_affected<-crop(Clipped_Lines_Data() ,clip_area_sf)
    
  })
  
  filtered_polygons_TR_Affected <- eventReactive(input$RefreshInfrastruvture, {

    leaflet() %>% 
      addProviderTiles(providers$OpenStreetMap, group = "OSM (default)") %>%
      addProviderTiles(providers$Esri.WorldImagery, group = "Esri") %>%addPolylines(data = Clipped_Lines_Data_Affected(),weight = 2,opacity = 1,fill = FALSE,fillOpacity = 0.7,smoothFactor = 0.2,color="#06283D",group = "Infrastructure")
    
  })
  

  # input$Select_Natural_Hazard_PoliticalBoundaries_Data
  
  output$Transportation_data_Affected<-renderLeaflet({
    filtered_polygons_TR_Affected()
  })
  
  Total_Line_Affected_Statistics<-eventReactive(input$RefreshInfrastruvture,{
    
    DP<-Clipped_Lines_Data_Affected() %>%
      subset(select = input$Select_TR_Attribute_Heading)    
    DP$LengtLine<-lengthLine(DP)
    
    split(DP,DP[[1]])->DP_Dtata
    lapply(DP_Dtata,function(x){x@data})->Data
    melt(Data)->Result
    Result_DT<-Result%>%dplyr::select("L1","value")
    split(Result_DT,Result_DT[[1]])->Result_DT_Dtata
    
    lapply(Result_DT_Dtata,function(x){
      dd<- x%>%group_by(unique(x[1]))%>%summarize(sum(value))
    })->Res_DT_Re
    melt(Res_DT_Re)->Result_Fi
    Result_DaTa<-Result_Fi%>%dplyr::select("L1","value")
    names(Result_DaTa)<-c("Classes","Length.Km")
    Result_DaTa$Length.Km<-round(Result_DaTa$Length.Km/1000,2)
    Result_DaTa
    
  })
  output$Transportation_Affected_Statistics<-renderDataTable({
    
    data.frame(Total_Line_Affected_Statistics())
  },options = list(
    lengthChange = FALSE,  # Disable entries dropdown
    paging = FALSE , # Disable pagination controls
    info = "" , # Remove entry text
    scrollX = TRUE, 
    scrollY = TRUE,  # Specify the height after which scrolling will be enabled
    searching = FALSE,  # Disable search box
    lengthMenu = c(10, 25, 50, 100),  # Set custom entry length options
    pageLength = 10  # Set default number of entries shown
  ))
  output$Total_Land_Cover <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Total_Land_Cover.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Total_Land_Cover.Rmd")
      file.copy("Total_Land_Cover.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(m = Land_cover_Total(), o = Land_Cover_exposed_To_Hazard_Areas_data_total(),  p = habitates_Percentage(), q = habitates_Aea_hectarea(),s = metardata())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  
  
  output$Land_Cover_Affected <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Land_Cover_Affected.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Land_Cover_Affected.Rmd")
      file.copy("Land_Cover_Affected.Rmd", tempReport, overwrite = TRUE)
      
      
      # Set up parameters to pass to Rmd document
      params <- list(m = habitates_plote(), o = Land_Cover_exposed_To_Hazard_Areas(),  p = habitates_Percentage(), q = habitates_Aea_hectarea(),s = metardata())
      
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv())
      )
    }
    
  )
  
  
  output$Population_Statistics_Total <- downloadHandler(
    filename = function(){"Total_Population_And_Socioeconomics_indicatorsStatistics.csv"}, 
    content = function(fname){
      
      
      write.csv(round(as.data.frame(Opening_Condition_statis_Total()),2), fname)
    }
  )
  output$Population_indicators_Affected_Statistics <- downloadHandler(
    filename = function(){"Total_Population_And_Socioeconomics_indicatorsStatistics_Affected.csv"}, 
    content = function(fname){
      
      
      write.csv(round(as.data.frame(Hazard_Affected_Condition_statis_Total()),2), fname)
    }
  )
  
  
  
  
  # Condition opening extent report 
  output$OpeningStastics_Condition <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Total_Population_Statistics.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Total_Population_Statistics.Rmd")
      file.copy("Total_Population_Statistics.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(m=Opening_Year_names() ,n=round(as.data.frame(Opening_Condition_statis_Total()),2),s=metardata())
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  
  
  # population affected stasticies 
  
  
  # Condition opening extent report 
  output$Opening_affected_Stastics <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "Total_Population_Affected_Statistics.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "Total_Population_Affected_Statistics.Rmd")
      file.copy("Total_Population_Affected_Statistics.Rmd", tempReport, overwrite = TRUE)
      # Set up parameters to pass to Rmd document
      params <- list(m=Hazard_Affected_names() ,n=round(as.data.frame(Hazard_Affected_Condition_statis_Total()),2),s=metardata())
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                        params = params,
                        envir = new.env(parent = globalenv()))})
  
  
  
  
  
  
  #  Step-1  Conditions 
  # Render Political Boundaries Attribute Header Selector
  output$Politecal_boundaries <- renderUI({
    req(map()) # Ensure that the map is uploaded
    selectInput("variable_mapAdminshp", "Attribute Heading", choices = names(map()))
  })
  # Render Attribute Name Selector
  output$Politecal_boundaries_Selection <- renderUI({
    req(Admin_boundaries_Data()) # Ensure that the data is extracted
    selectInput("variable_PoliticalBoundaries_Data", "Attribute Name", choices = names(Admin_boundaries_Data()), multiple = TRUE)
  })
  
  
  
  
  # Condition opining habitats 
  output$Opening_stock_Condition<-renderUI({
    selectInput("Opening_stock", "",choices=names(Opening_Year()))
  })
  #  Step-1  Hazard_Data 
  # Render Political Boundaries Attribute Header Selector
  output$Hazard_layer <- renderUI({
    req(map_2()) # Ensure that the map is uploaded
    selectInput("Hazard_Admin_layer", "Attribute Heading", choices = names(map_2()))
  })
  # Render Attribute Name Selector
  output$Select_Natural_Hazard_layer <- renderUI({
    req(Hazard_Boundaries_Data()) # Ensure that the data is extracted
    selectInput("Select_Natural_Hazard_PoliticalBoundaries_Data", "Attribute Name", choices = names(Hazard_Boundaries_Data()),multiple = TRUE)
  })
  
  
  
  #  Step-1  Hazard_Data 
  # Render Political Boundaries Attribute Header Selector
  output$POI_layer <- renderUI({
    req(Infrastructure()) # Ensure that the map is uploaded
    selectInput("Select_POI_Attribute_Heading", "Attribute Heading", choices = names(Infrastructure()))
  })
  # Render Attribute Name Selector
  output$Select_Natural_POI_layer <- renderUI({
    req(POI_Data()) # Ensure that the data is extracted
    selectInput("Select_POI_AttributeName", "Attribute Name", choices = names(POI_Data()),multiple = TRUE)
  })
  
  
  
  
  
  #  Step-1  Hazard_Data 
  # Render Political Boundaries Attribute Header Selector
  output$TR_layer <- renderUI({
    req(Transportationdata()) # Ensure that the map is uploaded
    selectInput("Select_TR_Attribute_Heading", "Attribute Heading", choices = names(Transportationdata()))
  })
  # Render Attribute Name Selector
  output$Select_Natural_TR_layer <- renderUI({
    req(TR_Infrastructure_Data()) # Ensure that the data is extracted
    selectInput("Select_TR_AttributeName", "Attribute Name", choices = names(TR_Infrastructure_Data()),multiple = TRUE)
  })
  
  
}
# Run the application 
shinyApp(ui = ui, server = server)