## app.R ##
library(shinydashboard)
library(png)
library(shinydashboardPlus)
library(shiny)
library(dplyr)
library(tidyverse)
library(magrittr)
library(highcharter)
library(shinyjs)
library(tmap)
library(tmaptools)
library(leaflet)
library(mapdeck)
library(sf)








setwd("C:/Users/iMMAP/Desktop/shinydashboard")



title <- tags$a(href='#',
                 "")





world <- st_read("shapefiles/world1.shp")


m_C <- c("Select All", as.character(sort(unique(world$name_long))))

#m_LGA <- c("Select All", as.character(sort(unique(admin2$ADM2_EN))))

s2_State <- as.character(sort(unique(world$name_long)))

#s2_LGA <- as.character(sort(unique(admin2$ADM2_EN)))



title1 = span(img(src = "https://gdurl.com/4JSH", height = 30, width = 30))


ui <- dashboardPage(skin='black',
                    
                        dashboardHeader(title = title
                                       ),
                    
                    
                    
                        dashboardSidebar(width=275,
                                         
                                         # The dynamically-generated user panel
                                         uiOutput("userpanel"),
                                         
                                         # Side Bar Menu
                                         sidebarMenu(style = "position: Scroll; overflow: visible;",id = "sidebarmenu",
                                                     
                                                     menuItem("Dashboard", tabName = "home", icon = icon("th")),
                                                     
                                                     menuItem("Analytics", tabName = "cso"),
                                                     
                                                   
                                                     
                                                     conditionalPanel(
                                                       "input.sidebarmenu === 'cso'",
                                                       # a. FILTERS
                                                       useShinyjs(),
                                                       div(id = "form",
                                                           tags$hr(),
                                                           selectInput("i2_state", "Country", choices = m_C,bookmarkButton(id = "bookmark1")),
                                                           column(6,offset = 6,height = 100,style='padding100px;',
                                                                  actionButton("reset_button", "Reset",icon = icon("repeat")))
                                                       ))
                                         )
                        ),
                    
                    
                    
                    
                        dashboardBody(
                          
                          # hide red errors
                          
                          tags$style(type="text/css",
                                     ".shiny-output-error { visibility: hidden; }",
                                     ".shiny-output-error:before { visibility: hidden; }"
                          ),
                          
                          tabItems( 
                         
                            tabItem(tabName = "cso",
                                
                                    fluidRow(style="height:50px;",
                                             
                                             valueBoxOutput("count1",width = 3),
                                             valueBoxOutput("count2",width = 3),
                                             valueBoxOutput("count3",width = 3),
                                             valueBoxOutput("count4",width = 3)
                                    ),
                                    fluidRow(column(12, offset = 2.5,leafletOutput('map1', width = 1800, height = 900)))
                                 
                            )
                            
                            
                          
                            
                            
                         
                          )
                        
                        )
)





server <- function(input, output) {
  
  addClass(selector = "body", class = "sidebar-collapse")
  
  # Reset Button
  
  # Need to exclude the buttons from themselves being bookmarked
  setBookmarkExclude(c("bookmark1", "bookmark2"))
  
  # Trigger bookmarking with either button
  observeEvent(input$bookmark1, {
    session$doBookmark()
  })
  observeEvent(input$bookmark2, {
    session$doBookmark()
  })
  
  js_click_line <- JS("function(event) {Shiny.onInputChange('line_clicked', [event.point.category]);}")
  
  observeEvent(input$reset_button, {
    reset("form")
  })
  
  id <- NULL
  
  observeEvent(input$reset_button, {
    
    id <<- showNotification(
      paste("Filters are Reset"),
      duration = 5, 
      type = "message"
    )
  })

  filt_iemap1 <- reactive({
    world %>%
      filter(
        if (input$i2_state == "Select All") {name_long %in% s2_State} else {name_long == input$i2_state}
       
      )
  })
  
  # Value Box 1
  output$count1 <- renderValueBox({
    hc12 <- filt_iemap1()$continent
    valueBox(paste0(hc12), "Continent", icon = icon("globe"),
             color = "blue"
    )
  })
  
  # Value Box 2
  output$count2 <- renderValueBox({
    hc12 <- filt_iemap1()$pop
     
    valueBox(paste0(prettyNum(hc12, big.mark = ",")), "Population", icon = icon("users"),
             color = "blue"
    )
  })
  
  # Value Box 3
  output$count3 <- renderValueBox({
    hc13 <- round( (filt_iemap1()$PERSONS_FU / filt_iemap1()$TOTAL_VACC ) * 100, 0)
      
      
    valueBox(paste0(hc13, "%"), "of persons fully vaccinated", icon = icon("syringe"),
             color = "blue"
    )
  })
  
  # Value Box 4
  output$count4 <- renderValueBox({
    hc14 <- filt_iemap1()$NUMBER_VAC
      
    valueBox(paste0(hc14), "Number of vaccine types used", icon = icon("bars"),
             color = "blue"
    )
  })
  
  

 
  
  output$map1 <- renderLeaflet({
    leaflet(filt_iemap1()) %>%
      addProviderTiles(providers$CartoDB) %>% 
      
      addPolygons( stroke = 2,
                   weight = 1,
                   color = "red"
                     
      )
    
  }) # render Leaflet
  
 
  

  
  
}




shinyApp(ui, server)