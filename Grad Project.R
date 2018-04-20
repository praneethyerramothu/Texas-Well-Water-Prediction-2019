#library(compare)
library(DT)
library(dplyr)
library(ggplot2)
require(leaflet)
require(htmltools)
require(e1071) 
require(eeptools)
require(ggvis)
require(shiny)
pop18<-read.csv("Population-18.csv")
pop18<-pop18[,c(1,2)]
pop19<-read.csv("Population-19.csv")
pop19<-pop19[,c(1,2)]
wells18<-read.csv("wells-18.csv")
#removing wells that are inactive and unusable
wells18<-wells18[!(wells18$status=="Inactive"),]



#considering only few attributes from the wells.csv file
#wells18<-wells18[,c(1,8,9,10)]


#newpop<- lm(y2015 ~ y2016, pop18)
#summary(newpop)
#predict(newpop)
#newpop18<-as.data.frame(county<-intersect(wells18[,1],pop18[,1]))
#inter<-intersect(wells18[,1],pop18[,1])
#newpop18<-wells18[inter,]


to_model1<-inner_join( pop18,wells18, by="County")
to_model2<-inner_join(pop19,to_model1,by="County")
#if(wells18$county==pop18$County)
#{
#wells18<-c(wells18,(wells18$population<-pop18$Population))  
#}
#wells18<-as.data.frame(wells18)
#pop18<-as.data.frame(pop18)
#for(i in 1:nrow(wells18))
#{
  
 # for(j in 1:nrow(pop18))
 # {
 #   if(wells18[i,1]==pop18[j,1])
 #   {
  #    vector[i]<-pop18[j,2]
  #  }
  #}
  #j<-1
#}
#to_model_new<-to_model2[,c(2,3,4)]
#to_model_new<-to_model_new[!((Population>75) )]
to_model2<-to_model2[!to_model2$Population.x>40.00,]

#to_model_new<-to_model_new[!to_model_new$Population.x > 40.00,]
# developing a linear model for the cleaned data
fit_init<-lm(daily_high_water_level ~ Population,data=to_model1)
#drawing a plot for the data

summary(fit_init)
new_predictor<-data.frame(Population<-to_model2$Population.y)
Well_water_usage_2019<-rep(-25,nrow(to_model2))
m<-coef(summary(fit_init)) [1,1]+coef(summary(fit_init))[1,3]
prediction<-(predict(fit_init,new_predictor)+as.data.frame(Well_water_usage_2019))*m
to_model2<-as.data.frame(c(to_model2,prediction))


data<-dplyr::select(wells18,
                    County,
                    state_well_number,
                    aquifer,
                    daily_high_water_level,
                    latitude,
                    longitude,
                    status,
                    period_of_record_inactive)

well_info <- paste("County:",data[['County']], "<br>", "Population:",data[['Population']], "<br> ", 
                          "State well number:",data[['state_well_number']], "<br> ","Aquifer:",data[['aquifer']],"<br>", 
                         "<b> Water Level:" ,data[['daily_high_water_level']],"</b><br>","Latitude:", data[['latitude']],
                         "<br>", "Longitude:",data[['longitude']],"<br>","Status:",data[['status']], sep='')


data$info<-well_info



#creating webapp with shiny package
require('shinythemes')
require('shinydashboard')
# Define UI for app that draws a histogram ----
ui <- dashboardPage( skin = 'red',
                     dashboardHeader(title = "Texas Ground well water level prediction"),
                     dashboardSidebar(
                       sidebarMenu(
                         menuItem("Geographical Map", tabName='map',icon=icon("globe", lib = "glyphicon")),
                         menuItem("Linear Model Graph",tabName = 'TestResults',icon=icon('dashboard')),
                         menuItem("Prediction",tabName = 'Predict',icon = icon('th'))
                       )
                       
                     ),
                     
                     dashboardBody(
                       
                       title ='Water data Prediction',
                       tabItems(
                         
                         #First tab content
                         tabItem(tabName = "map",
                                 fluidRow(
                                   box(height ='200',
                                       
                                       textInput(inputId = "County",
                                                 label = "Enter the name of the county"
                                       ),
                                       textInput(inputId = "daily_high_water_level",
                                                 label="Enter the minimum amount of water level"),
                                       
                                       actionButton('action2','Generate Map')
                                   ),
                                   box( height = '100%',
                                        title="Map",
                                        status = "warning",
                                        leafletOutput('leaflet')
                                   ))
                                 
                                 
                         ),
                         
                         
                         
                         # Second tab content
                         tabItem(tabName = "TestResults",
                                 fluidRow(
                                   box(title="Linear Model for well water data set",status = 'primary',
                                       plotOutput("plot",height = '450'),width = '450')
                                 
                                 )
                         ),
                         
                         # Third tab content
                         tabItem(tabName = "Predict",
                                 #box( height='350',
                                  #    textInput(inputId = "GRE",
                                   #             label = "Enter Your Gre score",
                                    #            value = 200),
                                    #  textInput(inputId = "Toefl",
                                    #            label = "Enter Your Toefl score",
                                    #            value = 120),
                                    #  sliderInput(inputId = "AWA",
                                   #               label = "Enter Your AWA score",
                                  #                min = 1,
                                     #             max = 5,
                                    #              value = 10),
                                    # sliderInput(inputId = "per",
                                  #                label = "Enter Your Under Grad percentage score",
                                   #               min = 1,
                                  #                max = 10,
                                 #                 value = 300),
                                 #     actionButton('action1','predict')
                                # ),
                                # box(
                               #    title = 'Prediction For User Input.',
                                #   status = 'warning',
                                 #  tableOutput('table1')
                                # )
                               ui <- basicPage(
                                 h2("Predicted Values for Well water 2019"),
                                 DT::dataTableOutput("mytable")
                               )
                         )
                         
                          
                         
                       )
                       
                     )
)


server <- function(input, output) {
  
  output$mytable = DT::renderDataTable({
    to_model2
  })
  
  
  
  
  
  
  
 output$plot <- renderPlot({
    
    
   ggplot(to_model1,aes(x=Population,y=daily_high_water_level)) + geom_point()
    
  })
  
  
 
  
  map1<-eventReactive(input$action2,
                      
                      {
                        #for all universities with limited tuition fee
                        if(input$County=="ALL")
                        {
                          data1<- filter(data,
                                         daily_high_water_level>=input$daily_high_water_level  &
                                              is.na(latitude)==FALSE &
                                              is.na(longitude)==FALSE &
                                              latitude>20 & latitude<50 &         ## Location is US 48
                                              longitude>(-130) & longitude<(-60))
                          
                          
                          
                          
                          blingIcon <- makeIcon(
                            iconUrl = "/Users/praneeththomas/Downloads/School Stuff/R/Project1/college-scorecard/image.png",
                            iconWidth = 17, iconHeight = 17,
                          )
                          
                          
                          
                          map <- leaflet(data1) %>% 
                            setView(-99.9, 31.96, zoom = 5) %>%
                            addTiles() %>%
                            addMarkers(~longitude, ~latitude, popup=~info,
                                       options = popupOptions(closeButton = TRUE),
                                       ##clusterOptions = markerClusterOptions(), 
                                       icon = blingIcon)
                          map
                          
                        }
                        
                        #for all universities of different tuition fee
                        else if(input$daily_high_water_level==99999 && input$County=="ALL")
                        {
                          data1<- filter(data,
                                         #daily_high_water_level>=input$daily_high_water_level  &
                                              is.na(latitude)==FALSE &
                                              is.na(longitude)==FALSE &
                                              latitude>20 & latitude<50 &         ## Location is US 48
                                              longitude>(-130) & longitude<(-60))
                          
                          
                          
                          
                          blingIcon <- makeIcon(
                            iconUrl = "/Users/praneeththomas/Downloads/School Stuff/R/Project1/college-scorecard/image.png",
                            iconWidth = 30, iconHeight = 30,
                          )
                          
                          
                          
                          map <- leaflet(data1) %>% 
                            setView(-99.9, 31.96, zoom = 5) %>%
                            addTiles() %>%
                            addMarkers(~longitude, ~latitude, popup=~info,
                                       options = popupOptions(closeButton = TRUE),
                                       ##clusterOptions = markerClusterOptions(), 
                                       icon = blingIcon)
                          map
                          
                        }
                        
                        
                        else
                        {
                          data1<- filter(data,
                                           
                                              County==input$County &
                                           daily_high_water_level>=input$daily_high_water_level  &
                                              is.na(latitude)==FALSE &
                                              is.na(longitude)==FALSE &
                                           latitude>20 & latitude<50 &       
                                           longitude>(-99.9) & longitude<(31.96))
                          
                          
                          
                          
                          blingIcon <- makeIcon(
                            iconUrl = "/Users/praneeththomas/Downloads/School Stuff/R/Project1/college-scorecard/image.png",
                            iconWidth = 30, iconHeight = 30,
                          )
                          
                          
                          
                          map <- leaflet(data1) %>% 
                            setView(-99.9, 31.96, zoom = 5) %>%
                            addTiles() %>%
                            addMarkers(~longitude, ~latitude, popup=~info,
                                       options = popupOptions(closeButton = TRUE),
                                       ##clusterOptions = markerClusterOptions(), 
                                       icon = blingIcon)
                          map
                          
                        }
                        
                      })
  
  
  output$leaflet<- renderLeaflet({
    map1()
    
  })
  
  
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)









