#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
install.packages("shinythemes")
library(shinythemes)

library(shiny)
library(leaflet)

# Define UI for application that draws a histogram

shinyUI(fluidPage(
    
    theme = shinytheme("simplex"),
    # Application title
    titlePanel("Airbnb"),
    
    
    
    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(condition="input.tabselected==1",
                             h2("Introduction"),
                             h5("This Shiny App allows the user to visualize several Airbnb datas, from different cities. Two types of analysis are proposed: a comparison of the features of 2 or more cities, or a deeper analysis of one particular city. The user can select the cities to analyse and the features to display."),
                             helpText("The data are provided by data.insideairbnb.com. This app was created by Angela, Ludovic and Omer for academic purposes.")
            ),
            
            conditionalPanel(condition="input.tabselected==2",
                             h2("Compare two or more cities"),
                             
                             checkboxGroupInput("city", 
                                                label = "Choose at least 2 cities", 
                                                choices = c("san-diego","berlin","amsterdam")),
                             
                             selectInput("feature",
                                         label = "Choose a feature to look at", 
                                         choices = c("revenue_30", "availability_30", "price_30")),
                             
                             selectInput("plot_type", 
                                         label ="Plot type", 
                                         choices = c("Histogram", "Density", "Boxplot","Average")),
                             
                             
                             selectInput("feature2", 
                                         label = "Add another feature to look at", 
                                         choices = c("None","room_type", "bedrooms")),
                            
                             
                             
                             helpText("The data are provided by data.insideairbnb.com. This app was created by Angela, Ludovic and Omer for academic purposes.")
                             
            ),
            
            conditionalPanel(condition="input.tabselected==3",
                             
                             h2("Deep dive into one city"),
                             
                             selectInput("city_deep", 
                                         label = "Choose one city", 
                                         choices = c("san-diego","berlin","amsterdam")),
                             
                             selectInput("feature_deep", 
                                         label = "Choose a feature to look at", 
                                         choices = c("revenue_30", "availability_30", "price_30")),
                             
                             selectInput("plot_type_deep", 
                                         label ="Plot type", 
                                         choices = c("Histogram", "Density", "Boxplot","Average")),
                             
                             
                             selectInput("feature2_deep", 
                                         label = "Add another feature to look at", 
                                         choices = c("None","room_type", "bedrooms")),
                             
                             selectInput("map_feature", 
                                         label ="Select feature to see on map", 
                                         choices = c("price","bedrooms","minimum_nights", "maximum_nights","property_type", "availability_30" , "price_30", "revenue_30","room_type")),
                             
                             
                             uiOutput('date_begin'),
                             uiOutput('date_end'),
                             textOutput("text"),
                             
                             helpText("The data are provided by data.insideairbnb.com. This app was created by Angela, Ludovic and Omer for academic purposes.")
            )
            
        ),
        
        mainPanel(
            tabsetPanel(type="tab",
                        tabPanel("Introduction", 
                                 value=1, h2("Introduction"), 
                                 leafletOutput("map_intro")),
                        tabPanel("Compare two or more cities", 
                                 value=2, tableOutput("average"), 
                                 plotOutput("listings") ),
                        tabPanel("Deep dive into one city",
                                 value=3,tableOutput("average_deep"), 
                                 plotOutput("listings_deep"),
                                 leafletOutput("map_deep")
                        ),
                        id = "tabselected"
            )
        )
    )
))

