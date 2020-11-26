#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(leaflet)
library(stringr)
library(ggplot2)
library(data.table)




# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        ######################################## Introduction ########################################
        
        output$mymap <- renderLeaflet({
                df <- data.frame(lat = listings$latitude,
                                 lng = listings$longitude)
                df %>%
                        leaflet() %>%
                        addTiles() %>%
                        addMarkers(clusterOptions = markerClusterOptions(),
                                   popup = as.character(listings$price))
        })
        
        output$map_intro <- renderLeaflet({
                m <- leaflet() %>%
                        addTiles() %>%
                        addMarkers(lng = -117.1918 ,
                                   lat = 32.7516,
                                   popup = "San-Diego") %>%
                        addMarkers(lng = 4.8896900,
                                   lat = 52.3740300,
                                   popup = "Amsterdam") %>%
                        addMarkers(lng = 13.4105300,
                                   lat = 52.5243700,
                                   popup = "Berlin")
        })
        
        ######################################## Introduction ########################################
        
        
        
        ####################################### Tab2 Compare City ###################################
        
      
        
        
        
        
        output$listings <- renderPlot({
                if (is.null(input$city))
                {
                        print("Choose City to begin")
                        
                }
                
                else{
                        if (input$plot_type == "Boxplot")
                                
                        {
                                if (input$feature == "availability_30")
                                {
                                        if (input$feature2 == "None")
                                        {
                                                listings %>%
                                                        filter(city == input$city) %>%
                                                        ggplot(aes_string(city, input$feature)) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        labs(title = "Distribution of estimated availability for the next 30 days of listings
       (per city)", y = "availability")
                                        }
                                        else{
                                                listings <- listings %>%
                                                        filter(city == input$city)
                                                
                                                listings %>%
                                                        ggplot(aes(city, availability_30)) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        ylim(0, 30) +
                                                        facet_grid(. ~ get(input$feature2)) +
                                                        theme(axis.text.x = element_blank()) +
                                                        labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                             y = "availability_30")
                                        }
                                        
                                }
                                
                                else if (input$feature == "price_30")
                                {
                                        if (input$feature2 == "None")
                                        {
                                                listings %>%
                                                       
                                                        filter(city == input$city) %>%
                                                        ggplot(aes_string(city, input$feature)) + ylim(0, 300) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        labs(title = "Distribution of estimated availability for the next 30 days of listings
       (per city)", y = "availability")
                                        }
                                        
                                        
                                        else if (input$feature2 == "room_type")
                                        {
                                                listings <- listings %>%
                                                        filter(city == input$city)
                                                listings %>%
                                                        ggplot(aes_string(city, input$feature)) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        ylim(0, 300) +
                                                        facet_grid(. ~ get(input$feature2)) +
                                                        theme(axis.text.x = element_blank()) +
                                                        labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                             y = input$feature)
                                                
                                                
                                        }
                                        
                                        
                                        else if (input$feature2 == "bedrooms")
                                        {
                                                listings <- listings %>%
                                                        filter(city == input$city)
                                                listings %>%
                                                        ggplot(aes_string(city, input$feature)) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        ylim(0, 900) +
                                                        facet_grid(. ~ get(input$feature2)) +
                                                        theme(axis.text.x = element_blank()) +
                                                        labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                             y = input$feature)
                                                
                                        }
                                        
                                }
                                
                                else if (input$feature == "revenue_30")
                                {
                                        if (input$feature2 == "None")
                                        {
                                                listings %>%
                                                        
                                                        filter(city == input$city) %>%
                                                        ggplot(aes_string(city, input$feature)) + ylim(0, 500) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        labs(title = "Distribution of estimated availability for the next 30 days of listings
       (per city)", y = "availability")
                                        }
                                        
                                        
                                        else if (input$feature2 == "room_type")
                                        {
                                                listings <- listings %>%
                                                        
                                                        filter(city == input$city)
                                                listings %>%
                                                        ggplot(aes_string(city, input$feature)) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        ylim(0, 4500) +
                                                        facet_grid(. ~ get(input$feature2)) +
                                                        theme(axis.text.x = element_blank()) +
                                                        labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                             y = input$feature)
                                                
                                        }
                                        else if (input$feature2 == "bedrooms")
                                        {
                                                listings <- listings %>%
                                                        
                                                        filter(city == input$city)
                                                listings %>%
                                                        ggplot(aes_string(city, input$feature)) +
                                                        geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                        ylim(0, 5500) +
                                                        facet_grid(. ~ get(input$feature2)) +
                                                        theme(axis.text.x = element_blank()) +
                                                        labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                             y = input$feature)
                                                
                                        }
                                        
                                }
                                
                                
                                
                                
                        }
                        
                        else if (input$plot_type == "Average") {
                                output$average <- renderTable({
                                        if (input$plot_type == "Average") {
                                                if (input$feature == "availability_30")
                                                {
                                                        listings %>%
                                                               
                                                                filter(city == input$city) %>%
                                                                group_by(city) %>%
                                                                summarise(availibility_30_mean = mean(availability_30))
                                                }
                                                
                                                else if (input$feature == "revenue_30")
                                                {
                                                        listings %>%
                                                               
                                                                filter(city == input$city) %>%
                                                                group_by(city) %>%
                                                                summarise(revenue_30_mean = mean(revenue_30))
                                                }
                                                else if (input$feature == "price_30")
                                                {
                                                        listings %>%
                                                                
                                                                filter(city == input$city) %>%
                                                                group_by(city) %>%
                                                                summarise(price_30_mean = mean(price_30))
                                                }
                                                
                                        }
                                        
                                })
                                
                                
                        }
                        else if (input$plot_type == "Density") {
                                if (input$feature == "availability_30")
                                {
                                        listings %>%
                                               
                                                filter(city == input$city) %>%
                                                ggplot(aes(x = availability_30, color = city)) + xlim(0, 30) +
                                                geom_density()
                                }
                                
                                else if (input$feature == "price_30")
                                {
                                        listings %>%
                                                
                                                filter(city == input$city) %>%
                                                ggplot(aes(x = price_30, color = city)) + xlim(0, 500) +
                                                geom_density()
                                }
                                else if (input$feature == "revenue_30")
                                {
                                        listings %>%
                                                
                                                filter(city == input$city) %>%
                                                ggplot(aes(x = revenue_30, color = city)) + xlim(0, 500) +
                                                geom_density()
                                }
                                
                        }
                        
                        
                        else if (input$plot_type == "Histogram") {
                                if (input$feature == "availability_30")
                                {
                                        listings %>%
                                                
                                                filter(city == input$city) %>%
                                                ggplot(aes(x = availability_30, color = city)) + geom_histogram(binwidth =
                                                                                                                        5) + scale_color_brewer(palette = "Accent")
                                }
                                
                                else if (input$feature == "price_30")
                                {
                                        listings %>%
                                                
                                                filter(city == input$city) %>%
                                                ggplot(aes(x = price_30, color = city)) + xlim(0, 500) + geom_histogram() +
                                                scale_color_brewer(palette = "Accent")
                                }
                                else if (input$feature == "revenue_30")
                                {
                                        listings %>%
                                               
                                                filter(city == input$city) %>%
                                                ggplot(aes(x = revenue_30, color = city)) + xlim(0, 5000) + geom_histogram() +
                                                scale_color_brewer(palette = "Accent")
                                }
                                
                        }
                }
                
        })
        
        
        
        
        ######################################## Tab2 Compare City #################################################
        
        
        
        ######################################## Tab3 Deep dive into one city #######################################
        output$listings_deep <- renderPlot({
                if (input$plot_type_deep == "Boxplot")
                        
                {
                        listings <- listings %>%
                                filter(data_date >= input$date_begin_input &
                                               data_date <= input$date_end_input) 
                        if (input$feature_deep == "availability_30")
                        {
                                if (input$feature2_deep == "None")
                                {
                                        listings %>%
                                                filter(city == input$city_deep) %>%
                                                ggplot(aes_string(city, input$feature_deep)) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                labs(title = "Distribution of estimated availability for the next 30 days of listings
       (per city)", y = "availability")
                                }
                                else{
                                        listings <- listings %>%
                                                filter(city == input$city_deep)
                                        
                                        listings %>%
                                                ggplot(aes(city, availability_30)) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                ylim(0, 30) +
                                                facet_grid(. ~ get(input$feature2_deep)) +
                                                theme(axis.text.x = element_blank()) +
                                                labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                     y = "availability_30")
                                }
                                
                        }
                        
                        else if (input$feature_deep == "price_30")
                        {
                                if (input$feature2_deep == "None")
                                {
                                        listings %>%
                                                filter(city == input$city_deep) %>%
                                                ggplot(aes_string(city, input$feature_deep)) + ylim(0, 300) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                labs(title = "Distribution of estimated availability for the next 30 days of listings
       (per city)", y = "availability")
                                }
                                
                                
                                else if (input$feature2_deep == "room_type")
                                {
                                        listings <- listings %>%
                                                filter(city == input$city_deep)
                                        listings %>%
                                                ggplot(aes_string(city, input$feature_deep)) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                ylim(0, 300) +
                                                facet_grid(. ~ get(input$feature2_deep)) +
                                                theme(axis.text.x = element_blank()) +
                                                labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                     y = input$feature_deep)
                                        
                                        
                                }
                                
                                
                                else if (input$feature2_deep == "bedrooms")
                                {
                                        listings <- listings %>%
                                                filter(city == input$city_deep)
                                        listings %>%
                                                ggplot(aes_string(city, input$feature_deep)) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                ylim(0, 900) +
                                                facet_grid(. ~ get(input$feature2_deep)) +
                                                theme(axis.text.x = element_blank()) +
                                                labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                     y = input$feature_deep)
                                        
                                }
                                
                        }
                        
                        else if (input$feature_deep == "revenue_30")
                        {
                                if (input$feature2_deep == "None")
                                {
                                        listings %>%
                                                filter(city == input$city_deep) %>%
                                                ggplot(aes_string(city, input$feature_deep)) + ylim(0, 500) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                labs(title = "Distribution of estimated availability for the next 30 days of listings
       (per city)", y = "availability")
                                }
                                
                                
                                else if (input$feature2_deep == "room_type")
                                {
                                        listings <- listings %>%
                                                filter(city == input$city_deep)
                                        listings %>%
                                                ggplot(aes_string(city, input$feature_deep)) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                ylim(0, 4500) +
                                                facet_grid(. ~ get(input$feature2_deep)) +
                                                theme(axis.text.x = element_blank()) +
                                                labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                     y = input$feature)
                                        
                                }
                                else if (input$feature2_deep == "bedrooms")
                                {
                                        listings <- listings %>%
                                                filter(city == input$city_deep)
                                        listings %>%
                                                ggplot(aes_string(city, input$feature_deep)) +
                                                geom_boxplot(aes(fill = city, city), alpha = 0.85) +
                                                ylim(0, 5500) +
                                                facet_grid(. ~ get(input$feature2_deep)) +
                                                theme(axis.text.x = element_blank()) +
                                                labs(title = "Distribution of estimated revenue for the next 30 days of listings
       (per city & per house size)",
                                                     y = input$feature)
                                        
                                }
                                
                        }
                        
                }
                else if (input$plot_type_deep == "Density") {
                        if (input$feature_deep == "availability_30")
                        {
                                listings %>%
                                        filter(data_date >= input$date_begin_input &
                                                       data_date <= input$date_end_input) %>%
                                        filter(city == input$city_deep) %>%
                                        ggplot(aes(x = availability_30, color = city)) + xlim(0, 30) +
                                        geom_density()
                        }
                        
                        else if (input$feature_deep == "price_30")
                        {
                                listings %>%
                                        filter(data_date >= input$date_begin_input &
                                                       data_date <= input$date_end_input) %>%
                                        filter(city == input$city_deep) %>%
                                        ggplot(aes(x = price_30, color = city)) + xlim(0, 500) +
                                        geom_density()
                        }
                        else if (input$feature_deep == "revenue_30")
                        {
                                listings %>%
                                        filter(data_date >= input$date_begin_input &
                                                       data_date <= input$date_end_input) %>%
                                        filter(city == input$city_deep) %>%
                                        ggplot(aes(x = revenue_30, color = city)) + xlim(0, 500) +
                                        geom_density()
                        }
                        
                }
                
                
                else if (input$plot_type_deep == "Histogram") {
                        if (input$feature_deep == "availability_30")
                        {
                                listings %>%
                                        filter(data_date >= input$date_begin_input &
                                                       data_date <= input$date_end_input) %>%
                                        filter(city == input$city_deep) %>%
                                        ggplot(aes(x = availability_30, color = city)) + geom_histogram(binwidth =
                                                                                                                5) + scale_color_brewer(palette = "Accent")
                        }
                        
                        else if (input$feature_deep == "price_30")
                        {
                                listings %>%
                                        filter(data_date >= input$date_begin_input &
                                                       data_date <= input$date_end_input) %>%
                                        filter(city == input$city_deep) %>%
                                        ggplot(aes(x = price_30, color = city)) + xlim(0, 500) + geom_histogram() +
                                        scale_color_brewer(palette = "Accent")
                        }
                        else if (input$feature_deep == "revenue_30")
                        {
                                listings %>%
                                        filter(data_date >= input$date_begin_input &
                                                       data_date <= input$date_end_input) %>%
                                        filter(city == input$city_deep) %>%
                                        ggplot(aes(x = revenue_30, color = city)) + xlim(0, 5000) + geom_histogram() +
                                        scale_color_brewer(palette = "Accent")
                        }
                        
                }
                
                
        })
        
        
        output$map_deep <- renderLeaflet({
                listings <- listings %>%
                        filter(data_date >= input$date_begin_input &
                                       data_date <= input$date_end_input) %>%
                        filter(city == input$city_deep)
                df <- data.frame(lat = as.numeric(listings$latitude),
                                 lng = as.numeric(listings$longitude))
                
                if (input$map_feature == "price")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$price))
                        
                }
                else if (input$map_feature == "bedrooms")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$bedrooms))
                }
                else if (input$map_feature == "minimum_nights")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$minimum_nights))
                }
                else if (input$map_feature == "maximum_nights")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$maximum_nights))
                }
                
                else if (input$map_feature == "property_type")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$property_type))
                }
                
                else if (input$map_feature == "availability_30")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$availability_30))
                }
                
                else if (input$map_feature == "price_30")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$price_30))
                }
                
                else if (input$map_feature == "revenue_30")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$revenue_30))
                }
                else if (input$map_feature == "room_type")
                {
                        df %>%
                                leaflet() %>%
                                addTiles() %>%
                                addMarkers(clusterOptions = markerClusterOptions(),
                                           popup = as.character(listings$room_type))
                }
                
                
        })
        
        output$date_begin = renderUI({
                selectInput(
                        "date_begin_input",
                        label = "Minimum date",
                        choices = listings %>% filter(city == input$city_deep) %>% distinct(data_date)
                )
        })
        
        output$date_end = renderUI({
                selectInput(
                        "date_end_input",
                        label = "Maximum date",
                        choices = listings %>% filter(city == input$city_deep) %>% distinct(data_date)
                )
        })
        
        observeEvent(input$date_end_input, {
                if (input$date_begin_input > input$date_end_input)
                {
                        showNotification("Pay attention to the date range!" , type = "error")
                }
                
        })
        observeEvent(input$date_begin_input, {
                if (input$date_begin_input > input$date_end_input)
                {
                        showNotification("Pay attention to the date range!", type = "error")
                }
                
        })
        
        
        
        ######################################## Tab3 Deep dive into one city ########################################
        
        
})
