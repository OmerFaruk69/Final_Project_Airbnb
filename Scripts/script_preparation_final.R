library(dplyr)
library(rstudioapi)
library("stringr")    


current_path = rstudioapi::getActiveDocumentContext()$path 
setwd(dirname(current_path ))
print( getwd() )
options(warn = - 1)   

prepare_data <- function(city, data_date ,url)
{
  
  con <- gzcon(url(paste(url,"listings.csv.gz", sep="")))
  con2 <- gzcon(url(paste(url,"calendar.csv.gz", sep="")))
  txt <- readLines(con)
  txt2  <- readLines(con2)
  print(paste0("reading data from ", url , "listings.csv.gz"))
  listings <- read.csv(textConnection(txt))
  print(paste0("reading data from ", url ,"calendar.csv.gz"))
  calendar <- read.csv(textConnection(txt2))
  # Cleaning listings dataframe
  
  ## Add Keys: columns city and day date
  listings$city <- city
  listings$data_date <- data_date
  
  ## Select interesting columns
  ### most columns don't contain interesting information
  columns_listings <- c("city", "data_date", "id", "neighbourhood_cleansed", 
                        "latitude", "longitude", 
                        "property_type", "room_type", "accommodates", "bedrooms", 
                        "beds", "price", "minimum_nights",  "maximum_nights")
  
  listings$id <- as.integer(listings$id)
  
  listings <- listings %>% 
    select(columns_listings) %>% 
    filter(listings!="" & listings$id!=0) %>% 
    arrange(id)
  
  
  # Cleaning calendar dataframe
  
  ## Arrange by id and date
  calendar <- calendar %>% 
    arrange(listing_id, date)
  
  ## Add day number (starting first day)
  calendar <- calendar %>%
    group_by(listing_id) %>%
    mutate(day_nb = row_number()) %>%
    ungroup()
  
  ## Change available column to binary
  calendar <- calendar %>%
    mutate(available = ifelse(available=="t", 1, 0))
  
  ## Clean price column and transform to numeric
  calendar <- calendar %>%
    mutate(price = str_replace(price, "\\$", ""),
           adjusted_price = str_replace(adjusted_price, "\\$", ""))
  calendar <- calendar %>%
    mutate(price = str_replace(price, ",", ""),
           adjusted_price = str_replace(adjusted_price, ",", ""))
  calendar <- calendar %>%
    mutate(price = as.numeric(price),
           adjusted_price = as.numeric(adjusted_price))
  
  ## Calculate estimated revenue for upcoming day
  calendar <- calendar %>%
    mutate(revenue = price*(1-available))
  
  ## Calculate availability, price, revenue for next 30, 60 days ... for each listing_id
  calendar <- calendar %>%
    group_by(listing_id) %>%
    summarise(availability_30 = sum(available[day_nb<=30], na.rm = TRUE),
              price_30 = mean(price[day_nb<=30 & available==0], na.rm = TRUE),
              revenue_30 = sum(revenue[day_nb<=30], na.rm = TRUE)        
    )
  
  calendar_cleansed <- calendar 
  listings_cleansed <- listings %>% left_join(calendar, by = c("id" = "listing_id"))
  
  dir.create(file.path("data_cleansed", city, data_date), recursive = TRUE)
  
  write.csv(listings_cleansed, file.path("data_cleansed", city, data_date, "listings.csv"))
  print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "listings.csv")))
  
  write.csv(calendar_cleansed, file.path("data_cleansed", city, data_date, "calendar.csv"))
  print(paste0("saving data into ", file.path("data_cleansed", city, data_date, "calendar.csv")))
}



#PARAMETER
cities <- c("berlin", "amsterdam", "san-diego")
data_dates <- matrix (c("2020-05-14", "2020-06-13", "2020-08-30", "2020-07-09", "2020-08-18", "2020-09-09" ,"2020-07-27", "2020-06-17", "2020-05-21"), 3, 3, byrow=TRUE)
url_list <- matrix (c("http://data.insideairbnb.com/germany/be/berlin/2020-05-14/data/",
                      "http://data.insideairbnb.com/germany/be/berlin/2020-06-13/data/",
                      "http://data.insideairbnb.com/germany/be/berlin/2020-08-30/data/" ,
                      "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-07-09/data/",
                      "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-08-18/data/",
                      "http://data.insideairbnb.com/the-netherlands/north-holland/amsterdam/2020-07-09/data/",
                      "http://data.insideairbnb.com/united-states/ca/san-diego/2020-07-27/data/",
                      "http://data.insideairbnb.com/united-states/ca/san-diego/2020-06-17/data/",
                      "http://data.insideairbnb.com/united-states/ca/san-diego/2020-05-21/data/"), 3, 3, byrow=TRUE)


for(i in 1:length(cities)){
  city <- cities[i]
  for(k in 1:3){
    data_date <- data_dates[i,k]
    url<- url_list[i,k]
    print("-------------------------------------------------")
    print(paste(c("Preparing data for", city, "compiled at", data_date), collapse = " "))
    prepare_data(city, data_date ,url)
  }
}




files_paths <- c()

# Read data in cities between min_date and max_date
for(city in cities){
  file_dir <- file.path(".", "data_cleansed", city)
  file_subdirs <- list.dirs(file_dir)
  file_subdirs <- file_subdirs[-1]
  files_paths <- c(files_paths, file_subdirs)
}

# Preparing listings
files_paths_l <- file.path(files_paths, "listings.csv")
listings <- 
  do.call(rbind,
          lapply(files_paths_l, read.csv, row.names=1))

## Preprocess
listings$bedrooms <- ifelse(as.numeric(listings$bedrooms) >= 5, "5+", listings$bedrooms)

lapply(files_paths_l, read.csv, row.names=1)


# final cleaning

listings <- listings %>%
  filter(!(is.na(listings$neighbourhood_cleansed)) & !(is.na(listings$latitude)) & !(is.na(listings$longitude)) 
         & !(is.na(listings$property_type)) & !(is.na(listings$room_type)) & !(is.na(listings$accommodates)) 
         & !(is.na(listings$bedrooms)) & !(is.na(listings$beds)) 
         & !(is.na(listings$price)) & !(is.na(listings$minimum_nights))
         & !(is.na(listings$maximum_nights)) & !(is.na(listings$availability_30))
         & !(is.na(listings$price_30)) & !(is.na(listings$revenue_30)))
listings <- listings %>%
  filter(listings$neighbourhood_cleansed!="" & listings$latitude!="" & listings$longitude!="" 
         &  listings$property_type!="" & listings$room_type!="" & listings$accommodates!="" 
         &  listings$bedrooms!="" & listings$beds!="" 
         &  listings$price!="" & listings$minimum_nights!=""
         &  listings$maximum_nights!="" & listings$availability_30!=""
         &  listings$price_30!="" & listings$revenue_30!="")


listings <- subset(listings, listings$id!=22642664 & listings$id!=43543658 & listings$id!=43976784 )