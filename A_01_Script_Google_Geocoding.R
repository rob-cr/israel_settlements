# Geocoding script for large list of addresses. (modified)
# original script by Shane Lynn 10/10/2013, available at https://www.shanelynn.ie/massive-geocoding-with-r-and-google-maps/

library(ggmap)
library(readxl)
#API key (enter personal Google API key here)
register_google("AIzaSyBzfDu3G-ZDKAFflao-FVoXbFeGYJtOaMg")
# set working directory
setwd("...")
###if created by Excel, files CSV files must be stored in "CSV-Trennzeichen-getrennt", not UTF-8 encoding
#infile <- "Israel_cities_data"
#infile <- "Israel_settlements"
#infile <-  "israel_settlements_peacenow"
infile <- "palestine_cities_census_2017" #source: http://www.pcbs.gov.ps/census2017/
  data <- read_excel(paste0('./', infile, '.xlsx'))
#for csv files:
#data <- read.csv(paste0('./', infile, '.csv'))

# get the address list, and append "Middle East" to increase geocoding accuracy 
addresses = as.character(data$address)
addresses = paste0(addresses, ", Middle East")

#define a function that will process googles server responses
getGeoDetails <- function(address){   
  #use the gecode function to query google servers
  geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
  #now extract the bits that we need from the returned list
  answer <- data.frame(lat=NA, long=NA, accuracy=NA, formatted_address=NA, address_type=NA, status=NA)
  answer$status <- geo_reply$status
  #if we are over the query limit - want to pause for an hour
  while(geo_reply$status == "OVER_QUERY_LIMIT"){
    print("OVER QUERY LIMIT - Pausing for 1 hour at:") 
    time <- Sys.time()
    print(as.character(time))
    Sys.sleep(60*60)
    geo_reply = geocode(address, output='all', messaging=TRUE, override_limit=TRUE)
    answer$status <- geo_reply$status
  }
  #return Na's if we didn't get a match:
  if (geo_reply$status != "OK"){
    return(answer)
  }   
  #else, extract what we need from the Google server reply into a dataframe:
  answer$lat <- geo_reply$results[[1]]$geometry$location$lat
  answer$long <- geo_reply$results[[1]]$geometry$location$lng
  if (length(geo_reply$results[[1]]$types) > 0){
    answer$accuracy <- geo_reply$results[[1]]$types[[1]]
  }
  answer$address_type <- paste(geo_reply$results[[1]]$types, collapse=',')
  answer$formatted_address <- geo_reply$results[[1]]$formatted_address
  return(answer)
}
#initialise a dataframe to hold the results
geocoded <- data.frame()
# find out where to start in the address list (if the script was interrupted before):
startindex <- 1
#if a temp file exists - load it up and count the rows!
tempfilename <- paste0(infile, '_temp_geocoded.rds')
if (file.exists(tempfilename)){
  print("Found temp file - resuming from index:")
  geocoded <- readRDS(tempfilename)
  startindex <- nrow(geocoded)
  print(startindex)
}
# Start the geocoding process - address by address. geocode() function takes care of query speed limit.
for (ii in seq(startindex, length(addresses))){
  print(paste("Working on index", ii, "of", length(addresses)))
  #query the google geocoder - this will pause here if we are over the limit.
  result = getGeoDetails(addresses[ii]) 
  print(result$status)     
  result$index <- ii
  #append the answer to the results file.
  geocoded <- rbind(geocoded, result)
  #save temporary results as we are going along
  saveRDS(geocoded, tempfilename)
}
#now we add the latitude and longitude to the main data
data$lat <- geocoded$lat
data$long <- geocoded$long
data$accuracy <- geocoded$accuracy
#data$postal_code <- geocoded$postal_code

#finally write it all to the output files
saveRDS(data, paste0("..", infile ,"_geocoded.rds"))
write.table(data, file=paste0("..", infile ,"_geocoded.csv"), sep=",", row.names=FALSE)

