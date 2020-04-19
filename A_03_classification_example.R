library(raster)
library(rgdal)
library(randomForest)
library(tidyverse)

set.seed(321)
setwd("F:/OneDrive/Studium/M6/new/settlements")
dir()


## load dataset and prepare for classification
input <- shapefile("01_1975_final_1.shp")
tab <- input@data

wrangeled <- as_tibble(tab) %>% 
  transmute(Area = as.numeric(Area), Elevation_mean = Elevmean, 
            Elevation_std = Elevstdev, Elevation_range = Elevrange,
            Distance_Mosque = Distance_M, Distance_Synagogue = Distance_S,
            State = as.factor(state)) %>% 
  rowid_to_column(var = "Row_ID")


## build training and test datasets
training <- filter(wrangeled, !is.na(State))
test <- wrangeled %>% 
  filter(is.na(State)) %>% 
  select(-State) %>% 
  drop_na()


## train and assess random forest classifier, apply to data
rf_trained <- randomForest(State~.-Row_ID, data = na.omit(training),
                           ntree = 500, importance = T)

rf_trained
plot(rf_trained$err.rate[,1], type = "l")
varImpPlot(rf_trained)
rf_predict <- predict(rf_trained, newdata = test)


## combine classified and reference data, assign back to polygons and export
class_result <- bind_cols(test, enframe(rf_predict))
all_class <- bind_rows(training, class_result)

final <- full_join(wrangeled, all_class, by = "Row_ID") %>% 
  transmute(Row_ID = Row_ID, Area = Area.x,
            Elevation_mean = Elevation_mean.x,
            Elevation_std = Elevation_std.x,
            Elevation_range = Elevation_range.x,
            Distance_Mosque = Distance_Mosque.x,
            Distance_Synagogue = Distance_Synagogue.x,
            State = if_else(is.na(State.x), 
                            value, State.x),
            Base = if_else(!is.na(value), 
                           "Classified", "Unknown"),
            State.x = State.x) %>% 
  mutate(Base = as.factor(if_else(!is.na(State.x), "Reference", Base))) %>% 
  select(-State.x)

output <- input
output@data <- final

writeOGR(output, dsn = "01_1975_class.shp", layer = "1975", driver = "ESRI Shapefile")
