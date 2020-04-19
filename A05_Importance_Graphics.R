library(raster)
library(randomForest)
library(rgdal)
library(tidyverse)
library(cowplot)

set.seed(321)


## load datasets & train classifiers
setwd("F:/OneDrive/Studium/M6/new/settlements")
dir()

input <- shapefile("01_1975_final_1.shp")
tab <- input@data

wrangeled <- as_tibble(tab) %>% 
  transmute(Area = as.numeric(Area), Elevation_mean = Elevmean, 
            Elevation_std = Elevstdev, Elevation_range = Elevrange,
            Distance_Mosque = Distance_M, Distance_Synagogue = Distance_S,
            State = as.factor(state)) %>% 
  rowid_to_column(var = "Row_ID")

training <- filter(wrangeled, !is.na(State))

test <- wrangeled %>% 
  filter(is.na(State)) %>% 
  select(-State) %>% 
  drop_na()

rf_1975 <- randomForest(State~.-Row_ID, data = na.omit(training),
                           ntree = 500, importance = T)




input <- shapefile("02_1990_final_1.shp")
tab <- input@data

wrangeled <- as_tibble(tab) %>% 
  transmute(Area = as.numeric(Area), Elevation_mean = Elevmean, 
            Elevation_std = Elevstdev, Elevation_range = Elevrange,
            Distance_Mosque = Distance_M, Distance_Synagogue = Distance_S,
            State = as.factor(state)) %>% 
  rowid_to_column(var = "Row_ID")

training <- filter(wrangeled, !is.na(State))

test <- wrangeled %>% 
  filter(is.na(State)) %>% 
  select(-State) %>% 
  drop_na()

rf_1990 <- randomForest(State~.-Row_ID, data = na.omit(training),
                           ntree = 500, importance = T)




input <- shapefile("03_2000_final_1.shp")
tab <- input@data

wrangeled <- as_tibble(tab) %>% 
  transmute(Area = as.numeric(Area), Elevation_mean = Elevmean, 
            Elevation_std = Elevstdev, Elevation_range = Elevrange,
            Distance_Mosque = Distance_M, Distance_Synagogue = Distance_S,
            State = as.factor(state)) %>% 
  rowid_to_column(var = "Row_ID")

training <- filter(wrangeled, !is.na(State))

test <- wrangeled %>% 
  filter(is.na(State)) %>% 
  select(-State) %>% 
  drop_na()

rf_2000 <- randomForest(State~.-Row_ID, data = na.omit(training),
                           ntree = 500, importance = T)



input <- shapefile("04_2015_final_1.shp")
tab <- input@data

wrangeled <- as_tibble(tab) %>% 
  transmute(Area = as.numeric(Area), Elevation_mean = Elevmean, 
            Elevation_std = Elevstdev, Elevation_range = Elevrange,
            Distance_Mosque = Distance_M, Distance_Synagogue = Distance_S,
            State = as.factor(state)) %>% 
  rowid_to_column(var = "Row_ID")

training <- filter(wrangeled, !is.na(State))

test <- wrangeled %>% 
  filter(is.na(State)) %>% 
  select(-State) %>% 
  drop_na()

rf_2015 <- randomForest(State~.-Row_ID, data = na.omit(training),
                           ntree = 500, importance = T)


## assess variable importances individually
varImpPlot(rf_1975)
varImpPlot(rf_1990)
varImpPlot(rf_2000)
varImpPlot(rf_2015)


## extract variable importances
imp_1975 <- as_tibble(rf_1975$importance)
imp_1990 <- as_tibble(rf_1990$importance)
imp_2000 <- as_tibble(rf_2000$importance)
imp_2015 <- as_tibble(rf_2015$importance)


## create one table for decreases in accuracy and plot
dec_acc <- as_tibble(rbind(imp_1975$MeanDecreaseAccuracy, imp_1990$MeanDecreaseAccuracy,
                  imp_2000$MeanDecreaseAccuracy, imp_2015$MeanDecreaseAccuracy)) %>% 
  rename_all(funs(rownames(rf_1975$importance))) %>% 
  # mutate(Year = as.factor(c(1975, 1990, 2000, 2015))) %>% 
  # gather(Area:Distance_Synagogue, key = Parameter, value = Decrease) %>% 
  summarize_all(funs(mean)) %>% 
  gather(Area:Distance_Synagogue, key = Parameter, value = Decrease) %>% 
  mutate(Parameter = as.factor(Parameter)) %>% 
  arrange(desc(Decrease))
dec_acc

p1 <- ggplot(dec_acc) +
  geom_col(aes(x = reorder(Parameter, Decrease), 
               y = Decrease, fill = Decrease), alpha = 0.8) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Mean Decrease in Accuracy", x = "Parameter")
# ggsave("decrease_accuracy.png", device = "png", width = 11, height = 8, units = "cm", dpi = 450)


## create one table for decreases in gini coefficient and plot
dec_gin <- as_tibble(rbind(imp_1975$MeanDecreaseGini, imp_1990$MeanDecreaseGini,
                           imp_2000$MeanDecreaseGini, imp_2015$MeanDecreaseGini)) %>% 
  rename_all(funs(rownames(rf_1975$importance))) %>% 
  # mutate(Year = as.factor(c(1975, 1990, 2000, 2015))) %>% 
  # gather(Area:Distance_Synagogue, key = Parameter, value = Decrease) %>% 
  summarize_all(funs(mean)) %>% 
  gather(Area:Distance_Synagogue, key = Parameter, value = Decrease) %>% 
  mutate(Parameter = as.factor(Parameter)) %>% 
  arrange(desc(Decrease))
dec_gin

p2 <- ggplot(dec_gin) +
  geom_col(aes(x = reorder(Parameter, Decrease), 
               y = Decrease, fill = Decrease), alpha = 0.8) +
  coord_flip() +
  theme_classic() +
  theme(legend.position = "none") +
  labs(y = "Mean Decrease in Gini Coefficient", x = "Parameter")
# ggsave("decrease_gini.png", device = "png", width = 11, height = 8, units = "cm", dpi = 450)


## combine plots and save
plot_grid(p1, p2, labels = c("a)", "b)"))
ggsave("importances.png", device = "png", width = 20, height = 8, units = "cm", dpi = 450)
