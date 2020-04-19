library(raster)
library(rgdal)
library(tidyverse)
library(cowplot)

setwd("F:/OneDrive/Studium/M6/new/Settlements")
dir()


## load classification results
class_1975 <- shapefile("01_1975_class.shp")
tab_1975 <- as_tibble(class_1975@data) %>% 
  mutate(Area_sqm = Area / 1000000, State = if_else(is.na(State), "Unclassified", State)) %>% 
  group_by(State) %>% 
  summarise(Settlement_Area = sum(Area_sqm), Settlement_Count = n())

class_1990 <- shapefile("02_1990_class.shp")
tab_1990 <- as_tibble(class_1990@data) %>% 
  mutate(Area_sqm = Area / 1000000, State = if_else(is.na(State), "Unclassified", State)) %>% 
  group_by(State) %>% 
  summarise(Settlement_Area = sum(Area_sqm), Settlement_Count = n())

class_2000 <- shapefile("03_2000_class.shp")
tab_2000 <- as_tibble(class_2000@data) %>% 
  mutate(Area_sqm = Area / 1000000, State = if_else(is.na(State), "Unclassified", State)) %>% 
  group_by(State) %>% 
  summarise(Settlement_Area = sum(Area_sqm), Settlement_Count = n())

class_2015 <- shapefile("04_2015_class.shp")
tab_2015 <- as_tibble(class_2015@data) %>% 
  mutate(Area_sqm = Area / 1000000, State = if_else(is.na(State), "Unclassified", State)) %>% 
  group_by(State) %>% 
  summarise(Settlement_Area = sum(Area_sqm), Settlement_Count = n())


## combine classification results and prepare data for visualization
all_class <- bind_cols(tab_1975, tab_1990, tab_2000, tab_2015) %>% 
  select(-State1, -State2, -State3) %>% 
  rename_at(vars(ends_with("Area") | ends_with("Count")),
            funs(paste0(., "_1975"))) %>% 
  rename_at(vars(ends_with("1")), funs(str_replace(., "1", "_1990"))) %>% 
  rename_at(vars(ends_with("2")), funs(str_replace(., "2", "_2000"))) %>% 
  rename_at(vars(ends_with("3")), funs(str_replace(., "3", "_2015"))) %>% 
  gather(Settlement_Area_1975:Settlement_Count_2015, key = Ident, value = Val) %>% 
  transmute(State = State, 
            Type = substr(Ident, 12, nchar(Ident) - 5),
            Year = as.numeric(substr(Ident, nchar(Ident)-3, nchar(Ident))), 
            Value = Val)
all_class
areas <- all_class %>% filter(Type == "Area")
counts <- all_class %>% filter(Type == "Count")


## plot 
p1 <- ggplot(areas) +
  geom_col(aes(x = as.factor(Year), y = Value, fill = State), alpha = .8) +
  labs(x = "Year", y = expression(paste("Settlement Area (km"^"2",")"))) +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#0037b8", "#007a3d", "darkgrey"), 
                    labels = c("Israel", "Palestine", "Unclassified"))
# ggsave("Areas_01.png", device = "png", dpi = 450)

p2 <- ggplot(counts) +
  geom_col(aes(x = as.factor(Year), y = Value, fill = State), alpha = .8) +
  labs(x = "Year", y = "Number of Settlements") +
  theme_classic() +
  theme(legend.position = "bottom") +
  scale_fill_manual(values = c("#0037b8", "#007a3d", "darkgrey"), 
                    labels = c("Israel", "Palestine", "Unclassified"))
# ggsave("Counts_01.png", device = "png", dpi = 450)

## Combine and save plots
plot_grid(p1, p2, labels = c("a)", "b)"))
ggsave("trends.png", device = "png", width = 20, height = 8, units = "cm", dpi = 450)
