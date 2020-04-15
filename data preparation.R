library(readr)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("data/zip codes/modified")

#### places of worship #### 
#source: openstreetmap.org

pofw <- read.csv("pofw.csv")
#aggregating and filtering to desired information:
pofw <- pofw%>%
  mutate(religion = case_when(str_detect(fclass, "^chris") ~ "christian",
                              str_detect(fclass, "^musl") ~ "muslim",
                              str_detect(fclass, "^jew")~"jewish"))
NAs_rec <- which(is.na(pofw$religion))
pofw <- pofw[-NAs_rec, ]
pofw_export <- select(pofw, c("X", "Y", "religion"))
write.csv(pofw_export, "religious_buildings.csv")


#### phone codes, postcodes, settlements data ####

###phone codes, source: https://www.countrycallingcodes.com/country.php###
phonecodes_isr <- read_csv("02_israel_cities_phonecodes.csv")
phonecodes_isr <- phonecodes_isr%>%
  mutate(state = "IL")

phonecodes_pal <- read_csv("02_b_palestine_cities_phonecode.csv")
phonecodes_pal <- phonecodes_pal%>%
  mutate(state = "PAL")

#merge phone codes IL and PAL
phonecodes <- rbind(phonecodes_pal, phonecodes_isr)
phonecodes <- phonecodes%>%
  mutate(source = "phone code")

###Israeli Settlements in the Westbank###
#source: wikipedia (https://en.wikipedia.org/wiki/List_of_Israeli_settlements) (deprecated, testing only)
il_settlements <- read_csv("..Israel_settlements_geocoded.csv")
il_settlements <-il_settlements%>%
  mutate(state = "IL", source = "wikipedia")%>%
  rename(c(Y = lat, X = long))

#source: peacenow (https://peacenow.org.il/en/settlements-watch/israeli-settlements-at-the-west-bank-the-list)
il_settlements_pn <- read_csv("..Israel_settlements_peacenow_geocoded.csv")
il_settlements_pn <-il_settlements_pn%>%
  mutate(state = "IL", source = "peacenow")%>%
  rename(c(Y = lat, X = long))

###Palestine Cities from Census 2017### source: Palestine Census 2017 http://www.pcbs.gov.ps/census2017/
pal_cities <- read_csv("..palestine_cities_census_2017_geocoded.csv")
pal_cities <-pal_cities%>%
  mutate(state = "PAL", source = "palestine census 2017")%>%
  rename(c(Y = lat, X = long))


###zip codes (extracted from openstreetmap.org with query on overpass-turbo.eu (see query request in repository)###
postcodes <- read_csv("01_addr_postcode_isr_modified.csv")
#cleaning data
postcodes$zip_code <- postcodes$`addr:postcode`%>%
  str_trim()%>% #remove empty spaces
  str_replace_all("\\+", "")%>%  #remove "+"
  str_remove("^0+") #remove preceding and "lonely" zeros

postcodes$zip_code[postcodes$zip_code == ""] <- NA #define NA's ("00[...]" from old dataframe as well (now empty))

NAs <- which(is.na(postcodes$zip_code))
postcodes <- postcodes[-NAs, ] #removing rows with NAs

#recoding to IL / PAL (beginning with 1-8: IL, beginning with 9: PAL)
#postcodes <- postcodes%>%
# mutate(state = case_when(str_detect(zip_code, "^9") ~ "PAL",
#       str_detect(zip_code, "^1|^2|^3|^4|^5|^6|^7|^8") ~ "IL"))

#new approach relating to several sources: availability of zip codes indicates IL nationality (beginning with 1-9) #deprecated
postcodes <- postcodes%>%
  mutate(state = case_when(str_detect(zip_code, "^1|^2|^3|^4|^5|^6|^7|^8|^9") ~ "IL"))

NAs_rec <- which(is.na(postcodes$state))
postcodes <- postcodes[-NAs_rec, ]

#adding source information
postcodes <- postcodes%>%
  mutate(source = "postal code")

#cleaning
postcodes_clean <- postcodes%>%select(1, 2, 10, 11)

###combining data###
#common_cols <- intersect(colnames(phonecodes), colnames(postcodes_clean))

cols_e <- c("X", "Y", "state", "source")

data_all <- rbind(
  subset(phonecodes, select = cols_e), 
  #subset(postcodes_clean, select = cols_e),
  subset(il_settlements_pn, select = cols_e),
  subset(pal_cities, select = cols_e)
)

write_csv(postcodes_clean, "postcodes_mod_clean.csv")
write_csv(postcodes, "postcodes_mod.csv")
#write_csv(phone_zip, "phone_zip.csv")
#write_csv(data_all, "database_israel_Update_with_zip.csv")
write_csv(data_all, "database_israel.csv")


#####glimpse at distribution of leading number:
postcodes <- postcodes%>%
  mutate(first_no = case_when(str_detect(zip_code, "^9") ~ "9",
                              str_detect(zip_code, "^1") ~ "1",
                              str_detect(zip_code, "^2") ~ "2",
                              str_detect(zip_code, "^3") ~ "3",
                              str_detect(zip_code, "^4") ~ "4",
                              str_detect(zip_code, "^5") ~ "5",
                              str_detect(zip_code, "^6") ~ "6",
                              str_detect(zip_code, "^7") ~ "7",
                              str_detect(zip_code, "^8") ~ "8"))
#histogram:

ggplot(postcodes, aes(first_no))+
  geom_histogram(alpha = 0.5, aes(), stat = 'count', position = 'identity')+
  theme_light()+
  theme(axis.text.x = element_text(angle = , vjust = 1, hjust=1))+
  labs(title = "Distribution of postal code areas", subtitle = "Number of observations", caption = "Source: openstreetmap.org")+
  xlab("first letter of postal code")
