library(data.table)
library(googleLanguageR)
Sys.setenv(LANG = "en")
Sys.setlocale("LC_TIME", "C")

gl_auth("C:/Users/marci/Desktop/UW/barcelona_app/google_cloud/plated-reducer-309918-1289cf7bb31a.json")

trees <- read.csv("./data_raw/trees/2021_1T_OD_Arbrat_Parcs_BCN.csv", encoding = "UTF-8")
trees2 <- read.csv("./data_raw/trees/2021_1T_OD_Arbrat_Viari_BCN.csv", encoding = "UTF-8")
trees3<- read.csv("./data_raw/trees/2021_1T_OD_Arbrat_Zona_BCN.csv", encoding = "UTF-8")
trees <- rbind(trees, trees2, trees3)
rm(trees2, trees3)

setDT(trees)
tree_agg_original <- trees[, .(count = .N), by = .(nom_catala)]

trees$nom_castella <- gl_translate(trees$nom_castella, target = "en")$translatedText
tree_names_dict <- data.frame(name_sp = unique(trees$nom_castella))
tree_names_dict$name_en <- gl_translate(tree_names_dict$name_sp, target = "en")$translatedText

trees <- merge(trees, tree_names_dict, by.x = "nom_castella", by.y = "name_sp", all.x = T)

trees[, nom_catala:= NULL]
setnames(trees, c("longitud", "latitud", "nom_districte"), c("lon", "lat", "District"))
setnames(trees, "name_en", "nom_catala")

tree_agg <- trees[, .(count = .N), by = .(nom_catala)]
tree_agg <- tree_agg[order(-count)]

trees <- readRDS("./data/trees.rds")

tree_agg[substr(nom_catala, 1, 17) == "Globose silk tree", nom_catala := "Globose silk tree"]
tree_agg[substr(nom_catala, 1, 25) == "Orange blossom from China", nom_catala := "Orange blossom from China"]
tree_agg[nom_catala == "Banana", nom_catala := "Platanus"]
tree_agg[nom_catala == "Does not take", nom_catala := "European nettle tree"]
tree_agg[nom_catala == "Pink stick; white chick", nom_catala := "Tipuana"]
tree_agg[nom_catala == "stone pine", nom_catala := "Stone pine"]
tree_agg[nom_catala == "-", nom_catala := "Unknown"]

other <- tree_agg[count <5000,]$nom_catala

trees[substr(nom_catala, 1, 17) == "Globose silk tree", nom_catala := "Globose silk tree"]
trees[substr(nom_catala, 1, 25) == "Orange blossom from China", nom_catala := "Orange blossom from China"]
trees[nom_catala == "Banana", nom_catala := "Platanus"]
trees[nom_catala == "Does not take", nom_catala := "European nettle tree"]
trees[nom_catala == "Pink stick; white chick", nom_catala := "Tipuana"]
trees[nom_catala == "stone pine", nom_catala := "Stone pine"]
trees[nom_catala == "-", nom_catala := "Unknown"]


trees[, `Tree specie` := nom_catala]
trees[`Tree specie` %in% other, `Tree specie` := "Other"]
trees <- trees[District != "None",]

trees[,`Tree specie`:= str_to_title(`Tree specie`)]
trees[, nom_catala:= str_to_title(nom_catala)]
tree_agg[,nom_catala:= str_to_title(nom_catala)]

trees[,District:= str_to_title(str_to_lower(District))]

trees[,data_plantacio:= substr(data_plantacio, 1, 10)]
trees[data_plantacio == "None",data_plantacio:= "Unknown"]

trees$popup = paste("Tree specie:", trees$nom_catala, "<br>",
                    "Scientific name:", trees$nom_cientific, "<br>",
                    "Plantation date:", trees$data_plantacio, "<br>",
                        "Address:", trees$adreca, "<br>",
                        "District:", trees$District,  "<br>") %>% 
  lapply(HTML)

trees[, c("nom_castella", "x_etrs89", "y_etrs89", "tipus_element", "espai_verd", 
          "cat_especie_id", "categoria_arbrat", "tipus_aigua",
          "tipus_reg", "geom", "catalogacio", "codi_barri", "nom_barri", "codi_districte") := NULL]

saveRDS(trees, "./data/trees.rds")
saveRDS(tree_agg, "./data/tree_agg.rds")

accidents <- as.data.table(read.csv("./data_raw/accidents/2020_accidents_gu_bcn.csv", encoding = "UTF-8" ))
accidents[, c("Numero_expedient", "Codi_barri", "Nom_carrer", "Codi_carrer", "Num_postal_caption", 
              "Coordenada_UTM_Y" , "Coordenada_UTM_X") := NULL]

setnames(accidents, c("district_code", "district_name", "neighborhood_name", 
                      "day_name", "day_number",
                      "day_type", "year", "month_number", "month_name", 
                      "day_of_month", "hour", "day_or_night",
                      "cause", "dead", "slightly_injured", "heavily_injured", 
                      "victims", "cars_involved", 
                      "Longitud", "Latitud"))


day_names <- data.frame(day_name = unique(accidents$day_name))
month_names <- data.frame(month_name = unique(accidents$month_name))
day_type <- data.frame(day_type = unique(accidents$day_type))
day_or_night <- data.frame(day_or_night = unique(accidents$day_or_night))
cause <- data.frame(cause = unique(accidents$cause))

day_names$day_name_english <- gl_translate(day_names$day_name, target = "en")$translatedText
month_names$month_name_english <- gl_translate(month_names$month_name, target = "en")$translatedText
day_type$day_type_english <- gl_translate(day_type$day_type, target = "en")$translatedText
day_or_night$day_or_night_english <- gl_translate(day_or_night$day_or_night, target = "en")$translatedText
cause$cause_english <- gl_translate(cause$cause, target = "en")$translatedText

accidents <- merge(accidents, day_names, by = "day_name")
accidents <- merge(accidents, month_names, by = "month_name")
accidents <- merge(accidents, day_type, by = "day_type")
accidents <- merge(accidents, day_or_night, by = "day_or_night")
accidents <- merge(accidents, cause, by = "cause")

accidents <- accidents[, 6:25]
names(accidents) <- sub("_english", "", names(accidents))

accidents[, date := as.Date(paste(year, month_number,day_of_month, sep = "-"))]
accidents[, date:= format(date, "%d-%b-%Y")]

setnames(accidents, c("Longitud", "Latitud"), c("lon", "lat"))

accidents$popup = paste("District:", accidents$district_name, "<br>",
                        "Date:", accidents$date, "<br>",
                        "Vehicles involved:", accidents$cars_involved, "<br>",
                        "Victims:", accidents$victims, "<br>",
                        "Dead:", accidents$dead, "<br>",
                        "Cause:", accidents$cause,  "<br>") %>% 
  lapply(HTML)

accidents$day_name <- factor(accidents$day_name, levels = c("Monday", "Tuesday", 
                                                            "Wednesday", "Thursday", 
                                                            "Friday", "Saturday", "Sunday"))

accidents$month_name <- factor(accidents$month_name, levels = c("January", "February", "March", "April",
                                                                "May", "June", "July", "August", "September", 
                                                                "October", "November", "December"))

saveRDS(accidents, "./data/accidentsCleaned.rds")

petitions <- as.data.table(read.csv("./data_raw/petitions/2021_IRIS_Peticions_Ciutadanes_OpenData.csv", encoding = "UTF-8"))

pet_type = data.table(TIPUS = unique(petitions$TIPUS))
pet_area= data.table(AREA= unique(petitions$AREA))
pet_element= data.table(ELEMENT = unique(petitions$ELEMENT))
pet_details= data.table(DETALL = unique(petitions$DETALL))
pet_suport= data.table(SUPORT = unique(petitions$SUPORT))
pet_riposta= data.table(CANALS_RESPOSTA = unique(petitions$CANALS_RESPOSTA))

pet_type$TIPUS_EN <- gl_translate(pet_type$TIPUS, target = "en")$translatedText
pet_area$AREA_EN <- gl_translate(pet_area$AREA, target = "en")$translatedText
pet_element$ELEMENT_EN <- gl_translate(pet_element$ELEMENT, target = "en")$translatedText
pet_details$DETAILS_EN <- gl_translate(pet_details$DETALL, target = "en")$translatedText
pet_suport$SUPORT_EN <- gl_translate(pet_suport$SUPORT, target = "en")$translatedText
pet_riposta$CANALS_RESPOSTA_EN <- gl_translate(pet_riposta$CANALS_RESPOSTA, target = "en")$translatedText

petitions[pet_type, on="TIPUS",TIPUS:=str_to_title(i.TIPUS_EN)]
petitions[pet_area, on="AREA",AREA:=i.AREA_EN]
petitions[pet_element, on="ELEMENT",ELEMENT:=i.ELEMENT_EN]
petitions[pet_details, on="DETALL",DETALL:=i.DETAILS_EN]
petitions[pet_suport, on="SUPORT",SUPORT:=str_to_title(i.SUPORT_EN)]
petitions[pet_riposta, on="CANALS_RESPOSTA",CANALS_RESPOSTA:=str_to_title(i.CANALS_RESPOSTA_EN)]

colnames(petitions)
setnames(petitions, c("ID", "Type", "Area", "Subject", "Detailed", 
                      "day_open", "month_open", "year_open", 
                      "day_closure", "month_closure", "year_closure", 
                      "district_code", "district",
                      "neighberhood_code", "neighberhood","census_section",
                      "street_type", "street", "str_number",
                      "x", "y", "lon", "lat", 
                      "support", "answer_channel"))

petitions[answer_channel == "He doesn't want an answer", answer_channel:= "No answer needed"]
colnames(petitions)

library(readr)
petitions[, date_open := as.Date(paste(year_open, month_open,day_open, sep = "-"))]
petitions[, date_close := as.Date(paste(year_closure, month_closure,day_closure, sep = "-"))]
petitions[, dealing_time := difftime(date_close, date_open, units = c("days"))]
petitions[, date_open := format(date_open, "%d-%b-%Y")]
petitions[, date_close := format(date_close, "%d-%b-%Y")]

petitions$popup = paste("Type of inquiry:", petitions$Type, "<br>",
                        "Area:", petitions$Area, "<br>",
                        "Subject:", petitions$Subject, "<br>",
                        "Details:", petitions$Detailed, "<br>",
                        "Date of inquiry:", petitions$date_open, "<br>",
                        "Submitted via:", petitions$support, "<br>",
                        "Dealing time:", petitions$dealing_time, "days", "<br>",
                        "Street:", petitions$street, petitions$str_number) %>% 
  lapply(HTML)

saveRDS(petitions, "./data/petitions.rds")

###bikes
bikes <- as.data.table(read.csv("./data_raw/bicing/2019_03_Marc_BICING_ESTACIONS (1).csv", encoding = "UTF-8"))
bikes <- bikes[status=="OPN",]
bikes$type <- str_to_title(bikes$type)
bikes[, slots_total := bikes+slots]

#changing format to date
typeof(bikes$updateTime)
library(lubridate)
bikes[,updateTime := parse_date_time(updateTime, orders = "dmy HMS")] 
bikes_unique <- bikes %>% 
  group_by(id) %>%
  slice(which.max(updateTime))

setnames(bikes_unique, c("longitude", "latitude"), c("lon", "lat"))

bikes_unique$popup = paste("Street:", bikes$streetName, bikes$streetNumber, "<br>",
                    "Altitude:", bikes$altitude, "<br>",
                    "Slots available:", bikes$slots_total, "<br>",
                    "Last update:", bikes$updateTime,"<br>") %>% 
  lapply(HTML)

saveRDS(bikes_unique, "./data/bikes.rds")


#air quality stations
air <- as.data.table(read.csv("./data_raw/air_quality/2021_qualitat_aire_estacions.csv", encoding = "UTF-8"))
air$Clas_1 <- gl_translate(air$Clas_1, target = "en")$translatedText
air$Clas_2 <- gl_translate(air$Clas_2, target = "en")$translatedText

air[Clas_2 == "Fund", Clas_2 := "Ground"]

air <- unique(air[,1:14])

setnames(air, c("Longitud", "Latitud"), c("lon", "lat"))

air$popup = paste("District:", air$Nom_districte, "<br>",
                  "Class 1:", air$Clas_1, "<br>",
                  "Class 2:", air$Clas_2) %>% 
  lapply(HTML)



saveRDS(air, "./data/air.rds")


#wifi hotspots

wifi <- as.data.table(read.csv("./data_raw/wifi/opendatabcn_Internet_Wifi-BCN-CSV.csv", encoding = "UTF-8"))
wifi <- wifi[secondary_filters_fullpath %like%  "Planol BCN >> Internet",]
wifi <- wifi[, c("register_id", "institution_name", "created", "modified", "addresses_road_name", "addresses_start_street_number",
                 "addresses_district_name", "secondary_filters_name", "secondary_filters_fullpath", "geo_epgs_4326_x", "geo_epgs_4326_y")]

setnames(wifi, c("geo_epgs_4326_x", "geo_epgs_4326_y"), c("lat", "lon"))

wifi[, modified := as.Date(substr(modified, 1, 10))]

wifi$popup = paste("District:", wifi$addresses_district_name, "<br>",
                  "Institution:", wifi$institution_name, "<br>",
                  "Street:", wifi$addresses_road_name, wifi$addresses_start_street_number, "<br>",
                  "Last update:", wifi$modified) %>% 
  lapply(HTML)
saveRDS(wifi, "./data/wifi.rds")

