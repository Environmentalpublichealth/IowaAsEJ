setwd("~/Desktop/Jiali/TAMU/environment/starr/As EJ/")

# install.packages('ipumsr')
#install.packages("maps")
#install.packages("maptools")
if (!require("ipumsr")) stop("Reading IPUMS data into R requires the ipumsr package. It can be installed using the following command: install.packages('ipumsr')")

library(maps)
library(maptools)
library(ggplot2)

ddi <- read_ipums_ddi("usa_00001.xml")
data <- read_ipums_micro(ddi)
iowa <- data[data$STATEFIP == 19,]
# texas <- data[data$STATEFIP == 48,]
# starr <- texas[texas$COUNTYFIP == 427,]
# table(texas$COUNTYFIP)

iowa$CBSERIAL <- as.character(iowa$CBSERIAL)
iowa$CLUSTER <- as.character(iowa$CLUSTER)

# census tract
tractLookup <- function(x, y) {
  pt <- SpatialPoints(data.frame(x = x, y = y))
  overlay.pt <- over(pt, tracts) # what index number does pt fall inside?
  return(levels(overlay.pt$GEOID)[overlay.pt$GEOID]) # give the GEOID number from the census layer
}

#pt <- SpatialPoints(data.frame(x =-93.4063 , y = 43.1110)) # give a latitude and longitude
#overlay.pt <- over(pt, tracts) # function to find the overlap place

  
tracts <- readShapePoly("cb_2020_19_tract_500k/cb_2020_19_tract_500k.shp")
# test the function, it works!
tractLookup(-93.176147,43.177437)

# PWTS
library("foreign")
PWTS <- read.dbf("~/Downloads/PWTS_Wells/PWTS_Wells.dbf", as.is = FALSE)
PWTS_WQ <- read.dbf("~/Downloads/PWTS_Wells/PWTS_Water_Quality.dbf", as.is = FALSE)
PWTS_active <- PWTS[grep("Active",PWTS$STATUS),]
PWTS_household <- PWTS_active[grep("Household", PWTS_active$WELL_USE),]
for (line in 1:50855) {
  PWTS_household$GEOID[line] <- tractLookup(PWTS_household$LONGITUDE[line], PWTS_household$LATITUDE[line])  
}
#save filtered data
write.csv(PWTS_household[,c(2,40)], "PWTS active household use with GEOID.csv", quote = F, row.names = F)

# filter water quality data, keep the data from the latest test
PWTS_WQ_order <- PWTS_WQ[order(PWTS_WQ$SMPL_DATE, decreasing = T),]
PWTS_WQ_filtered <- PWTS_WQ_order[!duplicated(PWTS_WQ_order$wellnmbr),]
length(intersect(PWTS_household$WELLNMBR, PWTS_WQ_filtered$wellnmbr)) # 38314

# load private well data
PWTS_household <- read.csv("PWTS active household use with GEOID.csv", header = T, sep = ",")
# add water quality to household
PWTS_household_WQ <- merge(PWTS_household, PWTS_WQ_filtered, by.x = "WELLNMBR", by.y = "wellnmbr", all.x=T)

As_Data <- read.csv("Iowa Priavte Well As-SHL data to 2020.csv", header = T)
filter_data <- As_Data[which(is.na(As_Data$Latitude) == F),]
# map GEOID from longitude and latitude
for (line in 1:12087) {
filter_data$GEOID[line] <- tractLookup(filter_data$Longitude[line], filter_data$Latitude[line])  
}
#save filtered data
write.csv(filter_data, "As Data 2015-2020 filtered with GEOID.csv", quote = F, row.names = F)

# histogram of GEOID
filter_data <- read.csv("As Data 2015-2020 filtered with GEOID.csv", header = T)
dim(table(filter_data$GEOID))
plot(tracts)

# bacteria and nitrate report from 2015 - 2020
BacReport <- read.csv("BacteriaNitrateDetail.csv", header = T, skip = 3)
BacReport_order <- BacReport[order(BacReport$Date_Tested, decreasing = T),]
BacReport_filtered <- BacReport_order[!duplicated(BacReport_order$Well_Number),]
WaterQuality_As_N <- na.omit(merge(filter_data[,c(8,11,20)], BacReport_filtered[,c(1,5,6,7)], by.x="Well.Number", by.y="Well_Number", all.x=T))
# none of the high As well has high Nitrate....

# As data from PWTS
PWTS_As <- read.csv("PWTS ContaminantReport As since 2015.csv", header = T, skip = 3)
length(intersect(PWTS_As[!duplicated(PWTS_As$WellNumber),]$WellNumber, PWTS_household$WELLNMBR))
length(intersect(PWTS_WQ_filtered$wellnmbr, PWTS_As$WellNumber))

# get census package
# install.packages("tidycensus")
library(tidycensus)
options(tigris_use_cache = T)

iwoa_map <- get_acs(
  geography = "tract",
  variables = "B19013_001",
  state = "IA",
  year = 2020,
  geometry = TRUE
)

# clean up data, convert < detection limit to 0.0005
filter_data$Arsenic[filter_data$Arsenic == "<0.001"] <- 0.0005
filter_data$Arsenic[filter_data$Arsenic == "<0.0001"] <- 0.00005
filter_data$Arsenic <- as.numeric(filter_data$Arsenic)
filter_data <- filter_data[-which(is.na(filter_data$Arsenic)),] # 12067
# tested well basic stats
summary(filter_data)
# number of wells < MCL
dim(filter_data[filter_data$Arsenic >= 0.01,]) # 1119
dim(filter_data[filter_data$Arsenic < 0.01,]) # 10948
# which wells get tested multiple times
dup_tested <- filter_data[duplicated(filter_data$Location.Address) | duplicated(filter_data$Location.Address, fromLast = TRUE),]
dim(dup_tested[dup_tested$Arsenic >= 0.01,]) # 482
dim(dup_tested[dup_tested$Arsenic < 0.01,]) # 2679

# keep highest test result for each well
filter_data_sort <- filter_data[order(filter_data$Arsenic, decreasing = T),]
clean_data <- filter_data_sort[!duplicated(filter_data_sort[,c(4,5)]),]
summary(clean_data)
dim(clean_data[clean_data$Arsenic >= 0.01,]) # 947
dim(clean_data[clean_data$Arsenic < 0.01,]) # 9483

# compute the number of data points in each tracts
sample_size <- data.frame(table(clean_data$GEOID)) # 10424
sum(sample_size$Freq)

names(sample_size) <- c("GEOID", "Samples")

iwoa_map <- merge(iwoa_map, sample_size, by= "GEOID", all.x = T)

sample_high <- data.frame(table(clean_data$GEOID[clean_data$Arsenic >= 0.01]))
names(sample_high) <- c("GEOID", "highAs")
iwoa_map <- merge(iwoa_map, sample_high, by= "GEOID", all.x = T)


ggplot(data = iwoa_map, aes(fill = Samples)) + 
  geom_sf()+
  labs(title = "Private well sample size",
       fill = "# of samples") + 
  scale_fill_distiller(palette = "Blues", 
                       direction = 1) +
  theme_void()
ggsave("Overview PW samples.pdf", width = 6, height = 4)

## EJ data
iowa_EJ <- read.csv("Iowa EJI from CDC.csv")
HUD_data <- read.csv("HUD_data_2020tab1.csv", header = T)
iowa_HUD <- HUD_data[HUD_data$state == 19,]
# tract_id - census tract ID, column 12
# metro - metropolitan or not 1/0
# ave_hhd_size - average household size
# avg_pov_rate - average poverty rate, average from 3 five-year ACS estimate

iwoa_map <- merge(iwoa_map, iowa_EJ[, c(5,17)], by ="GEOID", all.x=T) 
ggplot(data = iwoa_map, aes(fill = RPL_SVM)) + 
  geom_sf()+
  scale_fill_distiller(palette = "YlOrRd", 
                                 direction = 1) +
  labs(title = "State environmental justice",
       fill = "SVM",
       caption = "Data source: CDC ATSDR 2022 EJI") + 
  theme_void()
ggsave("State SVM.pdf", width = 6, height = 4)

# Point out well locations on map
ggplot() + 
  geom_sf(data = iwoa_map, fill = "white")+
  geom_point(data = PWTS_household, aes(x=LONGITUDE, y=LATITUDE), size = 0.02)+
  labs(title = "Active private well household use")+ 
  theme_void()

ggsave("All household private well map.pdf", width = 6, height = 4)

# private well number overall
PWTS_tractSum <- data.frame(table(PWTS_household$GEOID))
names(PWTS_tractSum) <- c("GEOID","PWTS")
iwoa_map <- merge(iwoa_map, PWTS_tractSum, by="GEOID", all.x=T)
ggplot() + 
  geom_sf(data = iwoa_map, aes(fill = PWTS))+
  labs(title = "Active household-use private well",
       fill = "# of activate wells") + 
  scale_fill_distiller(palette = "Reds", 
                       direction = 1) +
  theme_void()

ggsave("Number of household private well tracts.pdf", width = 6, height = 4)


# Group data by tracts and calculate mean/median
tract_Data <- clean_data %>%
  group_by(GEOID) %>%
  summarise(Mean = mean(Arsenic),
            SD = sd(Arsenic),
            Max = max(Arsenic))

iwoa_map <- merge(iwoa_map, tract_Data, by= "GEOID", all.x = T)

library(RColorBrewer)
library(scales)
iwoa_map$Max <- iwoa_map$Max*1000
iwoa_map$Mean <- iwoa_map$Mean *1000

ggplot(data = iwoa_map, aes(fill = Max)) + 
  geom_sf()+
  scale_fill_gradientn(colours=c("#FFFFFF","#66B2FF","#3399FF","#0080FF","#0066CC"),na.value = "gray50",
                       breaks=c(10,50,100,400),labels=c(10, 50,100,400),
                       limits=c(0,400))+
  labs(title = "Maximum Arsenic concentrations tested",
       fill = "Consentration (ppb)") + 
  theme_void()+
  theme(legend.key.height = unit(1,"cm"),
        legend.key.width = unit(0.3,"cm"),
    legend.text = element_text(size = 8, hjust = 1))
ggsave("Max As map.pdf", width = 6, height = 4)

# for mean concentration
ggplot(data = iwoa_map, aes(fill = Mean*1000)) + 
  geom_sf()+
  scale_fill_gradientn(colours=c("#FFFFFF","#cc756b","#c05e55","#b4463f","brown"),na.value = "gray50",
                       breaks=c(0,5, 10, 25, 50),labels=c(0,5,10,25,50),
                       limits=c(0,50))+
  labs(title = "Private well average arsenic concentrations",
       fill = "Conc. (ppb)") + 
  theme_void()+
  theme(legend.key.height = unit(1,"cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.text = element_text(size = 8, hjust = 1))
ggsave("img/Private well average As map.pdf", width = 6, height = 4)


# divide tracts to be private only, pub only, and both
iwoa_map$group <- NA
iwoa_map$group[iwoa_map$GEOID %in% both] <- "Both"
iwoa_map$group[iwoa_map$GEOID %in% private] <- "Private"
iwoa_map$group[iwoa_map$GEOID %in% public] <- "Public"
#### the GEOID for both, public and private is run in file <EJ tables.R>.
ggplot(data = iwoa_map, aes(fill = group)) + 
  geom_sf()+
  labs(title = "Cencus tracts classification",
       fill = "Include") + 
  scale_fill_manual(values = alpha(c("#0066CC","brown","#663399"), 0.45))+
  theme_void()+
  theme(legend.key.height = unit(1,"cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.text = element_text(size = 8, hjust = 1))
ggsave("group tracts map.pdf", width = 6, height = 4)

# extract the date from the Collection time
dup_tested$Date <- sapply(strsplit(dup_tested$Collected.Date," "), `[`, 1)

## rural data from nhgis
rural <- read.csv("nhgis0002_csv/nhgis0002_ds258_2020_tract.csv", header = T)
rural_ia <- rural[rural$STATEA == 19,]
names(rural_ia)
# U9W001-total houses, U9W002-urban, U9W003-rural, U9W004-not defined
hist(rural_ia$U9W003)
plot(iwoa_map$T_house, iwoa_map$U9W001)
summary(iwoa_map$U9W001)
sd(iwoa_map$U9W001) 
sum(iwoa_map$U9W002) # 891330 - urban (63%) # 521459 - rural (37%)

library(ggpmisc)
ggplot(data = iwoa_map[iwoa_map$GEOID %in% both,], aes(x = Mean, y = Mean_PWS)) +
  stat_poly_line() +
  stat_poly_eq() +
  geom_point() +
  labs(x="private well arsenic (mg/L)", y = "PWS arsenic (mg/L)")+
  theme_classic()
ggsave("img/PWSvsPrivateAs.pdf", width = 4, height = 3)
