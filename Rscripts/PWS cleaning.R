setwd("~/Desktop/Jiali/TAMU/environment/starr/As EJ/")
library(dplyr)
library(ggplot2)
library(margins)

# all data tables obtained from https://programs.iowadnr.gov/iowadrinkingwater/
# Time range from 01/01/2015 to 12/31/2020, covering the same time span of our private well data
all_PWS <- read.csv("PWSSearch.csv", header = T) # has well ID, address, population and connection
sanity <- read.csv("CompletedSanitarySurveys 10-23-2023.csv", header = T) # has population served
sample_data <- read.csv("pwsSamplesReport.csv", header = T) # has As concentration results
violation <- read.csv("PWSViolations.csv", header = T) # has all violations

# clean up the sanity data
names(sanity)[1] <- "PWSID"

ggplot(all_PWS, aes(x=Population.Served)) + 
  geom_histogram(binwidth=100)+
  theme_bw()

dim(all_PWS[all_PWS$Population.Served <=250000,])
summary(all_PWS$Population.Served)
sum(all_PWS$Population.Served, na.rm = T) # 3,085,384
sum(all_PWS[all_PWS$Population.Served >100000,]$Population.Served, na.rm = T)

# clean up violation data
violation_As <- violation[violation$Analyte.Name == "Arsenic",] # 67 violations
dim(table(violation_As$PWS.ID)) # 17 wells

# sample data clean up
# add population
names(sample_data)
sample_data$Detect <- as.numeric(sample_data$Detect)
sample_data$Detect[is.na(sample_data$Detect)] <- 0.0005
names(sample_data)[1] <- "PWSID"
sample_data_pop <- left_join(sample_data, sanity[,c(1,10)], by="PWSID", keep=FALSE, multiple="first")
ggplot(data = na.omit(sample_data_pop), aes(x=PopulationServed, y =Detect, color=PWS.Type))+
  geom_point(alpha = 0.6)+
  labs(x = "PWS serving population", y = "Arsenic (mg/L)")+
  scale_y_continuous(trans = 'log10')+
  geom_hline(yintercept=0.01,lwd=0.5, color="blue")+
  theme_bw()

# tested PWS stats
table(na.omit(unique(sample_data_pop[,c(1,21)]))$PopulationServed <=250000)

highSample <- sample_data_pop[sample_data_pop$Detect >= 0.01,]
table(na.omit(unique(highSample[,c(1,21)]))$PopulationServed <=10000)

table(na.omit(highSample)$PopulationServed <=2500000)

# add GEOID based on zipcode
#install.packages("zipcodeR")
library(zipcodeR)
get_tracts("50597") # give muitiple tracts
# assign lat and lon to city
##install.packages("tidygeocoder")
library(tidygeocoder)
# split the location into city, state and zipcode
all_PWS$State <- rep("IA",1813)
PWS_geo <- geocode(all_PWS, city = City, state = State, method="osm")
# city Lu Verne is spelled wrong, all lat, long
PWS_geo[1049,13] <- 42.957878
PWS_geo[1049,14] <- -94.108749

# assign census tract ID to each PWS
# census tract
tractLookup <- function(x, y) {
  pt <- SpatialPoints(data.frame(x = x, y = y))
  overlay.pt <- over(pt, tracts) # what index number does pt fall inside?
  return(levels(overlay.pt$GEOID)[overlay.pt$GEOID]) # give the GEOID number from the census layer
}

tracts <- readShapePoly("cb_2020_19_tract_500k/cb_2020_19_tract_500k.shp")

for (line in 1:1813) {
  PWS_geo$GEOID[line] <- tractLookup(PWS_geo$long[line], PWS_geo$lat[line])  
}

write.csv(PWS_geo,"PWS_geoID.csv", quote = F, row.names = F)

# add GEOID to the data table
sample_data_pop$PWSID <- gsub(" ","",sample_data_pop$PWSID)
sample_data_pop <- merge(sample_data_pop,PWS_geo[,c(1,15)], by.x="PWSID",by.y="PWS.ID", all.x=T)
# clean up dataset, remove the inactive wells and NAs
sample_data_clean <- sample_data_pop[-which(is.na(sample_data_pop$GEOID)),]

summary(sample_data_clean)
lm1 <- lm(data=sample_data_clean[sample_data_clean$Detect>0,], log(Detect) ~ log(PopulationServed))
summary(lm1)
hist(resid(lm1))
BIC(lm1)

iowa_EJ <- read.csv("Iowa EJI from CDC.csv")

PWS_EJ <- merge(PWS_geo, iowa_EJ[, c(5,17,70)], by ="GEOID", all.x=T)
sample_data_clean_EJ <- merge(sample_data_clean, iowa_EJ[, c(5,17,70,95:107)], by ="GEOID", all.x=T)
sample_data_clean_EJ <- merge(sample_data_clean_EJ, iowa_HUD[,c(12,51)], by.x ="GEOID", by.y="tract_id", all.x=T)

# logistic regression
sample_data_clean_EJ$highAs <- ifelse(sample_data_clean_EJ$Detect < 0.01, 0, 1)
lm2 <- glm(data=sample_data_clean_EJ, highAs~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE
           ,family = "binomial")
summary(lm2)
OR_data <- as.data.frame(exp(cbind(OR = coef(lm2), confint(lm2))))
OR_data$var <- rownames(OR_data)
OR_data$pvalue <- summary(lm2)$coefficients[,4]
names(OR_data) <- c("OR","lower","upper","var","Pvalue")

dependent_var <- c("Uninsured","Unemployed","Renter","Poverty","No Internet",
                   "No High School Degree","Mobile House","Minority","Limited English",
                   "House burden","Disable",">Age 65","<Age 17")

ggplot(OR_data[-1,], aes(x=var, y=OR, color=Pvalue)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 1, linetype="dotted")+
  scale_color_gradient2(low = "#663399", mid = "#663399", high = "grey", midpoint = .05,limits=c(0,0.5), breaks=c(0,0.05,0.1,0.2,0.5))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="odds ratio",color="p-value")
ggsave("img/OR_PublicAll.pdf",height = 7, width = 6)

# logistic regression, public only
lm2 <- glm(data=single_all_data_EJ[single_all_data_EJ$GEOID %in% public,], highAs~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE
           ,family = binomial(link=logit))
summary(lm2)
OR_data <- as.data.frame(exp(cbind(OR = coef(lm2), confint(lm2))))
OR_data$var <- rownames(OR_data)
OR_data$pvalue <- summary(lm2)$coefficients[,4]
names(OR_data) <- c("OR","lower","upper","var","Pvalue")

dependent_var <- c("Uninsured","Unemployed","Renter","Poverty","No Internet",
                   "No High School Degree","Mobile House","Minority","Limited English",
                   "House burden","Disable",">Age 65","<Age 17")

ggplot(OR_data[-1,], aes(x=var, y=OR, color=Pvalue)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 1, linetype="dotted")+
  scale_color_gradient2(low = "#663399", mid = "grey", high = "grey", midpoint = .5,limits=c(0,1), breaks=c(0,0.05,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="odds ratio",color="p-value")
ggsave("img/OR_PublicOnly.pdf",height = 7, width = 6)

# linear regression
lm3 <- glm(data=single_all_data_EJ[single_all_data_EJ$GEOID %in% public,][-10030,], log(Arsenic) ~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE) 
summary(lm3)
hist(resid(lm3))
BIC(lm3)
length(resid(lm3))
margins_summary(lm3)
AME_data <- as.data.frame(margins_summary(lm3))

dependent_var <- c("Uninsured","Unemployed","Renter","Poverty","No Internet",
                   "No High School Degree","Mobile House","Minority","Limited English",
                   "House burden","Disable",">Age 65","<Age 17")

ggplot(AME_data[-c(1,4,5,7,8,12),], aes(x=factor, y=AME, color=p)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype="dotted")+
  scale_color_gradient2(low = "#663399", mid = "grey", high = "grey", midpoint = .5,limits=c(0,1), breaks=c(0,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var)[-c(1,4,5,7,8,12)])+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14), axis.title.x = element_text(size=14))+
  labs(x = "", y="Average Marginal effects",color="p-value")
ggsave("img/AME_PublicOnly_0301.pdf",height = 4, width = 6)


lm4 <- glm(data=sample_data_clean_EJ, log(Detect) ~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE) 
summary(lm4)
hist(resid(lm4))
BIC(lm4)
AME_data <- as.data.frame(margins_summary(lm4))
ggplot(AME_data, aes(x=factor, y=AME, color=p)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype="dotted")+
  scale_color_gradient2(low = "#663399", mid = "grey", high = "grey", midpoint = .5,limits=c(0,1), breaks=c(0,0.05,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="Average Marginal effects",color="p-value")
ggsave("img/AME_PublicAll.pdf",height = 7, width = 6)

ggplot(sample_data_clean_EJ, aes(x=EP_POV200, y = Detect))+
  geom_point()+
  theme_bw()

lm5 <- glm(formula = log(Detect) ~ EP_NOHSDP + EP_UNINSUR + EP_MINRTY + 
      EP_POV200 + EP_UNEMP + EP_RENTER + EP_HOUBDN + EP_AGE65 + 
      EP_NOINT + EP_LIMENG + EP_MOBILE, data = sample_data_clean_EJ)
summary(lm5)

# plot histogram for the SV distribution
hist(sample_data_clean_EJ$RPL_SVM, main = "PWS sample distribution", xlab = "SV metrics")
hist(iowa_EJ$RPL_SVM, main = "Iowa state SV distribution", xlab = "SV metrics")
hist(filter_data_EJ$RPL_SVM, main = "Private well sample distribution", xlab = "SV metrics")


# Group data by tracts and calculate mean/median
PWS_Data <- sample_data_clean %>%
  group_by(GEOID) %>%
  summarise(Mean_PWS = mean(Detect),
            SD = sd(Detect),
            Max = max(Detect))

iwoa_map <- merge(iwoa_map, PWS_Data[,1:2], by= "GEOID", all.x = T)

iwoa_map$Mean_PWS <- iwoa_map$Mean_PWS*1000
ggplot(data = iwoa_map, aes(fill = Mean_PWS)) + 
  geom_sf()+
  scale_fill_gradientn(colours=c("#FFFFFF","#9b75bb","#8a5fb0","#7849a4","#663399"),na.value = "gray50",
                       breaks=c(0,5, 10, 25, 50),labels=c(0,5,10,25,50),
                       limits=c(0,50))+
  labs(title = "PWS average arsenic concentrations",
       fill = "Conc. (ppb)") + 
  theme_void()+
  theme(legend.key.height = unit(1,"cm"),
        legend.key.width = unit(0.3,"cm"),
        legend.text = element_text(size = 8, hjust = 1))
ggsave("average PWS As map.pdf", width = 6, height = 4)

# IA8200855 - remove the 210 ppb, as a pre treatment, keep the post-treatment
