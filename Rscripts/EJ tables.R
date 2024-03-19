setwd("~/Desktop/Jiali/TAMU/environment/starr/As EJ/")
## making summary tabels and plot for the EJ regression results,
# iowa HUD is from the preprocess.R

## demographics for all tracts
summary(iowa_HUD)
mean(iowa_EJ$RPL_SVM, na.rm=T)
sd(iowa_EJ$RPL_SVM, na.rm=T)
sd(iowa_HUD$p0010001, na.rm = T) # population SD
sd(iowa_HUD$p0150001, na.rm = T) # household number SD
sd(iowa_HUD$ave_hhd_size, na.rm = T)

# stats for private well tracts
HUD_private <- iowa_HUD[iowa_HUD$tract_id %in% clean_data$GEOID,]
summary(HUD_private)
sd(HUD_private$p0010001) # population SD
sd(HUD_private$p0150001) # household number SD
sd(HUD_private$ave_hhd_size) # household size SD
# B19013est1_17 median household income ACS 13-17, B19013me1_17 95% CI margin
mean(filter_data_EJ$Arsenic)
sd(filter_data_EJ$Arsenic)

# stats for public water tracts
HUD_public <- iowa_HUD[iowa_HUD$tract_id %in% sample_data_clean$GEOID,]
summary(HUD_public)
sd(HUD_public$p0010001) # population SD
sd(HUD_public$p0150001) # household number SD
sd(HUD_public$ave_hhd_size) # household size SD
mean(sample_data_clean_EJ$Detect)
sd(sample_data_clean_EJ$Detect)

mean(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% HUD_private$tract_id])
sd(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% HUD_private$tract_id])
mean(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% HUD_public$tract_id])
sd(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% HUD_public$tract_id])

# split the tracts as private only, public only and both
both <- intersect(HUD_private$tract_id, HUD_public$tract_id)
private <- HUD_private$tract_id[!(HUD_private$tract_id %in% both)]
public <- HUD_public$tract_id[!(HUD_public$tract_id %in% both)]
# private only
summary(HUD_private[!(HUD_private$tract_id %in% both),])
table(HUD_private[!(HUD_private$tract_id %in% both),]$metro)
sd(HUD_private[!(HUD_private$tract_id %in% both),]$p0010001)
sd(HUD_private[!(HUD_private$tract_id %in% both),]$p0150001)
sd(HUD_private[!(HUD_private$tract_id %in% both),]$ave_hhd_size)
mean(filter_data_EJ$Arsenic[filter_data_EJ$GEOID %in% private])
sd(filter_data_EJ$Arsenic[(filter_data_EJ$GEOID %in% private)])
# both
summary(HUD_private[(HUD_private$tract_id %in% both),])
table(HUD_private[(HUD_private$tract_id %in% both),]$metro)
sd(HUD_private[(HUD_private$tract_id %in% both),]$p0010001)
sd(HUD_private[(HUD_private$tract_id %in% both),]$p0150001)
sd(HUD_private[(HUD_private$tract_id %in% both),]$ave_hhd_size)
mean(all_data_EJ$Arsenic[all_data_EJ$GEOID %in% both], na.rm=T)
sd(all_data_EJ$Arsenic[all_data_EJ$GEOID %in% both], na.rm=T)

#public only
summary(HUD_public[!(HUD_public$tract_id %in% both),])
table(HUD_public[!(HUD_public$tract_id %in% both),]$metro)
sd(HUD_public[!(HUD_public$tract_id %in% both),]$p0010001)
sd(HUD_public[!(HUD_public$tract_id %in% both),]$p0150001)
sd(HUD_public[!(HUD_public$tract_id %in% both),]$ave_hhd_size)
mean(sample_data_clean_EJ$Arsenic[sample_data_clean_EJ$GEOID %in% public], na.tm=T)
mean(sample_data_clean_EJ$Arsenic[!(sample_data_clean_EJ$GEOID %in% both)], na.tm=T)
sd(sample_data_clean_EJ$Arsenic[sample_data_clean_EJ$GEOID %in% public])

mean(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% both])
sd(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% both])

mean(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% public])
sd(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% public])

mean(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% private])
sd(iowa_EJ$RPL_SVM[iowa_EJ$GEOID %in% private])


### sample distribution
# private
single_all_data_EJ$highAs <- as.factor(single_all_data_EJ$highAs)
ggplot(data = filter_data_EJ, aes(x = highAs)) +
  geom_bar(color = "brown", fill="brown")+
  scale_x_discrete(labels=c("0" = "Low", "1" = "High")) +
  labs(x= "Arsenic levels", y = "# of samples")+
  theme_classic(base_size = 14)
ggsave("img/Private bar distibution.pdf", height = 4, width = 3)

ggplot(data = filter_data_EJ, aes(x = log10(Arsenic*1000))) +
  geom_density(color = "brown")+
  labs(x= "Arsenic concentration log10(ppb)", y = "Density")+
  theme_classic(base_size = 14)
ggsave("img/Private data distibution.pdf", height = 4, width = 3.5)

ggplot(data = single_all_data_EJ[single_all_data_EJ$GEOID %in% private,], aes(x = highAs)) +
  geom_bar(color = "brown", fill="brown")+
  scale_x_discrete(labels=c("0" = "Low", "1" = "High")) +
  labs(x= "Arsenic levels", y = "# of samples")+
  theme_classic(base_size = 14)
ggsave("img/PrivateOnly bar distibution.pdf", height = 4, width = 3)

ggplot(data = single_all_data_EJ[single_all_data_EJ$GEOID %in% private,], aes(x = log10(Arsenic*1000))) +
  geom_density(color = "brown")+
  labs(x= "Arsenic concentration log10(ppb)", y = "Density")+
  theme_classic(base_size = 14)
ggsave("img/PrivateOnly data distibution_0301.pdf", height = 2.5, width = 3.5)

# public
sample_data_clean_EJ$highAs <- as.factor(sample_data_clean_EJ$highAs)
ggplot(data = sample_data_clean_EJ, aes(x = highAs)) +
  geom_bar(color = "#663399", fill="#663399")+
  scale_x_discrete(labels=c("0" = "Low", "1" = "High")) +
  labs(x= "Arsenic levels", y = "# of samples")+
  theme_classic(base_size = 14)
ggsave("img/Public bar distibution.pdf", height = 4, width = 3)

ggplot(data = sample_data_clean_EJ, aes(x = log10(Detect*1000))) +
  geom_density(color = "#663399")+
  labs(x= "Arsenic concentration log10(ppb)", y = "Density")+
  theme_classic(base_size = 14)
ggsave("img/Public data distibution.pdf", height = 4, width = 3.5)

sample_data_clean_EJ$highAs <- as.factor(sample_data_clean_EJ$highAs)
ggplot(data = single_all_data_EJ[single_all_data_EJ$GEOID %in% public,], aes(x = highAs)) +
  geom_bar(color = "#663399", fill="#663399")+
  scale_x_discrete(labels=c("0" = "Low", "1" = "High")) +
  labs(x= "Arsenic levels", y = "# of samples")+
  ylim(0,1000)+
  theme_classic(base_size = 14)
ggsave("img/PublicOnly bar distibution.pdf", height = 4, width = 3)

ggplot(data = single_all_data_EJ[single_all_data_EJ$GEOID %in% public,], aes(x = log10(Arsenic*1000))) +
  geom_density(color = "#663399")+
  labs(x= "Arsenic concentration log10(ppb)", y = "Density")+
  theme_classic(base_size = 14)
ggsave("img/PublicOnly data distibution_0301.pdf", height = 2.5, width = 3.5)

# both, need to convert all 0 to a very small number
all_data_EJ$highAs <- as.factor(all_data_EJ$highAs)
all_data_EJ$Arsenic[all_data_EJ$Arsenic == 0] <- 0.0005
ggplot(data = single_all_data_EJ[single_all_data_EJ$GEOID %in% both,], aes(x = highAs)) +
  geom_bar(aes(fill=Type))+
  scale_x_discrete(labels=c("0" = "Low", "1" = "High")) +
  scale_fill_manual(values = c("brown",'#663399'))+
  labs(x= "Arsenic levels", y = "# of samples", fill = "")+
  theme_classic(base_size = 14)+
  theme(legend.position = c(0.8, 0.9))
ggsave("img/Both bar distibution.pdf", height = 4, width = 3.5)

ggplot(data = single_all_data_EJ[single_all_data_EJ$GEOID %in% both,], aes(x = log10(Arsenic*1000))) +
  geom_density(aes(color=Type))+
  labs(x= "Arsenic concentration log10(ppb)", y = "Density", color="")+
  scale_color_manual(values = c("brown",'#663399'))+
  theme_classic(base_size = 14)+
  theme(legend.position = c(0.8, 0.9))
ggsave("img/Both data distibution_0301.pdf", height = 2.5, width = 3.5)


