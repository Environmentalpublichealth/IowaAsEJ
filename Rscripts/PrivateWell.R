# private well data
install.packages("margins")
library("margins")

filter_data <- read.csv("As Data 2015-2020 filtered with GEOID.csv", header = T)
dim(table(filter_data$GEOID))

iowa_EJ <- read.csv("Iowa EJI from CDC.csv")

filter_data$Arsenic
filter_data$Arsenic[grep("<",filter_data$Arsenic)] <- 0.0005
filter_data$Arsenic <- as.numeric(filter_data$Arsenic)
# remove data with Arsenic level is NA
filter_data <- filter_data[-which(is.na(filter_data$Arsenic)),]

filter_data$Arsenic[is.na(filter_data$Arsenic)] # 0
# this leave 12074 data points


# add EJ by GEOID
filter_data_EJ <- merge(filter_data, iowa_EJ[, c(5,17,70,95:107,109:113)], by ="GEOID", all.x=T)
# add HUB poverty rate 
filter_data_EJ <- merge(filter_data_EJ, sample_size[,c(1,3)], by="GEOID", all.x=T)
filter_data_EJ <- merge(filter_data_EJ, iowa_HUD[,c(12,51)], by.x ="GEOID", by.y="tract_id", all.x=T)
filter_data_EJ$avg_pov_pct <- filter_data_EJ$avg_pov_rate*100

# logistic regression
filter_data_EJ$highAs <- ifelse(filter_data_EJ$Arsenic < 0.01, 0, 1)
lm2 <- glm(data=filter_data_EJ, highAs~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE
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
  scale_color_gradient2(low = "brown", mid = "brown", high = "grey", midpoint = .05,limits=c(0,0.5), breaks=c(0,0.05,0.1,0.2,0.5))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="odds ratio",color="p-value")
ggsave("img/OR_PrivateAll.pdf",height = 7, width = 6)

# logistic regression for tracts has private well only
lm2 <- glm(data=single_all_data_EJ[single_all_data_EJ$GEOID %in% private,], highAs~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE
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
  scale_color_gradient2(low = "brown", mid = "grey", high = "grey", midpoint = 0.5,limits=c(0,1), breaks=c(0,0.05,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="odds ratio",color="p-value")
ggsave("img/OR_PrivateOnly.pdf",height = 7, width = 6)


lm3 <- glm(data=single_all_data_EJ[single_all_data_EJ$GEOID %in% private,], log(Arsenic)~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE
       ) 
summary(lm3)
hist(resid(lm3))
BIC(lm3)
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
  scale_color_gradient2(low = "brown", mid = "grey", high = "grey", midpoint = .5,limits=c(0,1), breaks=c(0,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var)[-c(1,4,5,7,8,12)])+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14), axis.title.x = element_text(size=14))+
  labs(x = "", y="Average Marginal effects",color="p-value")
ggsave("img/AME_PrivateOnly_0301.pdf",height = 4, width = 6)



lm4 <- glm(formula = log(Arsenic) ~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+
             EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE, data = filter_data_EJ) 
summary(lm4)
hist(resid(lm4))
BIC(lm4)
AME_data <- as.data.frame(margins_summary(lm4))
ggplot(AME_data, aes(x=factor, y=AME, color=p)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype="dotted")+
  scale_color_gradient2(low = "brown", mid = "grey", high = "grey", midpoint = .5,limits=c(0,1), breaks=c(0,0.05,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="Average Marginal effects",color="p-value")
ggsave("img/AME_PrivateAll.pdf",height = 7, width = 6)

# combine PWS and private data
names(sample_data_clean_EJ)[12] <- "Arsenic"
all_data_EJ <- rbind(sample_data_clean_EJ[,c(1,12,23:37)], filter_data_EJ[,c(1,9,21:35)])
all_data_EJ$Type <- c(rep("Public",1490),rep("Private",12067))

# logistic regression
all_data_EJ$highAs <- ifelse(all_data_EJ$Arsenic < 0.01, 0, 1)
lm5 <- glm(data=single_all_data_EJ[single_all_data_EJ$GEOID %in% both,], highAs~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE
           ,family = "binomial")
summary(lm5)
OR_data <- as.data.frame(exp(cbind(OR = coef(lm5), confint(lm5))))
OR_data$var <- rownames(OR_data)
OR_data$pvalue <- summary(lm5)$coefficients[,4]
names(OR_data) <- c("OR","lower","upper","var","Pvalue")

dependent_var <- c("Uninsured","Unemployed","Renter","Poverty","No Internet",
                   "No High School Degree","Mobile House","Minority","Limited English",
                   "House burden","Disable",">Age 65","<Age 17")

ggplot(OR_data[-1,], aes(x=var, y=OR, color=Pvalue)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 1, linetype="dotted")+
  scale_color_gradient2(low = "#0066CC", mid = "grey", high = "grey", midpoint = .5,limits=c(0,0.5), breaks=c(0,0.05,0.1,0.2,0.5))+
  scale_x_discrete(labels=rev(dependent_var))+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14))+
  labs(x = "", y="odds ratio",color="p-value")
ggsave("img/OR_combineBoth.pdf",height = 7, width = 6)

# linear regression
lm5 <- glm(data=single_all_data_EJ[single_all_data_EJ$GEOID %in% both,], log(Arsenic)~ EP_NOHSDP+EP_UNINSUR+EP_MINRTY+EP_POV200+EP_UNEMP+EP_RENTER+EP_HOUBDN+EP_AGE65+EP_AGE17+EP_DISABL+EP_NOINT+EP_LIMENG+EP_MOBILE) 
summary(lm5)
hist(resid(lm5))
BIC(lm5)

AME_data <- as.data.frame(margins_summary(lm5))

ggplot(AME_data[-c(1,4,5,7,8,12),], aes(x=factor, y=AME, color=p)) + 
  geom_point()+
  geom_errorbar(aes(ymin=lower, ymax=upper))+
  coord_flip()+
  geom_hline(yintercept = 0, linetype="dotted")+
  scale_color_gradient2(low = "#0066CC", mid = "grey", high = "grey", midpoint = .5,limits=c(0,1), breaks=c(0,0.1,0.2,0.5,1))+
  scale_x_discrete(labels=rev(dependent_var)[-c(1,4,5,7,8,12)])+
  theme_classic(base_size = 10)+
  theme(axis.text.y = element_text(size = 14), axis.title.x = element_text(size=14))+
  labs(x = "", y="Average Marginal effects",color="p-value")
ggsave("img/AME_both.pdf",height = 4, width = 6)

lm5 <- lm(data=all_data_EJ, log(Arsenic+0.0005)~ RPL_SVM, weights = Weights) 
summary(lm5)
hist(resid(lm5))
BIC(lm5)

ggplot(all_data_EJ, aes(x=RPL_SVM, y = Arsenic))+
  geom_point()+
  theme_classic()+
  labs(x="SV metrics", y ="Arsenic (ppm)")

ggplot(filter_data_EJ, aes(x=EP_POV200, y = Arsenic))+
  geom_point()+
  theme_classic()

dim(all_data_EJ[all_data_EJ$RPL_SVM <= 0.7,]) # 343/13564 2.5%
dim(sample_data_clean_EJ[sample_data_clean_EJ$RPL_SVM <= 0.7,]) # 150/1490 10%
dim(filter_data_EJ[filter_data_EJ$EP_NOHSDP <= 20,]) # 193/12074 1.6%
dim(iowa_EJ[iowa_EJ$EP_NOHSDP > 10,]) # 101/825 12%

### sample distribution
filter_data_EJ$highAs <- as.factor(filter_data_EJ$highAs)
ggplot(data = filter_data_EJ, aes(x = highAs)) +
  geom_bar(color = "brown", fill="brown")+
  scale_x_discrete(labels=c("0" = "Low", "1" = "High")) +
  labs(x= "Arsenic levels", y = "# of samples")+
  theme_classic(base_size = 14)
ggsave("img/Private bar distibution.pdf", height = 4, width = 3)

ggplot(data = filter_data_EJ, aes(x = Arsenic*1000)) +
  geom_density(color = "brown")+
  labs(x= "Arsenic concentration (ppb)", y = "Density")+
  theme_classic(base_size = 14)
ggsave("img/Private data distibution.pdf", height = 4, width = 3)
