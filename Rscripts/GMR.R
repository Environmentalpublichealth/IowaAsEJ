# geometric mean ratio
setwd("~/Desktop/Jiali/TAMU/environment/starr/As EJ/")
library(Rmisc)
# Use the saved Rdata from previous analyzed variables
# calculate the state average for a reference value
ref_u <- exp(mean(log(iowa_EJ$EP_UNINSUR[iowa_EJ$EP_UNINSUR>0]),na.rm=T)) # uninsurance
ref_p <- exp(mean(log(iowa_EJ$EP_POV200[iowa_EJ$EP_POV200>0]))) # poverty
ref_i <- exp(mean(log(iowa_EJ$EP_NOINT[iowa_EJ$EP_NOINT>0]),na.rm=T)) # no internet
ref_h <- exp(mean(log(iowa_EJ$EP_NOHSDP[iowa_EJ$EP_NOHSDP>0]),na.rm=T)) # no high school degree
ref_d <- exp(mean(log(iowa_EJ$EP_DISABL[iowa_EJ$EP_DISABL>0]),na.rm=T)) # disable
ref_a <- exp(mean(log(iowa_EJ$EP_AGE65[iowa_EJ$EP_AGE65>0]),na.rm=T)) # > age 65
ref_m <- exp(mean(log(iowa_EJ$EP_MINRTY[iowa_EJ$EP_MINRTY>0]),na.rm=T)) # minority


# geometric mean ratio compare to state average
# split private < 10 ppb, 10 - 20ppb, > 20 ppb
private_tier1 <- filter_data_EJ[filter_data_EJ$Arsenic < 0.001,]
private_tier2 <- filter_data_EJ[filter_data_EJ$Arsenic >= 0.001 & filter_data_EJ$Arsenic < 0.01,]
private_tier3 <- filter_data_EJ[filter_data_EJ$Arsenic>=0.01,]
vars <- c("Minority","Poverty","No HSD","Unemployed","Renter","House burden","Uninsured",
          "No internet","Age65","Age17","Disablity","Limited English","Mobile home")
# loop over all variables
GMR_private <- data.frame(Var=character(0),Tier=character(0),upper=numeric(0),mean=numeric(0),lower=numeric(0))
for (i in c(23:35)) {
  ref <- exp(mean(log(iowa_EJ[which(iowa_EJ[,i+72]>0),][,i+72]),na.rm=T))
  T1 <- round(exp(CI(log(na.omit(private_tier1[private_tier1[,i]>0,][,i]/ref)),
               ci=0.95)),3)
  T2 <- round(exp(CI(log(na.omit(private_tier2[private_tier2[,i]>0,][,i]/ref)),
                     ci=0.95)),3)
  T3 <- round(exp(CI(log(na.omit(private_tier3[private_tier3[,i]>0,][,i]/ref)),
                     ci=0.95)),3)
  all <- round(exp(CI(log(na.omit(filter_data_EJ[filter_data_EJ[,i]>0,][,i]/ref)),
               ci=0.95)),3)
  GMR_private <- rbind(GMR_private,c(vars[i-22],1,T1))
  GMR_private <- rbind(GMR_private,c(vars[i-22],2,T2))
  GMR_private <- rbind(GMR_private,c(vars[i-22],3,T3))
  GMR_private <- rbind(GMR_private,c(vars[i-22],"all",all))
}
names(GMR_private) <- c("Var","Tier","upper","mean","lower")
GMR_private$Type <- "private"


GM_private <- data.frame(Var=character(0),Tier=character(0),upper=numeric(0),mean=numeric(0),lower=numeric(0))
for (i in c(23:35)) {
  ref <- exp(mean(log(iowa_EJ[which(iowa_EJ[,i+72]>0),][,i+72]),na.rm=T))
  T1 <- round(exp(CI(log(na.omit(private_tier1[private_tier1[,i]>0,][,i])),
                     ci=0.95)),3)
  T2 <- round(exp(CI(log(na.omit(private_tier2[private_tier2[,i]>0,][,i])),
                     ci=0.95)),3)
  T3 <- round(exp(CI(log(na.omit(private_tier3[private_tier3[,i]>0,][,i])),
                     ci=0.95)),3)
  all <- round(exp(CI(log(na.omit(filter_data_EJ[filter_data_EJ[,i]>0,][,i])),
                      ci=0.95)),3)
  GM_private <- rbind(GM_private,c(vars[i-22],1,T1))
  GM_private <- rbind(GM_private,c(vars[i-22],2,T2))
  GM_private <- rbind(GM_private,c(vars[i-22],3,T3))
  GM_private <- rbind(GM_private,c(vars[i-22],"all",all))
}
names(GM_private) <- c("Var","Tier","upper","mean","lower")
GM_private$Type <- "private"

# split public < 10 ppb, 10 - 20ppb, > 20 ppb
public_tier1 <- sample_data_clean_EJ[sample_data_clean_EJ$Detect < 0.001,]
public_tier2 <- sample_data_clean_EJ[sample_data_clean_EJ$Detect < 0.01 & sample_data_clean_EJ$Detect >=0.001,]
public_tier3 <- sample_data_clean_EJ[sample_data_clean_EJ$Detect >= 0.01,]

GMR_pub <- data.frame(Var=character(0),Tier=character(0),upper=numeric(0),mean=numeric(0),lower=numeric(0))
for (i in c(25:37)) {
  ref <- exp(mean(log(iowa_EJ[which(iowa_EJ[,i+70]>0),][,i+70]),na.rm=T))
  T1 <- round(exp(CI(log(na.omit(public_tier1[public_tier1[,i]>0,][,i]/ref)),
                     ci=0.95)),3)
  T2 <- round(exp(CI(log(na.omit(public_tier2[public_tier2[,i]>0,][,i]/ref)),
                     ci=0.95)),3)
  T3 <- round(exp(CI(log(na.omit(public_tier3[public_tier3[,i]>0,][,i]/ref)),
                     ci=0.95)),3)
  all <- round(exp(CI(log(na.omit(sample_data_clean_EJ[sample_data_clean_EJ[,i]>0,][,i]/ref)),
                      ci=0.95)),3)
  GMR_pub <- rbind(GMR_pub,c(vars[i-24],1,T1))
  GMR_pub <- rbind(GMR_pub,c(vars[i-24],2,T2))
  GMR_pub <- rbind(GMR_pub,c(vars[i-24],3,T3))
  GMR_pub <- rbind(GMR_pub,c(vars[i-24],"all",all))
}
names(GMR_pub) <- c("Var","Tier","upper","mean","lower")
GMR_pub$Type <-"public"

GM_pub <- data.frame(Var=character(0),Tier=character(0),upper=numeric(0),mean=numeric(0),lower=numeric(0))
for (i in c(25:37)) {
  ref <- exp(mean(log(iowa_EJ[which(iowa_EJ[,i+70]>0),][,i+70]),na.rm=T))
  T1 <- round(exp(CI(log(na.omit(public_tier1[public_tier1[,i]>0,][,i])),
                     ci=0.95)),3)
  T2 <- round(exp(CI(log(na.omit(public_tier2[public_tier2[,i]>0,][,i])),
                     ci=0.95)),3)
  T3 <- round(exp(CI(log(na.omit(public_tier3[public_tier3[,i]>0,][,i])),
                     ci=0.95)),3)
  all <- round(exp(CI(log(na.omit(sample_data_clean_EJ[sample_data_clean_EJ[,i]>0,][,i])),
                      ci=0.95)),3)
  GM_pub <- rbind(GM_pub,c(vars[i-24],1,T1))
  GM_pub <- rbind(GM_pub,c(vars[i-24],2,T2))
  GM_pub <- rbind(GM_pub,c(vars[i-24],3,T3))
  GM_pub <- rbind(GM_pub,c(vars[i-24],"all",all))
}
names(GM_pub) <- c("Var","Tier","upper","mean","lower")
GM_pub$Type <-"public"


GMR_data <- rbind(GMR_private, GMR_pub)
GMR_data$upper <- as.numeric(GMR_data$upper)
GMR_data$lower <- as.numeric(GMR_data$lower)
GMR_data$mean <- as.numeric(GMR_data$mean)

ggplot(GMR_data[-which(GMR_data$Tier == "all"),], aes(x=Tier, y=mean, group=Type, color=Type)) + 
  geom_point(aes(color = Type, shape=Type), position = position_dodge(0.5), size=1.4)+
  geom_errorbar(aes(ymin=lower, ymax=upper),position = position_dodge(0.5), width = 0.4)+
  coord_flip()+
  geom_hline(yintercept = 1, linetype="dashed",color="gray50")+
  facet_wrap(~Var,ncol = 7)+
  scale_color_manual(values=c("brown",'#663399'))+
  scale_shape_manual(values = c(1,5))+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.text = element_text(color = "black"))+
  labs(y=paste("GMR ", " 95% CI", sep = "\u00B1"))+theme(legend.position = c(0.95, 0.2))
ggsave("img/GMR by As tier 0311.pdf",height = 4, width = 8)
