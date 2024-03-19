remotes::install_github("jtextor/dagitty/r")
library(dagitty)


g <- dagitty('dag {
    Pov [pos="0,1"]
    Edu [pos="1,2"]
    Ins [pos="2,1"]
    Minority [pos="1,0"]
    Emp [pos="1,1"]
    Age65 [pos="3,0"]
    As [pos="3,2"]
    Cancer [pos="2,3"]
      

    Minority -> As
    Minority -> Cancer
    Pov -> As
    Pov -> Cancer
    Edu -> As
    Edu -> Cancer
    Emp -> As
    Emp -> Cancer
    Ins -> As
    Ins -> Cancer
    Age65 -> As
    Age65 -> Cancer
    As -> Cancer
    
Minority -> Pov
Minority -> Edu
Minority -> Emp
Minority -> Ins
Pov -> Edu
Pov -> Ins
Pov -> Emp
Edu -> Pov
Edu -> Emp
Edu -> Ins
Emp -> Ins
Age65 -> Pov
}')
plot(g)
impliedConditionalIndependencies(g)

Welldata <- filter_data_EJ %>% 
  dplyr::select(Arsenic, RPL_SVM, EP_MINRTY, EP_POV200, EP_NOHSDP, EP_UNEMP, EP_UNINSUR, EP_AGE65, 
         EP_BPHIGH, EP_ASTHMA, EP_CANCER, EP_MHLTH, EP_DIABETES) %>%
  na.omit()

names(Welldata) <- c("As","SVM","Minority","Pov","Edu","Emp","Ins","Age65","HighBP","Asthma","Cancer","Mental","Diabetes")

localTests( g, Welldata, type="cis")

library(lavaan)
corr <- lavCor(Welldata)
localTests(g, Welldata)

model.BP <- lm(HighBP ~ SVM+As, data = Welldata)
hist(resid(model.BP))

ggplot(data = Welldata, aes(x = Diabetes, y = As))+
  geom_point()+
  geom_smooth(method = "loess")+
  theme_bw()

## mediation analysis
install.packages("mediation")
library(mediation)
library(flowCore)

model.M <- lm(Arsenic ~ EP_MINRTY, data = all_data_EJ)
hist(resid(model.M))
summary(model.M)
model.Y <- lm(EP_DIABETES~ EP_MINRTY+Arsenic, data = all_data_EJ)
summary(model.Y)
hist(resid(model.Y))
resultDiabetes <- mediate(model.M, model.Y, treat="EP_MINRTY", mediator="Arsenic",
                   boot=TRUE, sims=500)
summary(resultDiabetes)
plot_model

# interaction effect plots
install.packages("sjPlot")
library(sjPlot)

lm <- lm(EP_DIABETES ~ Arsenic*EP_MINRTY*Private, data = all_data_EJ)
summary(lm)
plot_model(lm, type = "int")

all_data_EJ$As_breaks <- cut(all_data_EJ$Arsenic,breaks = c(0.0001,0.001,0.0038,0.01,0.1,0.5))
all_data_EJ$EP_MINRTY_cut <- cut(all_data_EJ$EP_MINRTY, breaks = c(0.3, 2.9, 4.7, 7.1, 68.2))
all_data_EJ$EP_POV200_cut <- cut(all_data_EJ$EP_POV200, breaks = c(5.7,19.9 ,23.7,30.0,76.9))
all_data_EJ$EP_UNINSUR_cut <- cut(all_data_EJ$EP_UNINSUR, breaks = c(0.2,2.3,3.6,4.9,33.1))

single_all_data_EJ$Private <- as.factor(single_all_data_EJ$Private)
single_all_data_EJ$As_ppb <- single_all_data_EJ$Arsenic*1000
single_all_data_EJ$Type <- as.character(single_all_data_EJ$Type)
lm <- lm(EP_DIABETES ~ As_ppb*EP_POV200*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_POV200[5.71,23.6,76.9]", "Type"))+
  labs(title = "", y="Diabetes %", color="Poverty %", x = "Arsenic (ppb)")+
  xlim(0,200)+ylim(-20,100)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/Diabetes-Pov0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_DIABETES ~ As_ppb*EP_MINRTY*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_MINRTY[0.3,4.7,68.2]", "Type"))+
  labs(title = "", y="Diabetes %", color="Minority %", x = "Arsenic (ppb)")+
  xlim(0,200)+ ylim(0,90)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/Diabetes-minorty0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_DIABETES ~ As_ppb*EP_UNINSUR*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_UNINSUR[0.2,4.2,33.1]", "Type"))+
  labs(title = "", y="Diabetes %", color="Uninsured %", x = "Arsenic (ppb)")+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  xlim(0,200)+ylim(-20,180)+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/Diabetes-uninsur0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_BPHIGH ~ As_ppb*EP_POV200*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_POV200[5.71,23.6,76.9]", "Type"))+
  labs(title = "", y="High blood pressure %", color="Poverty %", x = "Arsenic (ppb)")+
  xlim(0,200)+ylim(-50,250)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/HiBP-Pov0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_BPHIGH ~ As_ppb*EP_MINRTY*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_MINRTY[0.3,4.7,68.2]", "Type"))+
  labs(title = "", y="High blood pressure %", color="Minority %", x = "Arsenic (ppb)")+
  xlim(0,200)+ylim(0,180)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/HiBP-minorty0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_BPHIGH ~ As_ppb*EP_UNINSUR*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_UNINSUR[0.2,4.2,33.1]", "Type"))+
  labs(title = "", y="High blood pressure %", color="Uninsured %", x = "Arsenic (ppb)")+
  xlim(0,200)+ylim(-50,500)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/HiBP-uninsur0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_CANCER ~ As_ppb*EP_POV200*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_POV200[5.71,23.6,76.9]", "Type"))+
  labs(title = "", y="Cancer %", color="Poverty %", x = "Arsenic (ppb)")+
  xlim(0,200)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/cancer-Pov0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_CANCER ~ As_ppb*EP_MINRTY*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_MINRTY[0.3,4.7,68.2]", "Type"))+
  labs(title = "", y="Cancer %", color="Minority %", x = "Arsenic (ppb)")+
  xlim(0,200)+ ylim(0,50)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/cancer-minorty0312.pdf", width = 4.8, height = 2.8)

lm <- lm(EP_CANCER ~ As_ppb*EP_UNINSUR*Type, data = single_all_data_EJ)
summary(lm)
plot_model(lm, type = "pred", terms = c("As_ppb", "EP_UNINSUR[0.2,4.2,33.1]", "Type"))+
  labs(title = "", y="Cancer %", color="Uninsured %", x = "Arsenic (ppb)")+
  xlim(0,200)+ylim(-20,150)+
  theme_bw()+
  scale_color_manual(values=c("#FABEAD","#C9CB8C","#74AAA5"))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
ggsave("img/cancer-uninsur0312.pdf", width = 4.8, height = 2.8)
