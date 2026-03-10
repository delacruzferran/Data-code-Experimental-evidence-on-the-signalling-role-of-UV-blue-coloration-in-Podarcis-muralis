
# Experimental evidence on the signaling role of UV-blue coloration in male European wall lizards (Podarcis muralis)----

#Ferran de la Cruz*1,2,3, Shania Lam4, Ella Guilhem5, Miguel Ángel Carretero1,2,3, Guillem Pérez i de Lanuza6 & Enrique Font6

#1 CIBIO Centro de Investigação em Biodiversidade e Recursos Genéticos, InBIO Laboratório Associado, Campus de Vairão, Universidade do Porto, 4485-661 Vairão, Portugal
#2 Departamento de Biologia, Faculdade de Ciências, Universidade do Porto, 4099-002 Porto, Portugal
#3 BIOPOLIS Program in Genomics, Biodiversity and Land Planning, CIBIO, Campus de Vairão, 4485-661 Vairão, Portugal
#4 Faculty of Science, University of Amsterdam, 1098 XH Amsterdam, Netherlands
#5 Faculty of Science and Engineering, University of Toulouse III Paul Sabatier, 31400 Toulouse, France
#6 Ethology Lab, Cavanilles Institute of Biodiversity and Evolutionary Biology, University of Valencia, 46071 Valencia, Spain

#*Corresponding author: Ferran de la Cruz, delacruz.ferran@gmail.com


## Libraries and data----
library(lme4)        
library(nlme) 
library(ggplot2) 
library(lubridate)
library(hms)
library(car)
library(MuMIn)
library(tidyverse)
library(performance)
library(see)
library(effects)
library(corrplot)
library(stats)
library(Hmisc)
library(igraph)
library(ggraph)
library(pavo)

data <- read.csv2("Trials.csv", header=TRUE)
View(data)

data <- data[-c(69,70,75,87),] # Removing lizard behaving abnormally and trial with no response of the large male in 1 hour

#Coding variables
data$Time <- as_hms(paste0(data$Time, ":00"))
data$Group <- as.factor(data$Group)
data$Trial <- as.numeric(data$Trial)
data$ID <- as.factor(data$ID)
data$Rival_ID <- as.factor(data$Rival_ID)
data$Rival_trial <- as.numeric(data$Rival_trial)
data$Acclimation <- as.factor(data$Acclimation)
data$B_Bite <- as.numeric(data$B_Bite)
data$s_Foot <- as.numeric(data$s_Foot)
data$Population_B <- as.factor(data$Population_B)
data$B_aggre_enc <- as.numeric(data$B_aggre_enc)
data$s_aggre_enc <- as.numeric(data$s_aggre_enc)
data$s_submi_enc <- as.numeric(data$s_submi_enc)
data$B_submi_enc <- as.numeric(data$B_submi_enc)
data$Frequency <- as.numeric(data$Frequency)

#Setting control structures for mixed model fitting
options(contrasts=c(factor="contr.sum", ordered="contr.poly"))
control.lmer <- lmerControl(check.conv.grad=.makeCC(action ="ignore", tol=1e-6, relTol=NULL), optimizer="bobyqa", optCtrl=list(maxfun=100000))


 ## Data preparation----

# Time transformation and escalation of variables
data$Time_num <- as.numeric(data$Time) # Time can give issues in the model, so we have to transform it into a numerical variable
data$Time_z <- scale(data$Time_num)
data$Size_dif_z <- scale(data$Size_dif)
data$Duration_2_z <- scale(data$Duration_2)
data$s_aggre_z <- scale(data$s_aggre)
data$s_aggre_enc_z <- scale(data$s_aggre_enc)
data$B_aggre_z <- scale(data$B_aggre)
data$B_aggre_enc_z <- scale(data$B_aggre_enc)
data$s_submi_z <- scale(data$s_submi)
data$s_submi_enc_z <- scale(data$s_submi_enc)
data$B_submi_z <- scale(data$B_submi)
data$B_submi_enc_z <- scale(data$B_submi_enc)

# Score of aggressive behaviours
data$Score_B <- data$B_Chase+(data$B_Display*2)+(data$B_MouthG*3)+(data$B_Lunge*4)+(data$B_Bite*5)
data$Score_s <- data$s_Chase+(data$s_Display*2)+(data$s_MouthG*3)+(data$s_Lunge*4)+(data$s_Bite*5)

## Analyses----

 ### + Manipulation effect----

  # Differences in behaviour for small males depending on treatment group

c <- subset(data, data$Group=="Control")
t<- subset(data, data$Group=="Treatment")
shapiro.test(c$s_aggre_z) # H1
shapiro.test(t$s_aggre_z) # H1
wilcox.test(s_aggre_z ~ Group, data=data, exact=FALSE)

shapiro.test(c$s_submi_z) # H1
shapiro.test(t$s_submi_z) # H1
wilcox.test(s_submi_z ~ Group, data=data, exact=FALSE)


 ### + Aggressive behaviours----

  # Model for the number of aggressive behaviours displayed by the large male

mod <- lmer(B_aggre_z ~  Acclimation +  Size_dif_z +Trial + Duration_2_z + Time_z+ s_aggre_z  + B_submi_z +  s_submi_z + Group + Rival_trial + (1|ID) + (1|Rival_ID)+(1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq") 
AICc(mod)

mod <- lmer(B_aggre_z ~  Time_z+ s_aggre_z +  s_submi_z + Group  + (1|ID) + (1|Rival_ID) + (1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Final model with the lowest AICc
Anova(mod, type=3,ddf="Kenward-Roger")
summary(mod)

 # Figure 5
mu <- mean(data$B_aggre, na.rm = TRUE)
sigma <- sd(data$B_aggre, na.rm = TRUE)

df_obs <- data.frame(Group = data$Group, B_aggre = data$B_aggre)
eff <- predictorEffects(mod, ~ Group)
df_eff <- as.data.frame(eff$Group)
df_eff$fit   <- df_eff$fit   * sigma + mu
df_eff$lower <- df_eff$lower * sigma + mu
df_eff$upper <- df_eff$upper * sigma + mu

ggplot() +
  geom_jitter(data=df_obs,aes(x=Group,y=B_aggre,color="Observed data"),width=0.1,alpha=0.6, size=4) +
  geom_point(data=df_eff, aes(x=Group, y=fit, color="Model estimate"), size=6) +
  geom_errorbar(data=df_eff, aes(x=Group, ymin=lower, ymax=upper, color="Model estimate"),width=0.2, linewidth=1.4) +
  geom_line(data=df_eff, aes(x=Group, y=fit, group=1, color="Model estimate"), linewidth=1.5) +
  labs(y = "Number of aggressive behaviors (Large male)",  x = "Group (Small male)", color = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", axis.line = element_line(color = "black", linewidth = 0.8),
        axis.ticks = element_line(color = "black", linewidth = 1))


 ### + Ratio of aggression----

  # Model for the ratio of aggressive behaviours displayed by the large male 

mod <- lmer(B_aggre_enc_z ~ Acclimation +  Size_dif_z + Trial + Duration_2_z + Time_z + Rival_trial + s_aggre_enc_z + s_submi_enc_z + Group + (1|ID) + (1|Rival_ID)+(1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq")
AICc(mod)

mod <- lmer(B_aggre_enc_z ~ Time_z + s_submi_enc_z + Group + (1|ID) + (1|Rival_ID)+(1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Final model with the lowest AICc
Anova(mod, type=3,ddf="Kenward-Roger")
summary(mod)

 #Figure 5
mu <- mean(data$B_aggre_enc, na.rm = TRUE)
sigma <- sd(data$B_aggre_enc, na.rm = TRUE)

df_obs <- data.frame(Group = data$Group, B_aggre_enc = data$B_aggre_enc)
eff <- predictorEffects(mod, ~ Group)
df_eff <- as.data.frame(eff$Group)
df_eff$fit   <- df_eff$fit   * sigma + mu
df_eff$lower <- df_eff$lower * sigma + mu
df_eff$upper <- df_eff$upper * sigma + mu

ggplot() +
  geom_jitter(data=df_obs, aes(x=Group, y=B_aggre_enc, color="Observed data"), width=0.1, alpha=0.6, size=4) +
  geom_point(data=df_eff, aes(x=Group, y=fit, color="Model estimate"), size=6) +
  geom_errorbar(data=df_eff, aes(x=Group, ymin=lower, ymax=upper, color="Model estimate"),width=0.2, linewidth=1.4) +
  geom_line(data=df_eff, aes(x=Group, y=fit, group=1, color="Model estimate"), linewidth=1.5) +
  labs(y = "Ratio of aggression (Large male)",  x = "Group (Small male)", color = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", axis.line = element_line(color = "black", linewidth = 0.8),
        axis.ticks = element_line(color = "black", linewidth = 1))


 ### + Score of aggression----

  # Model for the score of aggressive behaviours displayed by the large male 

mod <- lmer(scale(Score_B) ~  Acclimation +  Size_dif_z + Trial + Duration_2_z + Time_z + Rival_trial + scale(Score_s) + s_submi_z + Group + (1|ID) + (1|Rival_ID) +(1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq") 
AICc(mod)

mod <- lmer(scale(Score_B) ~  Time_z +  s_submi_z+Group + (1|ID) + (1|Rival_ID) +(1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Final model with the lowest AICc
Anova(mod, type=3,ddf="Kenward-Roger")
summary(mod)

 # Figure 5
mu <- mean(data$Score_B, na.rm = TRUE)
sigma <- sd(data$Score_B, na.rm = TRUE)

df_obs <- data.frame(Group = data$Group, Score_B = data$Score_B)
eff <- predictorEffects(mod, ~ Group)
df_eff <- as.data.frame(eff$Group)
df_eff$fit   <- df_eff$fit   * sigma + mu
df_eff$lower <- df_eff$lower * sigma + mu
df_eff$upper <- df_eff$upper * sigma + mu

ggplot() +
  geom_jitter(data=df_obs, aes(x=Group, y=Score_B,color="Observed data"),width=0.1,alpha=0.4,size=4) +
  geom_point(data=df_eff, aes(x=Group, y=fit, color="Model estimate"), size=6) +
  geom_errorbar(data=df_eff, aes(x=Group, ymin=lower, ymax=upper, color="Model estimate"),width=0.2, linewidth=1.4) +
  geom_line(data=df_eff, aes(x=Group, y=fit, group=1, color="Model estimate"), linewidth=1.5) +
  labs(y = "Score of aggression (Large male)",  x = "Group (Small male)", color = "") +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top", axis.line = element_line(color = "black", linewidth = 0.8),
        axis.ticks = element_line(color = "black", linewidth = 1))


 ### + Latency to aggressive behaviour----

  # Model for the latency to first display an aggressive behaviour by the large male 

mod <- lmer(scale(Lat_first) ~  Acclimation +  Size_dif_z +Trial + Duration_2_z + Time_z+ s_aggre_z + s_submi_z + Group + Rival_trial + (1|ID) + (1|Rival_ID) + (1| Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq") 
AICc(mod)

mod <- lmer(scale(Lat_first) ~ Duration_2_z  + s_submi_z + (1|ID) + (1|Rival_ID) + (1| Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Final model with the lowest AICc
Anova(mod, type=3,ddf="Kenward-Roger")
summary(mod)


 ### + Frequency of aggressive behaviours----

  # Model for the frequency of aggressive behaviours by the large male 

mod <- lmer(scale(Frequency) ~  Acclimation +  Size_dif_z +Trial + Duration_2_z + Time_z+ s_aggre_z + s_submi_z+ Group + Rival_trial + (1|ID) + (1|Rival_ID) + (1| Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq") 
AICc(mod)

mod <- lmer(scale(Frequency) ~   Duration_2_z + Time_z+ s_aggre_z + s_submi_z + (1|ID) + (1|Rival_ID) + (1| Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) 
Anova(mod, type=3,ddf="Kenward-Roger") # Final model with the lowest AICc
summary(mod)


 ### + HTs----

  # Model for the time spent in the High-quality territory by the small lizard

mod <- lmer(scale(Duration_rock) ~ Group +  Time_z + Trial + Acclimation + Rival_trial + Duration_2_z + Size_dif_z + (1|ID) + (1|Rival_ID)+ (1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq")
AICc(mod)

mod <- lmer(scale(Duration_rock) ~  Duration_2_z + (1|ID) + (1|Rival_ID)+ (1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Final model with the lowest AICc
Anova(mod, type=3,ddf="Kenward-Roger")
summary(mod)


 ### + HTb----

  # Model for the time spent in the High-quality territory by both lizards

mod <- lmer(scale(Duration_both) ~ Group +  Time_z + Trial + Acclimation + Rival_trial + Duration_2_z + Size_dif_z + (1|ID) + (1|Rival_ID) + (1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Saturated model
drop1(mod, test="Chisq")
AICc(mod)

mod <- lmer(scale(Duration_both) ~    Time_z   + Duration_2_z   + (1|ID) + (1|Rival_ID) + (1|Population_B), data=data, control=control.lmer, REML=F, na.action=na.exclude) # Final model with the lowest AICc
Anova(mod, type=3,ddf="Kenward-Roger")
summary(mod)


## Spectra----

data <- read.csv2("Spectra.csv", header=TRUE)

spec<-as.rspec(data) 
specs <- procspec(spec, opt="smooth", span=0.5, fixneg="addmin")

byc <- gsub("\\.[0-9].*$", "", names(specs))[-1]
table(byc)

aggplot(specs, byc, lcol=c("lightblue3", "dark golden rod", "lightblue3", "dark golden rod" ), lty=c(1,2,2,1), lwd = 2)
legend("topright", bty="n", legend=c("UV-blue patches row", "Flank"), lwd=3, col=c("lightblue3", "dark golden rod"))
legend("bottomright",bty="n", legend=c("No manipulation", "Manipulation"), lwd=3, col=c("black"), lty=c(1,2))
