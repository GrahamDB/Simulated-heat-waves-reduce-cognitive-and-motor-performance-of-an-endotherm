#####################
#Analyses of behavioral trials

#R script to accompany Danner, Coomes, and Derryberry. Simulated heat waves reduce cognitive and motor performance of an endotherm. Proceedings of the Royal Society B. 
#####################

#####################
#Table of contents

#I. Color Association
  #1. Time to finish trial 
  #2. Time between flips
  #3. Time to husk seeds
  #4. Latency to begin task
  #5. Accuracy of color association
  #6. Missed food rewards
  
#II. Detour Reaching
  #1. Number of trials
#####################

#####################
#I. Color Association
#####################
library(nlme)
library(lme4)
library(dplyr)

drop.levels <- function (dat) {
  if (is.factor(dat)) dat <- dat[, drop = TRUE] else dat[] <-
      lapply(dat, function(x) x[, drop = TRUE]);
  return(dat) ;}; #define the function to drop removed levels (i.e. individuals of unknown sex)

mods<-list()
color_trials <- 
  read.csv("color_association_thermal_trials.csv", 
           header=TRUE) %>% 
  mutate(Date=as.Date(Date, format = "%m/%d/%y", origin = "1899-12-30")) %>%
  mutate(fExam.temp = Exam.temp)

# Convert time during video to time format.
time_cols <- 
  c("Cage.door.closed","Touch.tray",
    "cor.col.1", "cor.col.2", "cor.col.3",
    "cor.col.4", "cor.col.5", "cor.col.6",
    "cor.col.7", "cor.col.8", "cor.col.9",
    
    "inc.col.1", "inc.col.2", "inc.col.3",
    "inc.col.4", "inc.col.5", "inc.col.6",
    "inc.col.7", "inc.col.8", "inc.col.9")
color_trials[, time_cols] <- 
  as.POSIXct(as.character(unlist(color_trials[ , time_cols])), 
             tz="EST", format="%H:%M:%S")

#Then subtract times to get intervals. 
dif_cols <- paste0("c.dif",1:8)
color_trials <-
  color_trials %>% 
  mutate(
latency= Touch.tray - Cage.door.closed,
finish= cor.col.9 - Touch.tray,
c.dif1= cor.col.2 - cor.col.1,
c.dif2= cor.col.3 - cor.col.2,
c.dif3= cor.col.4 - cor.col.3,
c.dif4= cor.col.5 - cor.col.4,
c.dif5= cor.col.6 - cor.col.5,
c.dif6= cor.col.7 - cor.col.6,
c.dif7= cor.col.8 - cor.col.7,
c.dif8= cor.col.9 - cor.col.8 )

color_trials$c.dif.mean <- 
  apply(color_trials %>% select(one_of(dif_cols)), MARGIN=1, FUN=mean, na.rm=TRUE)
color_trials$c.dif.sd <- 
  apply(color_trials %>% select(one_of(dif_cols)), MARGIN=1, FUN=sd, na.rm=TRUE)
color_trials$c.dif.max <- 
  apply(color_trials %>% select(one_of(dif_cols)), MARGIN=1, FUN=max, na.rm=TRUE)

color_trials <-
  color_trials %>% filter(Finish. == "Yes") %>%
  drop.levels() %>% 
  mutate(fExam.temp = Exam.temp) # drop removed levels

# Foraging efficiency data 

forage <- 
  read.csv("color_association_foraging_efficiency.csv", 
           header=TRUE)  %>%
  mutate(birddate=paste(Bird, Date, sep=""),
         fTemp.set=factor(Temp.set))
# convert time measurements to time difference
time_cols_f <- c("Seed.1.duration","Seed.2.duration", "Seed.3.duration")
forage[, time_cols_f] <- 
  as.POSIXct(as.character(unlist(forage[ , time_cols_f])), 
             tz="EST", format="%H:%M:%S") -
  as.POSIXct("0:00:00",tz="EST", format="%H:%M:%S") 

forage$Seed.duration.mean <- 
  apply(forage %>% select(one_of(time_cols_f)), MARGIN=1, FUN=mean, na.rm=F)

#1. Time to finish trial

mods$ttfin_nlme <-list()
# summary(mods$ttfin_glm$Pant <-lm(finish ~ Pant.during.trial + Bird, data=d4))
# summary(mods$ttfin_glm$temp <-lm(finish ~ fExam.temp + Bird, data=d4))
# summary(mods$ttfin_glm$const <-lm(finish ~  Bird, data=d4))
summary(mods$ttfin_nlme$temp <-lme(finish ~ fExam.temp,random = ~1|Bird, data=color_trials, method = "ML"))
summary(mods$ttfin_nlme$const <-lme(finish ~ 1,random = ~1|Bird, data=color_trials, method = "ML"))
# summary(mods$ttfin_lme4$temp <-lme4::lmer(finish ~ fExam.temp+(1|Bird), data=d4,REML=F))
# summary(mods$ttfin_lme4$const <-lme4::lmer(finish ~ 1+(1|Bird), data=d4,REML=F))
# print(with(mods$ttfin_glm,anova(const,temp)))
print(with(mods$ttfin_nlme,anova(const,temp)))
# print(with(mods$ttfin_lme4,anova(const,temp)))

# # #Figure 1A:
# quartz(width=8.7/2.54, title="Draft Figure [DROP1]")
# par(mfrow=c(3,1))
# par(oma=c(0,0,0,0))
# par(mar=c(2.5,4,0.1,0.1) + 0.1) #c(bottom, left, top, right)
# 
# plot(finish ~ Pant.during.trial, data=d4, ylab="Time to finish trial (seconds)", 
#      xlab="", xaxt="n", col=c("blue", "red"))
# axis(side=1, at=c(1, 2), labels=c("No", "Yes"))
# legend("topleft", "A", bty="n")

#2. Time between flips
# mods$ttflip_glm <-list()
# mods$ttflip_lme4 <-list()
mods$ttflip_nlme <-list()
# summary(mods$ttflip_glm$pant<- lm(c.dif.mean ~ Pant.during.trial + Bird, data=d4 %>% filter(is.finite(c.dif.mean))))
# summary(mods$ttflip_glm$temp <- lm(c.dif.mean ~ fExam.temp + Bird, data=d4 %>% filter(is.finite(c.dif.mean))))
# summary(mods$ttflip_glm$const <- lm(c.dif.mean ~ 1 + Bird, data=d4 %>% filter(is.finite(c.dif.mean))))
summary(mods$ttflip_nlme$temp <-
          lme(c.dif.mean ~ fExam.temp,random = ~1|Bird, 
              data=color_trials %>% filter(is.finite(c.dif.mean)), method = "ML"))
summary(mods$ttflip_nlme$const <-
          lme(c.dif.mean ~ 1,random = ~1|Bird, 
              data=color_trials %>% filter(is.finite(c.dif.mean)), method = "ML"))
# print(with(mods$ttflip_glm,anova(const,temp)))
print(with(mods$ttflip_nlme,anova(const,temp)))
# #Figure 1B:
# plot(c.dif.mean ~ Pant.during.trial, data=d4, ylab="Time between lids (seconds)", 
#      xlab="", xaxt="n", col=c("blue", "red"))
# axis(side=1, at=c(1, 2), labels=c("No", "Yes"))
# legend("topleft", "B", bty="n")

#3. Time to husk seeds
# mods$tthusk_glm <-list()
# mods$tthusk_lme4 <-list()
mods$tthusk_nlme <-list()
# f_husk <- f %>% filter(is.finite(Seed.duration.mean))
f_husk <- forage %>% filter(is.finite(Seed.duration.mean))
# summary(mods$tthusk_glm$pant <- lm(Seed.duration.mean ~ Pant + Bird, data=f_husk))
# summary(mods$tthusk_glm$temp <- lm(Seed.duration.mean ~ fTemp.set + Bird, data=f_husk))
# summary(mods$tthusk_glm$const <- lm(Seed.duration.mean ~ Bird, data=f_husk))
summary(mods$tthusk_nlme$temp <-
          lme(Seed.duration.mean ~ fTemp.set,random = ~1|Bird, data=f_husk, method = "ML"))
summary(mods$tthusk_nlme$const <-
          lme(Seed.duration.mean ~ 1,random = ~1|Bird, data=f_husk, method = "ML"))
# print(with(mods$tthusk_glm,anova(const,temp)))
print(with(mods$tthusk_nlme,anova(const,temp)))
print(summary(mods$tthusk_nlme$temp)$tTable)
# print(summary(mods$tthusk_glm$temp)$coefficients)

## XXX Latency to begin
mods$latency_glm <-list()
mods$latency_lme4 <-list()
mods$latency_nlme <-list()
d_latency<- d4 %>% filter(is.finite(latency))
summary(mods$latency_glm$temp <-lm(latency ~ fExam.temp + Bird, data=d_latency))
summary(mods$latency_glm$const <-lm(latency ~  Bird, data=d_latency))
summary(mods$latency_nlme$temp <-lme(latency ~ fExam.temp,random = ~1|Bird, data=d_latency, method = "ML"))
summary(mods$latency_nlme$const <-lme(latency ~ 1,random = ~1|Bird, data=d_latency, method = "ML"))
print(with(mods$latency_glm,anova(const,temp)))
print(with(mods$latency_nlme,anova(const,temp)))

#Figure 1C:
par(mar=c(4,4,0.1,0.1) + 0.1) #c(bottom, left, top, right) 
boxplot(Seed.duration.mean ~ Pant, data=f, xlab="Sign of thermal stress", 
        ylab="Time to eat one seed (sec)", yaxt="n", xaxt="n", col=c("blue", "red"))
axis(2, at=sort(unique(round(f$Seed.duration.mean))), labels=c(1,2,3))
axis(side=1, at=c(1, 2), labels=c("No", "Yes"))
legend("topleft", "C", bty="n")

#4. Accuracy of color association
mods$acc_glm <-list()
mods$acc_lme4 <-list()
mods$acc2_lme4 <-list()
mods$acc_nlme <-list()
summary(mods$acc_glm$pant <- 
          glm(Error.ratio ~ Pant.during.trial + Bird, data=d4, 
              family = binomial, weights=rep(9,nrow(d4))))
summary(mods$acc_glm$temp <- 
          glm(Error.ratio ~ fExam.temp + Bird, data=d4, 
             family = binomial, weights=rep(9,nrow(d4))))
summary(mods$acc_glm$const <- 
          glm(Error.ratio ~ Bird, data=d4, 
             family = binomial, weights=rep(9,nrow(d4))))
summary(mods$acc_lme4$temp <-
          lme4::glmer(Error.ratio ~ fExam.temp+(1|Bird), data=d4,
                      family = binomial, weights=rep(9,nrow(d4))))
summary(mods$acc_lme4$const <-
          lme4::glmer(Error.ratio ~ 1+(1|Bird), data=d4,
                      family = binomial, weights=rep(9,nrow(d4))))
summary(mods$acc2_lme4$temp <-
          lme4::glmer(cbind(Error.count,9-Error.count) ~ fExam.temp+(1|Bird), 
                      data=d4 %>% mutate(Error.count=Error.ratio*9),
                      family = binomial))
summary(mods$acc2_lme4$const <-
          lme4::glmer(cbind(Error.count,9-Error.count) ~ 1+(1|Bird), 
                      data=d4 %>% mutate(Error.count=Error.ratio*9),
                      family = binomial))
# summary(mods$acc_nlme$temp <-lme(Error.ratio ~ fExam.temp,random = ~1|Bird, data=d4, method = "ML"))
# summary(mods$acc_nlme$const <-lme(Error.ratio ~ 1,random = ~1|Bird, data=d4, method = "ML"))
print(with(mods$acc_glm,anova(const,temp)))
print(with(mods$acc_lme4,anova(const,temp)))
print(with(mods$acc2_lme4,anova(const,temp)))
summary(mods$acc2_lme4$temp)
 #Figure 2A:
quartz(width=8.7/2.54, title="Draft Figure 2 [REWORK]")
par(mfrow=c(2,1))
par(oma=c(0,0,0,0))
par(mar=c(2.5,4.1,0.2,0.1) + 0.1) #c(bottom, left, top, right)
par(cex=1)
boxplot(Error.ratio ~ Pant.during.trial, data=d4, ylab="Error ratio", 
        xaxt="n", col=c("blue", "red"), cex=1, cex.axis=1, cex.lab=1)
axis(side=1, at=c(1, 2), labels=c("", ""), cex.axis=1)
axis(side=1, at=c(1, 2.05), tick=FALSE, labels=c("No", "Yes"), cex.axis=1) 
legend("topleft", "A", bty="n", cex=1)

#5. Missed food rewards
mods$seed_glm <-list()
mods$seed_lme4 <-list()
mods$seed_nlme <-list()
f_seeds <- f %>% filter(is.finite(Num.seeds.eaten))
summary(mods$seed_glm$pant <-lm(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ Pant + Bird, data=f_seeds, family= binomial)) 
summary(mods$seed_glm$temp <-lm(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ fTemp.set + Bird, data=f_seeds)) 
summary(mods$seed_glm$const <-lm(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ Bird, data=f_seeds)) 
summary(mods$seed_lme4$temp <-
          lme4::glmer(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ fTemp.set+( 1|Bird), 
                data=f_seeds, family=binomial))
summary(mods$seed_lme4$const <-
          glmer(cbind(Num.seeds.eaten,3-Num.seeds.eaten) ~ 1+( 1|Bird), 
                data=f_seeds, family=binomial))
print(with(mods$seed_lme4,anova(const,temp)))

#Figure 2B:
par(mar=c(4,4.1,0.1,0.1) + 0.1) #c(bottom, left, top, right) 
boxplot(Num.seeds.eaten ~ Pant, data=f, xlab="Sign of thermal stress",
        ylab="Number of seeds eaten", yaxt="n", xaxt="n", col=c("blue", "red"), cex=1, cex.axis=1, cex.lab=1)
axis(2, at=sort(unique(round(f$Num.seeds.eaten))), labels=c(0,1,2,3), cex.axis=1)
axis(side=1, at=c(1, 2), labels=c("", ""), cex.axis=1)
axis(side=1, at=c(1, 2.05), tick=FALSE, labels=c("No", "Yes"), cex.axis=1) 
legend("topleft", "B", bty="n", cex=1)

#########################
#End I. Color Association
#########################

####################
#II. Detour Reaching
####################  
 
## Missing???
library(dplyr)
detour_details <- read.csv("Detour Reaching Thermal Trials/Detour Reaching Thermal Trials Notes.csv", header=TRUE)
length(unique(detour_details$Bird))
names(detour_details)
print(with(detour_details %>% filter(Pass..y.n. %in% "y") %>%
             mutate(HDB=paste0(Pant..y.n.,
                               substr(Stand.tall..y.n.,1,1),
                               substr(Spread.wings.y.n.,1,1))),
           table(Trial.type,HDB,Bird)),zero.print=".")


#1. Number of trials
detour <- read.csv("Detour Reaching Thermal Trials/Detour Reaching Thermal Trials Results (for Dryad).csv", header=TRUE)
detour$Date <- as.Date(detour$Date, format = "%m/%d/%y", origin = "1899-12-30") 
detour$Trial.temp <- c(Ambient=22, High=44)[as.character(detour$Trial.type)]
library("nlme")
# m1 <- lme(Num.trials ~ Trial.type, random = ~ 1|Ind, data=detour)
mods$detour <-list()
mods$detour$temp <- glmer(Num.trials ~ Trial.type+( 1|Ind),
                          data=detour, family=poisson)
mods$detour$const <- glmer(Num.trials ~ 1+( 1|Ind),
                          data=detour, family=poisson)
summary(mods$detour$temp)
with(mods$detour,anova(const,temp))
library("r2glmm")
r2beta(m1, method="sgv")

 #Figure 3:
quartz(width=8.7/2.54, height=8.7/2.54, title="Draft Figure 4")
par(mfrow=c(1,1))
par(oma=c(0,0,0,0))
par(mar=c(4.1,4.1,0.2,0.1) + 0.1) #c(bottom, left, top, right)

plot(Num.trials ~ rep(c(1,2), length(detour$Num.trials)/2), data=detour, 
     ylab="Number of trials", xlab="Trial temperature (ºC)", xlim=c(0.75, 2.25), xaxt="n", type="n")
axis(side=1, at=c(1, 2), labels=c("22", "44"))
for(p in unique(detour$Ind)){
		g <- subset(detour, Ind == p)
		lines(x=c(g[1, 2], g[2, 2]), y=c(g[1, 5], g[2, 5]), col="gray", type="l")
		}
 #Add means and s.e.
library("Hmisc")
errbar(x=1, y=6.67, 6.67+1.13, 6.67-1.13, add=TRUE, errbar.col="blue", col="blue", cex=1.5)

library("Hmisc")
errbar(x=2, y=9.67, 9.67+0.96, 9.67-0.96, add=TRUE, errbar.col="red", col="red", cex=1.5)

m2 <- lme(Num.trials ~ Sess.num, random = ~ 1|Ind, data=detour)
summary(m2)
mods$detour$sess <- glmer(Num.trials ~ Sess.num+( 1|Ind),
                          data=detour, family=poisson)
summary(mods$detour$sess)
with(mods$detour,anova(const,sess))

########################
#End II. Detour Reaching
########################  
