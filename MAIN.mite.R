

#all needed packages
  #general
library(dplyr)
library(ggplot2)
library(tidyr)
library(glmmTMB)
library(DHARMa)
library(performance)

  #for maps
library(geodata)
library(raster)
library(sf)
library(elevatr)

#import data
indiv.data <- read.csv("ALL.INDIV.csv")
nest.data <- read.csv("ALL.NEST.csv")

#to save data set as its in R
#write.csv(nestling.cond.data, "nest.BC.data.csv", row.names = FALSE)


                        #### REPRODUCTIVE VARIABLES ####


                          ###### Nestling Number ######

#first check all variables for NAs
table(nest.data$Nestling_Number, useNA = "ifany")
table(nest.data$nest.status, useNA = "ifany")
table(nest.data$M.banding.year, useNA = "ifany")
table(nest.data$area, useNA = "ifany")       #Has NAs

#create data set 
mod.1.data <- nest.data %>%
  filter(M.species == "MOCH" | F.species == "MOCH",  #keep only MOCH
         !Nestling_Number=="0", #remove failed nests
         !area == "NA") #remove NAs
str(mod.1.data)

          ##DATA EXPLORATION
#check for outliers
boxplot(mod.1.data$Nestling_Number,  ylab = "Nestling Number")

#check for normality
hist(mod.1.data$Nestling_Number, xlim = c(0,10))
#looks kinda normal but check with shapiro test
shapiro.test((mod.1.data$Nestling_Number))
#says its not normal. count data as well so I will be using some sort of poisson glmm

#check variance = mean? (poisson assumption i think)
mean(mod.1.data$Nestling_Number) #5.263158
var(mod.1.data$Nestling_Number) #3.04386

#mean > variance so I will use compois family

          ##MODELS
#banding year random effect
mod1.year <- glmmTMB(Nestling_Number ~  nest.status + (1|M.banding.year), 
                     data = mod.1.data, family = compois)

#area random effect
mod1.area <- glmmTMB(Nestling_Number ~  nest.status +  (1|area), data = mod.1.data,
                     family = compois)

#both random effects
mod1.both <- glmmTMB(Nestling_Number ~  nest.status +  (1|area) + 
                       (1|M.banding.year), data = mod.1.data, family = compois)

#neither random effects
mod1.noRE <- glmmTMB(Nestling_Number ~  nest.status, data = mod.1.data, 
                     family = compois) 

#null model
mod1.null <- glmmTMB(Nestling_Number ~ 1, data = mod.1.data, family = compois) 

AIC(mod1.year, mod1.area, mod1.both,mod1.noRE, mod1.null)


##mod1.no.RE is the. best fit
summary(mod1.noRE)

          ##INTERPRETATION

# log(Nestling number) = 1.69562 -0.19459(infected)

logNNinf <- 1.69562 - 0.14870*(1)
NNinf <- exp(logNNinf)

logNNuninf <- 1.69562 -0.14870*(0)
NNuninf <- exp(logNNuninf)

#get prob difference between M and F MOCH
NNinf/NNuninf # = 0.8618276

(1 - 0.8618276)*100

# There are 13.81724% less nestlings in infected nests than uninfected nests


meandata1 <- mod.1.data %>%
  filter(nest.status == "1")
mean(meandata1$Nestling_Number)
#average Nestling number for infected nests = 4.5

meandata0 <- mod.1.data %>%
  filter(nest.status == "0")
mean(meandata0$Nestling_Number)
#average nestling number for uninfected nests = 5.47

            ##ASSUMPTIONS

simulationOutput1 <- DHARMa::simulateResiduals(mod1.noRE)
plot(simulationOutput1)
#all looks good!

#test dispersion assumption for compois
#test model with and without dispersion formula (taking into account different dispersion across x )

dispcheck <- glmmTMB(Nestling_Number ~  nest.status, data = mod.1.data, family = compois, dispformula = ~ nest.status)
AIC(mod1.noRE, dispcheck)

#the AICs are not very different so I will not add the formuala

           ##PLOT
library(ggsignif)


mod.1.data %>%
  ggplot(aes(x = as.factor(nest.status), y = Nestling_Number, fill = as.factor(nest.status)))+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.5)+
  #labels
  labs( x = "Nest Infection Status", y = "Nestling Number", 
        title = "Parents Infection Status vs Nestling Number")+
  #change size of axis labels
  theme_grey(base_size = 10)+
  #change colors for boxes 
  scale_fill_manual(values = c("lightblue","salmon"))+
  #remove legend for fill
  guides(fill = "none")+
  #change x axis labels
  scale_x_discrete(labels = c("Uninfected", "Infected"))+
  #change y axis values
  scale_y_continuous(breaks=c(0,2,4,6,8,10), limits = c(0,10))+
  theme_classic()
  





                            ###### Clutch Size #######



#add column for nest status
#nest.data$nest.status <- as.factor(ifelse(nest.data$Num.mite.per.nest > 0, 1, 0))

# check all variables for NAs
table(nest.data$Egg_Number, useNA = "ifany")
table(nest.data$nest.status, useNA = "ifany")
table(nest.data$M.banding.year, useNA = "ifany")
table(nest.data$area, useNA = "ifany")
table(nest.data$elevation.m, useNA = "ifany")
table(nest.data$julian.CI.date, useNA = "ifany")


#remove NAs etc

#for removal of julian date NAs
##egg.num.data <- nest.data %>%
#  filter(!Egg_Number == "0", !Egg_Number == "", !Egg_Number == "NA",!area == "NA",M.species == "MOCH",!julian.CI.date == "NA")

egg.num.data <- nest.data %>%
    filter(!Egg_Number == "0", !Egg_Number == "", !Egg_Number == "NA",!area == "NA",M.species == "MOCH")

table(nest.data$M.banding.year)
nest.data %>%
  ggplot(aes(x = nest.status, y = Egg_Number))+
  geom_boxplot()

#if variance is less than mean, use compois 
mean(egg.num.data$Egg_Number) #6.612903
var(egg.num.data$Egg_Number) #1.940092

#banding year and area
egg.num.lm.1 <- glmmTMB(Egg_Number ~  nest.status + (1|M.banding.year) + (1|area), data = egg.num.data, family = compois)

#banding year
egg.num.lm.2 <- glmmTMB(Egg_Number ~  nest.status + (1|M.banding.year), data = egg.num.data, family = compois)

#banding year and elevation
egg.num.lm.3 <- glmmTMB(Egg_Number ~  nest.status + (1|M.banding.year) + elevation.m, data = egg.num.data, family = compois)

#banding year, area, and julian date
egg.num.lm.julian <- glmmTMB(Egg_Number ~  nest.status + (1|M.banding.year) + (1|area) + julian.CI.date, data = egg.num.data, family = compois)


#NULL
egg.num.lm.NULL <- glmmTMB(Egg_Number ~  1, data = egg.num.data, family = compois)
 

AIC(egg.num.lm.1,egg.num.lm.2,egg.num.lm.3,egg.num.lm.NULL)

#this is the best model ##significant too
summary(egg.num.lm.2)

###interpretation 

# log(egg number) = 1.91070 -0.09517(infected)

logENinf <- 1.91070 - 0.09517*(1)
ENinf <- exp(logENinf)

logENuninf <- 1.91070 -0.09517*(0)
ENuninf <- exp(logENuninf)

#get prob difference between M and F MOCH
ENinf/ENuninf # = 0.9092184

(1 - 0.9092184)*100
# 9.07816

#nests where one or both parents are infected with mites lay ~9.08 % less eggs than uninfected nests

            #graph#

egg.num.data %>%
  ggplot(aes(x = as.factor(nest.status), y = Egg_Number, fill = as.factor(nest.status))) +
  labs(x = "Parent(s) infection status", y = "Clutch Size", title = "Parents Infection Status vs Clutch Size")+
  geom_boxplot()+
  geom_point(position = "jitter")+
  #change colors for boxes 
  scale_fill_manual(values = c("lightblue","salmon"))+
  #remove legend for fill
  guides(fill = "none")+
  #change x axis labels
  scale_x_discrete(labels = c("Uninfected", "Infected"))+
  #change y axis values
  scale_y_continuous(breaks=c(1,3,5,7,9,11), limits = c(1,11))+
  theme_classic()




#### do egg analysis again for female infected 
egg.num.f.data <- egg.num.data %>%
  filter(M.mite.status == "0")
egg.num.lm.F <- glmmTMB(Egg_Number ~  F.mite.status , data = egg.num.f.data, family = compois)

summary(egg.num.lm.F)

#for male infected
egg.num.m.data <- egg.num.data %>%
  filter(F.mite.status == "0") 

egg.num.lm.M <- glmmTMB(Egg_Number ~  M.mite.status ,data = egg.num.m.data, family = compois)

summary(egg.num.lm.M)
##no effect when female is infected but effect when male is infected?


                      ##### Nestling SMI #####


###calculating SMI 
nestling.data <- read.csv("ALL.NESTLING.csv")
#write.csv(nestl.BC.analysis.data, "ALL.MOCH.NESTLING.csv", row.names = FALSE)

#filter data and such

nestl.BC.data <- nestling.data %>%
  filter(Species == "MOCH", Banding.Year >= 2023) %>%
  mutate(across(c(Tarsus.Length, Bird.Weight), as.numeric)) 

#check for outliers
boxplot(nestl.BC.data$Tarsus.Length)
    # crazy large outlier, small ones are maybe fine?
boxplot(nestl.BC.data$Bird.Weight)
    #some very small ones, ask sara?

#remove outliers
nestl.BC.data <- nestl.BC.data %>%
  filter(Tarsus.Length <= 21, )

#calculate scaling component
library(smatr)

sma_fit_N <- sma(log(Bird.Weight) ~ log(Tarsus.Length), data = nestl.BC.analysis.data)
#run coef(sma_fit_N) and make slope the bSMA
b_SMA_N <- 1.354744  

#calculate reference length (mean of population tarsus)
L0_N <- mean(nestl.BC.data$Tarsus.Length, na.rm = TRUE)

#calculate SMI

nestl.BC.data$SMI_tarsus <-
  nestl.BC.data$Bird.Weight *
  (L0 / nestl.BC.data$Tarsus.Length)^b_SMA

#write.csv(nestl.BC.data, "ALL.MOCH.NESTLING.csv", row.names = FALSE)

#import nest data sheet
nest.data <- read.csv("ALL.NEST.csv")


  #ADD AVG NESTLING BC TO NEST SHEET

# Step 1–2: compute yearly averages
nestling.SMI.avg <- nestl.BC.data %>%
  group_by(nest.year) %>%
  summarise(mean_SMI_tarsus = mean(SMI_tarsus, na.rm = TRUE))

# Step 3: join to the analysis dataset
nest.data <- nest.data %>%
  left_join(nestling.SMI.avg, by = "nest.year")

  ##FILTER DATA FOR ANALYSIS

#analysis want to include
#area, julian date, bander ID
nestl.BC.analysis.data <- nest.data %>%
  filter(M.species == "MOCH", #make just MOCH
         !area == "NA", !nestl.bander.ID == "NA", !julian.CI.date == "NA", !Nestling_Number == 0) %>% #rm NAs for variables i will be using
  mutate(across(c(nest.status), as.factor))

#test normality of response var
hist(nestl.BC.analysis.data$mean_SMI_tarsus)
shapiro.test(nestl.BC.analysis.data$mean_SMI_tarsus)
  #Normal!


nestl.BC.lm.1 <- glmmTMB(mean_SMI_tarsus ~ nest.status  ,data =nestl.BC.analysis.data )

nestl.BC.lm.2 <- glmmTMB(mean_SMI_tarsus ~ nest.status + (1|area) ,data =nestl.BC.analysis.data )

#this one
nestl.BC.lm.3 <- glmmTMB(mean_SMI_tarsus ~ nest.status + (1|area) + (1|nestl.bander.ID) ,data =nestl.BC.analysis.data )
summary(nestl.BC.lm.3)

nestl.BC.lm.4 <- glmmTMB(mean_SMI_tarsus ~ nest.status + julian.CI.date + (1|area) + (1|nestl.bander.ID) ,data = nestl.BC.analysis.data )
summary(nestl.BC.lm.4)

nestl.BC.lm.5 <- glmmTMB(mean_SMI_tarsus ~ nest.status + julian.CI.date + (1|area) + (1|nestl.bander.ID) + Nestling_Number ,data =nestl.BC.analysis.data )

AIC(nestl.BC.lm.3,nestl.BC.lm.4,nestl.BC.lm.5)
#all the AICs are about the same but the only one that fits assumptions when plotting simulated residuals is lm.5 so I will be using that one

summary(nestl.BC.lm.5)

#all looks good!
sim2 <- simulateResiduals(nestl.BC.lm.5)
plot(sim2)

check_collinearity(nestl.BC.lm.5)
#looks good

testDispersion(sim2)
testZeroInflation(sim2)
testUniformity(sim2)
testOutliers(sim2)
#all look good

#FINAL MODEL
summary(nestl.BC.lm.5)

#GRAPH 
nestl.BC.analysis.data %>%
  ggplot(aes(x = nest.status, y = mean_SMI_tarsus, fill = nest.status)) +
  geom_boxplot()+
  geom_jitter(width = .2, alpha = .5)+
  labs(x = "Parent Infection Status", y = "Average Nestling SMI", title = "Parent infection and Average nestling body condition")+
  #change colors for boxes 
  scale_fill_manual(values = c("lightblue","salmon"))+
  #remove legend for fill
  guides(fill = "none")+
  #change x axis labels
  scale_x_discrete(labels = c("Uninfected", "Infected"))+
  #change y axis values
  scale_y_continuous(breaks=c(8,9,10,11,12,13,14,15), limits = c(8,15))+
  #make the background less busy
  theme_classic()



### analysis on if there is a general trend between nestling number and nestling body condition

NN.BC.analysis.data <- nest.data %>%
  filter(M.species == "MOCH", M.mite.status == "0" & F.mite.status == "0") %>%
  mutate(across(c(Nestling_Number), as.numeric))
NN.BC.lm <- glmmTMB(avg.nest.SMI.tarsus ~ Nestling_Number +(1|nestl.bander.ID), data = NN.BC.analysis.data)

NN.BC.analysis.data %>%
  ggplot(aes(x = Nestling_Number, y = avg.nest.SMI.tarsus))+
  geom_point()+
  geom_smooth(method="lm")
summary(NN.BC.lm)


              #### OTHER HONORS THESIS STUFF ####


                ###### Adult SMI ######

#filter data and such
indiv.data <- read.csv("ALL.INDIV.csv")

#CHECK FOR tarsus/weight OUTLIERS
boxplot(as.numeric(indiv.data$Bird.Weight))
boxplot(as.numeric(indiv.data$Tarsus.Length))

#standardize tarsus bc of differences in measurement methods between years (code from chat GPT)
#this uses 2025 as a reference year to convert the rest of the years to match

#plot of tarsus before scaling
indiv.data %>%
  ggplot(aes(x = as.factor(Banding.Year), y = as.numeric(Tarsus.Length)))+
  geom_boxplot()
#yikes

#make tarsus numeric
indiv.data$Tarsus.Length <- as.numeric(indiv.data$Tarsus.Length)

ref_year <- 2025

ref_mean <- mean(indiv.data$Tarsus.Length[indiv.data$Banding.Year == ref_year], na.rm = TRUE)
ref_sd   <- sd(indiv.data$Tarsus.Length[indiv.data$Banding.Year == ref_year], na.rm = TRUE)

indiv.data$Tarsus.scaled <- ave(
  indiv.data$Tarsus.Length,
  indiv.data$Banding.Year,
  FUN = function(x) {
    z <- (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
    z * ref_sd + ref_mean
  }
)

#plot to see
indiv.data %>%
  ggplot(aes(x = as.factor(Banding.Year), y = as.numeric(Tarsus.scaled)))+
  geom_boxplot()
#looks a lot better!


#make actual analysis data
ad.BC.analysis.data <- indiv.data %>%
  filter(Species == "MOCH", Sex == "F" |Sex == "M", !Bander.ID == "SAT", !Bander.ID == "EC",!Bander.ID == "BGH", !Bander.ID == "NA",!Bander.ID == "ERC", #remove banders with <2 occurances
         Banding.Year >= 2023,#remove years before 2023 bc of tarsus measurement differences
        ) %>%    
  mutate(across(c(Tarsus.Length, Bird.Weight, Nestling_Number), as.numeric))%>%
  mutate(across(c(Mite_status, Num.mite.per.nest, Bander.ID, Banding.Year), as.factor))



#calculate scaling component
library(smatr)

sma_fit <- sma(log(Bird.Weight) ~ log(Tarsus.scaled), data = ad.BC.analysis.data)
#run coef(sma_fit) and make slope the bSMA
b_SMA <- 1.517474    

#calculate reference length (mean of population tarsus)
L0 <- mean(ad.BC.analysis.data$Tarsus.scaled, na.rm = TRUE)

#calculate SMI

ad.BC.analysis.data$SMI_scaled.tarsus <-
  ad.BC.analysis.data$Bird.Weight *
  (L0 / ad.BC.analysis.data$Tarsus.scaled)^b_SMA

## Test normality
hist(ad.BC.analysis.data$SMI_scaled.tarsus)
shapiro.test(ad.BC.analysis.data$SMI_scaled.tarsus)
#not normal, will log transform in analysis

            ## analysis ##
#variables tested for AI
##fixed effects
  #julian band day
##random effects
  #area
  #sex
  #bander ID
  #banding year



#
adult.BC.lm1 <- glmmTMB(log(SMI_scaled.tarsus) ~ Mite_status + Sex+(1|area) + (1|Bander.ID), ad.BC.analysis.data)

#this is best fit
adult.BC.lm2 <- glmmTMB(log(SMI_scaled.tarsus) ~ Mite_status + (1|area) + (1|Bander.ID), ad.BC.analysis.data)
summary(adult.BC.lm2)

adult.BC.lm3 <- glmmTMB(log(SMI_scaled.tarsus) ~ Mite_status + julian.band.day + (1|area), ad.BC.analysis.data)

#NULL
adult.BC.NULL <- glmmTMB(log(SMI_scaled.tarsus) ~ 1  , ad.BC.analysis.data)

AIC(adult.BC.lm1,adult.BC.lm2,adult.BC.lm3,adult.BC.NULL)
summary(adult.BC.lm2)

sim1 <- simulateResiduals(adult.BC.lm2)
plot(sim1)

### PLOT ###
ad.BC.analysis.data %>%
  ggplot(aes(x = Mite_status , y = SMI_scaled.tarsus, fill = Mite_status)) + 
  geom_boxplot(outlier.shape = NA)+
  geom_jitter(width = .2, alpha = .5)+
  labs(x = "Infection Status", y = "Scaled Mass (g)", title = "Infection status vs Body condition")+
  #change colors for boxes 
  scale_fill_manual(values = c("lightblue","salmon"))+
  #remove legend for fill
  guides(fill = "none")+
  #change x axis labels
  scale_x_discrete(labels = c("Uninfected", "Infected"))+
  #change y axis values
  scale_y_continuous(breaks=c(6,8,10,12,14,16,18,20,22), limits = c(6,22))+
  theme_classic()
#there are two high outliers in infection side

boxplot(as.numeric(ad.BC.analysis.data$Tarsus.scaled))
boxplot(as.numeric(indiv.data$Bird.Weight))



              ###### Sex ######

          ## DATA SET CREATION

#filter data to make sure there are only M and F for sex
mod3.data <- indiv.data %>%
  filter(!is.na(Sex), !Sex == "", !Sex == "u", !Sex == "U", !Sex == "M ", 
         #remove any sex that is not M or F (thats too bad :/ )
         !area == "", !Banding.Year == "NA", !Mite_status == "NA", 
         #remove data with no area attached
         Species == "MOCH") 
#filter for MOCH

#reduce data so its just one individual per nest
set.seed(4399)

#filter so there is only one individual per nest
mod3.redu <- mod3.data %>%
  group_by(Nest.year) %>%
  slice_sample(n = 1) %>%
  ungroup()

str(mod3.redu)


          ## MODELS
mod3.area <- glmmTMB(Mite_status ~ Sex + (1|area), data = mod3.redu, 
                     family = binomial)

mod3.no.RE <- glmmTMB(Mite_status ~ Sex, data = mod3.redu, family = binomial)

mod3.year.area <- glmmTMB(Mite_status ~ Sex + (1|area)+ (1|Banding.Year), 
                          data = mod3.redu, family = binomial)

mod3.year <- glmmTMB(Mite_status ~ Sex + (1|Banding.Year), data = mod3.redu, 
                     family = binomial)


AIC(mod3.area, #area as RE
    mod3.no.RE, # no area as RE
    mod3.year.area,# area and year as RE
    mod3.year) #just year RE

#just area as RE is best

summary(mod3.area)


            ## INTERPRETAION

invlogit <- function(x) { exp(x) / (1 + exp(x)) }
#logit = -2.5140  + (M)1.0384

#convert estimates into actual prob
logprobMMOCH <- -2.5140+ (1)*1.0384
probMMOCH <- invlogit(logprobMMOCH)

logprobFMOCH <- -2.5140 + (0)*1.0384
probFMOCH <- invlogit(logprobFMOCH)

#get prob difference between M and F MOCH
probMMOCH/probFMOCH
# = 2.485131

#Male MOCH are infected 2.49 times as often as females

            ## ASSUMPTIONS

# DEVIANCE 
mod3.NULL <-glmmTMB(Mite_status ~ 1+ (1|area), data = mod3.redu, family = binomial)

deviance(mod3.area) #135.0731
deviance(mod3.NULL) #143.7447
#null deviance > model deviance so we are good!

check_overdispersion(mod3.area) # no overdispersion

check_model(mod3.area)
#binned residuals looks weird again, but piet says it ok


          ## PLOT
#make mite status a factor for plotting
mod3.redu$Mite_status <- factor(mod3.redu$Mite_status)

#opposite plot (x sex, y mite status)
ggplot(data = mod3.redu) +
  geom_mosaic(aes(x = product(Mite_status, Sex), fill = Mite_status, weight = 1)) +
  labs(
    x = "Sex",
    y = "Proportion Infected",
    title = "MOCH Infection status by Sex",
    fill = "Infection Status"   # Legend title
  ) +
  #change size of axis/title labels
  theme_grey(base_size = 11)+
  #change colors y axis and labels for legend
  scale_fill_manual(
    values = c("1" = "salmon", "0" = "lightblue"),
    labels = c("0" = "Uninfected", "1" = "Infected")  # Legend labels
  ) +
  # add sample sizes in the boxes
  geom_mosaic_text(aes(x = product(Mite_status, Sex), label = after_stat(.wt)),
                   as.label = TRUE, size = 3.5) +
  #change x axis group labels
  scale_x_productlist(labels = c("0" = "Female", "1" = "Male"))+ 
  #force y axis labels
  scale_y_continuous(limits = c(0, 1),  
                     breaks = seq(0, 1, by = 0.5),
                     labels = scales::number_format(accuracy = 0.01))


########## infection/ area (INDIV)###########
area.indiv.data <- indiv.data%>%
  mutate(across(c(Mite_status, area), as.factor)) %>%
  filter(!julian.band.day == "NA", !area == "NA", !area == "")

#change area reference level to MRS
area.indiv.data$area <- relevel(area.indiv.data$area, ref = "BLD")

#### infected nests
# i tested analysis with julian date as covariate and there was no difference in models
nest.area.lm1 <- glmmTMB(Mite_status ~ area, family = binomial,data = area.indiv.data)

nest.area.lm2 <- glmmTMB(Mite_status ~ area + julian.band.day, family = binomial,data = area.indiv.data)

nest.area.lm3 <- glmmTMB(Mite_status ~ area + Banding.Year, family = binomial,data = area.indiv.data)

nest.area.null <- glmmTMB(Mite_status ~ 1, family = binomial,data = area.indiv.data)

AIC(nest.area.lm1,nest.area.lm2,nest.area.lm3,nest.area.null)
#all the same so choosing lm1
summary(nest.area.lm1)

table(area.indiv.data$area,area.indiv.data$Mite_status)



#plot for area and individual infection (INDIV)
area.plot.data <- indiv.data %>%
  filter(!area == "CU", !area == "NA", !area=="")%>%
  mutate(across(Mite_status, as.factor))

#change order to plot is cleaner
area.indiv.data$area <- factor(
  area.indiv.data$area,
  levels = c("BLD", "FLG", "SGR", "NED", "MRS")
)

ggplot(data = area.indiv.data) +
  geom_mosaic(aes(x = product(Mite_status, area), fill = Mite_status, weight = 1)) +
  labs(x = "Area", y = " ", title = "Infection proportion per area", fill = "Infection Status" )+
  scale_fill_manual(
    values = c("1" = "salmon", "0" = "lightblue"),
    labels = c("0" = "Uninfected", "1" = "Infected")  # Legend labels
  )+ theme(
    panel.grid = element_blank(),# remove background pattern
    axis.ticks.y = element_blank()) + scale_y_continuous(limits = c(0, 1),
breaks = seq(0, 1, by = .5),
labels = scales::number_format(accuracy = 0.01))



                        #### INFECTIONS PER AREA ####



#remove NA area cases
mite.data.handcap = mite.data[(!is.na(mite.data$area)), ]

#make df for what i want to graph (number of mite cases per area)

area <- c("CU", "BLD", "FLG", "NED", "SGR","MRS")
mite_cases <- c(0, 8,2,4,37, 2)

# Create data frame
count.mite.area <- data.frame(
  Area = area,
  MiteCases = mite_cases
)
#want x axis as area and y axis as number of individuals with mites

ggplot(count.mite.area)+
 geom_boxplot(aes(x = Area, y = MiteCases) )


####infectoin vs elevation##
plot(x = mite.data$elevation, y = mite.data$Mite_status)








                      #### CLIMATE STUFF ####






library(tidyr)
library(geodata)
library(raster)




            #####- proportion vs precipitation-#####


##make data frame for year, and proportoin of 'pox' cases
p_data <- data.frame(
  year = c(2019,2020,2021,2022,2023,2024,2025),
  num_pox = c(6,5,7,11,10,14,7),
  total_samp = c(61,78,67,96,208,130,66)
)

#proportion for just MOCH
count(all.mite, Species == "MOCH", Banding.Year)

p.data.MOCH <- data.frame( year = c(2019,2020,2021,2022,2023,2024,2025), num.mite.MOCH = c(4,4,5,9,8,11,6), total_samp = c(30,29,37,69,104,87,56) )

#add proportion to df

p.data.MOCH$proportion <- (p.data.MOCH$num.mite.MOCH / p.data.MOCH$total_samp)

#plot
plot(p.data.MOCH$year, p.data.MOCH$proportion, 
     ylim = c(0,0.2))

#precipitation data
precip_data <- data.frame(
  year = c(2019,2020,2021,2022,2023,2024,2025),
  avg_precip = c(21.04, 17.10, 20.68,18.79,22.81,19.13,13.36)
)

#plot again, but this time with colors
{plot(p.data.MOCH$year, p.data.MOCH$proportion,
     ylim = c(0, 0.2),
     type = "b", pch = 19, col = "blue",
     ylab = "Proportion",
     xlab = "Year")
par(new = TRUE)  # allows overlaying a new plot

plot(precip_data$year, precip_data$avg_precip,
     type = "b", pch = 17, col = "red",
     axes = FALSE, xlab = "", ylab = "",
     ylim = c(10, 25),
     main = "Mite proportion and precipitation MOCH")  # range of precipitation values

axis(side = 4)  # add secondary axis on the right
mtext("Avg Precipitation (inches)", side = 4, line = 3)

legend("topleft", legend = c("Proportion", "Precipitation"),
       col = c("blue", "red"), pch = c(19,17), lty = 1)
}

              ## proportion for both MOCh and BCCH combined #

p_data$proportion <- (p_data$num_pox / p_data$total_samp
)
barplot (p_data$proportion, p_data$year)

plot(p_data$year, p_data$proportion, 
     ylim = c(0,0.2))


## data from Boulder/NOAA: https://psl.noaa.gov/boulder/Boulder.mm.precip.html
##here is another good co weather data: https://ccc.atmos.colostate.edu/data_access_new.html
##this is yearly avg.. is there a month or season where the weather matters more?
##"At the temporal scale, environmental variables included the average May–September air temperature and precipitation
##for a given year, as well as the abundance of a given host during this period." (Boris)
precip_data <- data.frame(
  year = c(2019,2020,2021,2022,2023,2024,2025),
  avg_precip = c(21.04, 17.10, 20.68,18.79,22.81,19.13,13.36)
)


# Plot proportion data
{plot(p_data$year, p_data$proportion,
     ylim = c(0, 0.2),
     type = "b", pch = 19, col = "blue",
     ylab = "Mite Prevelance",
     xlab = "Year")

# Add precipitation line (scale to fit proportion axis temporarily)
par(new = TRUE)  # allows overlaying a new plot

plot(precip_data$year, precip_data$avg_precip,
     type = "b", pch = 17, col = "red",
     axes = FALSE, xlab = "", ylab = "",
     ylim = c(10, 25))  # range of precipitation values

axis(side = 4)  # add secondary axis on the right
mtext("Avg Precipitation (inches)", side = 4, line = 3)

legend("topleft", legend = c("Mite Prevelance", "Precipitation"),
       col = c("blue", "red"), pch = c(19,17), lty = 1)
}


#see if they are correlated at all
########HERE FOR ANALYSIS #########

model.precip.mite.prop <- lm(p_data$proportion ~ precip_data$avg_precip + year, data = precip_data)
summary(model.precip.mite.prop)





                        ##### temp data  ###### 


BLD.temp.data <- read.csv('BLD.temp.data.csv')
t(BLD.temp.data)


#change plotting code from before for temp instead of precip
plot(p_data$year, p_data$proportion,
      ylim = c(0, 0.2),
      type = "b", pch = 19, col = "blue",
      ylab = "Mite Prevelance",
      xlab = "Year")
  
  # Add temp line (scale to fit proportion axis temporarily)
  par(new = TRUE)  # allows overlaying a new plot
  
  plot(BLD.temp.data$year, BLD.temp.data$Average,
       type = "b", pch = 17, col = "darkgreen",
       axes = FALSE, xlab = "", ylab = "",
       ylim = c(50, 55))  # range of temp values
  par(new = TRUE)
  
  plot(precip_data$year, precip_data$avg_precip,
       type = "b", pch = 17, col = "red",
       axes = FALSE, xlab = "", ylab = "",
       ylim = c(10, 25)) 
  
  
  axis(side = 4)  # add secondary axis on the right
  mtext("Avg Precipitation (inches)", side = 4, line = 3)
  
  legend("topleft", legend = c("Mite Prevelance", "Precipitation", "Temperature"),
         col = c("blue", "red", "darkgreen"), pch = c(19,17), lty = 1)


# tavg = average temperature
tavg <- worldclim_global(var = "tavg", res = 10, path = tempdir())

# tavg has 12 layers (1 per month)
tavg

SGR <- data.frame(lon = -105.46313, lat = 40.00354) 


##climate data from worldclim
tavg <- worldclim_global(var = "tavg", res = 0.5, path = tempdir())
town_coords <- data.frame(lon = -105.46313, lat = 40.00354)
SGR_temp <- raster::extract(tavg, town_coords)


################## mapping with world cover data ##############
library(sf)
# Load the raster package
library(raster) 

# Open the TIFF file
world_cover <- raster("2020_cover_Map.tif")

# View the coordinate reference system
crs(world_cover)

# Obtain the spatial extent
extent(world_cover)


#change raster to data frame
world.cover.df <- as.data.frame(raster::as.data.frame(world_cover, xy = TRUE))

#import data for just mite cases: 
past.mite <- read.csv('pastmiterev.csv')

#remove lines with no coords
past.mitex = past.mite[(!is.na(past.mite$long)), ]

#make shape file of mite cases
  #why do i need this???? idk....
mite_sf <- st_as_sf(past.mitex, coords = c("long", "lat"), crs = 4326)
max(past.mitex$long)
    
#dataset for all sgr cases

all.SGR <- subset(all.mite, area== "SGR")

#using all sgr mite data to get sgr map limits
#subset to eliminate coordinate NAs
all.SGR.mite.cord = all.SGR[(!is.na(all.SGR$long)), ]

#get max and min long and lat for the graph
max(all.SGR.mite.cord$long)
max(all.SGR.mite.cord$lat)
min(all.SGR.mite.cord$long)
min(all.SGR.mite.cord$lat)



#map world cover data?

                    ##### SGR world cover map of mite and no mite boxes


#make subset for just mite lines
only.mite.SGR <- subset(all.SGR.mite.cord, MiteStatus_yesno == "Yes")

#find cord
# max sgr long = -105.3239
# min sgr long =  -105.5324
# max sgr lat = 40.0411
# min sgr lat = 39.984

plot(world_cover, xlim = c(-105.5324, -105.3239 ), ylim = c(39.984, 40.0411), main = "Map of mite cases in SGR")
#points of all boxes
points(all.mite$long, all.mite$lat, 
       col= "blue", 
       pch = 15, 
       cex = .75)
#points of mite boxes
points(only.mite.SGR$long, 
       only.mite.SGR$lat, 
       col = "red",      # point color
       pch = 15,         # point shape (solid circle)
       cex = .75) 

## i believe grey is tree cover, tan is grassland



########## analysis of infection and area (NESTS)###########
area.nest.data <- nest.data%>%
  mutate(across(c(nest.status, area), as.factor)) %>%
  filter(!M.species == "BCCH", !julian.CI.date == "NA", !area == "NA")

table(area.nest.data$area, useNA = "ifany")


#### infected nests
  # i tested analysis with julian date as covariate and there was no difference in models
nest.area.lm1 <- glmmTMB(nest.status ~ area, family = binomial,data = area.nest.data)

nest.area.lm2 <- glmmTMB(nest.status ~ area + julian.CI.date, family = binomial,data = area.nest.data)

nest.area.lm3 <- glmmTMB(nest.status ~ area + M.banding.year, family = binomial,data = area.nest.data)

nest.are.null <- glmmTMB(nest.status ~ 1, family = binomial,data = area.nest.data)

AIC(nest.area.lm1,nest.area.lm2,nest.area.lm3,nest.are.null) #all are the same
#they are all pretty much the same, so im going to use the first one

summary(nest.area.lm1)



 

library(ggmosaic)




                          #### MAPS ####


library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(raster)
library(prettymapr)




#reading in with code might import weird (names of columsn shifted down?). importing manually fixes it
all_mite_elevvv <- indiv.data %>%
  filter(!long == "NA", !long == "", !lat == "NA", !lat == "")
#Create a shapefile of the boxes using the latitude and Longitude from the boxes
boxes_sf <- st_as_sf(all_mite_elevvv, coords = c("long", "lat"), crs = 4326)

#Create a bounding box of nest boxes--crop the map to plot only the area with nest box info
boxes_bbox <- st_bbox(boxes_sf)





  #####     mite per year, small section SGR ######

#make x and y lim for the small section of SGR boxes 
SGR.sm.xlim <- c(-105.48, -105.46)
SGR.sm.ylim <- c(39.99, 40.005)
#Plotting for a year (this works to change the year to year you want for new map) 
boxes_sf %>%
  filter(Banding.Year == "2019") %>% ##change year here for different year map
  ggplot() +
  #add map underneith
  annotation_map_tile(type = "osm", zoomin = 0) +
  # First layer: Mite_status = 0 (underneath)
  geom_sf(data = ~filter(., Mite_status == 0),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  # Second layer: Mite_status = 1 (on top)
  geom_sf(data = ~filter(., Mite_status == 1),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  coord_sf(xlim = SGR.sm.xlim,
           ylim = SGR.sm.ylim) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  labs(
    title = "Map of mite cases 2019",        ##change year here for different title to match map
    color = "Mite status"
  ) +
  guides(size = "none")





          ###### (most) of whole map  #####


#make x and y lim for the small section of SGR boxes 
most.xlim <- c(-105.6, -105.23)
most.ylim <- c(39.96, 40.06)
#Plotting for a year (this works to change the year to year you want for new map) 
boxes_sf %>% ##add filter year here for different years map
  ggplot() +
  #add map underneith
  annotation_map_tile(type = "osm", zoomin = 0) +
  # First layer: Mite_status = 0 (underneath)
  geom_sf(data = ~filter(., Mite_status == 0),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  # Second layer: Mite_status = 1 (on top)
  geom_sf(data = ~filter(., Mite_status == 1),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  coord_sf(xlim = most.xlim,
           ylim = most.ylim) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  labs(
    title = "Map of mite cases 2025",        ##change year here for different title to match map
    color = "Mite status"
  ) +
  guides(size = "none")




#######################     WHOLE map all years #####################

all.xlim <- c(-105.6, -105.2)
all.ylim <- c(39.96, 40.14)
#Plotting for a year (this works to change the year to year you want for new map) 
boxes_sf %>%
  ggplot() +
  #add map underneith
  annotation_map_tile(type = "osm", zoomin = 0) +
  # First layer: Mite_status = 0 (underneath)
  geom_sf(data = ~filter(., Mite_status == 0),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  # Second layer: Mite_status = 1 (on top)
  geom_sf(data = ~filter(., Mite_status == 1),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  coord_sf(xlim = all.xlim,
           ylim = all.ylim) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  labs(
    title = "Map of all mite cases 2019-2025",
    color = "Mite status"
  ) +
  guides(size = "none")




#Plotting all years for just MOCH
boxes_sf %>%
  filter(Species == "MOCH")%>%
  ggplot() +
  #add map underneith
  annotation_map_tile(type = "osm", zoomin = 0) +
  # First layer: Mite_status = 0 (underneath)
  geom_sf(data = ~filter(., Mite_status == 0),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  # Second layer: Mite_status = 1 (on top)
  geom_sf(data = ~filter(., Mite_status == 1),
          aes(color = as.factor(Mite_status)),
          alpha = 0.75) +
  coord_sf(xlim = all.xlim,
           ylim = all.ylim) +
  scale_color_manual(values = c("0" = "blue", "1" = "red")) +
  theme_minimal() +
  labs(
    title = "Map of all MOCH nests and mite cases 2019-2025",
    color = "Mite status"
  ) +
  guides(size = "none")


             


######### make a new DF that is data per nest ################
########!!!!!!!! need to come back to consider how when combining to uk_band-num for example 
########!!!!!!!! it will only show one of the 2 birds band number if there are two that are unk
########!!!!!!!! also, need ot make a Nest.mite.status column
library(dplyr)
library(tidyr)
rm(Nest.Data)


#read in data for this
all_mite_elevvv <- read.csv("all_mite_elevvv.csv")

# try and subset into M and F datasets, rename column names to be M.--- and F.--- and then recombine datasets

#male subset first, im keeping everything i want to keep, for females i will remove more to prevent duplicates

Male.sub <- all_mite_elevvv %>%
  filter(Sex == "M") %>%      #just males
  rename(M.mite.status = Mite_status,
         M.species = Species,
         M.band.number = Band.Number,
         M.age = Age,
         M.color.combo = Taylor.Lab.color.band.combo,
         M.banding.year = Banding.Year,
         M.banding.month = Banding.Month,
         M.banding.day = Banding.Day) %>%  #####rename things to specify male
  select (Location:M.banding.day, M.band.number, M.age, M.color.combo, Egg_Number:Num.mite.per.nest, elevation.m.elev_units) %>%            #select things i want to KEEP (if want to remove its "-column")
  rename(elevation.m = elevation.m.elev_units) #want to rename this as well


##subset females
Fem.sub <- all_mite_elevvv %>%
  filter(Sex == "F") %>%
  rename(F.mite.status = Mite_status,
         F.species = Species,
         F.band.number = Band.Number,
         F.age = Age,
         F.color.combo = Taylor.Lab.color.band.combo,
         F.banding.year = Banding.Year,
         F.banding.month = Banding.Month,
         F.banding.day = Banding.Day) %>% 
  select (F.species:F.banding.day, F.band.number, F.age, F.color.combo, Nest.year,F.mite.status)

#now combine data sets

#check if there are dupicate row for nest/year
Fem.sub %>% 
       count(Nest.year) %>% 
       filter(n > 1)
#there are 5 duplicates here

Male.sub %>% 
       count(Nest.year) %>% 
       filter(n > 1)
#there is one duplicate here

#i looked through, and most can just be either, but for 197-2020 i want to keep the one that was banded in June (Banding.Month == 6)

#This is in the female subset, this is the code to get that one

Fem.sub <- Fem.sub %>%
  group_by(Nest.year) %>%
  slice(
    if(unique(Nest.year) == "197-2020") {
      # For Nestbox 42, keep row where month = 5
      which("f.banding.month" == 6)[1]  # [1] in case multiple matches
    } else {
      # For all other Nestbox numbers, keep the first row
      1
    }
  ) %>%
  ungroup()

#now for the rest, I don't care which one is kept
Fem.sub <- Fem.sub %>%
  group_by(Nest.year) %>%
  slice(1) %>%   # keeps the first row for each Nest.year
  ungroup()

Male.sub <- Male.sub %>%
  group_by(Nest.year) %>%
  slice(1) %>%   # keeps the first row for each Nest.year
  ungroup()

#checked, and it works

#now combine
Nest.Data <- left_join(Male.sub, Fem.sub, by = "Nest.year")

#re order columns
Nest.Data <- Nest.Data %>%
  select(Nestbox,  Nest.year, M.band.number,  F.band.number,  M.mite.status,  F.mite.status, Num.mite.per.nest,  Egg_Number,  Nestling_Number,  Avg_Nestling_Weight, Egg_Nestling_Diff,  Avg.Nestling.Wing,  Avg.Nestling.Tarsus,  age.at.measure.day,  Nestling.data.notes,  M.banding.year,   M.banding.month,   M.banding.day, F.banding.year, F.banding.month,  F.banding.day, M.age, F.age, M.color.combo, F.color.combo,M.species, F.species, Location, area,Nestbox.num, long, lat,elevation.m, )

#save to computer
#write.csv(Nest.Data, "ALL.NEST.csv", row.names = FALSE)

#yay!!!!!!




                #### random plotting data ####
mite_data_filtered = mite_data %>%
  filter(
    Species == "MOCH",
    Sex == "F",
    !is.na(Num.mite.per.nest),        # remove NAs
    Num.mite.per.nest != ""           # remove blanks if any
  ) %>%
  mutate(
    Num_mite_bin = factor(Num.mite.per.nest)  # make proper factor
  )

ggplot(mite_data_filtered, aes(x = Mite_status, y = as.numeric(Egg_Number))) +
  geom_boxplot(aes(group = Mite_status)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  xlab("Number of Mites per Nest") +
  ylab("Egg-Nestling Difference") +
  theme_minimal()

ggplot(mite_data_filtered, aes(x = Mite_status, y = as.numeric(Egg_Number))) +
  geom_boxplot(aes(group = Mite_status)) +
  geom_jitter(width = 0.1, alpha = 0.5) +
  xlab("Number of Mites per Nest") +
  ylab("Egg Number Difference") +
  theme_minimal()

      ##### plots of number per species #####

#pure number per species

all_mite_elevvv %>%
  filter (Mite_status == "1") %>%
ggplot()+
  geom_bar(aes(x = Species))+
  labs(y = "Count", title = "Number of birds with mites per species")

#compare against total number samples

all_mite_elevvv %>%
  ggplot(aes(x = Species, fill = as.factor(Mite_status)))+
  geom_bar(position = "dodge")+
  labs (y = "Count", title = "Number infected birds per species", fill = "Infection Status") +
  scale_fill_manual(values = c("maroon", "orange"),    # colors for each category
    labels = c("Uninfected", "Infected"))


  #as a proportion

all_mite_elevvv %>%
  ggplot(aes(x = Species, fill = as.factor(Mite_status))) +
  geom_bar(position = "fill") +
  labs(y = "Proportion", title = "Proportion of infected birds per species", fill = "Infection status") +
  scale_y_continuous(labels = scales::percent) +
  scale_fill_manual(values = c("0" = "maroon", "1" = "orange"),
labels = c("Uninfected","Infected"))





#####how to add elevation into dataset######

library(sf)
library(elevatr)

#add elevation to data
# Copy data
indiv.data$Elevation.m <- NA  

# Drop missing coords for sf conversion
data.with.coord <- subset(indiv.data, !is.na(lat) & !is.na(long))

# Convert only clean rows
coords_sf <- st_as_sf(data.with.coord, coords = c("long", "lat"), crs = 4326)

# Get elevations
elev <- get_elev_point(coords_sf, prj = st_crs(coords_sf)$proj4string, src = "aws")

# Put values back into original df (matching by rownames)
indiv.data$Elevation.m[as.numeric(rownames(data.with.coord))] <- elev$elevation


#### adult fem bc vs clutch size
library(glmmTMB)
library(dplyr)
str(adult.BC.data)
adult.BC.data <-adult.BC.data %>% 
  mutate(
  Egg_Number   = suppressWarnings(as.numeric(Egg_Number)))
str(adult.BC.data)

BCxCSlm<- glmmTMB(body.cond.wing~Egg_Number + (1|Banding.Year) +(1|area),data = adult.BC.data)

summary(BCxCSlm)


##### infection and date banded?
library(glmmTMB)
library(dplyr)
#add day of year (1-365)
indiv.data$day_of_year <- yday(make_date(indiv.data$Banding.Year, indiv.data$Banding.Month, indiv.data$Banding.Day))

random.MOCH <- indiv.data %>%
  filter(Species == "MOCH")
summary(glmmTMB(Mite_status~day_of_year,data = random.MOCH))


#julian date and clutch size model

julian.EN.mod <- glmmTMB(Egg_Number ~ julian.CI.date, data = egg.num.data)
summary(julian.EN.mod)

julian.NN.mod <- glmmTMB(Nestling_Number ~ julian.CI.date ,data = mod.1.data)
summary(julian.NN.mod)


## elevation and infection
#not directional so.?
elev.lm <- glmmTMB(Mite_status~Elevation.m,data = indiv.data)
summary(elev.lm)

plot( indiv.data$Mite_status,indiv.data$Elevation.m)

table(indiv.data$Mite_status, indiv.data$Sex)


## infection and area -- chi square

infection.data <- table(indiv.data$Mite_status)

area.data <- table(indiv.data$area)

infection.distr <- infection.data/sum(infection.data)

area.table <- table(indiv.data$Mite_status,indiv.data$area)

addmargins(area.table)
 
area.chi.square <- chisq.test(x=indiv.data$area, y = indiv.data$Mite_status)
area.chi.square

chisq.posthoc.test(area.table,
                   method = "bonferroni")


### mite measurement ranges graph #### 
#(code from chat gpt...)

library(ggplot2)

mites <- data.frame(
  Species = c("K. intermedius", "K. jamaicensis", "K. mutans", "Current Sample"),
  width_mid  = c(379, 295, 470, 275),
  length_mid = c(434.5, 325, 370, 305),
  
  width_min  = c(360, 260, 445, 275),
  width_max  = c(398, 330, 495, 275),
  
  length_min = c(414, 291, 340, 305),
  length_max = c(455, 359, 400, 305)
)

ggplot(mites, aes(x = width_mid, y = length_mid, color = Species)) +
  
  # vertical ranges (length)
  geom_errorbar(aes(ymin = length_min, ymax = length_max),
                width = 0, size = 0.6) +
  
  # horizontal ranges (width)
  geom_errorbarh(aes(xmin = width_min, xmax = width_max),
                 height = 0, size = 0.6) +
  
  # central point
  geom_point(size = 3) +
  
  labs(x = "width (um)", y = "length (um)", title = "Dimensions of Knemidocoptid mites and current sample") +
  theme_minimal()






