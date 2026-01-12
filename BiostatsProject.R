#BIOSTATS PROJECT#

indiv.data <- read.csv("ALL.INDIV.csv")
nest.data <- read.csv("ALL.NEST.csv")
library(dplyr)
library(ggplot2)
library(ggmosaic)
library(DHARMa)
library(performance)
library(lmerTest)
library(glmmTMB)

#write.csv(mite.data, "all.mite.elevvv.csv", row.names = FALSE)

## general notes (from lab 13 lecture) ##
# if binomial RESPONSE (when mite status is RESPONSE) use binomial regression (logit-link function)
# for random effects use lmer package, glmer function
# if using gzlm back transform coefficients (exp() for poisson regression)
# use AIC to compare GzLMs
  #low AIC means better model, difference greater than 2 is significant, if they are the same use simpler one
# plotting 
  #need to use predict to generate fitted line (lab does this)
#HAVE PLOT OF FITTED MODEL IN PAPER!!!!!

#####data cleaning #####

{

  
    
# add infection status for nest
nest.data$nest.status <- as.factor(ifelse(nest.data$Num.mite.per.nest > 0, 1, 0))
# add success 1/0 per nest
nest.data$nest.success <- as.factor(ifelse(nest.data$prop.lived>0,1,0))



#change things to factor and numeric where necessary
nest.data <- nest.data %>%
  mutate(across(c(M.mite.status,F.mite.status, M.mite.status, F.mite.status,Num.mite.per.nest,Nestbox.num,M.banding.year), as.factor))%>%
  mutate(across(c(Egg_Number, Egg_Nestling_Diff, prop.lived), as.numeric))
}
str(nest.data)


#### Q1 - How does nest infection status (1/0) affect nestling number?  ####
  # Predictor - Nest adults infection status (binomial) - yes if one or both adults are infected
  # Response - Nestling number - count discrete + normal
             # - control for clutch size and species
             # - Nest ID as random effect?
### Test ###
      # mixed linear model with poisson link
### Assumptions ###
      # Goodness of fit - Deviance of model vs Null model
      # residual analysis - Pearson
      # Over dispersion
### TO DO: ###
  # check residuals to see if they are normal, if so use regular lm, if not use gzlm with poisson link function (bc count response)


### data set ###

#check for NAs for all used variables
table(nest.data$Nestling_Number, useNA = "ifany")
table(nest.data$nest.status, useNA = "ifany")
table(nest.data$M.banding.year, useNA = "ifany")
table(nest.data$area, useNA = "ifany")

#there are none for any

mod.1.data <- nest.data %>%
  filter(M.species == "MOCH" | F.species == "MOCH",  #keep only MOCH
         !Nestling_Number=="0", #remove failed nests
         !area == "NA") #remove NAs

####### Model #######


######data exploration######
##outliers?

boxplot(mod.1.data$Nestling_Number,  ylab = "Nestling Number")
#no outliers

##homogeneity of variance (done later after model is made)

##normality

hist(mod.1.data$Nestling_Number, xlim = c(0,10))

library(lattice)
histogram( ~ Nestling_Number | nest.status, type = "count",
           xlab = "Weight (g)",
           ylab = "Frequency",
           nint=30,layout=c(1,3),
           strip.left = strip.custom(bg = 'white'),
           strip = F,
           col.line = "black", col = "white",
           scales = list(x = list(relation = "same"),
                         y = list(relation = "same"),
                         draw = TRUE),
           subset = nest.status =="1" | nest.status == "0",
           data = mod.1.data)


shapiro.test((mod.1.data$Nestling_Number))
# non normal so glmmTMB is good


    #  MODEL  #


#if variance is less than mean, use compois 
mean(mod.1.data$Nestling_Number) #5.263158
var(mod.1.data$Nestling_Number) #3.04386

#banding year random effect
mod1.year <- glmmTMB(Nestling_Number ~  nest.status + (1|M.banding.year), data = mod.1.data, family = compois)

#area random effect
mod1.area <- glmmTMB(Nestling_Number ~  nest.status +  (1|area), data = mod.1.data, family = compois)

#both random effects
mod1.both <- glmmTMB(Nestling_Number ~  nest.status +  (1|area) + (1|M.banding.year), data = mod.1.data, family = compois)

#neither random effects
mod1.noRE <- glmmTMB(Nestling_Number ~  nest.status, data = mod.1.data, family = compois) 

mod1.null <- glmmTMB(Nestling_Number ~ 1, data = mod.1.data, family = compois) 

summary(mod1.year)
summary(mod1.area)
#neither area nor year account for much variance 

#see which is better
AIC(mod1.year, mod1.area, mod1.both,mod1.noRE, mod1.null)

#lowest AIC and area and year dont account for much variance so this one
summary(mod1.noRE)

#p = 0.00975


###### interpretation #####

# log(Nestling number) = 1.69867 -0.19459(infected)

logNNinf <- 1.69867 - 0.19459*(1)
NNinf <- exp(logNNinf)

logNNuninf <- 1.69867 -0.19459*(0)
NNuninf <- exp(logNNuninf)

#get prob difference between M and F MOCH
NNinf/NNuninf # = 0.8231721

(1 - 0.8231721)*100
# 17.68279

#infected nests have ~17.7% less nestlings than uninfected nests

######assumptions######
simulationOutput <- DHARMa::simulateResiduals(mod1.noRE)
plot(simulationOutput)

#all looks good. assumptions are met



###### plots ######

library(ggsignif)

#plot

mod.1.data %>%
  ggplot(aes(x = nest.status, y = Nestling_Number, fill = nest.status))+
  geom_boxplot()+
  geom_jitter(width = 0.1, alpha = 0.5)+
  #labels
  labs( x = "Nest Infection Status", y = "Nestling Number", title = "Parents Infection Status vs Nestling Number")+
  #change size of axis labels
  theme_grey(base_size = 15)+
  #change colors for boxes 
  scale_fill_manual(values = c("lightblue","salmon"))+
  #remove legend for fill
  guides(fill = "none")+
  #change x axis labels
  scale_x_discrete(labels = c("Uninfected", "Infected"))+
  #change y axis values
  scale_y_continuous(breaks=c(0,2,4,6,8,10))+ 
  # adds significance bar
  geom_signif(comparisons = list(c("0", "1")), #signifies i want uninfected ("0") and infected ("1") to be significant
              map_signif_level = TRUE, 
              textsize = 6,
              y_position = 9.5, #spot on y axis you want the bar
              tip_length = c(0.04,0.04))

  




##### Q2 - How does nest infection status (1/0) affect nest survival (1/0)?  ####
# Predictor - Nest adults infection status (binomial) - yes if one or both adults are infected
# Response - nestling survival (binomial)
# - control for elevation?
### Test ###
# mixed linear model - binomial
### TO DO: ###
# check residuals to see if they are normal, if so use regular lm, if not use gzlm with poisson link function (bc count response) (to back transform do exp())


#check variables for NAs
table(nest.data$nest.success, useNA = "ifany") #has NAs
table(nest.data$Egg_Number, useNA = "ifany") # has NAs
table(nest.data$elevation.m, useNA = "ifany") # no NAs
table(nest.data$nest.status, useNA = "ifany") #no NAs
table(nest.data$M.banding.year, useNA = "ifany") #no NAs



## filter data 

mod.2.data<- nest.data %>%
  filter(M.species == "MOCH" | F.species == "MOCH",#keep only MOCH
         !nest.success == "NA", !Egg_Number=="NA", !area == "NA") 

######Model######

#test both with and without elevation (MOCH)

#elevation covariate
mod.2.MOCH <- glmmTMB(nest.success ~ nest.status + elevation.m + (1|M.banding.year), data = mod.2.data, family = binomial(link="logit"))

#no elevation
mod.2.test <- glmmTMB(nest.success ~ nest.status + (1|M.banding.year), data = mod.2.data, family = binomial(link="logit"))

#no elevation and egg number covariate
mod.2.egg <- glmmTMB(nest.success ~ nest.status  + Egg_Number + (1|M.banding.year), data = mod.2.data, family = binomial(link="logit")) #***BEST 


#no RE for year
mod.2.noRE <- glmmTMB(nest.success ~ nest.status + Egg_Number, data = mod.2.data, family = binomial(link="logit"))

#all
mod.2.area <- glmmTMB(nest.success ~ nest.status  + Egg_Number + (1|M.banding.year) + (1|area), data = mod.2.data, family = binomial(link="logit"))


#test to see which is better
AIC(mod.2.MOCH,mod.2.test,mod.2.egg,mod.2.noRE,mod.2.area, mod.2.null) #no real difference in any. will use model with just banding year random effect


######results######
summary(mod.2.egg)
#no significance of nest status

#check against null 
mod.2.null <- glmmTMB(nest.success ~1 + (1|M.banding.year), data = mod.2.data, family = binomial)

AIC(mod.2.test,mod.2.null)
# same, but non significant results so that makes sense

######try chi square######
selected <- mod.2.data %>% mutate(
  nest.success = recode(nest.success, `1` = "success", `0` = "fail"),
  nest.status  = recode(nest.status,  `1` = "infected", `0` = "uninfected")
  ) %>%
  select(nest.success, nest.status)
print(selected)
table <- table(selected$nest.status, selected$nest.success)
print(table)

chi <- chisq.test(table)
#"Warning message:In chisq.test(table) : Chi-squared approximation may be incorrect print(chi)"
# it think bc sample sizes are low?

fish <-fisher.test(table, alternative = "two.sided")
print(fish)
######assumptions #####
check_model(mod.2.egg)
binned_residuals(mod.2.egg)

#something weird #something weird #something weird with binned residuals

###### plots ######

mod.2.data %>%
  ggplot()+
  geom_mosaic(aes(x = product(nest.success,nest.status), fill = nest.success, weight = 1))+
  labs(x = "Nest Infection status",y = "Nest Success", title = "Nest Infection status vs Nest success", fill = "Nest Success")+
  theme_grey(base_size = 15)+
  scale_x_productlist(labels = c("0" = "Uninfected", "1" = "Infected"))+
  scale_fill_manual(
    values = c("1" = "#FFB900", "0" = "#5773CC"),
    labels = c("0" = "Unsuccessful", "1" = "Successful")  # Legend labels
  )+
  geom_mosaic_text(aes(x = product(nest.success, nest.status), label = after_stat(.wt)), as.label = TRUE, size = 3.5)


##### Q3 - How does infection probability differ between sexes?  ####
# Predictor - Sex (binomial)
# Response - Infection status (binomial)
# - random effect - nest box_year (bc higher likelihood of having infection if partner does?)
# - co variate - month? (bc infection rate may be higher in certain months?)
## Test ##
# generalized linear model family = binomial

#filter data to make sure there are only M and F for sex
mod3.data <- indiv.data %>%
  filter(!is.na(Sex), !Sex == "", !Sex == "u", !Sex == "U", !Sex == "M ", #remove any sex that is not M or F (thats too bad :/ )
         !area == "", !Banding.Year == "NA", !Mite_status == "NA",#remove data with no area attached
         Species == "MOCH") #filter for MOCH


# reduce data

set.seed(4399)

#filter so there is only one individual per nest
mod3.redu <- mod3.data %>%
  group_by(Nest.year) %>%
  slice_sample(n = 1) %>%
  ungroup()



###### Models ######

 mod3.area <- glmmTMB(Mite_status ~ Sex + (1|area), data = mod3.redu, family = binomial)

mod3.no.RE <- glmmTMB(Mite_status ~ Sex, data = mod3.redu, family = binomial)

mod3.year.area <- glmmTMB(Mite_status ~ Sex + (1|area)+ (1|Banding.Year), data = mod3.redu, family = binomial)

mod3.year <- glmmTMB(Mite_status ~ Sex + (1|Banding.Year), data = mod3.redu, family = binomial)


AIC(mod3.area, #area as RE
    mod3.no.RE, # no area as RE
    mod3.year.area,# area and year as RE
    mod3.year) #just year RE

#mod3.RE.MOCH (just area as RE) is best

summary(mod3.area)

#same model but will ALL data
mod3.alldata <- glmmTMB(Mite_status ~ Sex + (1|area), data = mod3.data, family = binomial)
summary(mod3.alldata)

###### interpretation ######
invlogit <- function(x) { exp(x) / (1 + exp(x)) }

summary(mod3.RE.MOCH)
#logit = -2.4507  + (M)1.2217

#convert estimates into actual prob
logprobMMOCH <- -2.4507+ (1)*1.2217
probMMOCH <- invlogit(logprobMMOCH)

logprobFMOCH <- -2.4507 + (0)*1.2217
probFMOCH <- invlogit(logprobFMOCH)

#get prob difference between M and F MOCH
probMMOCH/probFMOCH
# = 2.8513

#Male MOCH are infected 2.85 times as often as females


####### Interpretation #######

#######TESTING ASSUMPTIONS######
# deviance (lab 12)
# summary(model)$deviance
# summary(model)#df.residual?
# ***you want the null deviance to be greater than your model***


###### assumptions ######

      # DEVIANCE #


mod3.NULL <-glmmTMB(Mite_status ~ 1+ (1|area), data = mod3.redu, family = binomial)
AIC(mod3.NULL)

deviance(mod3.area) #135.0731
deviance(mod3.NULL) #143.7447
#null deviance > model deviance so we are good!




check_overdispersion(mod3.area) # no overdispersion
check_model(mod3.area)
binned_residuals(mod3.area)

table(mod3.redu$Mite_status, mod3.redu$Sex)


###### plots #####

#make mite status a factor for plotting
mod3.redu$Mite_status <- factor(mod3.redu$Mite_status)


#opposite plot (x sex, y mite status)
ggplot(data = mod3.redu) +
  geom_mosaic(aes(x = product(Mite_status, Sex), fill = Mite_status, weight = 1)) +
  labs(
    x = "Sex",
    y = "Mite status",
    title = "MOCH Infection status by Sex",
    fill = "Infection Status"   # Legend title
  ) +
  #change size of axis/title labels
  theme_grey(base_size = 15)+
  #change colors y axis and labels for legend
  scale_fill_manual(
    values = c("1" = "salmon", "0" = "lightblue"),
    labels = c("0" = "Uninfected", "1" = "Infected")  # Legend labels
  ) +
  # add sample sizes in the boxes
  geom_mosaic_text(aes(x = product(Mite_status, Sex), label = after_stat(.wt)), as.label = TRUE, size = 3.5) +
  #change x axis group labels
  scale_x_productlist(labels = c("0" = "Female", "1" = "Male"))










#### Other figures ####



## plot for proportion infected indiv per area

library(ggmosaic)

indiv.data %>%
  filter(area != "", area != "CU", Species == "MOCH") %>%
  ggplot() +
  geom_mosaic(aes(x = product(Mite_status, area), fill = Mite_status)) +
  labs(
    y = "Infection status",
    x = "Area",
    title = "Proportion of infected MOCH individuals per area"
  )+
  scale_fill_manual(
    values = c("1" = "salmon", "0" = "lightblue"),
    labels = c("0" = "Uninfected", "1" = "Infected")  # Legend labels
  ) 

  
#test this?






str(indiv.data)



# table 1

#nest data
table(mod.1.data$nest.status,mod.1.data$area)
table(nest.data$nest.status,nest.data$area, nest.data$F.species)
count(nest.data, area == "SGR")

#indiv data

#plot proportion of nestlings lived (minus failed nests) 

nest.data %>%
  filter(!prop.lived == "0", !prop.lived == "1") %>% #
  ggplot(aes(x = nest.status, y = prop.lived))+
  geom_boxplot()+
  geom_jitter(
  )

prop.data <- nest.data %>%
  filter(!prop.lived == "0", !prop.lived == "1")

testmod <- glm(prop.lived ~ nest.status, data = prop.data)
summary(testmod)

hist(nest.data$prop.lived)





#test if females are more likely to eb infected if males are infected

testtest <- nest.data %>%
  filter(area == "SGR")
testmod <- glmmTMB(F.mite.status ~ M.mite.status + (1|area), data = nest.data, family = binomial)
summary(testmod)
