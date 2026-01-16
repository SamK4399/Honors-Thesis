

#all needed packages
  #general
library(dplyr)
library(ggplot2)
library(tidyr)
library(glmmTMB)

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
  

###### NESTLING BODY CONDITION ######

#import data 
m.nest.data <- read.csv("ALL.NESTLING.csv")


#write csv
#write.csv(nest.BC ,"nest.BC.csv", row.names = FALSE)

######Body condition with WING CHORD ######


#######data setup ########
#remove rows that do not have nestling weight data and save to new DF
nestling.cond.data = m.nest.data



##sara stuff translated from adult body condition to nestling#
##im not sure if this is a valid way to do this because its translated from wing to tarsus for residual##

nestling.cond.wing.data <- nestling.cond.data %>%
  mutate(
    wing   = suppressWarnings(as.numeric(Wing.Chord)),
    mass_g = suppressWarnings(as.numeric(Bird.Weight))
  )

# Keep only rows with both values
dat_cond_wing <- nestling.cond.wing.data %>%
  filter(!is.na(mass_g), !is.na(wing), mass_g > 0, wing > 0)

if (nrow(dat_cond_wing) < 10) {
  warning("Few rows with both mass and wing available (n = ", nrow(dat_cond_wing), ").")
}

# Fit regression of body mass on wing
cond_lm_wing <- lm(log(mass_g) ~ log(wing), data = dat_cond_wing)

# Add residuals back into the main dataframe
nestling.cond.wing.data$cond_resid <- NA_real_
nestling.cond.wing.data$cond_resid[as.integer(rownames(dat_cond_wing))] <- resid(cond_lm_wing)

#write.csv(nestling.cond.wing.data ,"nest.BC.wing.csv", row.names = FALSE)
######## actual analysis ########
nest.BC <- read.csv("ALL.NEST.csv")
str(nest.BC)


#add column for nest status
nest.BC$nest.status <- as.factor(ifelse(nest.BC$Num.mite.per.nest > 0, 1, 0))

#graph
nest.BC %>%
  ggplot(aes(x = nest.status, y = avg.nestling.BC.wing))+
  geom_boxplot()+
  labs(x = "Nest Infection Status", y = "Average Nestling Body Condition", title= "Nest infection status vs Nestling 
Body Condition (wing chord)")+
  scale_x_discrete(labels= c("Uninfected","Infected"))

#

wing.BC.lm <- glmmTMB(avg.nestling.BC.wing ~ nest.status + (1|area) + (1|Nest.year)+(1|Nestling_Number),nest.BC)
wing.BC.lm2 <- glmmTMB(avg.nestling.BC.wing ~ nest.status + (1|Nest.year)+(1|Nestling_Number),nest.BC)

AIC(wing.BC.lm,wing.BC.lm2)
summary(wing.BC.lm)


##NO SIGNIFICANCE

#####Body condition with TARSUS #####


                          ####### data setup #####
#remove rows that do not have nestling weight data and save to new DF
nestling.cond.data = m.nest.data



##sara stuff translated from adult body condition to nestling#
##im not sure if this is a valid way to do this because its translated from wing to tarsus for residual##

nestling.cond.data.tarsus <- nestling.cond.data %>%
  mutate(
    tarsus   = suppressWarnings(as.numeric(Tarsus.Length)),
    mass_g = suppressWarnings(as.numeric(Bird.Weight))
  )

# Keep only rows with both values
dat_cond_tarsus <- nestling.cond.data.tarsus %>%
  filter(!is.na(mass_g), !is.na(tarsus), mass_g > 0, tarsus > 0)

if (nrow(dat_cond_tarsus) < 10) {
  warning("Few rows with both mass and tarsus available (n = ", nrow(dat_cond_tarsus), ").")
}

# Fit regression of body mass on tarsus
cond_lm_tarsus <- lm(log(mass_g) ~ log(tarsus), data = dat_cond_tarsus)

# Add residuals back into the main dataframe
nestling.cond.data.tarsus$cond_resid <- NA_real_
nestling.cond.data.tarsus$cond_resid[as.integer(rownames(dat_cond_tarsus))] <- resid(cond_lm_tarsus)








                          ##### actual analysis ####

#graph
nest.BC %>%
  ggplot(aes(x = nest.status, y = avg.nestling.BC.tarsus))+
  geom_boxplot()+
  labs(x = "Nest Infection Status", y = "Average Nestling Body Condition", title= "Nest infection status vs Nestling 
Body Condition")+
  scale_x_discrete(labels= c("Uninfected","Infected"))

#

tarsus.BC.lm <- glmmTMB(avg.nestling.BC.tarsus~nest.status + (1|area) + (1|Nest.year)+(1|Nestling_Number),nest.BC)

summary(tarsus.BC.lm)

#NO SIGNIFICANCE




###### NESTLING NUMBER ######
#see biostsats project


###### EGG NUMBER #######

nest.data <- read.csv("ALL.NEST.csv")

#add column for nest status
nest.data$nest.status <- as.factor(ifelse(nest.data$Num.mite.per.nest > 0, 1, 0))

# check all variables for NAs
table(nest.data$Egg_Number, useNA = "ifany")
table(nest.data$nest.status, useNA = "ifany")
table(nest.data$M.banding.year, useNA = "ifany")
table(nest.data$area, useNA = "ifany")
table(nest.data$elevation.m, useNA = "ifany")
table(nest.data$julian.CI.date, useNA = "ifany")


#remove NAs etc
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


#### do egg analysis again for female infected 

egg.num.lm.F <- glmmTMB(Egg_Number ~  F.mite.status + (1|M.banding.year) + (1|area), data = egg.num.data, family = compois)

summary(egg.num.lm.F)

#for male infected
egg.num.lm.M <- glmmTMB(Egg_Number ~  M.mite.status + (1|M.banding.year) + (1|area), data = egg.num.data, family = compois)

summary(egg.num.lm.M)

##no effect when female is infected but effect when male is infected?

##### ADULT BODY CONDITION #####
indiv.data <- read.csv("individual.data.csv")




indiv.data <- indiv.data %>%
  mutate(
    wing   = suppressWarnings(as.numeric(Wing.Chord)),
    mass_g = suppressWarnings(as.numeric(Bird.Weight))
  )

# Keep only rows with both values
dat_cond_adult_wing <- indiv.data %>%
  filter(!is.na(mass_g), !is.na(wing), mass_g > 0, wing > 0)

if (nrow(dat_cond_adult_wing) < 10) {
  warning("Few rows with both mass and wing available (n = ", nrow(dat_cond_adult_wing), ").")
}

# Fit regression of body mass on wing
cond_adult_lm_wing <- lm(log(mass_g) ~ log(wing), data = dat_cond_adult_wing)

# Add residuals back into the main dataframe
indiv.data$body.cond.wing <- NA_real_
indiv.data$body.cond.wing[as.integer(rownames(dat_cond_adult_wing))] <- resid(cond_adult_lm_wing)

write.csv(indiv.data ,"individual.data.csv", row.names = FALSE)

                    ## Analysis ##
#analyze only MOCH
adult.BC.data <- indiv.data %>%
  filter(Species == "MOCH", !Sex == "U", !Sex == "")

table(adult.BC.data$Sex)

#possible predictors
    # area
    # sex
    # month/year
##I CHECKED MODELS WITH ALL OF THESE AND NONE OF THEM CHANGED THE AIC 
adult.BC.lm <- glmmTMB(body.cond.wing ~ Mite_status, adult.BC.data)

summary(adult.BC.lm)

#### INFECTIONS PER AREA ####


jhg
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











                          #### MAPS ####


library(ggplot2)
library(sf)
library(dplyr)
library(ggspatial)
library(raster)
library(prettymapr)




#reading in with code might import weird (names of columsn shifted down?). importing manually fixes it
all_mite_elevvv <- mite.data
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
mite.data$Elevation.m <- NA  

# Drop missing coords for sf conversion
data.with.coord <- subset(mite.data, !is.na(lat) & !is.na(long))

# Convert only clean rows
coords_sf <- st_as_sf(data.with.coord, coords = c("long", "lat"), crs = 4326)

# Get elevations
elev <- get_elev_point(coords_sf, prj = st_crs(coords_sf)$proj4string, src = "aws")

# Put values back into original df (matching by rownames)
mite.data$Elevation.m[as.numeric(rownames(data.with.coord))] <- elev$elevation


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
