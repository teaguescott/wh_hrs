library(tidyverse)
library(sp)
library(lubridate)
library(ctmm)
library(parallel)

movedata <- read.csv("errythang.csv")
movedata <- as_tibble(movedata)


levels(movedata$individual.local.identifier)[levels(movedata$individual.local.identifier)=="Tumaine - White-headed 88"] <- "WH88_16"


movedata <- movedata %>% filter(location.long > 30)


movedata <- movedata %>% filter(str_detect(individual.local.identifier, "WH"))
movedata <- droplevels(movedata)


movedata$timestamp <- ymd_hms(movedata$timestamp, tz = "UTC")
movedata$timestamp <- with_tz(movedata$timestamp, tzone = "Africa/Maputo")

##############################

movedata <- movedata %>% mutate(birdID = individual.local.identifier) %>% mutate(yrmo = str_sub(timestamp,1,7)) %>% mutate(individual.local.identifier = as.factor(paste0(birdID,"-",yrmo))) %>% select(-yrmo)

unique(movedata$individual.local.identifier)

movedata <- movedata %>% filter(between(timestamp, ymd_hms("2016-07-01 01:00:00"), ymd_hms("2019-07-01 00:00:00")))

movedata <- movedata %>% mutate(day = str_sub(movedata$timestamp,9,10))
movedata$day <- as.factor(movedata$day)
day_summary <- movedata %>% group_by(individual.local.identifier) %>% summarise(n_day = length(unique(day)))
day_summary <- day_summary %>% filter(n_day >= 25) %>% droplevels()

ids <- unique(day_summary$individual.local.identifier)
movedata <- movedata %>% filter(individual.local.identifier %in% ids) %>% select(-day)
movedata$individual.local.identifier <- droplevels(movedata$individual.local.identifier)

grp_move <- movedata %>% group_by(birdID) %>% summarise(start = min(timestamp), end = max(timestamp))
range(grp_move$timestamp)


###############################

calib <- read.csv("birds_calib.csv")
calib <- as_tibble(calib)
calib <- calib %>% filter(individual.local.identifier=="calib")
calib <- droplevels(calib)
calib$timestamp <- ymd_hms(calib$timestamp, tz = "UTC")
calib$timestamp <- with_tz(calib$timestamp, "Africa/Maputo")

###############################

movedata <- movedata[with(movedata,order(movedata$individual.local.identifier,movedata$timestamp)),]
calib <- calib[with(calib,order(calib$individual.local.identifier,calib$timestamp)),]

######################################

Models <- paste0(getwd(),"/output/models")
UDs <- paste0(getwd(),"/output/UDs")
Figs <- paste0(getwd(),"/output/figures")
Data <- paste0(getwd(),"/data")
Output <- paste0(getwd(),"/output")

library(move)

movedata <- as.data.frame(movedata)

wh.telem <- as.telemetry(movedata)

calib <- as.data.frame(calib)

calib.telem <- as.telemetry(calib) 

UERE <- uere.fit(calib.telem)

uere(wh.telem) <- UERE 

rm("calib.telem", "calib")

wh.out <- outlie(wh.telem, plot = FALSE)

for(i in 1:length(wh.telem))
{
  wh.telem[[i]] <- wh.telem[[i]][which(wh.out[[i]]$speed<30),]
}

wh.out <- outlie(wh.telem, plot = FALSE)

########
print("creating variograms")

SVF <-  lapply(wh.telem, variogram)

saveRDS(SVF, file = paste0("./output/varios", Sys.Date(), ".rda"))

print("guessing best fit")

GUESSES <- lapply(wh.telem, function(c){ctmm.guess(c, CTMM=ctmm(error = T), interactive = FALSE)})

saveRDS(GUESSES, file = paste0("./output/guesses", Sys.Date(), ".rda"))

print("testing movement models")

SELECTS <- lapply(1:length(wh.telem), function(i){ctmm.select(wh.telem[[i]], GUESSES[[i]], verbose=TRUE, trace = TRUE, cores = 20)})

saveRDS(SELECTS, file = paste0("./output/mods", Sys.Date(), ".rda"))

print("selecting best-fit MM")

best_fit <- list()
for (i in 1:length(SELECTS)) {
  if(class(SELECTS[[i]]) == "list") {best_fit[[i]] <- SELECTS[[i]][[1]]} else {
    best_fit[[i]] <- SELECTS[[i]]}
}

saveRDS(best_fit, file = paste0("./output/best_fit", Sys.Date(), ".rda"))

print("producing AKDE estimates using best-fit MM")

AKDES <- akde(wh.telem, best_fit)

saveRDS(AKDES, file = paste0("./output/akdes", Sys.Date(), ".rda"))