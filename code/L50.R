## L50.r ####
# Josh Mumm # 190925
# estimate length at 50% fem using logistic regression
# Sloppy, could use much tidying 
# Clunky, manually change area 1:3 in prep filter and last lines for by area file 

#LOAD ----
library(tidyverse)
read.csv('data/AWL_190920.csv') %>% 
  transmute(year = as.numeric(YEAR), Event = EVENT_ID, site = SITE_ID, Station = STATION_ID, pot = POT_ID, species = FK_SPECIES_CODE,
            sex = FK_SEX_CODE, freq = FREQUENCY, cl = CARAPACE_LENGTH_MM, wt = WEIGHT_GRAMS, eggDev = SHRIMP_EGG_DEVEL_CODE, 
            breed = SHRIMP_BREEDING_CODE, eggCol = SHRIMP_EGG_COLOR_CODE, eggDead = SHRIMP_DEAD_EGG_COUNT, parasite = SHRIMP_PARASITE_CODE) -> awl 
#Replicate freq 2s - these are from 2005 when only half males were measured. 
  awl %>% filter(freq == 2) -> twos
  rbind(awl,twos) -> awl
  awl$freq <- 1  
read.csv('data/potPerformance_190920.csv') %>% select( Event = EVENT_ID, site = SITE_ID, pot = POT_ID, Station = STATION, perf = FK_GEAR_PERFORMANCE_CODE, 
                                                       gearComment = GEAR_COMMENTS, sample = SAMPLE_POT ) -> pp 
read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT

##PREP ---- 
#join, filter, select
pp %>% select(Event, site, Station, pot, perf) %>%   
  right_join (awl %>% select(Event, site, Station, pot, year, species, sex, freq, cl))  %>% 
  filter (site != 11, !Station %in% c("E","E1","E2"), 
          perf == 1, species == 965, cl>0, sex %in% c(1,2)) %>% 
  left_join (siteStatLUT, by = c('site' ='SiteNum')) %>% arrange(year) %>% 
  mutate (fem = sex - 1) -> dat  

# Filter by area.  Manually changed for byAreaFile 
#dat%>% filter (ShrimpArea == 1) -> dat

## BY YEAR  ----
unique((dat$year)) -> yrs

par(mfcol=c(4,7))

#create df to hold fem50
f50 <- rep(999,length(yrs))
fem50 <-(cbind(yrs,f50))
Fem50 <- as.data.frame(fem50)

for( i in yrs ) {
  datT <- dat[dat$year == i, ]  
  plot(fem ~ cl, data = datT, pch = "|", xlim = c(32,58),
       ylab = "Prop Fem", xlab = "Carapace Length",
       main = i, type = "p", ylim = c(0, 1))
  
  BREAKS <- c(seq(32,56,1))  # break points for CL bins
  LN <- cut(datT$cl, BREAKS)         
  LN                       # Categorical variable with one level for each LN increment
  table(datT$sex, LN)      # summerizes number of males and fems points in each LN bin
  fem.prop <- tapply(datT$sex, LN, function(x) sum(x>1)/sum(x>0))
  fem.prop                  # propotion of fems of total obs in each LN bin
  
  
  x <- seq(32.5,55.5,1) # midpoints for each LN bin, for ploting. 
  points(x, fem.prop, pch=19, cex = 1.2, col = "red") # adds proportion points to plot 

  modT <- glm(fem ~ cl, family = "binomial", data = datT)  # logistic GLM of sex vs cl
  cfT <- coef(modT)     # etraxts coefficients from model, stores in vector cf[]
  curve( exp(cfT[1] + cfT[2]*x)/(1+exp(cfT[1] + cfT[2]*x)), 
         ylab = "Probability of fem", col ="red", add=T, lwd=2)  
  
  fem50T <- -cfT[1]/cfT[2]
  abline(v = fem50T, col = "red", lwd = 2 )
  
  Fem50[Fem50$yrs == i,2] <- fem50T
  
  curve( exp(cfT[1] + cfT[2]*x)/(1+exp(cfT[1] + cfT[2]*x)), 
         ylab = "Probability of fem", col ="blue", add=T, lwd=2)  

  fem50 <- -cfT[1]/cfT[2]
  abline(v = fem50, col = "blue", abline = 2 )
  }  

par(mfrow=c(1,1))
plot(f50~yrs, dat = Fem50,type = "l", main = "Length @ 50% Female. Blue is '92 to '16 mean", xlab = "year", ylab = "carapace length")
points(f50~yrs, dat = Fem50,  pch = 19, col="red", cex = 1.5, add = "TRUE")
abline(h=mean(Fem50$f50), col = "blue", lwd = 2)

# Fem50 -> Fem50_2  # mannually change for by area 
# colnames(Fem50_1) <- c('year', 'f50_1') 
# colnames(Fem50_2) <- c('year', 'f50_2') 
# colnames(Fem50_3) <- c('year', 'f50_3') 
# Fem50_1 %>% left_join(Fem50_2) %>% left_join(Fem50_3) -> Fem50_byArea

#write.csv(Fem50_byArea, "output/f50_byArea.csv")
#write.csv(Fem50,"output/f50_surveyWide.csv")
