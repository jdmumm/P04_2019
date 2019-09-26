## TabsFigs ####
# Josh Mumm 190920
# tables and figures for P04 FDS report
# run CPUE.r first to create cpue output tables
# modified from P04_2017BOF/TabsFigs_WVarFigs.R which was used for 2017 report  
# major changes include switch to use point estimates from CPUE.R rather than
# spreadsheet based on Access queries as KG and JR insisted in 2017.  

## Load and Prep ----
library(tidyverse)
library(reshape2) # ought to reshape with tidyr instead, so not required. 
library(extrafont)
font_import()
loadfonts(device="win")
windowsFonts(Times=windowsFont("TT Times New Roman"))
theme_set(theme_bw(base_size=12,base_family='Times New Roman')+ 
            theme(panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank()))
read.csv('output/cpue_surveyWide.csv') -> cpue # catch and cpue from cpue.r, survey-wide
read.csv('output/cpue_byArea.csv') -> cpue_area # catch and cpue from cpue.r, byArea
read.csv('data/AWL_190920.csv') %>% 
         transmute(year = YEAR, Event = EVENT_ID, site = SITE_ID, Station = STATION_ID, pot = POT_ID, species = FK_SPECIES_CODE,
         Sex = as.factor (FK_SEX_CODE), freq = FREQUENCY, cl = CARAPACE_LENGTH_MM, wt = WEIGHT_GRAMS, eggDev = SHRIMP_EGG_DEVEL_CODE, 
         breed = SHRIMP_BREEDING_CODE, eggCol = SHRIMP_EGG_COLOR_CODE, eggDead = SHRIMP_DEAD_EGG_COUNT, parasite = SHRIMP_PARASITE_CODE) -> awl 
  #Replicate freq 2s - these are from 2005 when only half males were measured. 
    awl %>% filter(freq == 2) -> twos
    rbind(awl,twos) -> awl
    awl$freq <- 1  
read.csv('data/potPerformance_190920.csv') %>% select( Event = EVENT_ID, site = SITE_ID, pot = POT_ID, Station = STATION, perf = FK_GEAR_PERFORMANCE_CODE, 
                                                        gearComment = GEAR_COMMENTS, sample = SAMPLE_POT ) -> pp 
read.csv("data/PWS Shrimp All.csv") %>% # from K:\MANAGEMENT\SHELLFISH\PWS Shrimp All.xlsx. Through 2017, ask CR for update with 18 & 19. 
  select (year = DOL.Year, species = Species.Code, stat=Stat.Area, pots = Effort..sum., lbs = Whole.Weight..sum.) -> harv
read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT
read.csv("data/yearArea_LUT.csv") -> yearAreaLUT
read.csv('output/f50_surveyWide.csv') %>% select(year = yrs, f50) -> l50 
read.csv('output/f50_byArea.csv') %>% select (year, 'Area 1' = f50_1, 'Area 2' = f50_2, 'Area 3' = f50_3) -> l50_a
k <- 2.20462 # kilogram to lb conversion factor 

## T2. Catch and CPUE, surveyWide ----
  cpue %>% transmute(year,N,
                  tau_all_lb = tau_all_kg * k, tau_all_cnt,
                  mu_all_lb = mu_all_kg * k, mu_all_cnt, 
                  tau_lrg_lb = tau_lrg_kg * k, tau_lrg_cnt,
                  mu_lrg_lb = mu_lrg_kg * k, mu_lrg_cnt) %>% 
  write.csv ('output/t2_catchAndCpue_surveywide.csv')

## T3. Biological sex ratio, prop ovigerous, and mean survey-wide ---- 
  # CL 
    pp %>% select(Event, site, Station, pot, perf) %>%   
      right_join (awl)  %>% 
      filter (site != 11, !Station %in% c("E","E1","E2"), 
              perf == 1, species == 965, Sex %in% c(1,2)  ) -> awls  # excluding transitionals 
    awls %>% group_by (year, Sex)  %>% summarise( # issue where freq = 2 in 2005 for males, but since grouping by sex and virtually all males in 2005 were 2, ok as is 
      n = n(),   
      len = mean (cl), 
      sd = var(cl)^.5,
      se = sd/(n^.5))-> meanLen
    
    meanLen %>% select (year, Sex, len) %>% spread(Sex,len) -> cl #clunky, should rewrite 
    meanLen %>% select (year, Sex, n) %>% spread(Sex,n) -> n 
    meanLen %>% select (year, Sex, sd) %>% spread(Sex,sd) -> sd 
    meanLen %>% select (year, Sex, se) %>% spread(Sex,se) -> se 
    left_join (n,cl, by = 'year') %>% left_join(se, by = 'year') -> CL
    colnames(CL) <- c('year','n_m', 'n_f', 'cl_m', 'cl_f', 'se_m', 'se_f') 
  
  # Sex proportions 
    awls %>% group_by(year,Sex) %>% summarise(cnt =  sum(freq)) %>%
      spread(Sex, cnt) -> sexCnt 
    colnames(sexCnt) <- c('year','m','f') 
    sexCnt %>% group_by(year) %>% summarise (pf = f/(m+f), pm = m/(m+f)) -> sexProp
  
  # Prop ovigerous 
    awls %>% filter (Sex == 2, eggDev %in% c(0,1,2)) %>%  group_by (year) %>% summarise (femValidEgg = n()) %>%# females with valid egg dev codes
      left_join(awls %>% filter (Sex == 2, eggDev %in% c(1,2)) %>%  group_by (year) %>% summarise (femWithEgg = n())) %>% #  females with eggs
      transmute (year, pOvig = femWithEgg/femValidEgg) -> ovigProp
  
  #join, reorder, format, and write 
    left_join(CL,sexProp) %>% left_join(ovigProp) %>% 
      transmute(perMal = round(100 * pm,1), 
                perFem = round(100 * pf, 1),
                perOvi = round(100 * pOvig,1), 
                cl_m = round(cl_m, 1),
                n_m, 
                se_m = round(se_m, 2), 
                cl_f = round(cl_f, 1),
                n_f,
                se_f = round (se_f, 2)) -> bio 
    
    write.csv(bio,"output/t3_sexEggPropCL.csv") # 1996 bio data are missing, use prev published values (1996, pM = 94.9, pf = 5.1; Wessel et al., 2015)

## T4. Catch and CPUE, byArea ----
  # Harvest - aggregate     
      harv %>% left_join (yearAreaLUT) %>% # join shrimpArea to harvest
      na.omit %>%  #exclude those 814 records with null effort
      group_by (year,area) %>% summarize (cpueAllLb = sum(lbs)/sum(pots)) %>%
      dcast(year ~ area, value.var = "cpueAllLb") -> cpueByArea_h #Commercial cpue by area. 
  # Survey - select and reshape
      cpue_area %>% transmute (year, Area, mu_all_lb = mu_all_kg * k)  %>%
        dcast(year ~ Area, value.var = "mu_all_lb") -> cpueByArea_s
  # Join harvest to survey and write  
    left_join(cpueByArea_s,cpueByArea_h, by = "year", suffix = c("_survey","_commerical")) -> cpueByArea
    
    write.csv(cpueByArea,"output/t4_CPUEallLb_byArea.csv") 
      #commercial values don't match those in draft 2017 report. Presumably mgmt overwrote values in report. 

## F3. CPUE, surveyWide ----
  # reshape cpue to long and convert to lb
    cpue %>% transmute(
        year,
        all = mu_all_kg * k,
        lrg = mu_lrg_kg * k) %>% 
      gather(class, mu, c(all, lrg)) %>% 
    left_join(  
      cpue %>% transmute(
          year,
          all = se_all_kg * k,
          lrg = se_lrg_kg * k) %>% 
        gather(class, se, c(all, lrg))) -> cpue_l
  # calc longterm avgs 
    cpue_l %>% group_by(class) %>% mutate (avg = mean(mu, na.rm = TRUE)) -> cpue_l
  # plot 
    cpue_l %>% ggplot(aes(x = year, y = mu, group = class, colour = class) )+
      scale_color_grey(start=.1, end=0.5,  name = '', labels = c("All Sizes", "Larges (>32mm)")) +
      theme(legend.position = c(.2,.8)) +
      scale_x_continuous(breaks = seq(1990,2018,2))  +
      scale_y_continuous(breaks = seq(0,3,.5)) + 
      labs( x= 'Year', y = 'Mean weight per pot (lb)') +
      geom_point(size = 2)+ 
      geom_line () +
      geom_errorbar(aes(ymin=mu-se, ymax=mu+se, width = 0),position = position_dodge(width = 0.00)) + 
      geom_hline(yintercept = unique(cpue_l$avg), colour = grey(c(.1,.5)), lty = 'dashed')
    
    ggsave("./figs/f3_CPUE_surveyWide.png", dpi=300, height=4.0, width=6.5, units="in")    

## F4. Mean CL, surveyWide ----
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)  %>% 
    filter (site != 11, !Station %in% c("E","E1","E2"), 
            perf == 1, species == 965) %>% 
    group_by(year) %>% summarise(    
      n = n(),   
      len = mean (cl), 
      sd = var(cl)^.5,
      se = sd/(n^.5))-> meanLen
    
    ggplot(meanLen, aes (x=year, y = len)) +
      scale_x_continuous(breaks = seq(1990,2018,2))  +      
      scale_y_continuous(breaks = seq(28,34,1)) +
      labs( x= 'Year', y = 'Mean CL (mm)') +
      geom_point(size = 2)+
      geom_line()+
      geom_errorbar(aes(ymin=len-se, ymax=len+se, width = 0)) + 
      geom_hline(yintercept = mean(meanLen_bth$len, na.rm=T),lty = 'dashed')
    ggsave("./figs/f4_meanCL.png", dpi=300, height=4., width=6.5, units="in")
    
## F5. CL histograms, surveyWide ----    
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)  %>%
    filter (site != 11, !Station %in% c("E","E1","E2"), 
            perf == 1, species == 965, Sex %in% c('1','2')) %>% # excluding transitionals
  
  ggplot(aes(cl, fill = Sex ))+ 
    scale_fill_manual(values=c("#bdbdbd", "#636363"), labels = c('Male','Female'), 
                      guide = guide_legend(direction = "horizontal")) +
    facet_wrap(~year, ncol = 1, dir = 'v', strip.position="right", scale='free_y')+
    geom_histogram(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),
                   alpha=.8, bins=60, color = 1)+
    ylab("Proportion")+
    scale_y_continuous(breaks = seq(.01,.04,.03)) +
    xlab("Carapace Length (mm)")+
    scale_x_continuous(breaks = seq(10,55,5), limits = c(15,55))+
    theme(panel.spacing.y = unit(0, "lines"), legend.title=element_blank(), 
          legend.position = c(.87,-.04), legend.background = element_rect (fill = "transparent" ))          
  
    ggsave("./figs/f5_CL_Hist_surv.png", dpi=300, height=8.7, width=6.5, units="in")

## F6. L50, surveyWide----
  avg <- mean(l50$f50, na.rm = T) # calc longterm avg
  
  l50 %>% ggplot(aes(x = year, y = f50) ) +
    scale_x_continuous(breaks = seq(1990,2016,2))  +
    scale_y_continuous(breaks = seq(38,43,1)) + 
    labs( x= 'Year', y = 'L50 (mm)') +
    geom_point(size = 2)+ 
    geom_line () +
    geom_hline(yintercept = avg, lty = 'dashed')
  
  ggsave("./figs/f6_L50_surveyWide.png", dpi=300, height=3.5, width=6.25, units="in")

## F7. L50, byArea ----
  l50_a %>% gather(area, l50, 2:4) -> l50_a_l # reshape
  l50_a_l %>% group_by(area) %>% summarise (avg = mean(l50, na.rm = T))-> avgs # calc longterm avgs
  
  l50_a_l %>% ggplot(aes(x = year, y = l50)) +
    scale_x_continuous(breaks = seq(1990,2016,2))  +
    scale_y_continuous(breaks = seq(37,43,1)) + 
    labs( x= 'Year', y = 'L50 (mm)') +
    geom_point(size = 1.5)+ 
    geom_line ()  +
    theme( axis.text.x  = element_text(angle=0, vjust=0.5)) +
    facet_wrap(~area, ncol=1, strip.position="right") + 
    geom_hline(aes (yintercept = avg), avgs, lty = 'dashed')
  
  ggsave("./figs/f7_L50_byArea.png", dpi=300, height=4.5, width=6.5, units="in")    
## F8. Prop fem, surveyWide ----
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)  %>% 
    filter (site != 11, !Station %in% c("E","E1","E2"), 
            perf == 1, species == 965, Sex %in% c('1','2')) %>% 
  group_by(year,Sex) %>% summarise(cnt =  sum(freq)) %>%
  spread(Sex, cnt) -> wid 
  colnames(wid) <- c('year','m','f')
  wid %>% group_by(year) %>% summarise (pf = f/(m+f)) -> pfem
  
  avg <- mean(pfem$pf) # calc longterm avg
  
  pfem %>% ggplot(aes(x = year, y = pf) ) +
    scale_x_continuous(breaks = seq(1990,2018,2))  +
    scale_y_continuous(breaks = seq(0,.4,.1)) +
    labs( x= 'Year', y = 'Female proportion') +
    geom_point(size = 2)+ 
    geom_line () +
    geom_hline(yintercept = avg, lty = 'dashed')
  
  ggsave("./figs/f8_propFem_surv.png", dpi=300, height=3.5, width=6.25, units="in")

## F9. Prop fem, byArea ----
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)  %>% 
    filter (site != 11, !Station %in% c("E","E1","E2"), 
            perf == 1, species == 965, Sex %in% c('1','2')) %>% 
    left_join (siteStatLUT, by = c("site"="SiteNum")) %>% 
    group_by(ShrimpArea, year,Sex) %>% summarise(cnt =  sum(freq)) %>%
    spread(Sex, cnt) -> wid
    colnames(wid) <- c('area','year','m','f') 
    wid  %>% group_by(area, year) %>% transmute (pf = f/m) -> pfem_a
    
  pfem_a %>% group_by(area) %>% summarise (avg = mean(pf, na.rm = T))-> avgs # calc longterm avgs
  labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")
  
  pfem_a %>% ggplot(aes(x = year, y = pf)) +
    scale_x_continuous(breaks = seq(1990,2018,2))  +
    scale_y_continuous(breaks = seq(0,.65,.1)) + 
    labs( x= 'Year', y = 'Female proportion') +
    ylim(0,.65) +
    geom_point(size = 1.5)+ 
    geom_line () +
    theme( axis.text.x  = element_text(angle=0, vjust=0.5)) +
    facet_wrap(~area, ncol=1, strip.position="right", labeller=labeller(area = labels)) + 
    geom_hline(aes (yintercept = avg), avgs, lty = 'dashed')
  
  ggsave("./figs/f9_propFem_area.png", dpi=300, height=4.5, width=6.5, units="in")
 
## F10. CPUE, byArea ----
  # reshape cpue to long and convert to lb  
    cpue_area %>% transmute(
      Area,
      year,
      all = mu_all_kg * k,
      lrg = mu_lrg_kg * k) %>% 
      gather(class, mu, c(all, lrg)) %>% 
      left_join(  
        cpue_area %>% transmute(
          Area,
          year,
          all = se_all_kg * k,
          lrg = se_lrg_kg * k) %>% 
          gather(class, se, c(all, lrg))) -> cpue_l   
  # calc longterm avgs 
    cpue_l %>% group_by(class, Area) %>% summarise (avg = mean(mu, na.rm = TRUE))-> avgs 
    #Specific values included in results
      cpue_l %>% filter (Area == '1', class == 'lrg', year > 2004) %>% group_by(Area) %>% summarise (avg =  mean(mu, na.rm = T))
      cpue_l %>% filter (Area == '1', class == 'lrg', year < 2004) %>% group_by(Area) %>% summarise (avg =  mean(mu , na.rm = T))
      cpue_l %>% filter (class == 'all') %>% group_by(Area) %>% summarise (avg =  mean(mu, na.rm = T))  
      
  labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")
  
  cpue_l %>%     
    ggplot(aes(x = year, y = mu, group = class, colour = class) ) +
    scale_color_grey(start=.1, end=0.5,  name = '', labels = c("All Sizes", "Larges (>32mm)")) +
    theme(legend.position = c(.85,.8), legend.background = element_rect (fill = "transparent" )) +
    scale_x_continuous(breaks = seq(1990,2018,2))  +
    scale_y_continuous(breaks = seq(0,4,.5)) + 
    labs( x= 'Year', y = 'Mean weight per pot (lb)') +
    geom_point(size = 1.5)+ 
    geom_line ()  +
    geom_errorbar(aes(ymin=mu-se, ymax=mu+se, width = 0), position = position_dodge(width = 0.0)) + 
    theme( axis.text.x  = element_text(angle=90, vjust=0.5)) +
    facet_wrap(~Area, ncol=3, labeller=labeller(ShrimpArea = labels)) +
    geom_hline(aes (yintercept = avg), avgs, colour = rep(grey(c(.1,.5)),3), lty = 'dashed')
  
  ggsave("./figs/f10_cpue_byArea.png", dpi=300, height=2.9, width=9, units="in")  
 
## F11. Mean CL, byArea ----
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)   %>% 
    filter (site != 11, !Station %in% c("E","E1","E2"), 
            perf == 1, species == 965) %>%         
    left_join (siteStatLUT, by = c('site' ='SiteNum')) %>%
    group_by(ShrimpArea, year) %>% summarise(   
      n = n(),   
      len = mean (cl), 
      sd = var(cl)^.5,
      se = sd/(n^.5))-> meanLen_byArea
  
  meanLen_byArea %>% group_by(ShrimpArea) %>% summarise(avg = mean(len, na.rm = TRUE)) -> avgs
  
  labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")
  
  ggplot(data = meanLen_byArea,
              aes (x=year, y = len)) +
    scale_x_continuous(breaks = seq(1990,2018,2))  +      
    theme( axis.text.x  = element_text(angle=90, vjust=0.5)) +
    scale_y_continuous(breaks = seq(27,38,1)) +
    labs( x= 'Year', y = 'Mean CL (mm)')+
    geom_point()+
    geom_line()+ 
    facet_wrap(~ShrimpArea, labeller=labeller(ShrimpArea = labels)) +
    geom_errorbar(aes(ymin=len-se, ymax=len+se, width = 0))+
    geom_hline(aes(yintercept = avg) , avgs, lty = 'dashed')
  
  ggsave("./figs/f11_meanCL_byArea.png", dpi=300, height=2.9, width=9, units="in")

## F12. CL histograms, byArea ----
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)  %>% 
    filter (site != 11, !Station %in% c("E","E1","E2"), 
            perf == 1, species == 965, Sex %in% c('1','2')) %>% # excluding transitionals
    left_join (siteStatLUT, by = c('site' ='SiteNum')) %>% 
  
    ggplot(aes(cl, fill = Sex)) +
    scale_fill_manual(values=c("#bdbdbd", "#636363"), labels = c('Male','Female'), 
                      guide = guide_legend(direction = "horizontal")) +   
    geom_histogram(aes(y = ..count.. / sapply(PANEL, FUN=function(x) sum(count[PANEL == x]))),
                   alpha=.8, bins=60, color = 1)+
    facet_grid(year ~ ShrimpArea, scale='free_y',labeller=labeller(ShrimpArea = labels)) +
    scale_x_continuous(breaks = seq(10,55,5), limits = c(15,55))+
    ylab("Proportion")+
    scale_y_continuous(breaks = seq(.02,.06,.04)) +
    xlab("Carapace Length (mm)")+
    theme(panel.spacing.y = unit(0, "lines"), legend.title=element_blank(), 
          legend.position = c(.87,-.04), legend.background = element_rect (fill = "transparent" ))   
  
  ggsave("./figs/f12_CL_Hist_byArea.png", dpi=300, height=8.7, width=6.5, units="in")  


## apxB1. CPUE, survey and comm, by stat
  # commercial 
    harv %>% left_join (yearAreaLUT) %>% na.omit -> harv #exclude those 814 records with null effort
    # aggregate by StatArea
    harv %>% group_by (year,stat) %>% summarize (
      cpueAllLb = sum(lbs)/sum(pots)) -> cpueByStat  
    dcast(cpueByStat, year ~ stat, value.var = "cpueAllLb" ) -> cpueByStat_h
  
  # survey 
    
    
  
## apxC1. Ovig, by stat area  ----
  pp %>% select(Event, site, Station, pot, perf) %>%   
    right_join (awl)  %>% 
    filter (!Station %in% c("E","E1","E2"), 
            perf == 1, species == 965) %>% 
    left_join (siteStatLUT, by = c('site' = 'SiteNum')) -> awls   

    # Prop ovigerous by stat 
    awls %>% filter (Sex == 2, eggDev %in% c(0,1,2)) %>%  group_by (year, StatArea) %>% summarise (femValidEgg = n()) %>% # females with valid egg dev codes
    left_join (awls %>% filter (Sex == 2, eggDev %in% c(1,2)) %>%  group_by (year, StatArea) %>% summarise (femWithEgg = n())) %>% #  females with eggs
    mutate (pOvig = femWithEgg/femValidEgg) %>% select(year,StatArea,pOvig) %>% 
    spread(StatArea,pOvig) -> pOvig_byStat
    
    # Prop ovigerous, surveywide  
    awls %>% filter (site != 11, Sex == 2, eggDev %in% c(0,1,2)) %>%  group_by (year) %>% summarise (femValidEgg = n()) %>% # valdez excluded 
      left_join (awls %>% filter (site != 11,Sex == 2, eggDev %in% c(1,2)) %>%  group_by (year) %>% summarise (femWithEgg = n())) %>% 
      mutate (pOvig = femWithEgg/femValidEgg)  %>% select(year,surveyWide = pOvig)-> pOvig_surveyWide
    
    # join by stat to surveywide
    pOvig_byStat %>% left_join(pOvig_surveyWide) -> pOvig # Percent of females with eggs by stat area and year w surveywide. Vldz excluded from surveywide.
  
    write.csv(pOvig,"output/apxC1_ovigByStat.csv") # note ovigerity data from 95-97 are missing. 
  