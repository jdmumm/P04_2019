## TabsFigs ####
# Josh Mumm 190920
# assembles tables and figures for P04 FDS report
# modified from P04_2017BOF/TabsFigs_WVarFigs.R which was used for 2017 report  
# major changes include switch to use point estimates from CPUE.R rather than
# spreadsheet based on Access queries as KG and JR insisted in 2017.  

## Load ----
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
         select(year = YEAR, Event = EVENT_ID, site = SITE_ID, Station = STATION_ID, pot = POT_ID, species = FK_SPECIES_CODE,
         sex = FK_SEX_CODE, freq = FREQUENCY, cl = CARAPACE_LENGTH_MM, wt = WEIGHT_GRAMS, eggDev = SHRIMP_EGG_DEVEL_CODE, 
         breed = SHRIMP_BREEDING_CODE, eggCol = SHRIMP_EGG_COLOR_CODE, eggDead = SHRIMP_DEAD_EGG_COUNT, parasite = SHRIMP_PARASITE_CODE) -> awl
read.csv('data/potPerformance_190920.csv') %>% select( Event = EVENT_ID, site = SITE_ID, pot = POT_ID, Station = STATION, perf = FK_GEAR_PERFORMANCE_CODE, 
                                                        gearComment = GEAR_COMMENTS, sample = SAMPLE_POT ) -> pp 
read.csv("data/PWS Shrimp All.csv") %>% # from K:\MANAGEMENT\SHELLFISH\PWS Shrimp All.xlsx. Through 2017, ask CR for update with 18 & 19. 
  select (year = DOL.Year, species = Species.Code, stat=Stat.Area, pots = Effort..sum., lbs = Whole.Weight..sum.) -> harv
read.csv("data/SiteStatArea_LUT.csv") -> siteStatLUT
read.csv("data/yearArea_LUT.csv") -> yearAreaLUT
k <- 2.20462 # kilogram to lb conversion factor 

# eventually delete, once fully converted to using point estimates from cpue.r not required. (190923)
  #read.csv("data/surveyWide_from16SS.csv") -> surv  #Survey-wide summary by year
  #read.csv("data/bySite_from16SS.csv")%>%
  #  transmute(year = Year, site = Site_ID, pots = Pot_Count, all_cnt = Total_Spot_Count, all_lb = Total_Spot_Wt_KG * 2.20462, propLrg = Proportion_Large, 
  #         lrg_cnt = Est_Count_LG, lrg_lb = Est_Wt_Large * 2.20462, cpue_all_lbs = CPUE_All_LB, cpue_all_cnt = CPUE_All_Count, cpue_lrg_cnt=CPUE_Large_Count) ->site
  #read.csv("data/femEggBySite.csv") -> egg # shouldn't be necesasry once convert to use awl file. 

## T2. Catch and CPUE, survey-wide ----
  cpue %>% transmute(year,N,
                  tau_all_lb = tau_all_kg * k, tau_all_cnt,
                  mu_all_lb = mu_all_kg * k, mu_all_cnt, 
                  tau_lrg_lb = tau_lrg_kg * k, tau_lrg_cnt,
                  mu_lrg_lb = mu_lrg_kg * k, mu_lrg_cnt) %>% 
  write.csv ('output/t2_catchAndCpue_surveywide.csv')

## T3. Biological sex ratio, prob egg bearing, and mean survey-wide ---- 
  # CL 
    pp %>% select(Event, site, Station, pot, perf) %>%   
      right_join (awl)  %>% 
      filter (site != 11, !Station %in% c("E","E1","E2"), 
              perf == 1, species == 965, sex %in% c(1,2)  ) -> awls  # excluding transitionals 
    awls %>% group_by (year, sex)  %>% summarise( # issue where freq = 2 in 2005 for males, but since grouping by sex and virtually all males in 2005 were 2, ok as is 
      n = n(),   
      len = mean (cl), 
      sd = var(cl)^.5,
      se = sd/(n^.5))-> meanLen
    
    meanLen %>% select (year, sex, len) %>% spread(sex,len) -> cl #clunky, should rewrite 
    meanLen %>% select (year, sex, n) %>% spread(sex,n) -> n 
    meanLen %>% select (year, sex, sd) %>% spread(sex,sd) -> sd 
    meanLen %>% select (year, sex, se) %>% spread(sex,se) -> se 
    left_join (n,cl, by = 'year') %>% left_join(se, by = 'year') -> CL
    colnames(CL) <- c('year','n_m', 'n_f', 'cl_m', 'cl_f', 'se_m', 'se_f') 
  
  # Sex proportions 
    awls %>% group_by(year,sex) %>% summarise(cnt =  sum(freq)) %>%
      spread(sex, cnt) -> sexCnt 
    colnames(sexCnt) <- c('year','m','f') 
    sexCnt %>% group_by(year) %>% summarise (pf = f/(m+f), pm = m/(m+f)) -> sexProp
  
  # Prop fems w eggs
    awls %>% filter (sex == 2, eggDev %in% c(0,1,2)) %>%  group_by (year) %>% summarise (femValidEgg = n()) %>%# females with valid egg dev codes
      left_join(awls %>% filter (sex == 2, eggDev %in% c(1,2)) %>%  group_by (year) %>% summarise (femWithEgg = n())) %>% #  females with eggs
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

## F1. Survey-wide CPUE plot ----
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
    ggsave("./figs/f3_surveyWideCPUE.png", dpi=300, height=4.0, width=6.5, units="in")    
    
    
    
    read.csv('./P04_2017BOF/output/var_byYear_xz.csv') -> var_byYear 
    var_byYear %>%  transmute (year,
                               all = 2.20462 * se_all_kg,
                               lrg = 2.20462 * se_lrg_kg) -> se_byYear 
    se_byYear %>% gather(class, se, c(all, lrg)) -> se_byYear_l
    
    
    
    surv %>% select (year = Year, all = CPUE_All_LB, lrg = CPUE_Large_LB) %>%
      gather(class, cpue_lb, c(all, lrg)) -> surv_l
    
    
    surv_l %>% group_by(class) %>% mutate (avg = mean(cpue_lb, na.rm = TRUE)) -> surv_l # calc longterm avgs
    
    surv_l %>% left_join(se_byYear_l) %>%
      ggplot(aes(x = year, y = cpue_lb, group = class, colour = class) )+
      scale_color_grey(start=.1, end=0.5,  name = '', labels = c("All Sizes", "Larges (>32mm)")) +
      theme(legend.position = c(.2,.8)) +
      scale_x_continuous(breaks = seq(1990,2016,2))  +
      scale_y_continuous(breaks = seq(0,3,.5)) + 
      labs( x= 'Year', y = 'Mean weight per pot (lb)') +
      geom_point(size = 2)+ 
      geom_line () +
      geom_errorbar(aes(ymin=cpue_lb-se, ymax=cpue_lb+se, width = 0),position = position_dodge(width = 0.00)) + 
      geom_hline(yintercept = unique(surv_l$avg), colour = grey(c(.1,.5)), lty = 'dashed')
    
    
    ggsave("./figs/surveyWideCPUE_lbs_wVar_xz.png", dpi=300, height=4.0, width=6.5, units="in")    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
## ASSEMBLE TABLES ON JRs LIST ####

# CPUE (ALL_LB) by ShrimpArea and StatArea - both survey and harvest ----
#Aggregate SURVEY 
  # join statArea and ShrimpArea to CPUE 
    site %>% select(year,site,pots,all_lb,lrg_lb) %>%  
    left_join (
      siteStatLUT %>% select(-Comments), by = c("site" = "SiteNum")) -> cpueBySite
      
  #by ShrimpArea
    cpueBySite %>% filter(site != 11) %>% group_by (year,ShrimpArea) %>% summarize (    #exclude valdez 
      cpueAllLb = sum(all_lb)/sum(pots), 
      cpueLrgLb = sum(lrg_lb)/sum(pots)) -> cpueByArea  # added lrg_lbs for plot after writing cpueByArea csv    
    dcast(cpueByArea, year ~ ShrimpArea, value.var = "cpueAllLb") -> cpueByArea_s 

  #by StatArea
    cpueBySite %>% group_by (year,StatArea) %>% summarize (
      cpueAllLb = sum(all_lb)/sum(pots)) -> cpueByStat  
    dcast(cpueByStat, year ~ StatArea, value.var = "cpueAllLb" ) -> cpueByStat_s

# Aggregate HARVEST    
  # join shrimpArea to harvest
    harv %>% left_join (yearAreaLUT) %>% 
      na.omit -> harv #exclude those 814 records with null effort
   # by shrimpArea
    harv %>% group_by (year,area) %>% summarize (
      cpueAllLb = sum(lbs)/sum(pots)) -> cpueByArea
    dcast(cpueByArea, year ~ area, value.var = "cpueAllLb") -> cpueByArea_h 
    
    #by StatArea
    harv %>% group_by (year,stat) %>% summarize (
      cpueAllLb = sum(lbs)/sum(pots)) -> cpueByStat  
    dcast(cpueByStat, year ~ stat, value.var = "cpueAllLb" ) -> cpueByStat_h

# Join HARVEST to SURVEY and write 
  #by ShrimpArea
    left_join(cpueByArea_s,cpueByArea_h, by = "year", suffix = c("_s","_h")) -> cpueByArea
    #write.csv(cpueByArea,"output/CPUEallLb_byShirmpArea.csv")
  #by StatArea
    as.character(unique(siteStatLUT$StatArea)) %>% sort -> surveyedStats
    cpueByStat_h %>% select(c(year,one_of( surveyedStats))) -> cpueByStat_h_surveyed  # limit com stats to those that contain survey sites. 
      left_join(cpueByStat_s,cpueByStat_h_surveyed, by = "year", suffix = c("__s","_h")) %>%
      select(order(colnames(.)))  -> cpueByStat
      #write.csv(cpueByStat,"output/CPUEallLb_byStatArea.csv")

# prop egg bearing by stat ----   
    # join statArea to propEggBearing 
    egg %>% select(YEAR,SITE_ID,males,fems,femsWEggs,femsWValidEggCode) %>%
      left_join (
        siteStatLUT %>% select(SITE_ID=SiteNum,SiteName,StatArea,ShrimpArea)) -> eggsBySite 
    # Aggregate by year and stat area
    eggsBySite %>% group_by(YEAR,StatArea) %>% summarise(
      perFemWEgg = round(100*sum(femsWEggs, na.rm = TRUE)/sum(femsWValidEggCode, na.rm = TRUE),2)) -> eggByStat
      dcast(eggByStat, YEAR ~ StatArea, value.var = "perFemWEgg") -> eggByStat
    # Survey wide 
    eggsBySite %>% filter(SITE_ID != 11) %>% group_by(YEAR) %>% summarise(                   # excluding valdez for survey-wide 
      surveyWide = round(100*sum(femsWEggs, na.rm = TRUE)/sum(femsWValidEggCode, na.rm = TRUE),2)) -> eggByYear
    #join by stat area to survey-wide 
    left_join(eggByStat,eggByYear) -> eggsByStatYear  # Percent of females with eggs by stat area and year w surveywide. Vldz excluded from surveywide.
    #write.csv(eggsByStatYear,"output/eggByStat.csv")
# Main Survey Summary Table ----
    surv %>% transmute (Year, Pots = Pot_Count,
                        all_lb =  Total_Spot_Wt_KG  * 2.20462 ,
                        all_cnt = Total_Spot_Count, 
                        all_cpue_lb = CPUE_All_LB,
                        all_cpue_cnt = CPUE_All_Count,
                        lrg_lb = Est_Wt_Large * 2.20462, 
                        lrg_cnt = Est_Ct_LG, 
                        lrg_cpue_lb = CPUE_Large_LB, 
                        lrg_cpue_cnt = CPUE_Large_Count) -> main
    #write.csv(main,"output/main.csv")
# calculate prop sex and prop egg bearing
    # eggsBySite %>% filter(SITE_ID != 11) %>% group_by(YEAR) %>% summarise(                   # excluding valdez for survey-wide 
    #   surveyWidePropFem = round(100 * sum(fems)/(sum(males)+sum(fems)),1),
    #   surveyWidePropEgg = round(100*sum(femsWEggs)/sum(femsWValidEggCode),2)) -> eggAndSexByYear
         # this matches PropSexForBOF report table, except that some years are null here.  Plan to just copy and paste
         # prop sex and prop fem data from BOF table to the other main survey summary table. 

# Biological - calc mean length by year and sex.  ----
    #Survey-wide
        pp %>% select(Event, site, Station, pot, perf) %>%   
        right_join (awl)   %>% 
        filter (site != 11, !Station %in% c("E","E1","E2"), 
                perf == 1, species == 965, sex %in% c(1,2)  ) -> awls   # excluding transitionals 
      #sexes split 
      awls %>% group_by(year, sex) %>% summarise(   # there is issue where freq = 2 in 2005 for males, but since grouping by sex and almost all males in 2005 were 2, ok as is 
        n = n(),   
        len = mean (cl), 
        sd = var(cl)^.5,
        se = sd/(n^.5))-> meanLen
      #sexes combined 
      awls %>% group_by(year) %>% summarise(    
        n = n(),   
        len = mean (cl), 
        sd = var(cl)^.5,
        se = sd/(n^.5))-> meanLen_bth 
      
      meanLen %>% select (year, sex, len) %>% spread(sex,len) -> cl
      meanLen %>% select (year, sex, n) %>% spread(sex,n) -> n 
      meanLen %>% select (year, sex, sd) %>% spread(sex,sd) -> sd 
      meanLen %>% select (year, sex, se) %>% spread(sex,se) -> se 
       
      left_join (n,cl, by = 'year') %>% left_join(se, by = 'year') -> CL
      colnames(CL) <- c('Year','n_m', 'n_f', 'cl_m', 'cl_f', 'se_m', 'se_f')
      #write.csv(CL,'output/clByYearSex.csv') 
   
    #By Area
      #sexes split ----
      awls %>% left_join (siteStatLUT, by = c('site' ='SiteNum')) -> awls
      awls %>% group_by(ShrimpArea, year, sex) %>% summarise(   
        n = n(),   
        len = mean (cl), 
        sd = var(cl)^.5,
        se = sd/(n^.5))-> meanLen_byArea
      #sexes combined -----
      awls %>% group_by(ShrimpArea, year) %>% summarise(   
        n = n(),   
        len = mean (cl), 
        sd = var(cl)^.5,
        se = sd/(n^.5))-> meanLen_byArea_bth
      
      
      #Plot mean length 
      meanLen$Sex <- as.factor(meanLen$sex)
      meanLen_byArea$Sex <- as.factor(meanLen_byArea$sex)
      # sexes split ----
         y <- ggplot(data = meanLen,
              aes (x=year, y = len, group = Sex, colour = Sex)) +
              scale_x_continuous(breaks = seq(1990,2016,2))  +      
              geom_point()+
              geom_line()
        
        a <- ggplot(data = meanLen_byArea,
              aes (x=year, y = len, group = Sex, colour = Sex)) +
              scale_x_continuous(breaks = seq(1990,2016,2))  +      
              geom_point()+
              geom_line()+ 
              facet_wrap(~ShrimpArea)
      # sexes combined ----
        Y <- ggplot(data = meanLen_bth,
                    aes (x=year, y = len)) +
          scale_x_continuous(breaks = seq(1990,2016,2))  +      
          scale_y_continuous(breaks = seq(28,34,1)) +
          labs( x= 'Year', y = 'Mean CL (mm)') +
          geom_point(size = 2)+
          geom_line()+
          geom_errorbar(aes(ymin=len-se, ymax=len+se, width = 0)) + 
          geom_hline(yintercept = mean(meanLen_bth$len, na.rm=T),lty = 'dashed')
        Y
        #ggsave("./figs/surveyWideCL.png", dpi=300, height=4., width=6.5, units="in")       
        meanLen_byArea_bth %>% group_by(ShrimpArea) %>% summarise(avg = mean(len, na.rm = TRUE)) -> avgs
        labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")
        A <- ggplot(data = meanLen_byArea_bth,
                    aes (x=year, y = len)) +
          scale_x_continuous(breaks = seq(1990,2016,2))  +      
          theme( axis.text.x  = element_text(angle=90, vjust=0.5)) +
          scale_y_continuous(breaks = seq(27,38,1)) +
          labs( x= 'Year', y = 'Mean CL (mm)')+
          geom_point()+
          geom_line()+ 
          facet_wrap(~ShrimpArea, labeller=labeller(ShrimpArea = labels)) +
          geom_errorbar(aes(ymin=len-se, ymax=len+se, width = 0))+
          geom_hline(aes(yintercept = avg) , avgs, lty = 'dashed')
        A
        #ggsave("./figs/areaCL.png", dpi=300, height=2.9, width=9, units="in")
# Survey-wide CPUE plot ----
read.csv('./P04_2017BOF/output/var_byYear_xz.csv') -> var_byYear 
var_byYear %>%  transmute (year,
                          all = 2.20462 * se_all_kg,
                          lrg = 2.20462 * se_lrg_kg) -> se_byYear 
se_byYear %>% gather(class, se, c(all, lrg)) -> se_byYear_l
#se_byYear_l[se_byYear_l$class == 'lrg', 'se']  <- 0  # omit error bars for larges

surv %>% select (year = Year, all = CPUE_All_LB, lrg = CPUE_Large_LB) %>%
  gather(class, cpue_lb, c(all, lrg)) -> surv_l


surv_l %>% group_by(class) %>% mutate (avg = mean(cpue_lb, na.rm = TRUE)) -> surv_l # calc longterm avgs
    
surv_l %>% left_join(se_byYear_l) %>%
  ggplot(aes(x = year, y = cpue_lb, group = class, colour = class) )+
          scale_color_grey(start=.1, end=0.5,  name = '', labels = c("All Sizes", "Larges (>32mm)")) +
          theme(legend.position = c(.2,.8)) +
          scale_x_continuous(breaks = seq(1990,2016,2))  +
          scale_y_continuous(breaks = seq(0,3,.5)) + 
          labs( x= 'Year', y = 'Mean weight per pot (lb)') +
          geom_point(size = 2)+ 
          geom_line () +
          geom_errorbar(aes(ymin=cpue_lb-se, ymax=cpue_lb+se, width = 0),position = position_dodge(width = 0.00)) + 
          geom_hline(yintercept = unique(surv_l$avg), colour = grey(c(.1,.5)), lty = 'dashed')
        
      
    ggsave("./figs/surveyWideCPUE_lbs_wVar_xz.png", dpi=300, height=4.0, width=6.5, units="in")
    
# CPUE by area plot ----
    read.csv('./P04_2017BOF/output/var_byArea_xz.csv') -> var_byArea
    var_byArea %>%  transmute (year, 
                               ShrimpArea = as.factor(Area),
                               all = 2.20462 * se_all_kg,
                               lrg = 2.20462 * se_lrg_kg) -> se_byArea 
    se_byArea %>% gather(class, se, c(all, lrg)) -> se_byArea_l
    #se_byArea_l[se_byArea_l$class == 'lrg', 'se']  <- 0  # omit error bars for larges
    
cpueByArea %>% select (year = year, ShrimpArea, all = cpueAllLb, lrg = cpueLrgLb)  %>%  
    gather(class, cpue_lb, c(all,lrg)) -> cpueByArea_l
    
    cpueByArea_l %>% group_by(class, ShrimpArea) %>% summarise (avg = mean(cpue_lb, na.rm = TRUE))-> avgs # calc longterm avgs
        #Specific values included in resultes
          cpueByArea_l %>% filter (ShrimpArea == '1', class == 'lrg', year > 2004) %>% group_by(ShrimpArea) %>% summarise (avg =  mean(cpue_lb, na.rm = T))
          cpueByArea_l %>% filter (ShrimpArea == '1', class == 'lrg', year < 2004) %>% group_by(ShrimpArea) %>% summarise (avg =  mean(cpue_lb, na.rm = T))
          cpueByArea_l %>% filter (class == 'all') %>% group_by(ShrimpArea) %>% summarise (avg =  mean(cpue_lb, na.rm = T))
    
    labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")
    
cpueByArea_l %>% left_join(se_byArea_l) %>%    
    ggplot(aes(x = year, y = cpue_lb, group = class, colour = class) ) +
      scale_color_grey(start=.1, end=0.5,  name = '', labels = c("All Sizes", "Larges (>32mm)")) +
      theme(legend.position = c(.85,.8), legend.background = element_rect (fill = "transparent" )) +
      scale_x_continuous(breaks = seq(1990,2016,2))  +
      scale_y_continuous(breaks = seq(0,4,.5)) + 
      labs( x= 'Year', y = 'Mean weight per pot (lb)') +
      geom_point(size = 1.5)+ 
      geom_line ()  +
      geom_errorbar(aes(ymin=cpue_lb-se, ymax=cpue_lb+se, width = 0), position = position_dodge(width = 0.0)) + 
      theme( axis.text.x  = element_text(angle=90, vjust=0.5)) +
      facet_wrap(~ShrimpArea, ncol=3, labeller=labeller(ShrimpArea = labels)) +
      geom_hline(aes (yintercept = avg), avgs, colour = rep(grey(c(.1,.5)),3), lty = 'dashed')
    
    #ggsave("./figs/areaCPUE_lbs_w_wVar_xz.png", dpi=300, height=2.9, width=9, units="in")

    
        
# Histograms ----
awl %>% select(Event, site, Station, pot, year,species, sex, freq, cl) -> awl 
pp %>% select(Event, pot,perf) -> pp 
left_join(awl, pp) %>%
filter (site != 11, Station %in% c('A','B','C','D','W','X','Y','Z'), species == 965, perf == 1) -> awl

# Replicate freq 2s - these are from 2005, when only half males were measured. 
awl %>% filter(freq == 2) -> twos
rbind(awl,twos) -> awl
awl$freq <- 1
awl %>% mutate(Sex = as.factor(sex)) -> awl 
awl %>% mutate(Year = as.factor(year)) -> awl 
awl %>% left_join (siteStatLUT, by = c('site' ='SiteNum')) -> awl 

#histograms ----
#survey-wide 
awl %>% filter  (Sex %in% c('1','2')) %>% 
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
#ggsave("./figs/CL_Hist_surv.png", dpi=300, height=8.7, width=6.5, units="in")
#byArea  
  awl %>% filter  (Sex == '1' | Sex == '2') %>% 
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
  #ggsave("./figs/CL_Hist_area.png", dpi=300, height=8.7, width=6.5, units="in")
# sex ratio ----
# Survey-wide
awl %>% filter (sex %in% c(1,2)) %>% group_by(year,sex) %>% summarise(cnt =  sum(freq)) -> sx
sx %>% spread(sex, cnt) -> wid 
colnames(wid) <- c('year','m','f')
wid %>% group_by(year) %>% summarise (pf = f/(m+f)) -> pfem
#write.csv(pfem, 'output/propFemByYear.csv', row.names = FALSE) # mannually added 1996 value from BOF prop sex table .

read.csv('output/propFemByYear.csv') -> pfem
avg <- mean(pfem$pf) # calc longterm avg

pfem %>% ggplot(aes(x = year, y = pf) ) +
  scale_x_continuous(breaks = seq(1990,2016,2))  +
  scale_y_continuous(breaks = seq(0,.4,.1)) +
  labs( x= 'Year', y = 'Female proportion') +
  geom_point(size = 2)+ 
  geom_line () +
  geom_hline(yintercept = avg, lty = 'dashed')

ggsave("./figs/propFem.png", dpi=300, height=3.5, width=6.25, units="in")
#byArea
awl %>% filter (sex %in% c(1,2)) %>% group_by(ShrimpArea, year,sex) %>%
  summarise(cnt =  sum(freq)) -> sx
sx %>% spread(sex, cnt) -> wid 
colnames(wid) <- c('area','year','m','f')
wid %>% group_by(area, year) %>% summarise (pf = f/(m+f)) -> pfem_a
#write.csv(pfem_a, 'output/propFemByArea.csv', row.names = FALSE) # mannually added 1996 value from BOF prop sex table .

read.csv('output/propFemByArea.csv') -> pFem_a

pFem_a %>% group_by(area) %>% summarise (avg = mean(pf, na.rm = T))-> avgs # calc longterm avgs
labels <- c('1' = "Area 1", '2' = "Area 2", '3' = "Area 3")

pFem_a %>% ggplot(aes(x = year, y = pf)) +
  scale_x_continuous(breaks = seq(1990,2016,2))  +
  scale_y_continuous(breaks = seq(0,.4,.1)) + 
  labs( x= 'Year', y = 'Female proportion') +
  ylim(0,.4) +
  geom_point(size = 1.5)+ 
  geom_line () +
  theme( axis.text.x  = element_text(angle=0, vjust=0.5)) +
  facet_wrap(~area, ncol=1, strip.position="right", labeller=labeller(area = labels)) + 
  geom_hline(aes (yintercept = avg), avgs, lty = 'dashed')

ggsave("./figs/propFemByArea.png", dpi=300, height=4.5, width=6.5, units="in")

#Harvest Figure ----
read.csv("data/PWSShrimpHarvestComposite_60to2017.csv") -> histHarv
histHarv %>% select(Year, Total_c, Spots_nc) -> histHarv # select most complete time series from each of nc and c. 
histHarv %>% gather("fishery", "lbs" , 2:3)  %>% 
  mutate (lbs = lbs/1000 , 
          fishery = as.factor(fishery)) ->  histHarv_l


surv %>% transmute (Year, fishery = as.factor('Pot Survey CPUE'), lbs = CPUE_All_LB * 100) %>% rbind(histHarv_l) -> dat
cbind.data.frame(Year = c(1989,1990,1991),fishery = rep('Pot Survey CPUE',3), lbs = c(130,90,130)) -> oldSurv

rbind.data.frame(oldSurv,dat) -> dat


dat %>% ggplot(aes (x=Year, y = lbs, fill = fishery)) +
            scale_y_continuous(breaks = seq(0,300,50)) + ylab('Harvest (Thousands of Pounds)')+
            scale_x_continuous(breaks = seq(1960,2015, 5)) +
            geom_bar(data = filter (dat, fishery != 'Pot Survey CPUE'), stat = "identity", position = "stack") + 
            scale_fill_manual(values=c("white","gray60", "gray30"), drop = TRUE, 
                              labels = c("" ,'Noncommercial Harvest','Commercial Harvest'), guide = guide_legend(title = NULL)) + 
            theme(legend.position = c(.2,.8), legend.title=element_blank()) +
            geom_line (data = filter(dat,fishery == "Pot Survey CPUE"),
                       aes(lty = fishery), color = "black", lwd = 1 )+
            scale_y_continuous(sec.axis = sec_axis(~./100, name = "Survey CPUE (lbs/pot)"))

#ggsave("./figs/HarvestAndSurvey.png", dpi=300, height=4.5, width=6.5, units="in")

# L50 plots ----
    # Survey-wide
    read.csv('output/f50_92to16.csv') %>% select(year = yrs, f50) -> l50
    avg <- mean(l50$f50, na.rm = T) # calc longterm avg
    
    l50 %>% ggplot(aes(x = year, y = f50) ) +
      scale_x_continuous(breaks = seq(1990,2016,2))  +
      scale_y_continuous(breaks = seq(38,43,1)) + 
      labs( x= 'Year', y = 'L50 (mm)') +
      geom_point(size = 2)+ 
      geom_line () +
      geom_hline(yintercept = avg, lty = 'dashed')
    
   # ggsave("./figs/surveyWideL50.png", dpi=300, height=3.5, width=6.25, units="in")
    
    # ByArea
    read.csv('output/f50_byArea_92to16.csv') %>% select (year, 'Area 1' = f50_1, 'Area 2' = f50_2, 'Area 3' = f50_3) -> l50_a
    
    l50_a %>% gather(area, l50, 2:4) -> l50_a_l
    
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
    
    #ggsave("./figs/L50_byArea.png", dpi=300, height=4.5, width=6.5, units="in")



