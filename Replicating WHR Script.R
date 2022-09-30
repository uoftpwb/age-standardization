library(tidyverse)

#Reading Gallup data
gallup_main <- read.csv("/Users/MakBook/Downloads/GallupCleaned_Sep19.csv")
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16.LS) 

#Finding unweighted life satisfaction scores, counting observations
WHR_scores <- gallup_main %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(UW_Score = mean(Weighted.LS,na.rm=T),
            n = n()) 



  






test <- gallup_main %>%   
  filter(!is.na(Weighted.LS)) %>%
  filter(YEAR_CALENDAR == 2021) %>%
  count(COUNTRY_ISO3)
