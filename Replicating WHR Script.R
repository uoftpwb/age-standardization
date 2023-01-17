library(tidyverse)

#Reading Gallup data
gallup_main <- read.csv(
  "/Users/makototakahara/Downloads/GallupCleaned_Sep19.csv")
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16.LS)

#Calculating life satisfaction scores, counting observations
WHR_scores <- gallup_main %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(Weighted_Score = sum(Weighted.LS,na.rm=T)/sum(WGT, na.rm=T),
            n = n())

#Adding ranking column
WHR_scores$Rank <- rank(-WHR_scores$Weighted_Score)
