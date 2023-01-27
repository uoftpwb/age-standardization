library(tidyverse)
library(plm)
library(countrycode)

#Reading Gallup data
gallup_main <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/gallupCleaned230118_Makoto.csv")
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16)


#Calculating life satisfaction scores, counting observations
WHR_scores <- gallup_main %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(Weighted_Score = sum(Weighted.LS,na.rm=T)/sum(WGT, na.rm=T),
            n = n())


#Adding ranking column
WHR_scores$Rank <- rank(-WHR_scores$Weighted_Score)


# 1. gdp - Log GDP per Capita
gdp <- read.csv("/Users/makototakahara/Downloads/PWB Lab/gdp_per_capita.csv")
gdp <- gdp %>% 
  select(Country.Code, X2019, X2020)
growth1 <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/econ_growth_2021.csv")
growth1 <- growth %>% 
  select(LOCATION, Country, Value)
growth2 <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/growth2.csv")

growth2$LOCATION <- countrycode(
  growth2$Country,
  "country.name",
  "iso3c")
growth2 <- growth2[, c(3, 1, 2)]

growth <- rbind(growth1, growth2)
growth <- unique(growth[ , c("LOCATION") ] )


# 2. support - Social Support
gallup_main$Weighted.support <- as.numeric(gallup_main$WGT*gallup_main$WP27)
support <- gallup_main %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(support = sum(Weighted.support,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, support)
WHR_scores <- merge(WHR_scores, support, by = "COUNTRY_ISO3")


# 3. life_expectancy - Healthy Life Expectancy at Birth



# 4. freedom - Freedom to Make Life Choices
gallup_main$Weighted.freedom <- as.numeric(gallup_main$WGT*gallup_main$WP134)
freedom <- gallup_main %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(freedom = sum(Weighted.freedom,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, freedom)
WHR_scores <- merge(WHR_scores, freedom, by = "COUNTRY_ISO3")


# 5. generosity - Generosity



# 6. corruption - Perceptions of Corruption
gallup_main$Weighted.bcorruption <- as.numeric(gallup_main$WGT*gallup_main$WP145)
gallup_main$Weighted.gcorruption <- as.numeric(gallup_main$WGT*gallup_main$WP146)

bcorruption <- gallup_main %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(bcorruption = sum(Weighted.bcorruption,na.rm=T)/sum(WGT, na.rm=T),
            nb=n()) %>% 
  select(COUNTRY_ISO3, bcorruption)

gcorruption <- gallup_main %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(gcorruption = sum(Weighted.gcorruption,na.rm=T)/sum(WGT, na.rm=T),
            ng=n()) %>% 
  select(COUNTRY_ISO3, gcorruption)
gcorruption[gcorruption == 0] <- NA

corruption <- merge(bcorruption, gcorruption, by="COUNTRY_ISO3") %>% 
  select(COUNTRY_ISO3, bcorruption, gcorruption)
corruption$corruption <- rowMeans(corruption[c("bcorruption", "gcorruption")], 
                                  na.rm=TRUE)
corruption <- corruption %>% 
  select("COUNTRY_ISO3", "corruption")
WHR_scores <- merge(WHR_scores, corruption, by = "COUNTRY_ISO3")


#PLM
#plm(WHR_scores$Weighted_Score ~ 
#      gdp+support+life_expectancy+freedom+generosity+corruption, 
#    data = "WHAT", model = "pooling")
