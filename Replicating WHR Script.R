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


# 1. loggdp - Log GDP per Capita
#Read Past GDP Data
gdp <- read.csv("/Users/makototakahara/Downloads/PWB Lab/gdp_per_capita.csv")

#Read Growth Projections by OECD, Select Relevant Columns and Rename
growth1 <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/econ_growth_2021.csv")
growth1 <- growth1 %>%
  select(LOCATION, Country, Value)
colnames(growth1) <-  c("Country.Code", "Country", "Value")

#Read Growth Projections by World Bank, Reorder and Rename for Merging
growth2 <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/growth2.csv")
growth2$LOCATION <- countrycode(
  growth2$Country,
  "country.name",
  "iso3c")
growth2 <- growth2[, c(3, 1, 2)]
colnames(growth2) <-  c("Country.Code", "Country", "Value")

#Bind the Two Projections, distinct() prioritizes first instance so OECD kept
growth <- rbind(growth1, growth2)
growth <- growth %>%
  distinct(Country.Code, .keep_all = TRUE)
colnames(growth) <- c("Country.Code", "Country", "Value")
growth$Value <- as.numeric(growth$Value)

#Read Population Growth Data from World Bank, Select Relevant Columns and Rename
popgrowth <- read.csv("/Users/makototakahara/Downloads/PWB Lab/popgrowth.csv")
popgrowth <- popgrowth %>%
  select("Country.Code", "X2020")
colnames(popgrowth) <- c("Country.Code", "PopGrowth")

#Merge GDP Growth and Population Growth, Subtract Population Growth from GDP Growth
growth <- merge(growth, popgrowth, by = "Country.Code")
growth$PopGrowth <- as.numeric(growth$PopGrowth)
growth$AdjustedValue <- NA
growth$AdjustedValue <- growth$Value - growth$PopGrowth

#Merge GDP Growth Rate Adjusted for Population Growth with GDP Data, 
#Extrapolate for 2021 GDP per Capita by Applying Growth Rates then take logs
gdp <- merge(gdp, growth, by = "Country.Code")
gdp$X2021 <- gdp$X2020 * (1+0.01*gdp$AdjustedValue)
gdp$average <- rowMeans(gdp[c("X2019", "X2020", "X2021")],
                                  na.rm=TRUE)
gdp$loggdp <- log(gdp$average)
#temp
gdp <- gdp %>%
  select("Country.Code", "loggdp", "X2019", "X2020", "X2021", "AdjustedValue", "average")





gdp <- gdp %>%
  select("Country.Code", "loggdp")
colnames(gdp) <-  c("COUNTRY_ISO3", "loggdp")
WHR_scores <- merge(WHR_scores, gdp, by = "COUNTRY_ISO3", all.x=TRUE)


#"CYP" "HKG" "MLT" "SGP" "TKM" "TWN" "VEN" "XKX" "XNC" "YEM" NOT IN gdp YET



# 2. support - Social Support
gallup_main$Weighted.support <- as.numeric(gallup_main$WGT*gallup_main$WP27)
support <- gallup_main %>%
  filter(!is.na(Weighted.support)) %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(support = sum(Weighted.support,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, support)
WHR_scores <- merge(WHR_scores, support, by = "COUNTRY_ISO3", all.x=TRUE)


# 3. life_expectancy - Healthy Life Expectancy at Birth 
#TODO



# 4. freedom - Freedom to Make Life Choices
gallup_main$Weighted.freedom <- as.numeric(gallup_main$WGT*gallup_main$WP134)
freedom <- gallup_main %>% 
  filter(!is.na(Weighted.freedom)) %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(freedom = sum(Weighted.freedom,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, freedom)
WHR_scores <- merge(WHR_scores, freedom, by = "COUNTRY_ISO3", all.x=TRUE)


# 5. generosity - Generosity
# gallup_main$Weighted.generosity <- as.numeric(gallup_main$WGT*gallup_main$WP108)
# generosity <- gallup_main %>% 
#   filter(!is.na(Weighted.generosity)) %>% 
#   filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
#   group_by(COUNTRY_ISO3) %>% 
#   summarise(generosity = sum(Weighted.generosity,na.rm=T)/sum(WGT, na.rm=T)) %>% 
#   select(COUNTRY_ISO3, generosity)
# generosity$generosity <- as.numeric(generosity$generosity)
# WHR_scores <- merge(WHR_scores, generosity, by = "COUNTRY_ISO3")
# 
# m1 <- lm(generosity~loggdp, WHR_scores)
# residuals <- resid(m1)
# WHR_scores$generosity_residuals <- residuals
# WHR_scores <- WHR_scores %>% 
#   select(!"generosity")

# 6. corruption - Perceptions of Corruption
gallup_main$Weighted.bcorruption <- as.numeric(gallup_main$WGT*gallup_main$WP145)
gallup_main$Weighted.gcorruption <- as.numeric(gallup_main$WGT*gallup_main$WP146)
# Probably not what they mean
# gallup_main <- gallup_main %>%
#   mutate(Weighted.gcorruption = coalesce(
#     Weighted.gcorruption, Weighted.bcorruption))

biscorruption <- gallup_main %>% 
  filter(!is.na(Weighted.bcorruption)) %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(bcorruption = sum(Weighted.bcorruption, na.rm=T)/sum(WGT, na.rm=T),
            nb=n())

govcorruption <- gallup_main %>% 
  filter(!is.na(Weighted.gcorruption)) %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(gcorruption = sum(Weighted.gcorruption, na.rm=T)/sum(WGT, na.rm=T),
            ng=n())

corruption <- merge(biscorruption, govcorruption, by="COUNTRY_ISO3", all.x=TRUE) 

corruption$corruption <- ((corruption$bcorruption)*(corruption$nb) + 
  (corruption$gcorruption)*(corruption$ng))/(corruption$nb+corruption$ng)

corruption <- corruption %>% 
  select("COUNTRY_ISO3", "corruption")
WHR_scores <- merge(WHR_scores, corruption, by = "COUNTRY_ISO3", all.x=TRUE)

#PLM
#plm(WHR_scores$Weighted_Score ~ 
#      loggdp+support+life_expectancy+freedom+generosity+corruption, 
#    data = WHR_scores, model = "pooling")

