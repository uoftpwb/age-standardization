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
gdp <- read.csv("/Users/makototakahara/Downloads/PWB Lab/gdp_per_capita.csv")
growth1 <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/econ_growth_2021.csv")
growth1 <- growth1 %>% 
  select(LOCATION, Country, Value)
colnames(growth1) <-  c("Country.Code", "Country", "Value")
growth2 <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/growth2.csv")

growth2$LOCATION <- countrycode(
  growth2$Country,
  "country.name",
  "iso3c")
growth2 <- growth2[, c(3, 1, 2)]
colnames(growth2) <-  c("Country.Code", "Country", "Value")

growth <- rbind(growth1, growth2)
growth <- growth %>% 
  distinct(Country.Code, .keep_all = TRUE)
colnames(growth) <- c("Country.Code", "Country", "Value")
growth$Value <- as.numeric(growth$Value)

popgrowth <- read.csv("/Users/makototakahara/Downloads/PWB Lab/popgrowth.csv")
popgrowth <- popgrowth %>% 
  select("Country.Code", "X2020")
colnames(popgrowth) <- c("Country.Code", "PopGrowth")
growth <- merge(growth, popgrowth, by = "Country.Code")
growth$PopGrowth <- as.numeric(growth$PopGrowth)
growth$AdjustedValue <- NA
growth$AdjustedValue <- growth$Value - growth$PopGrowth
growth <- growth %>% 
  select("Country.Code", "AdjustedValue")

gdp <- merge(gdp, growth, by = "Country.Code")

gdp$X2021 <- gdp$X2020 * (1+0.01*gdp$AdjustedValue)
gdp$average <- rowMeans(gdp[c("X2005", "X2006", "X2007", "X2008", "X2009", 
                              "X2010", "X2011", "X2012", "X2013", "X2013",
                              "X2014", "X2015", "X2016", "X2017", "X2018",
                              "X2019", "X2020", "X2021")], 
                                  na.rm=TRUE)
gdp$loggdp <- log(gdp$average)
gdp <- gdp %>% 
  select("Country.Code", "loggdp")
colnames(gdp) <- c("COUNTRY_ISO3", "loggdp")

#"CYP" "HKG" "MLT" "SGP" "TKM" "TWN" "VEN" "XKX" "XNC" "YEM" NOT IN gdp YET

WHR_scores <- merge(WHR_scores, gdp, by = "COUNTRY_ISO3")


# 2. support - Social Support
gallup_main$Weighted.support <- as.numeric(gallup_main$WGT*gallup_main$WP27)
support <- gallup_main %>%
  filter(!is.na(Weighted.support)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(support = sum(Weighted.support,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, support)
WHR_scores <- merge(WHR_scores, support, by = "COUNTRY_ISO3")


# 3. life_expectancy - Healthy Life Expectancy at Birth TODO
# data <- read.csv(
#   "/Users/makototakahara/Downloads/PWB Lab/life_expectancy.csv")
# 
# 
# # Load the WDIYearHorizontal data
# #data <- read.dta("WDIYearHorizontal.dta")
# 
# # Keep only rows where indicatorcode is "SP.DYN.LE00.IN"
# data_long <- data %>% 
#   filter(IndicatorCode == "WHOSIS_000001")
# 
# # Reshape the data
# #data_long <- reshape(data, varying = c("2000", "2010", "2015", "2019"))
# 
# # Rename variables
# names(data_long)[names(data_long) == "year"] <- "leWDI"
# data_long$leWDI <- as.numeric(data_long$leWDI)
# data_long$time <- as.numeric(data_long$time)
# 
# # Add label to leWDI
# colnames(data_long)[colnames(data_long) == "leWDI"] <- "Life expectancy at birth, total (years)"
# 
# # Sort the data by isocode and year
# data_long <- data_long[order(data_long$isocode, data_long$time),]
# 
# # Save the data as transit.dta
# write.dta(data_long, "transit.dta")
# 
# # Load MidStage2 data
# data2 <- read.dta("MidStage2.dta")
# 
# # Sort the data by isocode and year
# data2 <- data2[order(data2$isocode, data2$year),]
# 
# # Merge the data with transit.dta
# merged_data <- merge(data2, data_long, by = c("isocode", "year"), all.x = TRUE)
# 
# # Remove rows where _merge == 2
# merged_data <- merged_data[merged_data$_merge != 2,]
# 
# # Collapse the data by country and isocode
# merged_data_collapsed <- aggregate(merged_data, by = list(merged_data$country, merged_data$isocode), mean)
# 
# # Browse data if _merge == 1
# merged_data_collapsed[merged_data_collapsed$_merge == 1,]
# 
# # Remove _merge column
# merged_data_collapsed$_merge <- NULL
# 
# # Save the data as MidStage2
# write.dta(merged_data_collapsed, "MidStage2.dta")
# 
# # Load WHO HLE data
# data_hle <- read.csv("rawDataNotGWP\WHO HLE Dec 2020.csv", header = TRUE, skip = 1)
# 
# # Rename variables
# colnames(data_hle)[colnames(data_hle) == "v1"] <- "country"
# colnames(data_hle)[colnames(data_hle) == "v2"] <- "bothsex2019"
# colnames(data_hle)[colnames(data_hle) == "v3"] <- "bothsex2015"
# colnames(data_hle)[colnames(data_hle) == "v4"] <- "bothsex2010"
# colnames(data_hle)[colnames(data_hle) == "v5"] <- "bothsex2000"
# 
# #######################################################
# # Read in data
# rawDataNotGWP <- read.csv("rawDataNotGWP/Lancet_table2.csv")
# 
# # Split variables and drop unnecessary parts
# rawDataNotGWP %>%
#   mutate_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")), 
#             funs(substring(.,1,regexpr("[(]",.)-1))) %>%
#   rename_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")), 
#             funs(substr(.,1,nchar(.)-1))) %>%
#   mutate_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")), 
#             funs(as.numeric)) %>%
#   rename_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")), 
#             funs(sub("1990","",.))) %>%
#   rename_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")), 
#             funs(sub("2010","",.))) %>%
#   select(-starts_with("lemale1990"),-starts_with("hlemale1990"),-starts_with("lefemale1990"),-starts_with("hlefemale1990")) -> rawDataNotGWP
# 
# # Assign column labels
# colnames(rawDataNotGWP) <- c("country", "lemale1990", "hlemale1990", "lefemale1990", "hlefemale1990", "lemale2010", "hlemale2010", "lefemale2010", "hlefemale2010")
# 
# # Summarize variables
# summary(rawDataNotGWP[,2:9])
# 
# # Keep only country column
# rawDataNotGWP %>% select(country) %>% saveRDS("transit.rds")
# 
# # Reshape data
# rawDataNotGWP %>%
#   gather(variable, value, -country) %>%
#   separate(variable, c("variable", "year"), sep = 1) %>%
#   spread(variable, value) %>%
#   mutate(year = as.numeric(year)) -> rawDataNotGWP
# 
# # Create new variables for population-wide LE and HLE, assuming 50-50 male-female ratio
# rawDataNotGWP %>%
#   mutate(leLancet = (lemale + lefemale) / 2,
#          hleLancet = (hlemale + hlefemale) / 2) %>%
#   select(country, year, leLancet, hleLancet) %>%
#   arrange(country, year) %>%
#   saveRDS("transit2.rds")
# 
# # Merge data with previous country-year data
# transit <- readR














# 4. freedom - Freedom to Make Life Choices
gallup_main$Weighted.freedom <- as.numeric(gallup_main$WGT*gallup_main$WP134)
freedom <- gallup_main %>% 
  filter(!is.na(Weighted.freedom)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(freedom = sum(Weighted.freedom,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, freedom)
WHR_scores <- merge(WHR_scores, freedom, by = "COUNTRY_ISO3")


# 5. generosity - Generosity
gallup_main$Weighted.generosity <- as.numeric(gallup_main$WGT*gallup_main$WP108)
generosity <- gallup_main %>% 
  filter(!is.na(Weighted.generosity)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(generosity = sum(Weighted.generosity,na.rm=T)/sum(WGT, na.rm=T)) %>% 
  select(COUNTRY_ISO3, generosity)
generosity$generosity <- as.numeric(generosity$generosity)
WHR_scores <- merge(WHR_scores, generosity, by = "COUNTRY_ISO3")

m1 <- lm(generosity~loggdp, WHR_scores)
residuals <- resid(m1)
WHR_scores$generosity_residuals <- residuals
WHR_scores <- WHR_scores %>% 
  select(!"generosity")

# 6. corruption - Perceptions of Corruption
gallup_main$Weighted.bcorruption <- as.numeric(gallup_main$WGT*gallup_main$WP145)
gallup_main$Weighted.gcorruption <- as.numeric(gallup_main$WGT*gallup_main$WP146)






# corruption1 <- gallup_main %>% 
#   filter(!is.na(Weighted.bcorruption),
#          #!is.na(Weighted.gcorruption)
#          ) %>% 
#   filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
#   group_by(COUNTRY_ISO3) %>% 
#   summarise(bcorruption = sum(Weighted.bcorruption,na.rm=T)/sum(WGT, na.rm=T),
#             gcorruption = sum(Weighted.gcorruption,na.rm=T)/sum(WGT, na.rm=T),
#             n=n())
# corruption1$corruption <- RowMeans()






biscorruption <- gallup_main %>% 
  filter(!is.na(Weighted.bcorruption)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(bcorruption = sum(Weighted.bcorruption,na.rm=T)/sum(WGT, na.rm=T),
            nb=n())

govcorruption <- gallup_main %>% 
  #filter(!is.na(Weighted.gcorruption)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(gcorruption = sum(Weighted.gcorruption,na.rm=T)/sum(WGT, na.rm=T),
            ng=n()) 
govcorruption[govcorruption$gcorruption == 0, 2:3] <- 0

corruption <- merge(biscorruption, govcorruption, by="COUNTRY_ISO3", all.x=TRUE) 

corruption$corruption <- ((corruption$bcorruption)*(corruption$nb) + 
  (corruption$gcorruption)*(corruption$ng))/(corruption$nb+corruption$ng)

corruption <- corruption %>% 
  select("COUNTRY_ISO3", "corruption")
WHR_scores <- merge(WHR_scores, corruption, by = "COUNTRY_ISO3")

WHR_scores[WHR_scores$COUNTRY_ISO3 == __, "corruptionM"] <- value

#PLM
#plm(WHR_scores$Weighted_Score ~ 
#      loggdp+support+life_expectancy+freedom+generosity+corruption, 
#    data = WHR_scores, model = "pooling")

