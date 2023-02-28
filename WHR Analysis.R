library(tidyverse)

#Read Data
gallup_main <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/gallupCleaned230118_Makoto.csv")
gdp <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/GDP Per Capita.csv")
support <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Social Support.csv")
life_expectancy <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Life Expectancy.csv")
freedom <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Freedom.csv")
generosity <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Generosity.csv")
corruption <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Corruption.csv")
positive_affect <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Positive Affect.csv")
negative_affect <- read.csv(
  "/Users/makototakahara/Downloads/Predictors/Negative Affect.csv")

#Calculating life satisfaction scores, counting observations
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16)
WHR_scores <- gallup_main %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3, COUNTRYNEW) %>% 
  summarise(Weighted_Score = sum(Weighted.LS,na.rm=T)/sum(WGT, na.rm=T),
            n = n())
WHR_scores$Rank <- rank(-WHR_scores$Weighted_Score)
colnames(WHR_scores) <- c("COUNTRY_ISO3", "Country", "LS_Score", "n", "Rank")

#Take log of gdp per capita
gdp$loggdp <- log(gdp$GDP)
gdp <- gdp[c(1, 3)]

#Adjust generosity for log gdp per capita
generosity_df <- merge(generosity, gdp, by="Country")
m1 <- lm(Generosity~loggdp, generosity_df)
generosity_df$Residuals<-m1$resid
generosity_df <- generosity_df[c(1, 4)]
colnames(generosity_df) <- c("Country", "Generosity")

#Merge Data
WHR_scores[WHR_scores$Country=="Bosnia Herzegovina", "Country"] <- 
  "Bosnia and Herzegovina"
WHR_scores[WHR_scores$Country=="Congo Brazzaville", "Country"] <- 
  "Congo-Brazzaville"
WHR_scores[WHR_scores$Country=="Czech Republic", "Country"] <- 
  "Czechia"
WHR_scores[WHR_scores$Country=="Hong Kong", "Country"] <- 
  "Hong Kong S.A.R. of China"
WHR_scores[WHR_scores$Country=="Palestine", "Country"] <- 
  "Palestinian Territories"
WHR_scores[WHR_scores$Country=="Taiwan", "Country"] <- 
  "Taiwan Province of China"

WHR_scores <- merge(WHR_scores, gdp, by = "Country", all.x=TRUE)
WHR_scores <- merge(WHR_scores, support, by = "Country", all.x=TRUE)
WHR_scores <- merge(WHR_scores, life_expectancy, by = "Country", all.x=TRUE)
WHR_scores <- merge(WHR_scores, freedom, by = "Country", all.x=TRUE)
WHR_scores <- merge(WHR_scores, generosity_df, by = "Country", all.x=TRUE)
WHR_scores <- merge(WHR_scores, corruption, by = "Country", all.x=TRUE)


#Impute missing data



#Perform Regression



#write.csv(WHR_scores, file="/Users/makototakahara/Downloads/WHR_Summary.csv")