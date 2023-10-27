library(tidyverse)
library(plm)
library(countrycode)

#Read Data
# gallup_main <- read.csv(
#   "/Users/makototakahara/Downloads/PWB Lab/gallupCleaned230118_Makoto.csv")
# gdp <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/GDP Per Capita.csv")
# support <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/Social Support.csv")
# life_expectancy <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/Life Expectancy.csv")
# freedom <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/Freedom.csv")
# generosity <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/Generosity.csv")
# corruption <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/Corruption.csv")
# positive_affect <- read.csv(
#   "/Users/makototakahara/Downloads/Predictors/Positive Affect.csv")
# negative_affect <- read.csv(
#  "/Users/makototakahara/Downloads/Predictors/Negative Affect.csv")
#WHR_data <- read.csv("/Users/makototakahara/Downloads/DataForTable2.1.csv")

#Calculating life satisfaction scores, counting observations
# gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16)
# WHR_scores <- gallup_main %>%
#   filter(!is.na(Weighted.LS)) %>%
#   filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
#   group_by(COUNTRY_ISO3, COUNTRYNEW) %>%
#   summarise(Weighted_Score = sum(Weighted.LS,na.rm=T)/sum(WGT, na.rm=T),
#             n = n())
# WHR_scores$Rank <- rank(-WHR_scores$Weighted_Score)
# colnames(WHR_scores) <- c("COUNTRY_ISO3", "Country", "LS_Score", "n", "Rank")
# 
# #Take log of gdp per capita
# gdp$loggdp <- log(gdp$GDP)
# gdp <- gdp[c(1, 3)]
# 
# #Adjust generosity for log gdp per capita
# generosity_df <- merge(generosity, gdp, by="Country")
# m1 <- lm(Generosity~loggdp, generosity_df)
# generosity_df$Residuals<-m1$resid
# generosity_df <- generosity_df[c(1, 4)]
# colnames(generosity_df) <- c("Country", "Generosity")
# 
# #Merge Data
# WHR_scores[WHR_scores$Country=="Bosnia Herzegovina", "Country"] <-
#   "Bosnia and Herzegovina"
# WHR_scores[WHR_scores$Country=="Congo Brazzaville", "Country"] <-
#   "Congo-Brazzaville"
# WHR_scores[WHR_scores$Country=="Czech Republic", "Country"] <-
#   "Czechia"
# WHR_scores[WHR_scores$Country=="Hong Kong", "Country"] <-
#   "Hong Kong S.A.R. of China"
# WHR_scores[WHR_scores$Country=="Palestine", "Country"] <-
#   "Palestinian Territories"
# WHR_scores[WHR_scores$Country=="Taiwan", "Country"] <-
#   "Taiwan Province of China"
# 
# WHR_scores <- merge(WHR_scores, gdp, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, support, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, life_expectancy, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, freedom, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, generosity_df, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, corruption, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, positive_affect, by = "Country", all.x=TRUE)
# WHR_scores <- merge(WHR_scores, negative_affect, by = "Country", all.x=TRUE)
# 
# 
# #Check missing data
# missing <- WHR_scores[!complete.cases(WHR_scores), ]
# 
# 
# #Impute missing data
# #For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
# WHR_scores[WHR_scores$Country=="Northern Cyprus", "loggdp"] <-
#   WHR_scores[WHR_scores$Country=="Cyprus", "loggdp"]
# WHR_scores[WHR_scores$Country=="Northern Cyprus", "Life.Expectancy"] <-
#   WHR_scores[WHR_scores$Country=="Cyprus", "Life.Expectancy"]
# WHR_scores[WHR_scores$Country=="Northern Cyprus", "Generosity"] <-
#   WHR_scores[WHR_scores$Country=="Cyprus", "Generosity"]
# 
# #Maldives, North Cyprus social support was calculated from Gallup
# gallup_main$Weighted.support <- as.numeric(gallup_main$WGT*gallup_main$WP27)
# support_calculated <- gallup_main %>%
#   filter(!is.na(Weighted.support)) %>%
#   filter(!is.na(Weighted.LS)) %>%
#   filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
#   group_by(COUNTRY_ISO3) %>%
#   summarise(support = sum(Weighted.support,na.rm=T)/sum(WGT, na.rm=T)) %>%
#   select(COUNTRY_ISO3, support)
# 
# WHR_scores[WHR_scores$Country=="Maldives", "Social.Support"] <-
#   round(support_calculated[support_calculated$COUNTRY_ISO3 == "MDV", "support"],
#         digits=3)
# WHR_scores[WHR_scores$Country=="Northern Cyprus", "Social.Support"] <-
#   round(support_calculated[support_calculated$COUNTRY_ISO3 == "XNC", "support"],
#         digits=3)
# 
# #Maldives, North Cyprus, Tajikstan social support was calculated from Gallup
# gallup_main$Weighted.freedom <- as.numeric(gallup_main$WGT*gallup_main$WP134)
# freedom_calculated <- gallup_main %>%
#   filter(!is.na(Weighted.freedom)) %>%
#   filter(!is.na(Weighted.LS)) %>%
#   filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
#   group_by(COUNTRY_ISO3) %>%
#   summarise(freedom = sum(Weighted.freedom,na.rm=T)/sum(WGT, na.rm=T)) %>%
#   select(COUNTRY_ISO3, freedom)
# 
# WHR_scores[WHR_scores$Country=="Maldives", "Freedom"] <-
#   round(freedom_calculated[freedom_calculated$COUNTRY_ISO3 == "MDV", "freedom"],
#         digits=3)
# WHR_scores[WHR_scores$Country=="Northern Cyprus", "Freedom"] <-
#   round(freedom_calculated[freedom_calculated$COUNTRY_ISO3 == "XNC", "freedom"],
#         digits=3)
# WHR_scores[WHR_scores$Country=="Tajikstan", "Freedom"] <-
#   round(freedom_calculated[freedom_calculated$COUNTRY_ISO3 == "TJK", "freedom"],
#         digits=3)

#Perform Regression



#write.csv(WHR_scores, file="/Users/makototakahara/Downloads/WHR_Summary.csv")







#Read data, summarize
WHR_data <- read.csv("/Users/makototakahara/Downloads/DataForTable2.1.csv")

summarized_WHR <- WHR_data %>% filter(year >= 2019, year <= 2021)%>% 
  group_by(Country.name) %>% 
  summarize(Life.Ladder = mean(Life.Ladder, na.rm = TRUE),
            Log.GDP.per.capita = mean(Log.GDP.per.capita, na.rm = TRUE),
            Social.support = mean(Social.support, na.rm = TRUE),
            Healthy.life.expectancy.at.birth = mean(Healthy.life.expectancy.at.birth, na.rm = TRUE),
            Freedom.to.make.life.choices = mean(Freedom.to.make.life.choices, na.rm = TRUE),
            Generosity = mean(Generosity, na.rm = TRUE),
            Perceptions.of.corruption = mean(Perceptions.of.corruption, na.rm = TRUE))

#Create Dystopia, Subtract to find variance
df <- data.frame(Country.name="Dystopia", Life.Ladder=NA, Log.GDP.per.capita=NA, 
                 Social.support=NA, Healthy.life.expectancy.at.birth=NA, 
                 Freedom.to.make.life.choices=NA, Generosity=NA, 
                 Perceptions.of.corruption=NA)
summarized_WHR <- rbind(summarized_WHR, df)

summarized_WHR[summarized_WHR$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(summarized_WHR$Log.GDP.per.capita, na.rm = TRUE)
summarized_WHR[summarized_WHR$Country.name=="Dystopia", "Social.support"] <-
  min(summarized_WHR$Social.support, na.rm = TRUE)
summarized_WHR[summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(summarized_WHR$Healthy.life.expectancy.at.birth, na.rm = TRUE)
summarized_WHR[summarized_WHR$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(summarized_WHR$Freedom.to.make.life.choices, na.rm = TRUE)
summarized_WHR[summarized_WHR$Country.name=="Dystopia", "Generosity"] <-
  min(summarized_WHR$Generosity, na.rm = TRUE)
summarized_WHR[summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(summarized_WHR$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
summarized_WHR[summarized_WHR$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  summarized_WHR[summarized_WHR$Country.name=="Cyprus", "Log.GDP.per.capita"]
summarized_WHR[summarized_WHR$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  summarized_WHR[summarized_WHR$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
summarized_WHR[summarized_WHR$Country.name=="North Cyprus", "Generosity"] <-
  summarized_WHR[summarized_WHR$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
summarized_WHR[summarized_WHR$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(0.02884543) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
summarized_WHR[summarized_WHR$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(0.02884543) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
summarized_WHR[summarized_WHR$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(0.02884543) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
summarized_WHR[summarized_WHR$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(0.02884543) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
summarized_WHR[summarized_WHR$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(1.305307) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
summarized_WHR[summarized_WHR$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(-0.7041473) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
summarized_WHR[summarized_WHR$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(-0.7041473) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
summarized_WHR[summarized_WHR$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(-0.7041473) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
summarized_WHR[summarized_WHR$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(-0.7041473) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
summarized_WHR[summarized_WHR$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(-0.7041473) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
summarized_WHR[summarized_WHR$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(-0.7041473) + as.numeric(summarized_WHR[
    summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])


value.minus.dystopia <- summarized_WHR[1:146, 3:8]- summarized_WHR[rep(147, 146), 3:8]
countries <- summarized_WHR %>% 
  select(Country.name) %>% 
  filter(Country.name != "Dystopia")
value_minus_dystopia <- cbind(countries, value.minus.dystopia)


#Regression
WHR_data_for_regression <- WHR_data %>%
  filter(!is.na(Life.Ladder),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg1 <- plm(Life.Ladder ~ Log.GDP.per.capita+Social.support+
              Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
              Generosity+Perceptions.of.corruption, data = WHR_data_for_regression,
            index = c("year","Country.name"), model="within")


#Find Contributions for Each Predictor
contributions <- value_minus_dystopia$Country.name
contributions <- cbind(contributions, value_minus_dystopia[2] * coef(summary(reg1))[1,1])
contributions <- cbind(contributions, value_minus_dystopia[3] * coef(summary(reg1))[2,1])
contributions <- cbind(contributions, value_minus_dystopia[4] * coef(summary(reg1))[3,1])
contributions <- cbind(contributions, value_minus_dystopia[5] * coef(summary(reg1))[4,1])
contributions <- cbind(contributions, value_minus_dystopia[6] * coef(summary(reg1))[5,1])
contributions <- cbind(contributions, value_minus_dystopia[7] * coef(summary(reg1))[6,1])

contributions$sum <- apply(contributions[,-1], 1, sum) 
contributions <- cbind(contributions, WHR_score = summarized_WHR[1:146,]$Life.Ladder)
contributions$residual <- contributions$WHR_score - contributions$sum


#AGE STANDARDIZED
#Reading Gallup data
gallup_main <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/gallupCleaned230118_Makoto.csv")
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16)

#Creating age groups for Gallup respondents
gallup_main <- gallup_main %>% 
  filter(!is.na(WP1220)) %>% 
  #group_by(COUNTRY_ISO3, YEAR_CALENDAR) %>% 
  mutate(
    age_group = case_when(
      WP1220 > 14 & WP1220 <= 29 ~ "15-29",
      WP1220 > 29 & WP1220 <= 44 ~ "30-44",
      WP1220 > 44 & WP1220 <= 59 ~ "45-59",
      WP1220 > 59 & WP1220 <= 74 ~ "60-74",
      WP1220 > 74 ~ "Over 75"))

#Finding mean life satisfaction scores for each age group in each
#year and country pair
gallup_age <- gallup_main %>% 
  group_by(COUNTRY_ISO3, YEAR_CALENDAR, age_group, .drop = FALSE) %>% 
  summarise(WeightedMeanLS = sum(Weighted.LS, na.rm=T)/sum(WGT, na.rm=T)) %>%
  pivot_wider(names_from = COUNTRY_ISO3, values_from = WeightedMeanLS) %>% 
  arrange(YEAR_CALENDAR, age_group)


#Filling missing age groups with LS scores of 0
#gallup_age[is.na(gallup_age_15)] = 0

#Creating weighted average based on WHO Standard Population
standard_population_15 <- rep(c(0.3333784699, 0.2890995261, 0.2161137441,
                                0.1203791469, 0.04150304671), 17)
gallup_age <- cbind(gallup_age, standardpop = standard_population_15)
age_standardized_scores_wide <- gallup_age  %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(across(AFG:ZWE, ~ weighted.mean(.x, standardpop)))
age_standardized_scores <- gather(age_standardized_scores_wide, COUNTRY_ISO3, AS.LS, AFG:ZWE)
colnames(age_standardized_scores) <- c("year", "COUNTRY_ISO3", "AS.LS", "Country.name")


#Merging WHR and Standardized Data
WHR_data$COUNTRY_ISO3 <- countrycode(WHR_data$Country.name, 'country.name', 'iso3c')
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Kosovo"] <- "XKX"
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Somaliland region"] <- "XSR" #temp
Standardized_WHR <- merge(WHR_data, age_standardized_scores, by=c("year","COUNTRY_ISO3"))



#Standardized Analysis
standardized_summarized_WHR <- Standardized_WHR %>% filter(year >= 2019, year <= 2021)%>% 
  group_by(Country.name) %>% 
  summarize(AS.LS = mean(AS.LS, na.rm = TRUE),
            Log.GDP.per.capita = mean(Log.GDP.per.capita, na.rm = TRUE),
            Social.support = mean(Social.support, na.rm = TRUE),
            Healthy.life.expectancy.at.birth = mean(Healthy.life.expectancy.at.birth, na.rm = TRUE),
            Freedom.to.make.life.choices = mean(Freedom.to.make.life.choices, na.rm = TRUE),
            Generosity = mean(Generosity, na.rm = TRUE),
            Perceptions.of.corruption = mean(Perceptions.of.corruption, na.rm = TRUE))


df <- data.frame(Country.name="Dystopia", AS.LS=NA, Log.GDP.per.capita=NA, 
                 Social.support=NA, Healthy.life.expectancy.at.birth=NA, 
                 Freedom.to.make.life.choices=NA, Generosity=NA, 
                 Perceptions.of.corruption=NA)
standardized_summarized_WHR <- rbind(standardized_summarized_WHR, df)

standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(standardized_summarized_WHR$Log.GDP.per.capita, na.rm = TRUE)
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Dystopia", "Social.support"] <-
  min(standardized_summarized_WHR$Social.support, na.rm = TRUE)
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(standardized_summarized_WHR$Healthy.life.expectancy.at.birth, na.rm = TRUE)
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(standardized_summarized_WHR$Freedom.to.make.life.choices, na.rm = TRUE)
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Dystopia", "Generosity"] <-
  min(standardized_summarized_WHR$Generosity, na.rm = TRUE)
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(standardized_summarized_WHR$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Cyprus", "Log.GDP.per.capita"]
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="North Cyprus", "Generosity"] <-
  standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(coef(summary(reg1))[3]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(coef(summary(reg1))[3]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(coef(summary(reg1))[3]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(coef(summary(reg1))[3]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(coef(summary(reg1))[4]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(coef(summary(reg1))[6]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(coef(summary(reg1))[6]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(coef(summary(reg1))[6]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(coef(summary(reg1))[6]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(coef(summary(reg1))[6]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_summarized_WHR[standardized_summarized_WHR$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(coef(summary(reg1))[6]) + as.numeric(standardized_summarized_WHR[
    standardized_summarized_WHR$Country.name=="Dystopia", "Perceptions.of.corruption"])



Standardized_WHR_data_for_regression <- Standardized_WHR %>%
  filter(!is.na(AS.LS),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg2 <- plm(AS.LS~ Log.GDP.per.capita+Social.support+
              Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
              Generosity+Perceptions.of.corruption, data = Standardized_WHR_data_for_regression,
            index = c("year","Country.name"), model="within")

standardized_contributions <- value_minus_dystopia$Country.name
standardized_contributions <- cbind(standardized_contributions, value_minus_dystopia[2] * coef(summary(reg2))[1,1])
standardized_contributions <- cbind(standardized_contributions, value_minus_dystopia[3] * coef(summary(reg2))[2,1])
standardized_contributions <- cbind(standardized_contributions, value_minus_dystopia[4] * coef(summary(reg2))[3,1])
standardized_contributions <- cbind(standardized_contributions, value_minus_dystopia[5] * coef(summary(reg2))[4,1])
standardized_contributions <- cbind(standardized_contributions, value_minus_dystopia[6] * coef(summary(reg2))[5,1])
standardized_contributions <- cbind(standardized_contributions, value_minus_dystopia[7] * coef(summary(reg2))[6,1])

standardized_contributions$sum <- apply(standardized_contributions[,-1], 1, sum) 
ASLS <- standardized_summarized_WHR[1:2]
standardized_contributions <- merge(standardized_contributions, ASLS, 
                                    by.x="standardized_contributions",
                                    by.y="Country.name")

#Manually impute missing data for Libya, Saudi Arabia
standardized_contributions[standardized_contributions$standardized_contributions=="Libya", "AS.LS"] <-
  5.069252
standardized_contributions[standardized_contributions$standardized_contributions=="Saudi Arabia", "AS.LS"] <-
  6.217615


standardized_contributions$residual <- standardized_contributions$AS.LS - standardized_contributions$sum
standardized_contributions1 <- standardized_contributions[, c(1, 9, 2, 3, 4, 5, 6, 7, 10)]
names(standardized_contributions1)[names(standardized_contributions1) == 'standardized_contributions'] <- 
  'Country'
standardized_contributions1$Rank <- rank(-standardized_contributions1$AS.LS)

write.csv(standardized_contributions1,
          file="/Users/makototakahara/Downloads/Age_Standardized_Scores.csv")
