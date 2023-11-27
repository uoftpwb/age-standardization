library(tidyverse)
library(plm)
library(countrycode)
library(ggalluvial)

#Define Functions

#Reading WHR Data
WHR_data <- read.csv("/Users/makototakahara/Downloads/WHR2023Table.csv")

WHR_summary <- WHR_data %>% filter(year >= 2020, year <= 2022)%>% 
  group_by(Country.name) %>% 
  summarize(Life.Ladder = mean(Life.Ladder, na.rm = TRUE),
            Log.GDP.per.capita = mean(Log.GDP.per.capita, na.rm = TRUE),
            Social.support = mean(Social.support, na.rm = TRUE),
            Healthy.life.expectancy.at.birth = mean(Healthy.life.expectancy.at.birth, na.rm = TRUE),
            Freedom.to.make.life.choices = mean(Freedom.to.make.life.choices, na.rm = TRUE),
            Generosity = mean(Generosity, na.rm = TRUE),
            Perceptions.of.corruption = mean(Perceptions.of.corruption, na.rm = TRUE))

#Regression of Life Ladder to 6 predictors
regression_data <- WHR_data %>%
  filter(!is.na(Life.Ladder),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

WHR_regression <- plm(Life.Ladder ~ Log.GDP.per.capita+Social.support+
                        Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
                        Generosity+Perceptions.of.corruption, data = regression_data,
                      index = c("year","Country.name"), model="within")

#Create Dystopia
WHR_summary <- rbind(WHR_summary, data.frame(Country.name="Dystopia", Life.Ladder=NA, Log.GDP.per.capita=NA, 
                                             Social.support=NA, Healthy.life.expectancy.at.birth=NA, 
                                             Freedom.to.make.life.choices=NA, Generosity=NA, 
                                             Perceptions.of.corruption=NA))

WHR_summary[WHR_summary$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  5.527147 #Venezuela Log GDP per Capita?
WHR_summary[WHR_summary$Country.name=="Dystopia", "Social.support"] <-
  min(WHR_summary$Social.support, na.rm = TRUE)
WHR_summary[WHR_summary$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(WHR_summary$Healthy.life.expectancy.at.birth, na.rm = TRUE)
WHR_summary[WHR_summary$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(WHR_summary$Freedom.to.make.life.choices, na.rm = TRUE)
WHR_summary[WHR_summary$Country.name=="Dystopia", "Generosity"] <-
  min(WHR_summary$Generosity, na.rm = TRUE)
WHR_summary[WHR_summary$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(WHR_summary$Perceptions.of.corruption, na.rm = TRUE)

#Impute missing data
#For other missing values, work backwards from WHR
WHR_summary[WHR_summary$Country.name=="State of Palestine", "Log.GDP.per.capita"] <- 
  (1.144)/(summary(WHR_regression)$coefficients["Log.GDP.per.capita", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Log.GDP.per.capita"])

WHR_summary[WHR_summary$Country.name=="Taiwan Province of China", "Log.GDP.per.capita"] <-
  (1.897)/(summary(WHR_regression)$coefficients["Log.GDP.per.capita", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Log.GDP.per.capita"])

WHR_summary[WHR_summary$Country.name=="Venezuela", "Log.GDP.per.capita"] <-
  (0)/(summary(WHR_regression)$coefficients["Log.GDP.per.capita", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Log.GDP.per.capita"])

WHR_summary[WHR_summary$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.702)/(summary(WHR_regression)$coefficients["Healthy.life.expectancy.at.birth", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])

WHR_summary[WHR_summary$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.372)/(summary(WHR_regression)$coefficients["Healthy.life.expectancy.at.birth", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])

WHR_summary[WHR_summary$Country.name=="State of Palestine", "Healthy.life.expectancy.at.birth"] <-
  (0.292)/(summary(WHR_regression)$coefficients["Healthy.life.expectancy.at.birth", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])

WHR_summary[WHR_summary$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.492)/(summary(WHR_regression)$coefficients["Healthy.life.expectancy.at.birth", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])

WHR_summary[WHR_summary$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.599)/(summary(WHR_regression)$coefficients["Freedom.to.make.life.choices", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Freedom.to.make.life.choices"])

WHR_summary[WHR_summary$Country.name=="Venezuela", "Generosity"] <-
  (0.205)/(summary(WHR_regression)$coefficients["Generosity", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Generosity"])

WHR_summary[WHR_summary$Country.name=="State of Palestine", "Generosity"] <-
  (0.065)/(summary(WHR_regression)$coefficients["Generosity", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Generosity"])

WHR_summary[WHR_summary$Country.name=="Taiwan Province of China", "Generosity"] <-
  (0.067)/(summary(WHR_regression)$coefficients["Generosity", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Generosity"])

WHR_summary[WHR_summary$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.170)/(summary(WHR_regression)$coefficients["Perceptions.of.corruption", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Perceptions.of.corruption"])

WHR_summary[WHR_summary$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.138)/(summary(WHR_regression)$coefficients["Perceptions.of.corruption", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Perceptions.of.corruption"])

WHR_summary[WHR_summary$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.145)/(summary(WHR_regression)$coefficients["Perceptions.of.corruption", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Perceptions.of.corruption"])

WHR_summary[WHR_summary$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.247)/(summary(WHR_regression)$coefficients["Perceptions.of.corruption", "Estimate"]) + as.numeric(WHR_summary[
    WHR_summary$Country.name=="Dystopia", "Perceptions.of.corruption"])


#Find Contributions for Each Predictor
distance_from_dystopia <- cbind(WHR_summary %>% 
                                  select(Country.name) %>% 
                                  filter(Country.name != "Dystopia"),
                                (WHR_summary[1:137, 3:8] - WHR_summary[rep(138, 137), 3:8]))

WHR_ranking <- distance_from_dystopia$Country.name
WHR_ranking <- cbind(WHR_ranking, distance_from_dystopia[2] * coef(summary(WHR_regression))[1,1])
WHR_ranking <- cbind(WHR_ranking, distance_from_dystopia[3] * coef(summary(WHR_regression))[2,1])
WHR_ranking <- cbind(WHR_ranking, distance_from_dystopia[4] * coef(summary(WHR_regression))[3,1])
WHR_ranking <- cbind(WHR_ranking, distance_from_dystopia[5] * coef(summary(WHR_regression))[4,1])
WHR_ranking <- cbind(WHR_ranking, distance_from_dystopia[6] * coef(summary(WHR_regression))[5,1])
WHR_ranking <- cbind(WHR_ranking, distance_from_dystopia[7] * coef(summary(WHR_regression))[6,1])
WHR_ranking$sum <- apply(WHR_ranking[,-1], 1, sum) 

WHR_ranking <- data.frame(cbind(WHR_ranking, WHR_score = WHR_summary[1:137,]$Life.Ladder))
WHR_ranking$residual <- WHR_ranking$WHR_score - WHR_ranking$sum
WHR_ranking <- WHR_ranking %>% 
  select(!sum)
WHR_ranking <- WHR_ranking[,c(1, 8, 2, 3, 4, 5, 6, 7, 9)]


#AGE STANDARDIZED
#Reading Gallup data
gallup_main <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/gallupCleaned230118_Makoto.csv")
#CHANGE DATA
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
standard_population <- rep(c(0.3333784699, 0.2890995261, 0.2161137441,
                             0.1203791469, 0.04150304671), 17)
gallup_age <- cbind(gallup_age, standardpop = standard_population)


"____________________________________________________________________________"


#AGES 15-29
gallup_age_15_29 <- gallup_age %>% 
  filter(age_group == "15-29")
age_standardized_scores_wide_15_29 <- gallup_age_15_29  %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(across(AFG:ZWE, ~ weighted.mean(.x, standardpop)))
age_standardized_scores_15_29 <- gather(age_standardized_scores_wide_15_29, COUNTRY_ISO3, AS.LS, AFG:ZWE)

colnames(age_standardized_scores_15_29) <- c("year", "COUNTRY_ISO3", "AS.LS", "Country.name")

WHR_data$COUNTRY_ISO3 <- countrycode(WHR_data$Country.name, 'country.name', 'iso3c')
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Kosovo"] <- "XKX"
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Somaliland region"] <- "XSR" #temp
Standardized_WHR_15_29 <- merge(WHR_data, age_standardized_scores_15_29, by=c("year","COUNTRY_ISO3"))

standardized_WHR_summary_15_29 <- Standardized_WHR_15_29 %>% filter(year >= 2019, year <= 2021)%>% 
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
standardized_WHR_summary_15_29 <- rbind(standardized_WHR_summary_15_29, df)

standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(standardized_WHR_summary_15_29$Log.GDP.per.capita, na.rm = TRUE)
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Dystopia", "Social.support"] <-
  min(standardized_WHR_summary_15_29$Social.support, na.rm = TRUE)
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(standardized_WHR_summary_15_29$Healthy.life.expectancy.at.birth, na.rm = TRUE)
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(standardized_WHR_summary_15_29$Freedom.to.make.life.choices, na.rm = TRUE)
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Dystopia", "Generosity"] <-
  min(standardized_WHR_summary_15_29$Generosity, na.rm = TRUE)
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(standardized_WHR_summary_15_29$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Cyprus", "Log.GDP.per.capita"]
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="North Cyprus", "Generosity"] <-
  standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(coef(summary(reg1))[4]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_15_29[standardized_WHR_summary_15_29$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_15_29[
    standardized_WHR_summary_15_29$Country.name=="Dystopia", "Perceptions.of.corruption"])



Standardized_regression_data_15_29 <- Standardized_WHR_15_29 %>%
  filter(!is.na(AS.LS),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg2_15_29 <- plm(AS.LS~ Log.GDP.per.capita+Social.support+
                    Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
                    Generosity+Perceptions.of.corruption, data = Standardized_regression_data_15_29,
                  index = c("year","Country.name"), model="within")

standardized_contributions_15_29 <- distance_from_dystopia$Country.name
standardized_contributions_15_29 <- cbind(standardized_contributions_15_29, distance_from_dystopia[2] * coef(summary(reg2_15_29))[1,1])
standardized_contributions_15_29 <- cbind(standardized_contributions_15_29, distance_from_dystopia[3] * coef(summary(reg2_15_29))[2,1])
standardized_contributions_15_29 <- cbind(standardized_contributions_15_29, distance_from_dystopia[4] * coef(summary(reg2_15_29))[3,1])
standardized_contributions_15_29 <- cbind(standardized_contributions_15_29, distance_from_dystopia[5] * coef(summary(reg2_15_29))[4,1])
standardized_contributions_15_29 <- cbind(standardized_contributions_15_29, distance_from_dystopia[6] * coef(summary(reg2_15_29))[5,1])
standardized_contributions_15_29 <- cbind(standardized_contributions_15_29, distance_from_dystopia[7] * coef(summary(reg2_15_29))[6,1])

standardized_contributions_15_29$sum <- apply(standardized_contributions_15_29[,-1], 1, sum) 
ASLS_15_29 <- standardized_WHR_summary_15_29[1:2]
standardized_contributions_15_29 <- merge(standardized_contributions_15_29, ASLS_15_29, 
                                          by.x="standardized_contributions_15_29",
                                          by.y="Country.name")

#Manually impute missing data for Libya, Saudi Arabia
standardized_contributions_15_29[standardized_contributions_15_29$standardized_contributions=="Libya", "AS.LS"] <-
  5.069252
standardized_contributions_15_29[standardized_contributions_15_29$standardized_contributions=="Saudi Arabia", "AS.LS"] <-
  6.217615


standardized_contributions_15_29$residual <- standardized_contributions_15_29$AS.LS - standardized_contributions$sum
standardized_contributions1_15_29 <- standardized_contributions_15_29[, c(1, 9, 2, 3, 4, 5, 6, 7, 10)]
names(standardized_contributions1_15_29)[names(standardized_contributions1_15_29) == 'standardized_contributions'] <- 
  'Country'
standardized_contributions1_15_29$Rank <- rank(-standardized_contributions1_15_29$AS.LS)

write.csv(standardized_contributions1_15_29,
          file="/Users/makototakahara/Downloads/Age_Specific_15to29.csv")


"________________________________________________________________________________"


#AGES 30_44
gallup_age_30_44 <- gallup_age %>% 
  filter(age_group == "30-44")
age_standardized_scores_wide_30_44 <- gallup_age_30_44  %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(across(AFG:ZWE, ~ weighted.mean(.x, standardpop)))
age_standardized_scores_30_44 <- gather(age_standardized_scores_wide_30_44, COUNTRY_ISO3, AS.LS, AFG:ZWE)

colnames(age_standardized_scores_30_44) <- c("year", "COUNTRY_ISO3", "AS.LS", "Country.name")

WHR_data$COUNTRY_ISO3 <- countrycode(WHR_data$Country.name, 'country.name', 'iso3c')
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Kosovo"] <- "XKX"
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Somaliland region"] <- "XSR" #temp
Standardized_WHR_30_44 <- merge(WHR_data, age_standardized_scores_30_44, by=c("year","COUNTRY_ISO3"))

standardized_WHR_summary_30_44 <- Standardized_WHR_30_44 %>% filter(year >= 2019, year <= 2021)%>% 
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
standardized_WHR_summary_30_44 <- rbind(standardized_WHR_summary_30_44, df)

standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(standardized_WHR_summary_30_44$Log.GDP.per.capita, na.rm = TRUE)
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Dystopia", "Social.support"] <-
  min(standardized_WHR_summary_30_44$Social.support, na.rm = TRUE)
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(standardized_WHR_summary_30_44$Healthy.life.expectancy.at.birth, na.rm = TRUE)
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(standardized_WHR_summary_30_44$Freedom.to.make.life.choices, na.rm = TRUE)
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Dystopia", "Generosity"] <-
  min(standardized_WHR_summary_30_44$Generosity, na.rm = TRUE)
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(standardized_WHR_summary_30_44$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Cyprus", "Log.GDP.per.capita"]
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="North Cyprus", "Generosity"] <-
  standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(coef(summary(reg1))[4]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_30_44[standardized_WHR_summary_30_44$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_30_44[
    standardized_WHR_summary_30_44$Country.name=="Dystopia", "Perceptions.of.corruption"])



Standardized_regression_data_30_44 <- Standardized_WHR_30_44 %>%
  filter(!is.na(AS.LS),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg2_30_44 <- plm(AS.LS~ Log.GDP.per.capita+Social.support+
                    Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
                    Generosity+Perceptions.of.corruption, data = Standardized_regression_data_30_44,
                  index = c("year","Country.name"), model="within")

standardized_contributions_30_44 <- distance_from_dystopia$Country.name
standardized_contributions_30_44 <- cbind(standardized_contributions_30_44, distance_from_dystopia[2] * coef(summary(reg2_30_44))[1,1])
standardized_contributions_30_44 <- cbind(standardized_contributions_30_44, distance_from_dystopia[3] * coef(summary(reg2_30_44))[2,1])
standardized_contributions_30_44 <- cbind(standardized_contributions_30_44, distance_from_dystopia[4] * coef(summary(reg2_30_44))[3,1])
standardized_contributions_30_44 <- cbind(standardized_contributions_30_44, distance_from_dystopia[5] * coef(summary(reg2_30_44))[4,1])
standardized_contributions_30_44 <- cbind(standardized_contributions_30_44, distance_from_dystopia[6] * coef(summary(reg2_30_44))[5,1])
standardized_contributions_30_44 <- cbind(standardized_contributions_30_44, distance_from_dystopia[7] * coef(summary(reg2_30_44))[6,1])

standardized_contributions_30_44$sum <- apply(standardized_contributions_30_44[,-1], 1, sum) 
ASLS_30_44 <- standardized_WHR_summary_30_44[1:2]
standardized_contributions_30_44 <- merge(standardized_contributions_30_44, ASLS_30_44, 
                                          by.x="standardized_contributions_30_44",
                                          by.y="Country.name")

#Manually impute missing data for Libya, Saudi Arabia
standardized_contributions_30_44[standardized_contributions_30_44$standardized_contributions=="Libya", "AS.LS"] <-
  5.069252
standardized_contributions_30_44[standardized_contributions_30_44$standardized_contributions=="Saudi Arabia", "AS.LS"] <-
  6.217615


standardized_contributions_30_44$residual <- standardized_contributions_30_44$AS.LS - standardized_contributions$sum
standardized_contributions1_30_44 <- standardized_contributions_30_44[, c(1, 9, 2, 3, 4, 5, 6, 7, 10)]
names(standardized_contributions1_30_44)[names(standardized_contributions1_30_44) == 'standardized_contributions'] <- 
  'Country'
standardized_contributions1_30_44$Rank <- rank(-standardized_contributions1_30_44$AS.LS)

write.csv(standardized_contributions1_30_44,
          file="/Users/makototakahara/Downloads/Age_Specific_30to44.csv")

"________________________________________________________________________"



#AGES 45-59
gallup_age_45_59 <- gallup_age %>% 
  filter(age_group == "45-59")
age_standardized_scores_wide_45_59 <- gallup_age_45_59  %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(across(AFG:ZWE, ~ weighted.mean(.x, standardpop)))
age_standardized_scores_45_59 <- gather(age_standardized_scores_wide_45_59, COUNTRY_ISO3, AS.LS, AFG:ZWE)

colnames(age_standardized_scores_45_59) <- c("year", "COUNTRY_ISO3", "AS.LS", "Country.name")

WHR_data$COUNTRY_ISO3 <- countrycode(WHR_data$Country.name, 'country.name', 'iso3c')
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Kosovo"] <- "XKX"
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Somaliland region"] <- "XSR" #temp
Standardized_WHR_45_59 <- merge(WHR_data, age_standardized_scores_45_59, by=c("year","COUNTRY_ISO3"))

standardized_WHR_summary_45_59 <- Standardized_WHR_45_59 %>% filter(year >= 2019, year <= 2021)%>% 
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
standardized_WHR_summary_45_59 <- rbind(standardized_WHR_summary_45_59, df)

standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(standardized_WHR_summary_45_59$Log.GDP.per.capita, na.rm = TRUE)
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Dystopia", "Social.support"] <-
  min(standardized_WHR_summary_45_59$Social.support, na.rm = TRUE)
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(standardized_WHR_summary_45_59$Healthy.life.expectancy.at.birth, na.rm = TRUE)
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(standardized_WHR_summary_45_59$Freedom.to.make.life.choices, na.rm = TRUE)
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Dystopia", "Generosity"] <-
  min(standardized_WHR_summary_45_59$Generosity, na.rm = TRUE)
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(standardized_WHR_summary_45_59$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Cyprus", "Log.GDP.per.capita"]
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="North Cyprus", "Generosity"] <-
  standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(coef(summary(reg1))[4]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_45_59[standardized_WHR_summary_45_59$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_45_59[
    standardized_WHR_summary_45_59$Country.name=="Dystopia", "Perceptions.of.corruption"])



Standardized_regression_data_45_59 <- Standardized_WHR_45_59 %>%
  filter(!is.na(AS.LS),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg2_45_59 <- plm(AS.LS~ Log.GDP.per.capita+Social.support+
                    Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
                    Generosity+Perceptions.of.corruption, data = Standardized_regression_data_45_59,
                  index = c("year","Country.name"), model="within")

standardized_contributions_45_59 <- distance_from_dystopia$Country.name
standardized_contributions_45_59 <- cbind(standardized_contributions_45_59, distance_from_dystopia[2] * coef(summary(reg2_45_59))[1,1])
standardized_contributions_45_59 <- cbind(standardized_contributions_45_59, distance_from_dystopia[3] * coef(summary(reg2_45_59))[2,1])
standardized_contributions_45_59 <- cbind(standardized_contributions_45_59, distance_from_dystopia[4] * coef(summary(reg2_45_59))[3,1])
standardized_contributions_45_59 <- cbind(standardized_contributions_45_59, distance_from_dystopia[5] * coef(summary(reg2_45_59))[4,1])
standardized_contributions_45_59 <- cbind(standardized_contributions_45_59, distance_from_dystopia[6] * coef(summary(reg2_45_59))[5,1])
standardized_contributions_45_59 <- cbind(standardized_contributions_45_59, distance_from_dystopia[7] * coef(summary(reg2_45_59))[6,1])

standardized_contributions_45_59$sum <- apply(standardized_contributions_45_59[,-1], 1, sum) 
ASLS_45_59 <- standardized_WHR_summary_45_59[1:2]
standardized_contributions_45_59 <- merge(standardized_contributions_45_59, ASLS_45_59, 
                                          by.x="standardized_contributions_45_59",
                                          by.y="Country.name")

#Manually impute missing data for Libya, Saudi Arabia
standardized_contributions_45_59[standardized_contributions_45_59$standardized_contributions=="Libya", "AS.LS"] <-
  5.069252
standardized_contributions_45_59[standardized_contributions_45_59$standardized_contributions=="Saudi Arabia", "AS.LS"] <-
  6.217615


standardized_contributions_45_59$residual <- standardized_contributions_45_59$AS.LS - standardized_contributions$sum
standardized_contributions1_45_59 <- standardized_contributions_45_59[, c(1, 9, 2, 3, 4, 5, 6, 7, 10)]
names(standardized_contributions1_45_59)[names(standardized_contributions1_45_59) == 'standardized_contributions'] <- 
  'Country'
standardized_contributions1_45_59$Rank <- rank(-standardized_contributions1_45_59$AS.LS)

write.csv(standardized_contributions1_45_59,
          file="/Users/makototakahara/Downloads/Age_Specific_45to59.csv")


"_________________________________________________________________________"


#AGES 60-74
gallup_age_60_74 <- gallup_age %>% 
  filter(age_group == "60-74")
age_standardized_scores_wide_60_74 <- gallup_age_60_74  %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(across(AFG:ZWE, ~ weighted.mean(.x, standardpop)))
age_standardized_scores_60_74 <- gather(age_standardized_scores_wide_60_74, COUNTRY_ISO3, AS.LS, AFG:ZWE)

colnames(age_standardized_scores_60_74) <- c("year", "COUNTRY_ISO3", "AS.LS", "Country.name")

WHR_data$COUNTRY_ISO3 <- countrycode(WHR_data$Country.name, 'country.name', 'iso3c')
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Kosovo"] <- "XKX"
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Somaliland region"] <- "XSR" #temp
Standardized_WHR_60_74 <- merge(WHR_data, age_standardized_scores_60_74, by=c("year","COUNTRY_ISO3"))

standardized_WHR_summary_60_74 <- Standardized_WHR_60_74 %>% filter(year >= 2019, year <= 2021)%>% 
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
standardized_WHR_summary_60_74 <- rbind(standardized_WHR_summary_60_74, df)

standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(standardized_WHR_summary_60_74$Log.GDP.per.capita, na.rm = TRUE)
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Dystopia", "Social.support"] <-
  min(standardized_WHR_summary_60_74$Social.support, na.rm = TRUE)
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(standardized_WHR_summary_60_74$Healthy.life.expectancy.at.birth, na.rm = TRUE)
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(standardized_WHR_summary_60_74$Freedom.to.make.life.choices, na.rm = TRUE)
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Dystopia", "Generosity"] <-
  min(standardized_WHR_summary_60_74$Generosity, na.rm = TRUE)
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(standardized_WHR_summary_60_74$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Cyprus", "Log.GDP.per.capita"]
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="North Cyprus", "Generosity"] <-
  standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(coef(summary(reg1))[4]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_60_74[standardized_WHR_summary_60_74$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_60_74[
    standardized_WHR_summary_60_74$Country.name=="Dystopia", "Perceptions.of.corruption"])



Standardized_regression_data_60_74 <- Standardized_WHR_60_74 %>%
  filter(!is.na(AS.LS),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg2_60_74 <- plm(AS.LS~ Log.GDP.per.capita+Social.support+
                    Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
                    Generosity+Perceptions.of.corruption, data = Standardized_regression_data_60_74,
                  index = c("year","Country.name"), model="within")

standardized_contributions_60_74 <- distance_from_dystopia$Country.name
standardized_contributions_60_74 <- cbind(standardized_contributions_60_74, distance_from_dystopia[2] * coef(summary(reg2_60_74))[1,1])
standardized_contributions_60_74 <- cbind(standardized_contributions_60_74, distance_from_dystopia[3] * coef(summary(reg2_60_74))[2,1])
standardized_contributions_60_74 <- cbind(standardized_contributions_60_74, distance_from_dystopia[4] * coef(summary(reg2_60_74))[3,1])
standardized_contributions_60_74 <- cbind(standardized_contributions_60_74, distance_from_dystopia[5] * coef(summary(reg2_60_74))[4,1])
standardized_contributions_60_74 <- cbind(standardized_contributions_60_74, distance_from_dystopia[6] * coef(summary(reg2_60_74))[5,1])
standardized_contributions_60_74 <- cbind(standardized_contributions_60_74, distance_from_dystopia[7] * coef(summary(reg2_60_74))[6,1])

standardized_contributions_60_74$sum <- apply(standardized_contributions_60_74[,-1], 1, sum) 
ASLS_60_74 <- standardized_WHR_summary_60_74[1:2]
standardized_contributions_60_74 <- merge(standardized_contributions_60_74, ASLS_60_74, 
                                          by.x="standardized_contributions_60_74",
                                          by.y="Country.name")

#Manually impute missing data for Libya, Saudi Arabia
standardized_contributions_60_74[standardized_contributions_60_74$standardized_contributions=="Libya", "AS.LS"] <-
  5.069252
standardized_contributions_60_74[standardized_contributions_60_74$standardized_contributions=="Saudi Arabia", "AS.LS"] <-
  6.217615


standardized_contributions_60_74$residual <- standardized_contributions_60_74$AS.LS - standardized_contributions$sum
standardized_contributions1_60_74 <- standardized_contributions_60_74[, c(1, 9, 2, 3, 4, 5, 6, 7, 10)]
names(standardized_contributions1_60_74)[names(standardized_contributions1_60_74) == 'standardized_contributions'] <- 
  'Country'
standardized_contributions1_60_74$Rank <- rank(-standardized_contributions1_60_74$AS.LS)

write.csv(standardized_contributions1_60_74,
          file="/Users/makototakahara/Downloads/Age_Specific_60to74.csv")



"___________________________________________________________________"


#Over 75
gallup_age_over_75 <- gallup_age %>% 
  filter(age_group == "Over 75")
age_standardized_scores_wide_over_75 <- gallup_age_over_75  %>%
  group_by(YEAR_CALENDAR) %>%
  summarise(across(AFG:ZWE, ~ weighted.mean(.x, standardpop)))
age_standardized_scores_over_75 <- gather(age_standardized_scores_wide_over_75, COUNTRY_ISO3, AS.LS, AFG:ZWE)

colnames(age_standardized_scores_over_75) <- c("year", "COUNTRY_ISO3", "AS.LS", "Country.name")

WHR_data$COUNTRY_ISO3 <- countrycode(WHR_data$Country.name, 'country.name', 'iso3c')
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Kosovo"] <- "XKX"
WHR_data$COUNTRY_ISO3[WHR_data$Country.name=="Somaliland region"] <- "XSR" #temp
Standardized_WHR_over_75 <- merge(WHR_data, age_standardized_scores_over_75, by=c("year","COUNTRY_ISO3"))

standardized_WHR_summary_over_75 <- Standardized_WHR_over_75 %>% filter(year >= 2019, year <= 2021)%>% 
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
standardized_WHR_summary_over_75 <- rbind(standardized_WHR_summary_over_75, df)

standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Dystopia", "Log.GDP.per.capita"] <-
  min(standardized_WHR_summary_over_75$Log.GDP.per.capita, na.rm = TRUE)
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Dystopia", "Social.support"] <-
  min(standardized_WHR_summary_over_75$Social.support, na.rm = TRUE)
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"] <-
  min(standardized_WHR_summary_over_75$Healthy.life.expectancy.at.birth, na.rm = TRUE)
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Dystopia", "Freedom.to.make.life.choices"] <-
  min(standardized_WHR_summary_over_75$Freedom.to.make.life.choices, na.rm = TRUE)
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Dystopia", "Generosity"] <-
  min(standardized_WHR_summary_over_75$Generosity, na.rm = TRUE)
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"] <-
  max(standardized_WHR_summary_over_75$Perceptions.of.corruption, na.rm = TRUE)


#Impute missing data
#For Northern Cyprus GDP, Life Expectancy, Generosity - Use Cyprus Data
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="North Cyprus", "Log.GDP.per.capita"] <-
  standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Cyprus", "Log.GDP.per.capita"]
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="North Cyprus", "Healthy.life.expectancy.at.birth"] <-
  standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Cyprus", "Healthy.life.expectancy.at.birth"]
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="North Cyprus", "Generosity"] <-
  standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Cyprus", "Generosity"]

#For other missing values, calculate from WHR
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Kosovo", "Healthy.life.expectancy.at.birth"] <-
  (0.569)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Hong Kong S.A.R. of China", "Healthy.life.expectancy.at.birth"] <-
  (0.942)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Palestinian Territories", "Healthy.life.expectancy.at.birth"] <-
  (0.521)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Taiwan Province of China", "Healthy.life.expectancy.at.birth"] <-
  (0.733)/(coef(summary(reg1))[3]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Healthy.life.expectancy.at.birth"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Tajikistan", "Freedom.to.make.life.choices"] <-
  (0.572)/(coef(summary(reg1))[4]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Freedom.to.make.life.choices"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Saudi Arabia", "Perceptions.of.corruption"] <-
  (0.180)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Kuwait", "Perceptions.of.corruption"] <-
  (0.147)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="China", "Perceptions.of.corruption"] <-
  (0.142)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Bahrain", "Perceptions.of.corruption"] <-
  (0.155)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="United Arab Emirates", "Perceptions.of.corruption"] <-
  (0.250)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"])
standardized_WHR_summary_over_75[standardized_WHR_summary_over_75$Country.name=="Turkmenistan", "Perceptions.of.corruption"] <-
  (0.032)/(coef(summary(reg1))[6]) + as.numeric(standardized_WHR_summary_over_75[
    standardized_WHR_summary_over_75$Country.name=="Dystopia", "Perceptions.of.corruption"])



Standardized_regression_data_over_75 <- Standardized_WHR_over_75 %>%
  filter(!is.na(AS.LS),
         !is.na(Log.GDP.per.capita),
         !is.na(Social.support),
         !is.na(Healthy.life.expectancy.at.birth),
         !is.na(Freedom.to.make.life.choices),
         !is.na(Generosity),
         !is.na(Perceptions.of.corruption))

reg2_over_75 <- plm(AS.LS~ Log.GDP.per.capita+Social.support+
                      Healthy.life.expectancy.at.birth+Freedom.to.make.life.choices+
                      Generosity+Perceptions.of.corruption, data = Standardized_regression_data_over_75,
                    index = c("year","Country.name"), model="within")

standardized_contributions_over_75 <- distance_from_dystopia$Country.name
standardized_contributions_over_75 <- cbind(standardized_contributions_over_75, distance_from_dystopia[2] * coef(summary(reg2_over_75))[1,1])
standardized_contributions_over_75 <- cbind(standardized_contributions_over_75, distance_from_dystopia[3] * coef(summary(reg2_over_75))[2,1])
standardized_contributions_over_75 <- cbind(standardized_contributions_over_75, distance_from_dystopia[4] * coef(summary(reg2_over_75))[3,1])
standardized_contributions_over_75 <- cbind(standardized_contributions_over_75, distance_from_dystopia[5] * coef(summary(reg2_over_75))[4,1])
standardized_contributions_over_75 <- cbind(standardized_contributions_over_75, distance_from_dystopia[6] * coef(summary(reg2_over_75))[5,1])
standardized_contributions_over_75 <- cbind(standardized_contributions_over_75, distance_from_dystopia[7] * coef(summary(reg2_over_75))[6,1])

standardized_contributions_over_75$sum <- apply(standardized_contributions_over_75[,-1], 1, sum) 
ASLS_over_75 <- standardized_WHR_summary_over_75[1:2]
standardized_contributions_over_75 <- merge(standardized_contributions_over_75, ASLS_over_75, 
                                            by.x="standardized_contributions_over_75",
                                            by.y="Country.name")

#Manually impute missing data for Libya, Saudi Arabia
standardized_contributions_over_75[standardized_contributions_over_75$standardized_contributions=="Libya", "AS.LS"] <-
  5.069252
standardized_contributions_over_75[standardized_contributions_over_75$standardized_contributions=="Saudi Arabia", "AS.LS"] <-
  6.217615


standardized_contributions_over_75$residual <- standardized_contributions_over_75$AS.LS - standardized_contributions$sum
standardized_contributions1_over_75 <- standardized_contributions_over_75[, c(1, 9, 2, 3, 4, 5, 6, 7, 10)]
names(standardized_contributions1_over_75)[names(standardized_contributions1_over_75) == 'standardized_contributions'] <- 
  'Country'
standardized_contributions1_over_75$Rank <- rank(-standardized_contributions1_over_75$AS.LS)

write.csv(standardized_contributions1_over_75,
          file="/Users/makototakahara/Downloads/Age_Specific_over75.csv")


"______________________________________________________________________"


var_names=names(coef(reg2_15_29)) 
coef_vals=coef(reg2_15_29) 
a <- data.frame(Variables=var_names, "15_29"=coef_vals)

var_names=names(coef(reg2_30_44)) 
coef_vals=coef(reg2_30_44) 
b <- data.frame(Variables=var_names, "30_44"=coef_vals)

var_names=names(coef(reg2_45_59)) 
coef_vals=coef(reg2_45_59) 
c <- data.frame(Variables=var_names, "45_59"=coef_vals)

var_names=names(coef(reg2_60_74)) 
coef_vals=coef(reg2_60_74) 
d <- data.frame(Variables=var_names, "60_74"=coef_vals)

var_names=names(coef(reg2_over_75)) 
coef_vals=coef(reg2_over_75) 
e <- data.frame(Variables=var_names, "Over_75"=coef_vals)

coefficient_analysis <- list(a, b, c, d, e)      
coefficient_analysis <- coefficient_analysis %>% 
  reduce(full_join, by='Variables')

coefficient_analysis %>% 
  pivot_longer(-db) %>% 
  ggplot(aes(x=, y=value, fill=name))+
  geom_bar(position="stack", stat="identity")


write.csv(coefficient_analysis,
          file="/Users/makototakahara/Downloads/coefficient_analysis.csv")

long <- melt(setDT(coefficient_analysis), id.vars = c("Variables"), variable.name = "year")
barplot <- ggplot(long, aes(fill=Variables, y=value, x=year)) + 
  geom_bar(position="stack", stat="identity")

