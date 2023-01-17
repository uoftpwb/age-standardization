library(tidyverse)
library(GGally)

#Reading Gallup data
gallup_main <- read.csv("/Users/MakBook/Downloads/GallupCleaned_Sep19.csv")
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16.LS) 

#Finding unweighted life satisfaction scores, counting observations
WHR_scores <- gallup_main %>% 
  filter(!is.na(Weighted.LS)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>%
  group_by(COUNTRY_ISO3) %>% 
  summarise(WHR_Score = sum(Weighted.LS,na.rm=T)/sum(WGT, na.rm=T))

#Adding ranking column to WHR replication
WHR_scores$WHR_Rank <- rank(-WHR_scores$WHR_Score)

#Creating age groups for Gallup respondents
gallup_15 <- gallup_main %>% 
  filter(!is.na(WP1220.AGE)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>% 
  mutate(
    age_group = case_when(
      WP1220.AGE > 14 & WP1220.AGE <= 29 ~ "15-29",
      WP1220.AGE > 29 & WP1220.AGE <= 44 ~ "30-44",
      WP1220.AGE > 44 & WP1220.AGE <= 59 ~ "45-59",
      WP1220.AGE > 59 & WP1220.AGE <= 74 ~ "60-74",
      WP1220.AGE > 74 ~ "Over 75"))

#Finding mean life satisfaction scores for each age group
gallup_age_15 <- gallup_15 %>% 
  group_by(COUNTRY_ISO3, age_group, .drop = FALSE) %>% 
  summarise(WeightedMeanLS = sum(Weighted.LS, na.rm=T)/sum(WGT, na.rm=T)) %>%
  pivot_wider(names_from = COUNTRY_ISO3, values_from = WeightedMeanLS) 

#Filling missing age groups with LS scores of 0
gallup_age_15[is.na(gallup_age_15)] = 0

#Creating weighted average based on WHO Standard Population
standard_population_15 <- c(0.3333784699, 0.2890995261, 0.2161137441,
                         0.1203791469, 0.04150304671)

countries <- colnames(gallup_age_15)[-(1)]
age_standardized_scores_15 <- gallup_age_15 %>%
  summarise(across(all_of(countries), 
                   list(ASLS = ~ weighted.mean(., standard_population_15))))

#Transpose for uniformity
age_standardized_scores_15 <- pivot_longer(age_standardized_scores_15, 
                                           cols = everything(),
                                           names_to = "COUNTRY_ISO3", 
                                           values_to = "AS_Score")
age_standardized_scores_15$COUNTRY_ISO3 <- gsub(
  '_ASLS', '', age_standardized_scores_15$COUNTRY_ISO3)

#Adding ranking column to AS Scores
age_standardized_scores_15$AS_Score <- as.numeric(
  age_standardized_scores_15$AS_Score)
age_standardized_scores_15$AS_Rank <- rank(-age_standardized_scores_15$AS_Score)

#Merging two data frames
results <- merge(WHR_scores, age_standardized_scores_15, by = "COUNTRY_ISO3")
results$rank_difference <- (results$WHR_Rank - results$AS_Rank)
results$score_difference <- (results$AS_Score - results$WHR_Score)

#Visualization
result_ranking <- results[, c("COUNTRY_ISO3", "WHR_Rank", "AS_Rank")]
GGally::ggparcoord(result_ranking,
                   columns = 2:3, groupColumn = 1,  
                   scale="globalminmax", 
                   showPoints = TRUE, 
                   title = "Ranking Comparison"
) + scale_y_reverse()

#Ranking by age group
gallup_15_n <- gallup_15 %>% 
  group_by(COUNTRY_ISO3, age_group, .drop = FALSE) %>% 
  summarize(n = n())
  
ranking_15_29 <- gallup_age_15[1, -1] %>% 
  pivot_longer(cols = everything(),
               names_to = "COUNTRY_ISO3",
               values_to = "Age_Group_Score")
ranking_15_29$Age_Score_Rank <- rank(-ranking_15_29$Age_Group_Score)
ranking_15_29 <- merge(WHR_scores, ranking_15_29, by = "COUNTRY_ISO3")
ranking_15_29$rank_difference <- (ranking_15_29$WHR_Rank - 
                                    ranking_15_29$Score_Rank)
ranking_15_29$score_difference <- (ranking_15_29$Age_Group_Score - 
                                     ranking_15_29$WHR_Score)
n_15_29 <- gallup_15_n %>% 
  filter(age_group == "15-29") %>% 
  select(COUNTRY_ISO3, n)
ranking_15_29 <- merge(ranking_15_29, n_15_29, by = "COUNTRY_ISO3")


ranking_30_44 <- gallup_age_15[2, -1] %>% 
  pivot_longer(cols = everything(),
               names_to = "COUNTRY_ISO3",
               values_to = "Age_Group_Score")
ranking_30_44$Age_Score_Rank <- rank(-ranking_30_44$Age_Group_Score)
ranking_30_44 <- merge(WHR_scores, ranking_30_44, by = "COUNTRY_ISO3")
ranking_30_44$rank_difference <- (ranking_30_44$WHR_Rank - 
                                    ranking_30_44$Age_Score_Rank)
ranking_30_44$score_difference <- (ranking_30_44$Age_Group_Score - 
                                     ranking_30_44$WHR_Score)
n_30_44 <- gallup_15_n %>% 
  filter(age_group == "30-44") %>% 
  select(COUNTRY_ISO3, n)
ranking_30_44 <- merge(ranking_30_44, n_30_44, by = "COUNTRY_ISO3")


ranking_45_59 <- gallup_age_15[3, -1] %>% 
  pivot_longer(cols = everything(),
               names_to = "COUNTRY_ISO3",
               values_to = "Age_Group_Score")
ranking_45_59$Age_Score_Rank <- rank(-ranking_45_59$Age_Group_Score)
ranking_45_59 <- merge(WHR_scores, ranking_45_59, by = "COUNTRY_ISO3")
ranking_45_59$rank_difference <- (ranking_45_59$WHR_Rank - 
                                    ranking_45_59$Age_Score_Rank)
ranking_45_59$score_difference <- (ranking_45_59$Age_Group_Score - 
                                     ranking_45_59$WHR_Score)
n_45_59 <- gallup_15_n %>% 
  filter(age_group == "45-59") %>% 
  select(COUNTRY_ISO3, n)
ranking_45_59 <- merge(ranking_45_59, n_45_59, by = "COUNTRY_ISO3")


ranking_60_74 <- gallup_age_15[4, -1] %>% 
  pivot_longer(cols = everything(),
               names_to = "COUNTRY_ISO3",
               values_to = "Age_Group_Score")
ranking_60_74$Age_Score_Rank <- rank(-ranking_60_74$Age_Group_Score)
ranking_60_74 <- merge(WHR_scores, ranking_60_74, by = "COUNTRY_ISO3")
ranking_60_74$rank_difference <- (ranking_60_74$WHR_Rank - 
                                    ranking_60_74$Age_Score_Rank)
ranking_60_74$score_difference <- (ranking_60_74$Age_Group_Score - 
                                     ranking_60_74$WHR_Score)
n_60_74 <- gallup_15_n %>% 
  filter(age_group == "60-74") %>% 
  select(COUNTRY_ISO3, n)
ranking_60_74 <- merge(ranking_60_74, n_60_74, by = "COUNTRY_ISO3")

ranking_over_75 <- gallup_age_15[5, -1] %>% 
  pivot_longer(cols = everything(),
               names_to = "COUNTRY_ISO3",
               values_to = "Age_Group_Score")
ranking_over_75$Age_Score_Rank <- rank(-ranking_over_75$Age_Group_Score)
ranking_over_75 <- merge(WHR_scores, ranking_over_75, by = "COUNTRY_ISO3")
ranking_over_75$rank_difference <- (ranking_over_75$WHR_Rank - 
                                      ranking_over_75$Age_Score_Rank)
ranking_over_75$score_difference <- (ranking_over_75$Age_Group_Score - 
                                       ranking_over_75$WHR_Score)
n_over_75 <- gallup_15_n %>% 
  filter(age_group == "Over 75") %>% 
  select(COUNTRY_ISO3, n)
ranking_over_75 <- merge(ranking_over_75, n_over_75, by = "COUNTRY_ISO3")

#Ranking with age groups n >= 100
ranking_15_29_filtered <- ranking_15_29 %>% 
  filter(n >= 100)

ranking_30_44_filtered <- ranking_30_44 %>% 
  filter(n >= 100)

ranking_45_59_filtered <- ranking_45_59 %>% 
  filter(n >= 100)

ranking_60_74_filtered <- ranking_60_74 %>% 
  filter(n >= 100)

ranking_over_75_filtered <- ranking_over_75 %>% 
  filter(n >= 100)

#Writing files
write.csv(results,"/Users/MakBook/Downloads/AS_full_results.csv", 
          row.names = FALSE)
write.csv(ranking_15_29_filtered,"/Users/MakBook/Downloads/AS_15_29_results.csv", 
          row.names = FALSE)
write.csv(ranking_30_44_filtered,"/Users/MakBook/Downloads/AS_30_44_results.csv", 
          row.names = FALSE)
write.csv(ranking_45_59_filtered,"/Users/MakBook/Downloads/AS_45_59_results.csv", 
          row.names = FALSE)
write.csv(ranking_60_74_filtered,"/Users/MakBook/Downloads/AS_60_74_results.csv", 
          row.names = FALSE)
write.csv(ranking_over_75_filtered,"/Users/MakBook/Downloads/AS_over_75_results.csv", 
          row.names = FALSE)