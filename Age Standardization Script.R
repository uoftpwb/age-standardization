library(tidyverse)

#Reading Gallup data
gallup_main <- read.csv("/Users/MakBook/Downloads/GallupCleaned_Sep19.csv")
gallup_main$Weighted.LS <- as.numeric(gallup_main$WGT*gallup_main$WP16.LS) 

#Creating age groups for Gallup respondents
gallup_main <- gallup_main %>% 
  filter(!is.na(WP1220.AGE)) %>% 
  filter(YEAR_CALENDAR >= 2019 & YEAR_CALENDAR <= 2021) %>% 
  mutate(
    age_group = case_when(
      WP1220.AGE > 14 & WP1220.AGE <= 19 ~ "15-19",
      WP1220.AGE > 19 & WP1220.AGE <= 24 ~ "20-24",
      WP1220.AGE > 24 & WP1220.AGE <= 29 ~ "25-29",
      WP1220.AGE > 29 & WP1220.AGE <= 34 ~ "30-34",
      WP1220.AGE > 34 & WP1220.AGE <= 39 ~ "35-39",
      WP1220.AGE > 39 & WP1220.AGE <= 44 ~ "40-44",
      WP1220.AGE > 44 & WP1220.AGE <= 49 ~ "45-49",
      WP1220.AGE > 49 & WP1220.AGE <= 54 ~ "50-54",
      WP1220.AGE > 54 & WP1220.AGE <= 59 ~ "55-59",
      WP1220.AGE > 59 & WP1220.AGE <= 64 ~ "60-64",
      WP1220.AGE > 64 & WP1220.AGE <= 69 ~ "65-69",
      WP1220.AGE > 69 & WP1220.AGE <= 74 ~ "70-74",
      WP1220.AGE > 74 & WP1220.AGE <= 79 ~ "75-79",
      WP1220.AGE > 79 & WP1220.AGE <= 84 ~ "80-84",
      WP1220.AGE > 84 & WP1220.AGE <= 89 ~ "85-89",
      WP1220.AGE > 89 & WP1220.AGE <= 94 ~ "90-94",
      WP1220.AGE > 94 & WP1220.AGE <= 99 ~ "95-99",
      ))

#Finding mean life satisfaction scores for each age group
gallup_age <- gallup_main %>% 
  group_by(COUNTRY_ISO3, age_group, .drop = FALSE) %>% 
  summarise(MeanLS = mean(Weighted.LS,na.rm=T)) %>%
  pivot_wider(names_from = COUNTRY_ISO3, values_from = MeanLS)

#Filling missing age groups with ?????????



#Creating weighted average based on WHO Standard Population
standard_population <- c(0.114691943, 0.111306703, 0.107379824, 0.103046716,
                         0.096817874, 0.089234936, 0.081787407, 0.072714963,
                         0.061611374, 0.050372376, 0.040081246, 0.029925525,
                         0.020582261, 0.012322275, 0.005958023, 0.002031144,
                         0.000541638, 0.000067705)

countries <- colnames(gallup_age)[-(1)]
age_standardized_scores <- gallup_age %>%
  summarize(across(all_of(countries), 
                   list(ASLS = ~ weighted.mean(., standard_population))))

#Transpose tibble for uniformity
age_standardized_scores <- as_tibble(cbind(COUNTRY_IS03 = names(
  age_standardized_scores), t(age_standardized_scores)), 
  .name_repair)
colnames(age_standardized_scores) <- c("COUNTRY_IS03", "AS_Score")
