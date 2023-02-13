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