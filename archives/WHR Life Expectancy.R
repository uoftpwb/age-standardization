# Load the WDIYearHorizontal data
data <- read.csv(
  "/Users/makototakahara/Downloads/PWB Lab/life_expectancy.csv")


# Keep only rows where indicatorcode is "SP.DYN.LE00.IN"
data <- data %>%
  filter(IndicatorCode == "WHOSIS_000001") %>% 
  select("SpatialDimValueCode", "Location", "Period", "Dim1ValueCode", 
         "FactValueNumeric", "Value")

# Reshape the data
data_long <- reshape(data, idvar = c("Location", "Dim1ValueCode"), 
                     timevar = "Period", direction = "wide") %>% 
  select(!c("SpatialDimValueCode.2015", "SpatialDimValueCode.2010",
            "SpatialDimValueCode.2000", "Value.2019", "Value.2015",
            "Value.2010", "Value.2000"))

# Rename, reorder variables
colnames(data_long) <- c("CountryName", "Sex", "Country_ISO3", 
                         "2019", "2015", "2010", "2000")
data_long <- data_long[,c(1, 2, 3, 7, 6, 5, 4)]
data_long[c("2020", "2021")] <- NA




#Extrapolation
x <- colnames(data_long)[c(4, 5, 6, 7, 8, 9)]
y <- data_long %>% 
  filter(Country_ISO3 == "JPN",
         Sex == "MLE")
y <- y[c(4, 5, 6, 7, 8, 9)]

plot(x, y)








# Load MidStage2 data
data2 <- read.dta("MidStage2.dta")

# Sort the data by isocode and year
data2 <- data2[order(data2$isocode, data2$year),]

# Merge the data with transit.dta
merged_data <- merge(data2, data_long, by = c("isocode", "year"), all.x = TRUE)

# Remove rows where _merge == 2
merged_data <- merged_data[merged_data$_merge != 2,]

# Collapse the data by country and isocode
merged_data_collapsed <- aggregate(merged_data, by = list(merged_data$country, merged_data$isocode), mean)

# Browse data if _merge == 1
merged_data_collapsed[merged_data_collapsed$_merge == 1,]

# Remove _merge column
merged_data_collapsed$_merge <- NULL

# Save the data as MidStage2
write.dta(merged_data_collapsed, "MidStage2.dta")

# Load WHO HLE data
data_hle <- read.csv("rawDataNotGWP\WHO HLE Dec 2020.csv", header = TRUE, skip = 1)

# Rename variables
colnames(data_hle)[colnames(data_hle) == "v1"] <- "country"
colnames(data_hle)[colnames(data_hle) == "v2"] <- "bothsex2019"
colnames(data_hle)[colnames(data_hle) == "v3"] <- "bothsex2015"
colnames(data_hle)[colnames(data_hle) == "v4"] <- "bothsex2010"
colnames(data_hle)[colnames(data_hle) == "v5"] <- "bothsex2000"

#######################################################
# Read in data
rawDataNotGWP <- read.csv("rawDataNotGWP/Lancet_table2.csv")

# Split variables and drop unnecessary parts
rawDataNotGWP %>%
  mutate_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")),
            funs(substring(.,1,regexpr("[(]",.)-1))) %>%
  rename_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")),
            funs(substr(.,1,nchar(.)-1))) %>%
  mutate_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")),
            funs(as.numeric)) %>%
  rename_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")),
            funs(sub("1990","",.))) %>%
  rename_at(vars(starts_with("lemale"), starts_with("hlemale"), starts_with("lefemale"), starts_with("hlefemale")),
            funs(sub("2010","",.))) %>%
  select(-starts_with("lemale1990"),-starts_with("hlemale1990"),-starts_with("lefemale1990"),-starts_with("hlefemale1990")) -> rawDataNotGWP

# Assign column labels
colnames(rawDataNotGWP) <- c("country", "lemale1990", "hlemale1990", "lefemale1990", "hlefemale1990", "lemale2010", "hlemale2010", "lefemale2010", "hlefemale2010")

# Summarize variables
summary(rawDataNotGWP[,2:9])

# Keep only country column
rawDataNotGWP %>% select(country) %>% saveRDS("transit.rds")

# Reshape data
rawDataNotGWP %>%
  gather(variable, value, -country) %>%
  separate(variable, c("variable", "year"), sep = 1) %>%
  spread(variable, value) %>%
  mutate(year = as.numeric(year)) -> rawDataNotGWP

# Create new variables for population-wide LE and HLE, assuming 50-50 male-female ratio
rawDataNotGWP %>%
  mutate(leLancet = (lemale + lefemale) / 2,
         hleLancet = (hlemale + hlefemale) / 2) %>%
  select(country, year, leLancet, hleLancet) %>%
  arrange(country, year) %>%
  saveRDS("transit2.rds")

# Merge data with previous country-year data
transit <- readR
