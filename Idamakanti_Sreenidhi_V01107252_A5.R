# Set the working directory and verify it
setwd('D:\\CHRIST\\Boot camp\\DATA')
getwd()

# Function to install and load libraries
install_and_load <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

# Load required libraries
libraries <- c("dplyr", "readr", "sf", "readxl", "tidyr", "ggplot2", "BSDA","glue")
lapply(libraries, install_and_load)


# Reading the file into R
data <- read.csv("NSSO68.csv")

# Filtering for JRKD
df <- data %>%
  filter(state_1 == "JRKD")

# Display dataset info
cat("Dataset Information:\n")
print(names(df))
print(head(df))
print(dim(df))

# Finding missing values
missing_info <- colSums(is.na(df))
cat("Missing Values Information:\n")
print(missing_info)

# Sub-setting the data
jrkdnew <- df %>%
  select(state_1, District, Region, Sector, State_Region, Meals_Employer, Meals_Payment, Meals_At_Home, ricepds_v, Wheatpds_q, chicken_q, pulsep_q, wheatos_q, No_of_Meals_per_day)

# Check for missing values in the subset
cat("Missing Values in Subset:\n")
print(colSums(is.na(jrkdnew)))

# Impute missing values with mean for specific columns
impute_with_mean <- function(column) {
  if (any(is.na(column))) {
    column[is.na(column)] <- mean(column, na.rm = TRUE)
  }
  return(column)
}
jrkdnew$Meals_At_Home <- impute_with_mean(jrkdnew$Meals_At_Home)
jrkdnew$Meals_Employer <- impute_with_mean(jrkdnew$Meals_Employer)
jrkdnew$Meals_Payment <- impute_with_mean(jrkdnew$Meals_Payment)

# Check for missing values after imputation
cat("Missing Values After Imputation:\n")
print(colSums(is.na(jrkdnew)))

# Finding outliers and removing them
remove_outliers <- function(df, column_name) {
  Q1 <- quantile(df[[column_name]], 0.25)
  Q3 <- quantile(df[[column_name]], 0.75)
  IQR <- Q3 - Q1
  lower_threshold <- Q1 - (1.5 * IQR)
  upper_threshold <- Q3 + (1.5 * IQR)
  df <- subset(df, df[[column_name]] >= lower_threshold & df[[column_name]] <= upper_threshold)
  return(df)
}

outlier_columns <- c("ricepds_v", "chicken_q")
for (col in outlier_columns) {
  jrkdnew <- remove_outliers(jrkdnew, col)
}

# Summarize consumption
jrkdnew$total_consumption <- rowSums(jrkdnew[, c("ricepds_v", "Wheatpds_q", "chicken_q", "pulsep_q", "wheatos_q")], na.rm = TRUE)

# Summarize and display top and bottom consuming districts and regions
summarize_consumption <- function(group_col) {
  summary <- jrkdnew %>%
    group_by(across(all_of(group_col))) %>%
    summarise(total = sum(total_consumption)) %>%
    arrange(desc(total))
  return(summary)
}

district_summary <- summarize_consumption("District")
region_summary <- summarize_consumption("Region")

cat("Top 3 Consuming Districts:\n")
print(head(district_summary, 3))
cat("Bottom 3 Consuming Districts:\n")
print(tail(district_summary, 3))

cat("Region Consumption Summary:\n")
print(region_summary)

# Rename districts and sectors , get codes from appendix of NSSO 68th Round Data
district_mapping <- c("1" = "Garhwa", "2" = "Palamu", "3" = "Chatra", "4" = "Hazaribagh", "5" = "Kodarma", "6" = "Giridih", "7" = "Dcoghar", "8" = "Godda", "9" = "Sahibganj", "10" = "Pakaur", "11" = "Dumka", "12" = "Dhanbad", "13" = "Bokaro", "14" = "Ranchi", "15" = "Lohardaga", "16" = "Gumla", "17" = "Pashchimi Singhbhum", "18" = "Parbi Singhbhum", "19" = "Latchar", "20" = "Simdega", "21" = "Jamtara", "22" = "Seraikela-kharsawan")
sector_mapping <- c("2" = "URBAN", "1" = "RURAL")

jrkdnew$District <- as.character(jrkdnew$District)
jrkdnew$Sector <- as.character(jrkdnew$Sector)
jrkdnew$District <- ifelse(jrkdnew$District %in% names(district_mapping), district_mapping[jrkdnew$District], jrkdnew$District)
jrkdnew$Sector <- ifelse(jrkdnew$Sector %in% names(sector_mapping), sector_mapping[jrkdnew$Sector], jrkdnew$Sector)

View(jrkdnew)

hist(jrkdnew$total_consumption, breaks = 10, col = 'LightPink', border = 'DarkBlue', 
     xlab = "Consumption", ylab = "Frequency", main = "Consumption Distribution in Jharkhand State")

JRKD_consumption <- aggregate(total_consumption ~ District, data = jrkdnew, sum) 
View(JRKD_consumption)
??barplot
barplot(JRKD_consumption$total_consumption, 
        names.arg = JRKD_consumption$District, 
        las = 2, # Makes the district names vertical
        col = 'Green', 
        border = 'Blue', 
        xlab = "District", 
        ylab = "Total Consumption", 
        main = "Total Consumption per District",
        cex.names = 0.7) # Adjust the size of district names if needed


# b) Plot {'any variable of your choice'} on the JHARKHAND state map using NSSO68.csv data

library(ggplot2) 
library(sf) # mapping
library(dplyr) 
Sys.setenv("SHAPE_RESTORE_SHX" = "YES") 

data_map <- st_read("D:\\CHRIST\\Boot camp\\DATA\\JHARKHAND_DISTRICTS.geojson") 
View(data_map)

data_map <- data_map %>% 
  rename(District = dtname) 
colnames(data_map) 
data_map_data <- merge(JRKD_consumption,data_map,by = "District") 
View(data_map_data)
ggplot(data_map_data) + 
  geom_sf(aes(fill =total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "Yellow", high = "Red") + 
  ggtitle("Total Consumption_by_District") 
ggplot(data_map_data) + 
  geom_sf(aes(fill = total_consumption, geometry = geometry)) + 
  scale_fill_gradient(low = "Yellow", high = "Red") + 
  ggtitle("Total Consumption by District") +
  geom_sf_text(aes(label = District, geometry = geometry), size = 3, color = "Black")