# Set working directory
setwd("C:\\Users\\shook\\Desktop\\Y2S1\\data analysis r code")

# Load required libraries
library(dplyr)
library(stringr)

# Load data
data <- read.csv("retail_data.csv")
View(data)

# Clean Transaction_ID and Phone in one pipeline
clean_data <- data %>%
  mutate(
    # Clean Phone: Remove non-numeric characters
    Phone= gsub("[^0-9]", "", Phone),
    
    # Clean Country
    Country = str_squish(Country),# Remove extra spaces
    Country = str_to_title(Country), # Capitalize properly
    
    # Convert Age to numeric (in case it's stored as character)
    Age = as.numeric(Age)
    
  ) %>%
  filter(
    # Filter Transaction_ID
    !is.na(Transaction_ID),
    Transaction_ID != "",
    !Transaction_ID %in% Transaction_ID[duplicated(Transaction_ID) | duplicated(Transaction_ID, fromLast = TRUE)],
    !grepl("[a-zA-Z]", Transaction_ID),
    
    # Filter Phone
    !is.na(Phone),
    Phone != "",
    grepl("^[0-9]+$", Phone),
    nchar(Phone) >= 10,
    nchar(Phone) <= 12,
    
    # Filter City: Remove NA or empty string
    !is.na(City),
    City != "",
    
    # Filter State: Remove NA or empty string
    !is.na(State),
    State != "",
    
    # Filter Zipcode: Remove non-numeric values and check length
    !is.na(Zipcode),
    Zipcode != "",
    
    # Filter Country: Remove non-numeric values and check length
    !is.na(Country),
    Country != "",
    
    # Filter out NA, negative, zero, or implausibly high ages
    !is.na(Age), 
    Age > 0, 
    Age <= 100,
    Age != "",
    
    # Clean Gender column
    !is.na(Gender),
    Gender != "",
    
    # Clean Address
    !is.na(Address),
    Address != "",
    
    # Clean Income
    !is.na(Income),
    Income != ""
    
  ) %>%
  arrange(Transaction_ID)

# View cleaned data
View(clean_data)

# Optional checks
cat("Duplicated Transaction_IDs: ", any(duplicated(clean_data$Transaction_ID)), "\n")
cat("NA in Phone_clean: ", sum(is.na(clean_data$Phone)), "\n")
cat("Empty Phone_clean: ", sum(clean_data$Phone == ""), "\n")
cat("NA in City: ", sum(is.na(clean_data$City)), "\n")
cat("Empty City: ", sum(clean_data$City == ""), "\n")
cat("NA in State: ", sum(is.na(clean_data$State)), "\n")
cat("Empty State: ", sum(clean_data$State == ""), "\n")
cat("NA in Zipcode: ", sum(is.na(clean_data$Zipcode_clean)), "\n")
cat("Empty Zipcode: ", sum(clean_data$Zipcode_clean == ""), "\n")
cat("NA values in Country: ", sum(is.na(clean_data$Country)), "\n")
cat("Empty strings in Country: ", sum(clean_data$Country == ""), "\n")
cat("Unwanted characters in Country: ", 
    any(grepl("[^A-Za-z\\s]", clean_data$Country)), "\n")
cat("Improper capitalization in Country: ", 
    any(clean_data$Country != str_to_title(clean_data$Country)), "\n")
cat("NA values in Age: ", sum(is.na(clean_data$Age)), "\n")
cat("Ages <= 0: ", sum(clean_data$Age <= 0), "\n")
cat("Ages > 120: ", sum(clean_data$Age > 120), "\n")
cat("The largest age is:", max(data$Age, na.rm = TRUE), "\n")
cat("The largest age is:", max(clean_data$Age, na.rm = TRUE), "\n")
cat("Number of empty strings in Gender:", sum(clean_data$Gender == ""), "\n")
cat("Number of NA values in Gender:", sum(is.na(clean_data$Gender)), "\n")
cat("Gender value counts:\n")
print(table(clean_data$Gender, useNA = "ifany"))
cat("Unique Gender values:\n")
print(unique(clean_data$Gender))
cat("Number of NA values in Income:", sum(is.na(data$Income)), "\n")
cat("Empty Income: ", sum(data$Income == ""), "\n")
cat("Number of NA values in Income:", sum(is.na(clean_data$Income)), "\n")
cat("Empty Income: ", sum(clean_data$Income == ""), "\n")

redundant_rows <- data[data$Transaction_ID %in% data$Transaction_ID[duplicated(data$Transaction_ID) | duplicated(data$Transaction_ID, fromLast = TRUE)], ]
cat("Number of redundant rows (including first occurrence):", nrow(redundant_rows), "\n")

# Start with full dataset
starting_data <- data
removed_by_column <- list()

# 1. Transaction_ID
after_txn <- starting_data %>%
  filter(
    !is.na(Transaction_ID),
    Transaction_ID != "",
    !Transaction_ID %in% Transaction_ID[duplicated(Transaction_ID) | duplicated(Transaction_ID, fromLast = TRUE)],
    !grepl("[a-zA-Z]", Transaction_ID)
  )
removed_by_column$Transaction_ID <- nrow(starting_data) - nrow(after_txn)

# 2. Phone
after_phone <- after_txn %>%
  mutate(Phone_clean = gsub("[^0-9]", "", Phone)) %>%
  filter(
    !is.na(Phone_clean),
    Phone_clean != "",
    grepl("^[0-9]+$", Phone_clean),
    nchar(Phone_clean) >= 10,
    nchar(Phone_clean) <= 12
  )
removed_by_column$Phone <- nrow(after_txn) - nrow(after_phone)

# 3. City
after_city <- after_phone %>%
  filter(!is.na(City), City != "")
removed_by_column$City <- nrow(after_phone) - nrow(after_city)

# 4. State
after_state <- after_city %>%
  filter(!is.na(State), State != "")
removed_by_column$State <- nrow(after_city) - nrow(after_state)

# 5. Zipcode
after_zip <- after_state %>%
  filter(!is.na(Zipcode), Zipcode != "")
removed_by_column$Zipcode <- nrow(after_state) - nrow(after_zip)

# 6. Country
after_country <- after_zip %>%
  mutate(Country = str_squish(str_to_title(Country))) %>%
  filter(!is.na(Country), Country != "")
removed_by_column$Country <- nrow(after_zip) - nrow(after_country)

# 7. Age
after_age <- after_country %>%
  mutate(Age = as.numeric(Age)) %>%
  filter(!is.na(Age), Age > 0, Age <= 100, Age != "")
removed_by_column$Age <- nrow(after_country) - nrow(after_age)

# 8. Gender
after_gender <- after_age %>%
  filter(!is.na(Gender), Gender != "")
removed_by_column$Gender <- nrow(after_age) - nrow(after_gender)

# 9. Address
after_address <- after_gender %>%
  filter(!is.na(Address), Address != "")
removed_by_column$Address <- nrow(after_gender) - nrow(after_address)

# 10. Income
after_income <- after_address %>%
  filter(!is.na(Income), Income != "")
removed_by_column$Income <- nrow(after_address) - nrow(after_income)

# Show the final cleaned dataset
cleaned_data_final <- after_income
View(cleaned_data_final)

