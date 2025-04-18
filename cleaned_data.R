# Set working directory
setwd("C:\\Users\\shook\\Desktop\\Y2S1\\data analysis r code")

# Load required libraries
library(dplyr)
library(stringr)

data[data==""]<-NA

# Load data
data <- read.csv("retail_data.csv")
View(data)

# Clean Transaction_ID and Phone in one pipeline
data_clean <- data %>%
  mutate(
    # Clean Phone: Remove non-numeric characters
    Phone= gsub("[^0-9]", "", Phone),
    
    # Clean Country
    Country = str_squish(Country),# Remove extra spaces
    Country = str_to_title(Country), # Capitalize properly
    
    
    # Convert Age to numeric (in case it's stored as character)
    Age = as.numeric(Age),
    
    # Clean and standardize format to dd/mm/yyyy (without validation)
    Date_clean = Date %>%
      str_trim() %>%               # Remove whitespace
      str_replace_all("[-/]", "/") %>%  # Replace all separators with /
      str_replace_all("/+", "/") %>%    # Remove duplicate slashes
      str_replace("^(\\d{1})/(\\d{1})/(\\d{4})$", "0\\1/0\\2/\\3") %>%  # Add leading zeros
      str_replace("^(\\d{2})/(\\d{1})/(\\d{4})$", "\\1/0\\2/\\3") %>%   # for day/month
      str_replace("^(\\d{1})/(\\d{2})/(\\d{4})$", "0\\1/\\2/\\3") %>%
      str_replace("^(\\d)/(\\d)/(\\d{4})$", "0\\1/0\\2/\\3") %>%        # Catch single digits
      str_extract("^\\d{2}/\\d{2}/\\d{4}$"),# Keep only properly formatted
    
    Time_clean = str_replace_all(Time, "[^0-9:]", "") %>%
      str_replace("^([0-9]{2})([0-9]{2})([0-9]{2})$", "\\1:\\2:\\3") %>%
      str_replace("^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})$", 
                  sprintf("%02d:%02d:%02d", 
                          as.numeric(str_extract(., "^([0-9]{1,2})")),
                          as.numeric(str_extract(., ":([0-9]{1,2}):") %>% str_remove_all(":")),
                          as.numeric(str_extract(., "([0-9]{1,2})$")))),
    Time_clean = if_else(str_detect(Time_clean, "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"), 
                         Time_clean, 
                         NA_character_),
    
    across(c(Total_Purchases, Amount, Total_Amount), 
           ~ str_replace_all(., "[^0-9.]", "")),
    
    Total_Purchases_clean = as.integer(Total_Purchases),
    Amount_clean = as.numeric(Amount) %>% round(2),
    Total_Amount_clean = as.numeric(Total_Amount) %>% round(2),
    
    # Validate ranges
    Total_Purchases_clean = if_else(between(Total_Purchases_clean, 1, 1000), 
                                    Total_Purchases_clean, NA_integer_),
    Amount_clean = if_else(between(Amount_clean, 1, 9999), Amount_clean, NA_real_),
    
    # Ensure Total_Amount = Amount * Total_Purchases
    Total_Amount_clean = if_else(
      near(Total_Amount_clean, Amount_clean * Total_Purchases_clean, tol = 0.01),
      Total_Amount_clean,
      Amount_clean * Total_Purchases_clean
    ),
    Total_Amount_clean = if_else(between(Total_Amount_clean, 1, 1000000), 
                                 Total_Amount_clean, NA_real_)
    
  ) %>%
  filter(
    !Transaction_ID %in% Transaction_ID[duplicated(Transaction_ID) | duplicated(Transaction_ID, fromLast = TRUE)],
    grepl("^[0-9]+$", Transaction_ID),

    grepl("^[0-9]+$", Phone),
    grepl("^\\d{10,12}$", Phone),

    # Age range validation
    between(Age, 1, 100)
  ) %>%
  arrange(Transaction_ID)

data_clean <- na.omit(data)

# View cleaned data 
View(data_clean)

