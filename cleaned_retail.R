# Set working directory
setwd("C:\\Users\\shook\\Desktop\\Y2S1\\data analysis r code")

# Load required libraries
library(dplyr)
library(stringr)

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
    
    Product_Category_clean = str_to_title(str_trim(Product_Category)),
    Product_Brand_clean = str_trim(Product_Brand),
    Feedback_clean = str_to_title(str_trim(Feedback)),
    Shipping_Method_clean = str_to_title(str_trim(Shipping_Method)),
    Payment_Method_clean = str_trim(Payment_Method),
    Order_Status_clean = str_trim(Order_Status),
    
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
                                 Total_Amount_clean, NA_real_),
    
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
    # Clean and standardize format to dd/mm/yyyy (without validation)
    Date_clean = Date %>%
      str_trim() %>%               # Remove whitespace
      str_replace_all("[-/]", "/") %>%  # Replace all separators with /
      str_replace_all("/+", "/") %>%    # Remove duplicate slashes
      str_replace("^(\\d{1})/(\\d{1})/(\\d{4})$", "0\\1/0\\2/\\3") %>%  # Add leading zeros
      str_replace("^(\\d{2})/(\\d{1})/(\\d{4})$", "\\1/0\\2/\\3") %>%   # for day/month
      str_replace("^(\\d{1})/(\\d{2})/(\\d{4})$", "0\\1/\\2/\\3") %>%
      str_replace("^(\\d)/(\\d)/(\\d{4})$", "0\\1/0\\2/\\3") %>%        # Catch single digits
      str_extract("^\\d{2}/\\d{2}/\\d{4}$")  # Keep only properly formatted
    
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
    Income != "",
    
    !is.na(Customer_ID) & Customer_ID != "",
    !is.na(Product_Category) & Product_Category != "",
    !is.na(Product_Brand) & Product_Brand != "",
    Feedback != "",
    Shipping_Method != "",
    Payment_Method != "",
    Order_Status != "",
    
    !is.na(Total_Purchases_clean),
    !is.na(Amount_clean),
    !is.na(Total_Amount_clean),
    !is.na(Time_clean)
    
  ) %>%
  arrange(Transaction_ID)

# View cleaned data
View(data_clean)


