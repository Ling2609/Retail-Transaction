data = read.csv("C:\\Users\\User\\Desktop\\Y2S1\\Programming for DA\\DA work\\retail_data.csv")
View(data)

library(dplyr)    # For %>% and data manipulation
library(stringr)   # For string operations

# Define a function to check and report NA counts
report_na <- function(df, message) {
  na_counts <- sapply(df, function(x) sum(is.na(x)))
  cat(message, "\n")
  print(na_counts[na_counts > 0])
  invisible(df)
}

# Initial report
data %>% report_na("Initial NA counts:")


data_clean <- data %>%
  mutate(
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


  # Clean Time
  mutate(
    Time_clean = str_replace_all(Time, "[^0-9:]", "") %>%
      str_replace("^([0-9]{2})([0-9]{2})([0-9]{2})$", "\\1:\\2:\\3") %>%
      str_replace("^([0-9]{1,2}):([0-9]{1,2}):([0-9]{1,2})$", 
                  sprintf("%02d:%02d:%02d", 
                          as.numeric(str_extract(., "^([0-9]{1,2})")),
                          as.numeric(str_extract(., ":([0-9]{1,2}):") %>% str_remove_all(":")),
                          as.numeric(str_extract(., "([0-9]{1,2})$")))),
    Time_clean = if_else(str_detect(Time_clean, "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$"), 
                         Time_clean, 
                         NA_character_)
  ) %>%
  
  # Remove rows with invalid times
  filter(!is.na(Time_clean)) %>%
  
  # Clean numeric fields in one step
  mutate(
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
  
  # Final filtering of invalid numeric values
  filter(
    !is.na(Total_Purchases_clean),
    !is.na(Amount_clean),
    !is.na(Total_Amount_clean)
  )

# Final NA report
data_clean %>% report_na("Final NA counts after cleaning:")

# Keep both original and cleaned columns for comparison
# If you want to keep only cleaned columns, add:
# data_clean <- data_clean %>% select(-c(Date, Year, Month, Time, Total_Purchases, Amount, Total_Amount))

# Save cleaned data
write.csv(data_clean, "retail_data_cleaned.csv", row.names = FALSE)

## 1. Basic Structure Check
cat("\n=== Dataset Structure ===\n")
glimpse(data_clean)

cat("\nNumber of rows after cleaning:", nrow(data_clean))
cat("\nNumber of columns after cleaning:", ncol(data_clean))

## 2. NA Value Check
cat("\n\n=== NA Value Check ===\n")
na_summary <- sapply(data_clean, function(x) sum(is.na(x)))
print(na_summary[na_summary > 0])

if(sum(na_summary) == 0) {
  cat("\nSUCCESS: No NA values found in cleaned data_clean\n")
} else {
  cat("\nWARNING: NA values still present in these columns:\n")
  print(na_summary[na_summary > 0])
}


## 4. Time Format Validation
cat("\n\n=== Time Format Check ===\n")
time_check <- data_clean %>%
  mutate(
    is_valid_time = str_detect(Time_clean, "^([01][0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]$")
  )

cat("\nRows with invalid time format:",
    sum(!time_check$is_valid_time))


## 8. Visual Checks
cat("\n\n=== Visual Checks ===\n")

# Date distribution
ggplot(data_clean, aes(x = dmy(Date_clean))) +
  geom_histogram(bins = 30, fill = "steelblue") +
  labs(title = "Distribution of Dates", x = "Date", y = "Count") +
  theme_minimal()

# Time distribution
data_clean %>%
  mutate(hour = as.numeric(str_extract(Time_clean, "^\\d{2}"))) %>%
  ggplot(aes(x = hour)) +
  geom_bar(fill = "darkorange") +
  labs(title = "Distribution by Hour of Day", x = "Hour", y = "Count") +
  scale_x_continuous(breaks = 0:23) +
  theme_minimal()

# Numeric fields distribution
data_clean %>%
  select(ends_with("_clean") & where(is.numeric)) %>%
  pivot_longer(everything()) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "forestgreen") +
  facet_wrap(~name, scales = "free") +
  labs(title = "Distribution of Numeric Fields") +
  theme_minimal()

