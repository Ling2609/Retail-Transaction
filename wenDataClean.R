data = read.csv("C:\\Users\\Anwen\\Desktop\\data analysis assignment\\retail_data.csv")
View(data)

library(dplyr)
library(stringr)

data_clean <- data %>%
  # Filter rows where required fields are not missing or blank
  filter(
    !is.na(Customer_ID) & Customer_ID != "",
    !is.na(Product_Category) & Product_Category != "",
    !is.na(Product_Brand) & Product_Brand != "",
    Feedback != "",
    Shipping_Method != "",
    Payment_Method != "",
    Order_Status != ""
  ) %>%
  
  # Clean and standardize text columns
  mutate(
    Product_Category_clean = str_to_title(str_trim(Product_Category)),
    Product_Brand_clean = str_trim(Product_Brand),
    Feedback_clean = str_to_title(str_trim(Feedback)),
    Shipping_Method_clean = str_to_title(str_trim(Shipping_Method)),
    Payment_Method_clean = str_trim(Payment_Method),
    Order_Status_clean = str_trim(Order_Status)
  )
View(data_clean)
