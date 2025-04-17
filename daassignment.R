
data = read.csv("C:\\Users\\Anwen\\Desktop\\data analysis assignment\\retail_data.csv")


library(dplyr)
library(stringr)

#Customer_ID

duplicate_customers <- data %>%
  group_by(Customer_ID) %>%
  summarise(
    name_count = n_distinct(Name),
    email_count = n_distinct(Email),
    phone_count = n_distinct(Phone)
  ) %>%
  filter(name_count > 1 | email_count > 1 | phone_count > 1)

# View Customer_IDs that are linked to more than one customer
print(duplicate_customers)


#  1: Remove missing or blank Customer_ID
Customer_ID_clean <- data %>%
  filter(!is.na(Customer_ID) & Customer_ID != "")


View(Customer_ID_clean)
sum(is.na(Customer_ID_clean$Customer_ID))

# 2: Remove exact duplicate Customer_ID rows 
Customer_ID_clean <- data %>%
  distinct(Customer_ID, .keep_all = TRUE)


#Product_Category
Customer_ID_clean <- Customer_ID_clean %>%
  mutate(Product_Category_clean = Product_Category)

# Step 1: Remove rows where Product_Category is blank
Customer_ID_clean <- Customer_ID_clean %>%
  filter(Product_Category != "")


library(stringr)
# 2: Standardize text
# Trim whitespace and convert to Title Case (e.g., "electronics" -> "Electronics")
Customer_ID_clean <- Customer_ID_clean %>%
  mutate(Product_Category_clean = str_trim(Product_Category_clean),
         Product_Category_clean = str_to_title(Product_Category_clean))


#Product_Brand
# Create 'Product_Brand_clean' by copying from 'Product_Brand'
Customer_ID_clean <- Customer_ID_clean %>%
  mutate(Product_Brand_clean = Product_Brand)

#1. remove the blank rows
Customer_ID_clean <- Customer_ID_clean %>%
  filter(!is.na(Product_Brand_clean) & Product_Brand_clean != "")

#feedback
# 1. Clean the Feedback column
Customer_ID_clean <- Customer_ID_clean %>%
  mutate(Feedback_clean = ifelse(Feedback == "", NA, Feedback)) %>%
  filter(!is.na(Feedback_clean)) %>%
  mutate(Feedback_clean = str_trim(Feedback_clean),   # Trim whitespace
         Feedback_clean = str_to_title(Feedback_clean)) # Title case

#Shipping_Method
# 1. Clean Shipping_Method by removing blank values and trimming any spaces
Customer_ID_clean <- Customer_ID_clean %>%
  filter(Shipping_Method != "") %>%  # Remove rows with blank Shipping_Method
  mutate(Shipping_Method_clean = str_trim(Shipping_Method),  # Trim leading/trailing spaces
         Shipping_Method_clean = str_to_title(Shipping_Method_clean))  # Convert to Title Case

#Payment_Method
#1. Remove rows with Blank
Customer_ID_clean <- Customer_ID_clean %>%
  filter(Payment_Method != "") %>%
  mutate(Payment_Method_clean = Payment_Method)

#Order_Status
#1. Remove rows with Blank
Customer_ID_clean <- Customer_ID_clean %>%
  filter(Order_Status != "") %>%
  mutate(Order_Status_clean = Order_Status)

#Ratings #no issue

#products #no issue
