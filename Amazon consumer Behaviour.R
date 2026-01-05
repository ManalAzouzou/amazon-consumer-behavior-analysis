
# %%
# Load required packages for data manipulation and visualization
library(tidyverse)

# %%
# Set working directory (adjust path if necessary)
setwd("/Users/mac/Downloads/Data_Analysis")
# Import the dataset
data = read_csv("Amazon Customer Behavior Survey.csv")

head(data,10) # View dataset

#%%
# View variables names
names(data)

#%%
# Clean the dataset
# Remove variables not used in the analysis and delete inconsistencies within age
data = data%>%
  filter(age >= 12)%>%
  select(!Timestamp & !Personalized_Recommendation_Frequency...6 & !Add_to_Cart_Browsing & !Review_Helpfulness & !Recommendation_Helpfulness & !Improvement_Areas & !Customer_Reviews_Importance & !Cart_Completion_Frequency & !Personalized_Recommendation_Frequency...18 & !Search_Result_Exploration & !Product_Search_Method & !Cart_Abandonment_Factors & !Saveforlater_Frequency & !Rating_Accuracy)
#%%
# Find placeholders
placeholders = c("", ".", ",", "NA", "N/A", "None", "NULL")

placeholder_counts = sapply(data, \(x) sum(x %in% placeholders))
placeholder_counts

# Replace placeholders to NA 
data = data%>%
  mutate(across(
    where(is.character),
    ~ replace(., . %in% placeholders, NA)
  ))
#%%
# Split multi-select categorie "purchase_categories" into 5 ones
data %>%
  select("Purchase_Categories") %>%
  head(10) # View values 

data = data %>%
  mutate(Beauty_cat = as.numeric(grepl("Beauty and Personal Care", Purchase_Categories)),
         Clothing_cat = as.numeric(grepl( "Clothing and Fashion", Purchase_Categories)),
         Home_cat = as.numeric(grepl("Home and Kitchen", Purchase_Categories)),
         Groceries_cat = as.numeric(grepl("Groceries and Gourmet Food", Purchase_Categories)),
         Other_cat = as.numeric(grepl( "others", Purchase_Categories))
        )

#%%
# Convert numeric variables to integers
data = data%>%
  mutate(across(where(is.numeric), as.integer))

#%%
# Encode ordinal variables correctly  
data = data %>%
  mutate(
    Purchase_Frequency = factor(
    Purchase_Frequency, levels = c(
        "Less than once a month",
        "Once a month",
        "Few times a month",
        "Once a week",
        "Multiple times a week"),
      ordered = TRUE
    ),
  Browsing_Frequency = factor(
    Browsing_Frequency,
    levels = c(
      "Rarely",
      "Few times a month",
      "Few times a week",
      "Multiple times a day"),
      ordered = TRUE
    ),
   Shopping_Satisfaction = factor(
      Shopping_Satisfaction,
      levels = c(1, 2, 3, 4, 5),
      ordered = TRUE
    ),
    Review_Reliability = factor(
      Review_Reliability, 
      levels = c(
        "Never",
        "Rarely",
        "Occasionally",
        "Moderately",
        "Heavily"),
        ordered = TRUE
    )
  )   
#%%
# Question 1 - What is the demographic composition (age, gender) of Amazon customers in the dataset? 
# View gender composition
data %>%
  group_by(Gender)%>%
  summarise(gender_composition = n())

# View age & gender composition 
data %>%
  group_by(Gender, age)%>%
  summarise(age_composition = n())

# Create age groups first
data = data%>%
  mutate(age_group = cut(
    age, 
    breaks = c(12, 18, 25, 35, 45, 55, Inf),
    right = FALSE,
    labels = c(
      "12,17",
      "18,24",
      "25,34",
      "35,44",
      "45,54",
      "55+"
    )))

# Create composition tibble 
age_gender_comp = data %>%
  count(Gender, age_group)

# Display grouped bar chart of the composition 
ggplot(age_gender_comp, aes(x=age_group, y=n, fill=Gender))+
  geom_col(position = "dodge" )+
  scale_fill_manual(
  values = c(
    "Female" = "#1f78b4",        
    "Male" = "#ff7f00",          
    "Others" = "#a6cee3",        
    "Prefer not to say" = "#fdbf6f" 
  ))+ 
  theme_bw()+
  labs(title = "Age group distribution by gender",
        x = "Age group",
      y = "Number of respondents")

#%%
# Question 2 - How do purchasing frequency and product category preferences vary by age and gender? 
# Purchasing frequency by age 
data %>% # Heatmap of Purchasing frequency by age groups
  count(age_group, Purchase_Frequency) %>%
  group_by(age_group) %>%
  mutate(percent = n / sum(n)) %>%
  ggplot(aes(x = age_group, y = Purchase_Frequency, fill = percent)) +
  geom_tile() +
  scale_fill_gradient(
  low = "#deebf7",
  high = "#08519c",
  name = "Share"
)+ 
  theme_bw()+
  labs(
    title = "Purchasing Frequency by Age Group",
    x = "Age Group",
    y = "Purchasing Frequency",
    fill = "Share"
  )
# Purchasing frequency by gender
pf_gender = data%>%
  count( Gender, Purchase_Frequency)
# Stacked Bar chart of Purchasing frequency by gender
ggplot(pf_gender, aes(x = Gender,y=n, fill = Purchase_Frequency)) +
  geom_col(position = "fill") +
  scale_fill_manual(values = c(
    "Less than once a month" = "#deebf7",
    "Once a month"           = "#c6dbef",
    "Few times a month"      = "#9ecae1",
    "Once a week"            = "#6baed6",
    "Multiple times a week"  = "#2171b5"
  )) +
  labs(
    title = "Purchasing frequency by age group",
    x = "gender",
    y = "Proportion"
  )
# Purchasing categories by gender
# Create a tibble of purchase categories (percentage)
category_gender_tabel = data%>%
  group_by(Gender)%>%
  summarise(across(ends_with("_cat"),~ mean(.) * 100))
# View the tibble
head(category_gender_tabel)

#%%
# Question 3 - Is there a relationship between browsing frequency and purchasing frequency for female and male respondents? 
data%>%
  filter(Gender == "Female" | Gender =="Male")%>%
  group_by(Gender)%>%
  summarise(corr= cor(
    as.numeric(Purchase_Frequency),
    as.numeric(Browsing_Frequency),
))

#%%
# Question 4 - Do age and gender influence review behavior (leaving reviews and reliance on reviews)? 
# Create binary variable: 1 = review left, 0 = no review left
data = data %>% 
  mutate(Review_Left_bin = as.integer(Review_Left == "Yes"))
# Display average of leaving reaviews by gender 
data%>% 
  group_by(Gender) %>%
  summarise(
    Review_Left_pct = mean(Review_Left_bin, na.rm = TRUE) * 100
  )
# Convert ordinal review reliability to numeric values
data = data %>%
  mutate(Review_Reliability_num = as.numeric(Review_Reliability))
# Display average of reliance on reviews by gender 
data%>% 
  filter(Gender == "Female" | Gender =="Male")%>%
  group_by(Gender) %>%
  summarise(
    reliance_average = mean(Review_Reliability_num, na.rm = TRUE)
  )
# Average reliance and leaving review by age group
data %>%
  group_by(age_group)%>%
  summarise(
    review_reliance_average = mean(Review_Reliability_num, na.rm = TRUE),
    review_left_pct = mean(Review_Left_bin, na.rm = TRUE) * 100,
    .groups = "drop"
  )

#%%
# Question 5 - Does customer satisfaction and service appreciation affect purchasing frequency?
# Create a numeric purchase frequency outcome
data = data %>%
  mutate(Purchase_Frequency_num = as.numeric(Purchase_Frequency))
# Linear regression to assess the association between satisfaction, service appreciation, and purchasing frequency
regression = lm( Purchase_Frequency_num ~ Shopping_Satisfaction + Service_Appreciation, data = data ) 
summary(regression)