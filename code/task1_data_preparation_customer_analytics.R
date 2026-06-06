install.packages("tidyverse")
install.packages("readxl")
install.packages("lubridate")
install.packages("janitor")

library(tidyverse)
library(readxl)
library(lubridate)
library(janitor)

customer_data <- read_csv("C:/Users/hites/OneDrive/Desktop/Hitesh/Portfolio/Additional Skills/Data Analytics Job Simulation - Quantium/data/QVI_purchase_behaviour.csv")
view(customer_data)
transaction_data <- read_excel("C:/Users/hites/OneDrive/Desktop/Hitesh/Portfolio/Additional Skills/Data Analytics Job Simulation - Quantium/data/QVI_transaction_data.xlsx")
View(transaction_data)
glimpse(transaction_data)
glimpse(customer_data)

head(transaction_data)
head(customer_data)

transaction_data <- clean_names(transaction_data)
customer_data <- clean_names(customer_data)

glimpse(transaction_data)
glimpse(customer_data)

colSums(is.na(transaction_data))
colSums(is.na(customer_data))

sum(duplicated(transaction_data))
sum(duplicated(customer_data))

transaction_data[duplicated(transaction_data), ]

transaction_data <- transaction_data %>%
  distinct()

sum(duplicated(transaction_data))
# Data Quality Findings
# One fully duplicated transaction record was identified.
# The duplicate was removed to avoid overstating sales,
# transaction counts and customer purchasing metrics.

colSums(is.na(transaction_data))
colSums(is.na(customer_data))
# Missing Value Check
# No missing values were identified in either the transaction
# dataset or customer dataset.
# No imputation or data removal was required.

summary(transaction_data$date)
head(transaction_data$date)

transaction_data %>%
  count(prod_qty) %>%
  arrange(desc(prod_qty))

transaction_data %>%
  count(prod_name, sort = TRUE)

transaction_data <- transaction_data %>%
  mutate(date = as.Date(date, origin = "1899-12-30"))

head(transaction_data$date)
summary(transaction_data$date)
# Date Format Check
# Transaction dates were stored as Excel serial values.
# Dates were converted to standard Date format for analysis.

transaction_data %>%
  filter(prod_qty == 200)
# Outlier Investigation
# Two transactions were identified with a purchase quantity of 200 packs.
# Both transactions belonged to the same loyalty customer and represented
# purchases substantially larger than the rest of the dataset.
# As the objective is to understand typical customer purchasing behaviour,
# these transactions were considered outliers and removed from analysis.
transaction_data <- transaction_data %>%
  filter(prod_qty < 200)
max(transaction_data$prod_qty)

transaction_data %>%
  filter(str_detect(str_to_lower(prod_name), "salsa"))

nrow(
  transaction_data %>%
    filter(str_detect(str_to_lower(prod_name), "salsa"))
)

transaction_data %>%
  filter(str_detect(str_to_lower(prod_name), "salsa")) %>%
  count(prod_name, sort = TRUE)

transaction_data <- transaction_data %>%
  filter(!str_detect(str_to_lower(prod_name), "salsa"))
nrow(transaction_data)
# Category Validation
# 18,094 transactions were identified as salsa products.
# As the analysis focuses specifically on the chip category,
# salsa products were excluded from further analysis.

transaction_data <- transaction_data %>%
  mutate(
    pack_size = str_extract(prod_name, "\\d+[gG]"),
    pack_size = str_remove(pack_size, "[gG]"),
    pack_size = as.numeric(pack_size)
  )

summary(transaction_data$pack_size)

transaction_data %>%
  count(pack_size, sort = TRUE)

transaction_data <- transaction_data %>%
  mutate(
    brand = word(prod_name, 1)
  )

transaction_data %>%
  count(brand, sort = TRUE)

summary(transaction_data$pack_size)

transaction_data %>%
  count(pack_size, sort = TRUE)

transaction_data %>%
  count(brand, sort = TRUE)


transaction_data %>%
  filter(is.na(pack_size)) %>%
  distinct(prod_name)

sum(is.na(transaction_data$pack_size))
summary(transaction_data$pack_size)
transaction_data %>%
  count(pack_size, sort = TRUE)
# Feature Engineering Check
# Initial pack size extraction produced 6,064 missing values.
# Investigation showed several product descriptions used an uppercase "G"
# rather than lowercase "g".
# The extraction logic was updated to capture both formats,
# eliminating all missing pack size values.

transaction_data %>%
  count(brand, sort = TRUE) %>%
  print(n = 50)

transaction_data %>%
  filter(brand %in% c(
    "Red","RRD",
    "Dorito","Doritos",
    "Smith","Smiths",
    "Grain","GrnWves",
    "Infzns","Infuzions",
    "Snbts","Sunbites",
    "NCC","Natural",
    "WW","Woolworths"
  )) %>%
  distinct(prod_name) %>%
  arrange(prod_name)

transaction_data <- transaction_data %>%
  mutate(
    brand = case_when(
      brand == "Red" ~ "RRD",
      brand == "Dorito" ~ "Doritos",
      brand == "Smith" ~ "Smiths",
      brand == "Grain" ~ "Grain Waves",
      brand == "GrnWves" ~ "Grain Waves",
      brand == "Infzns" ~ "Infuzions",
      brand == "Snbts" ~ "Sunbites",
      brand == "Natural" ~ "Natural Chip Co",
      brand == "NCC" ~ "Natural Chip Co",
      brand == "Woolworths" ~ "WW",
      TRUE ~ brand
    )
  )

transaction_data %>%
  count(brand, sort = TRUE)
# Brand Standardisation
# Product descriptions were used to derive brand names.
# Several inconsistencies were identified (e.g. Dorito vs Doritos,
# Smith vs Smiths, Infzns vs Infuzions).
# Brand names were standardised to ensure accurate aggregation and
# analysis of brand performance.



combined_data <- transaction_data %>%
  left_join(customer_data, by = "lylty_card_nbr")

colSums(is.na(combined_data))

combined_data %>%
  summarise(
    total_sales = sum(tot_sales),
    total_transactions = n(),
    total_customers = n_distinct(lylty_card_nbr),
    total_quantity = sum(prod_qty),
    avg_transaction_value = sum(tot_sales) / n(),
    avg_units_per_transaction = sum(prod_qty) / n()
  )


sales_by_segment <- combined_data %>%
  group_by(lifestage, premium_customer) %>%
  summarise(
    total_sales = sum(tot_sales),
    customers = n_distinct(lylty_card_nbr),
    transactions = n(),
    total_qty = sum(prod_qty),
    avg_sales_per_customer = total_sales / customers,
    avg_units_per_customer = total_qty / customers,
    avg_price_per_unit = total_sales / total_qty
  ) %>%
  arrange(desc(total_sales))

sales_by_segment


colSums(is.na(combined_data))

combined_data %>%
  summarise(
    total_sales = sum(tot_sales),
    total_transactions = n(),
    total_customers = n_distinct(lylty_card_nbr),
    total_quantity = sum(prod_qty)
  )

sales_by_segment



sales_summary <- combined_data %>%
  group_by(lifestage, premium_customer) %>%
  summarise(
    total_sales = sum(tot_sales),
    .groups = "drop"
  )

#Chart 1 — Total Sales

ggplot(
  sales_summary,
  aes(
    x = lifestage,
    y = total_sales,
    fill = premium_customer
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Total Sales by Customer Segment",
    x = "Lifestage",
    y = "Total Sales"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Chart - 2 Number of Customers 

customer_summary <- combined_data %>%
  group_by(lifestage, premium_customer) %>%
  summarise(
    customers = n_distinct(lylty_card_nbr),
    .groups = "drop"
  )

ggplot(
  customer_summary,
  aes(
    x = lifestage,
    y = customers,
    fill = premium_customer
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Number of Customers by Segment",
    x = "Lifestage",
    y = "Customers"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Chart 3 — Average Units per Customer

units_summary <- combined_data %>%
  group_by(lifestage, premium_customer) %>%
  summarise(
    customers = n_distinct(lylty_card_nbr),
    total_units = sum(prod_qty),
    avg_units_per_customer = total_units / customers,
    .groups = "drop"
  )

ggplot(
  units_summary,
  aes(
    x = lifestage,
    y = avg_units_per_customer,
    fill = premium_customer
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Units per Customer",
    x = "Lifestage",
    y = "Units per Customer"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Chart 4 — Average Price per Unit

price_summary <- combined_data %>%
  group_by(lifestage, premium_customer) %>%
  summarise(
    avg_price_per_unit = sum(tot_sales) / sum(prod_qty),
    .groups = "drop"
  )

ggplot(
  price_summary,
  aes(
    x = lifestage,
    y = avg_price_per_unit,
    fill = premium_customer
  )
) +
  geom_col(position = "dodge") +
  labs(
    title = "Average Price per Unit by Segment",
    x = "Lifestage",
    y = "Average Price"
  ) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


combined_data <- combined_data %>%
  mutate(unit_price = tot_sales / prod_qty)

mainstream <- combined_data %>%
  filter(
    premium_customer == "Mainstream",
    lifestage %in% c(
      "YOUNG SINGLES/COUPLES",
      "MIDAGE SINGLES/COUPLES"
    )
  ) %>%
  pull(unit_price)

other <- combined_data %>%
  filter(
    premium_customer %in% c("Budget", "Premium"),
    lifestage %in% c(
      "YOUNG SINGLES/COUPLES",
      "MIDAGE SINGLES/COUPLES"
    )
  ) %>%
  pull(unit_price)

t.test(mainstream, other)

# Creating Target Segment

target_segment <- combined_data %>%
  filter(
    lifestage == "YOUNG SINGLES/COUPLES",
    premium_customer == "Mainstream"
  )

# Brand Preference of Target Segment

target_brand <- target_segment %>%
  count(brand) %>%
  mutate(
    target_prop = n / sum(n)
  )

other_brand <- combined_data %>%
  filter(
    !(lifestage == "YOUNG SINGLES/COUPLES" &
        premium_customer == "Mainstream")
  ) %>%
  count(brand) %>%
  mutate(
    other_prop = n / sum(n)
  )

brand_affinity <- target_brand %>%
  left_join(other_brand, by = "brand") %>%
  mutate(
    affinity = target_prop / other_prop
  ) %>%
  arrange(desc(affinity))

brand_affinity

# Pack Size Affinity

target_pack <- target_segment %>%
  count(pack_size) %>%
  mutate(
    target_prop = n / sum(n)
  )

# Other Population Pack Preference
other_pack <- combined_data %>%
  filter(
    !(lifestage == "YOUNG SINGLES/COUPLES" &
        premium_customer == "Mainstream")
  ) %>%
  count(pack_size) %>%
  mutate(
    other_prop = n / sum(n)
  )

# Compare
pack_affinity <- target_pack %>%
  left_join(other_pack, by = "pack_size") %>%
  mutate(
    affinity = target_prop / other_prop
  ) %>%
  arrange(desc(affinity))

pack_affinity


head(brand_affinity, 10)
# Insights:
# Mainstream Young Singles/Couples show a stronger preference for
# Tyrrells, Twisties, Doritos, Tostitos, Kettle and Pringles
# compared to the rest of the customer base.
#
# These brands over-index within the target segment and represent
# potential opportunities for targeted promotions and merchandising.
head(pack_affinity, 10)
# Insights:
# Mainstream Young Singles/Couples demonstrate a stronger preference
# for larger pack sizes, particularly 270g, 330g and 380g packs.
#
# This suggests that larger sharing packs are more appealing to
# this customer segment and may present opportunities for
# category growth.



# Recommendation:
# Mainstream Young Singles/Couples should be considered the primary
# target segment for the chips category. This segment contributes
# significant sales, pays a higher average price per unit, and
# shows stronger preferences for premium brands and larger pack sizes.
#
# Category growth opportunities may include increasing visibility
# of preferred brands such as Doritos, Kettle and Pringles, while
# promoting larger pack sizes that align with this segment's
# purchasing behaviour.