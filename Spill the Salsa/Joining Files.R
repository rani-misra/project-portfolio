library(jsonlite)
library(dplyr)
library(stringr)
library(tidyr)

file_name1 <- '~/Desktop/Spring 2025/Deep Learning on Cloud Platforms/Project/yelp_academic_dataset_business.json'
file_name2 <- '~/Desktop/Spring 2025/Deep Learning on Cloud Platforms/Project/yelp_academic_dataset_review.json'

business <-jsonlite::stream_in(textConnection(readLines(file_name1, n=500000)),verbose=F)
review <-jsonlite::stream_in(textConnection(readLines(file_name2, n=500000)),verbose=F)

# kept only businesses in philly
business_filtered <- business %>% filter(state == "PA", city == "Philadelphia")

# merged data by business ID
merged_df <- inner_join(business_filtered, review, by = "business_id") 

# created filtered dataset that keeps only open restaurants that sell Mexican food
# attributes was unnested so each attribute type is a variable
merged_df_filtered <- merged_df %>% 
  filter(is_open == 1)  %>%
  filter(str_detect(categories, "Restaurant")) %>%
  filter(str_detect(categories, "Mexi")) %>% 
  unnest(attributes, names_repair = "unique")

#check for missing values, columns that are missing across all obs are removed
missing_values <- colSums(is.na(merged_df_filtered))
columns_to_remove <- names(missing_values[missing_values == 3109])
merged_df_filtered <- merged_df_filtered %>% select(-all_of(columns_to_remove))

#check for missing values again, columns that are missing across more than half obs are removed
missing_values <- colSums(is.na(merged_df_filtered))
columns_to_remove <- names(missing_values[missing_values > 1555])
merged_df_filtered <- merged_df_filtered %>% 
  select(-all_of(columns_to_remove)) %>% 
  select(-contains("_id"))
                
# Save as CSV
write.csv(merged_df_filtered, "merged.csv", row.names = FALSE)

