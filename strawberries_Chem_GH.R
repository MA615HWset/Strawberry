library(dplyr)
library(stringr)

# Read the dataset
strw <- read.csv("...\\strawberries25_v3.csv")

# Start using pipes to manipulate the data
strw_chem <- strw %>%
  # Create a subset based on CHEMICAL
  filter(grepl("CHEMICAL", Domain)) %>%
  
  # Delete duplicate words in Domain.Category
  mutate(Domain.Category = mapply(function(domain, category) {
    gsub(domain, "", category)
  }, Domain, Domain.Category)) %>%
  
  # Delete "CHEMICAL" from the Domain
  mutate(Domain = gsub("CHEMICAL, ", "", Domain)) %>%
  
  # Extract numbers to create Chem.Value
  mutate(Chem.Value1 = str_extract(Domain.Category, "\\d+")) %>%
  
  # Remove symbols and unwanted characters from Domain.Category
  mutate(Domain.Category = gsub("[:=()]", "", Domain.Category)) %>%
  
  # Remove duplicate chemical names in Domain.Category
  mutate(Domain.Category = mapply(function(value, category) {
    gsub(value, "", category)
  }, Chem.Value1, Domain.Category)) %>%
  
  # Clean Data.Item by removing "STRAWBERRIES - " and "STRAWBERRIES,"
  mutate(Data.Item = gsub("STRAWBERRIES - ", "", Data.Item),
         Data.Item = gsub("STRAWBERRIES,", "", Data.Item)) %>%
  
  # Rearrange columns to put Chem.Value right after Domain.Category
  add_column(Chem.Value = .$Chem.Value1, .after = 19) %>%
  select(-Chem.Value1)

write.csv(strw_chem, "...\\strawberries_Chem.csv", row.names = FALSE)
