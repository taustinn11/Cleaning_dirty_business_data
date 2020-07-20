## Packages
library(tidyverse)
library(xlsx)

## Read Data
invoices <- read.xlsx("raw_data/Invoices-with-Merged-Categories-and-Merged-Amounts.xlsx", sheetIndex = 1)

## Clean these data in one swift chain of functions and save the cleaned data output
invoices %>% separate(Category, into = c("C1", "C2", "C3", "C4", "C5"), sep = "\\|") %>% 
  separate(Amount, into = c("V1", "V2", "V3", "V4", "V5"), sep = "\\|") %>% 
  unite(E1, C1, V1) %>% 
  unite(E2, C2, V2) %>% 
  unite(E3, C3, V3) %>% 
  unite(E4, C4, V4) %>% 
  unite(E5, C5, V5) %>% 
  pivot_longer(cols = -Order.ID) %>% 
  separate(value, c("Item", "Sale"), sep = "_", convert = T) %>% 
  mutate(Sale = round(Sale, 2)) %>% 
  select(-name) %>% 
  drop_na() %>% 
  write.xlsx(file = "cleaned_data/Cleaned_Invoices_with_Merged_Data.xlsx")
