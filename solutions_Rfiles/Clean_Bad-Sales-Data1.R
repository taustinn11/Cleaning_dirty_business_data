## Bad Sales Data 1 Solution

## Packages
library(tidyverse)
library(xlsx)

## Read Data
bsd1 <- read.xlsx("raw_data/Badly-Structured-Sales-Data-1.xlsx", sheetIndex = 1)

## Define functions that will do the bulk of the cleaning
clean1 <- function() {bsd1 %>% select(-grep("Total", colnames(.)))}
clean2 <- function(df, type) {
  df %>% 
    rename("OrderID" = colnames(.)[1], 
           "First" = colnames(.)[2], 
           "Same" = colnames(.)[3], 
           "Second" = colnames(.)[4], 
           "Standard" = colnames(.)[5]) %>% 
    slice(-c(1:2)) %>% 
    mutate(Segment = replicate(nrow(.), type)) %>% 
    pivot_longer(cols = c(First, Same, Second, Standard), 
                 names_to = "Shipping", 
                 values_to = "Sales") %>% 
    drop_na() %>% 
    filter(!str_detect(.$OrderID, "Total")) %>% 
    mutate(Sales = round(as.numeric(Sales), 2), Shipping = as.factor(Shipping), Segment = as.factor(Segment))
}

## Clean each subsection
cons <- clean1() %>% select(c(1:5)) %>% 
  clean2(type = "Consumer")

corps <- clean1() %>% select(c(1, 6:9)) %>% 
  clean2(type = "Corporate")

home <- clean1() %>% select(c(1, 10:13)) %>% 
  clean2(type = "Home_Office")

## Join the data
rbind(cons, corps, home) %>% write.xlsx(file = "cleaned_data/Cleaned_Badly-Structured-Sales-Data-1.xlsx")
