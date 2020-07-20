## Bad Sales Data 2 Solution

## Packages
library(tidyverse)
library(xlsx)

## Read Data
bsd2 <- read.xlsx("raw_data/Badly-Structured-Sales-Data-2.xlsx", sheetIndex = 1, startRow = 3)
bsdheads <- read.xlsx("raw_data/Badly-Structured-Sales-Data-2.xlsx", sheetIndex = 1, startRow = 1, endRow = 2) #This is to keep track of the data heads to use later
bsdheads

## Define functions that will do the bulk of the cleaning
clean3 <- function(df, ship) {
  df %>% 
    rename("Date" = colnames(.)[1], "Consumer" = colnames(.)[2], "Corporate" = colnames(.)[3], "Home" = colnames(.)[4]) %>% 
    pivot_longer(cols = c(Consumer, Corporate, Home),
                 names_to = "Segment",
                 values_to = "Sales") %>% 
    drop_na() %>% 
    mutate(Shipping = replicate(nrow(.), ship),
           Sales = round(Sales, 2))
}

## Clean each subsection
first <- bsd2 %>% select(c(1:4)) %>% 
  clean3("First")

same <- bsd2 %>% select(c(1, 5:7)) %>% 
  clean3("Same")

second <- bsd2 %>% select(c(1, 8:10)) %>% 
  clean3("Second")

standard <- bsd2 %>% select(c(1, 11:13)) %>% 
  clean3("Standard")

## Join the data

rbind(first, same, second, standard) %>% write.xlsx(file = "cleaned_data/Cleaned_Badly-Structured-Sales-Data-2.xlsx")
