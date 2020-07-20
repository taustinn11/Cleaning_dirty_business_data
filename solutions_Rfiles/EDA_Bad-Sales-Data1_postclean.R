## Brief exploratory analysis for Sales Data 1

## Directory nonsense
setwd("C:/Users/anuck/OneDrive - Emory University/Documents/clean_data_practice")

## Load gghisto function
source("src/gghisto.R")

## Plots

bsd1_clean_hist <- gghisto(bsd1_clean[,-1])
bsd1_clean_hist

bsd1_clean_hist$Sales+
  geom_histogram(aes(fill = Segment),binwidth = 10)+
  xlim(c(0, 6000))

bsd1_clean_hist$Sales+
  geom_histogram(aes(fill = Shipping),binwidth = 10, alpha = 0.5)+
  xlim(c(0, 1000))+
  facet_wrap(~Segment)

#There doesn't seem to be a clear relationship between the segment of customers and the value of sales or the distribution of sales at specific prices

bsd1_clean %>% group_by(Segment) %>% summarise(av = mean(Sales), sd = sd(Sales)) 

#Wow the standard deviation of this is huge -- definitely need to use a nonparametric plotting approach

bsd1_clean %>% ggplot(aes(Segment, log10(Sales), fill = Shipping))+
  geom_boxplot()

#There is no clear visible correlation between any of the variables and the Sales values

#The statistical test to use to determine if a sector or shipping type is correct would certainly be a Kruskal-Wallis