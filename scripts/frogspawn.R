# Packages ----
library(tidyverse)
library(rstatix)
library(performance)
library(here)

# data ----
frog <- read_csv(here("data", "frogs_messy_data.csv"))

# Basic analysis ----

# check the structure of the data
glimpse(frog)

# check data is in a tidy format
head(frog)

# check variable names
colnames(frog)


# clean up column names

frog <- janitor::clean_names(frog)

# check for duplication
frog %>% 
  duplicated() %>% 
  sum()

# check for typos - by looking at impossible values
frog %>% 
  summarise(min=min(temperature13, na.rm=TRUE), 
            max=max(temperature13, na.rm=TRUE))

# check for typos by looking at distinct characters/values

frog %>% 
  distinct(temperature13)

# missing values
frog %>% 
  is.na() %>% 
  sum() # 120 NA 

# quick summary

summary(frog)


