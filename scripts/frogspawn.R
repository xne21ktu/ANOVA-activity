#______________________----
# Packages ----
library(tidyverse)
library(rstatix)
library(performance)
library(here)

#______________________ ----
# Import Data ----
frog <- read_csv(here("data", "frogs_messy_data.csv"))

#______________________----
# Tidy Data ----

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

frog <- frog %>% 
  rename("13" = temperature13,
         "18" = temperature18,
         "25" = temperature25,
         frogspawn_id = `frogspawn_sample_id`) %>% 
  pivot_longer(`13`:`25`, names_to="temperature", values_to="days") %>% 
  drop_na(days)

# quick summary

summary(frog)

#_____________________----

# Analysis ----
lsmodel_frog <- lm(days ~ temperature, data = frog)

summary(lsmodel_frog)

anova(lsmodel_frog)
