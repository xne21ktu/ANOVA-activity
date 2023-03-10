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
broom::tidy(lsmodel_frog, conf.int=T, conf.level=0.95)
# The frogspawn hatching time in the 13 degrees treatment was an average of 26.3 [25.8-26.8](mean[95% CI]) days.
# The hatching time in the 18 degrees treatment was an average of 21 [19.77-22.23].
# The hatching time in the 25 degrees treatment was an average of 16.2 [15-17.43].

anova(lsmodel_frog)
# estimated signal is 386 times larger than the estimated noise 
# lots of the variance is being explained by temperature
#_____________________----
# Check Assumptions ----
performance::check_model(lsmodel_frog,
                         check = c("qq", "outliers", "homogeneity"))
# The values towards the middle of the fitted values seem to be more variable than the values at each end
# residuals normally distributed 

#_____________________----
# Data Visualization ----

#_____________________----
# Summary ----
# Frogspawn hatching time was reduced by increasing temperature (one-way ANOVA: F2,57 = 385.9, P < 0.001)
# Hatching time at 13 degrees Celsius was a mean of 26.3 days [25.8-26.8 95% CI], 
# which reduced by a mean of 5.3 days [4.57 - 6.02] when at 18 degrees Celsius,
# and by 10.1 days [9.37 - 10.82] at 25 degrees Celsius.

