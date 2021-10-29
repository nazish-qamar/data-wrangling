#Source:- https://data.austintexas.gov/Environment/Water-Quality-Sampling-Data/5tye-7ray
library(tidyverse)
library(lubridate)

#loading data from the URL
water <- read_csv(file="https://data.austintexas.gov/resource/5tye-7ray.csv")

glimpse(water)

#Selecting only the columns needed for analysis
water <- water %>%
  select(site_name, site_type, sample_date, param_type, parameter, result, unit)

glimpse(water)

#Renaming one column
water <- water %>%
  rename(parameter_type = param_type)

glimpse(water)

#Viewing list of all unique parameter types
unique(water$parameter_type)

#Assuming we are interested only in the analysis of Conventionals parameter type
filtered_water <- water %>%
  filter(parameter_type == 'Conventionals')

glimpse(filtered_water)

#Viewing what paramter are present in filtered data
unique(filtered_water$parameter)


#Checking summary before proceeding
summary(filtered_water)

#Converting few variables to factor
filtered_water <- filtered_water %>%
  mutate(site_type = as.factor(site_type),
         parameter_type = as.factor(parameter_type),
         parameter = as.factor(parameter),
         unit = as.factor(unit))


#Viewing summary again for checking observations in each factor level
summary(filtered_water)

#As we are dealing with water temperature, we can remove reading recorded in other units 
filtered_water <- filtered_water %>%
  filter(!unit == 'MV')

summary(filtered_water)

#Remove measurements of level with 0 cases 
filtered_water <- filtered_water %>%
  mutate(unit = droplevels(unit), parameter = droplevels(parameter))

summary(filtered_water)
     
#checking for outliers    
ggplot(filtered_water, mapping = aes(x=sample_date, y = result))+
  geom_point()

#boxplots
ggplot(data=filtered_water, mapping = aes(x=unit, y = result))+
  geom_boxplot()

#Removing other unnecessary columns before using pivot_wider
filtered_water <- filtered_water %>%
  select(-parameter_type, -unit)

summary(filtered_water)

filtered_water_wide <- pivot_wider(filtered_water,
                                   names_from = parameter,
                                   values_from = result)

filtered_water_wide
