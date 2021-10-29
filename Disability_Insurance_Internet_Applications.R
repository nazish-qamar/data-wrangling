#Data Wrangling in R
#Case Study: Disability Insurance Internet Applications trend on Social Security Administration (SSA) website
#Source: https://www.ssa.gov/open/data/initial-disability-insurance-online-apps-2012-onward.html
library(tidyverse)
library(lubridate)
library(stringr)

types = 'ccnncc'
first.half = read_csv("https://www.ssa.gov/open/data/fy08-present-dib-filed-via-internet.csv", skip=5, col_types = types)
glimpse(first.half)
first.half = rename(first.half, 
                    "FY"= "Fiscal Year",
                    "Total Applications" = "Total Initial Social Security Disability Insurance Applications",
                    "Internet Applications" = "Internet Initial Social Security Disability Insurance Applications",
                    "Internet Applications Percentage"="Percentage Filed via the Internet")

glimpse(first.half)
print(first.half, n=50)
first.half <- first.half %>%
  filter(first.half$Month!="TOTAL") %>% #Remove year TOTAL rows
  select(-Comments) %>% #Remove Comments column
  mutate(Month=substr(Month,1,3)) %>% # Convert month to standard abbreviations
  mutate(FY=substr(FY,1,4)) # Convert month to standard abbreviations
  
#Removing % symbol from "Internet Applications Percentage" column
first.half$`Internet Applications Percentage` = substr(first.half$`Internet Applications Percentage`,1,nchar(first.half$`Internet Applications Percentage`)-1)


print(first.half, n=50)

  
second.half = read_csv("https://www.ssa.gov/open/data/fy12-onward-dib-filed-via-internet.csv", col_types = types)
glimpse(second.half)
print(second.half, n=15)

second.half = rename(second.half, 
                    "FY"= "Fiscal Year (Field A)",
                    "Month" = "Month  (Field B)",
                    "Total Applications" = "Total Initial Social Security Disability Insurance Applications (Field C)",
                    "Internet Applications" = "Internet Initial Social Security Disability Insurance Applications (Field D)",
                    "Internet Applications Percentage"="% Percentage Filed via the Internet  (Field E)")


second.half <- second.half %>%
  select(-"Comments (Field F)")

glimpse(second.half)
print(second.half, n=15)


full.data = bind_rows(first.half, second.half, .id = NULL) # Combine the first.half and second.half tibbles

glimpse(full.data)

full.data <- full.data %>%
  mutate(date=my(paste(full.data$Month, full.data$FY))) #Combine Month and FY column

glimpse(full.data)

print(full.data, n=15)

#Converting Fiscal years to calender years
full.data <- full.data %>%
  mutate(FY=as.numeric(FY)) %>%
  mutate(FY=ifelse(month(date)>=10, FY-1, FY)) %>%
  mutate(date=my(paste(Month, FY)))

print(full.data, n=55)
summary(full.data)

# Remove FY and Month columns
full.data <- full.data %>%
  select(-FY, -Month)

# Check the summary
summary(full.data)

full.data$`Internet Applications Percentage` = as.numeric(full.data$`Internet Applications Percentage`)
# Plot the results
ggplot(data=full.data, mapping=aes(x=date,y=`Internet Applications Percentage`)) +
  geom_point()

#Plot results can be found in 'Internet_Applications_Trend_Rplot.pdf' file
