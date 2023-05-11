#___________________________----
# SET UP ----

# An analysis of temperature over time.

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library (lubridate) # make sure dates are processed properly
library(ggpubr) # regression line to scatter plot

#__________________________----

# 📂 IMPORT DATA ----

butterfly <- read_csv("Data/univoltine_butterfly.csv")

#__________________________----

# 🔍 CHECK DATA----

butterfly # call the dataframe

str(butterfly) # check structure of dataframe

#__________________________----

# 🧹 TIDY ----

butterfly <- clean_names(butterfly) # snake case all col names

colnames(butterfly) # check the new variable names

butterfly %>% 
  duplicated() %>% #check for duplicate rows in the data
  sum() 

butterfly %>% 
  is.na() %>% #check for missing values
  sum()

# fix sex names
butterfly$sex <- str_replace(butterfly$sex, "Maes", "Males")
butterfly$sex <- str_replace(butterfly$sex, "Female", "Females")
butterfly$sex <- str_replace(butterfly$sex, "Femaless", "Females")

# fix rain value
butterfly$rain_jun <- replace(butterfly$rain_jun, 19, "57.7")
butterfly$rain_jun <- as.numeric(butterfly$rain_jun)

# convert year into date values
butterfly$year <- as.Date(as.character(butterfly$year), format = "%Y")

# check data distributions
butterfly %>%
  ggplot(aes(x=rain_jun,
             y=forewing_length))+
  geom_jitter(aes(colour=sex))

butterfly %>%
  ggplot(aes(x=year,
             y=rain_jun))+
  geom_jitter()+
  geom_smooth(method="lm")

butterfly %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_jitter(aes(colour=sex))+
  geom_smooth(method="lm")

#__________________________----

# 📊PLOT ----

# scatter plot to show changes in temperature over time

butterfly %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_point(aes(colour=jun_mean))+
  geom_smooth(method="lm", colour = "#140b34")+
  scale_color_viridis_c(option = "inferno")+
  theme_light()+
  theme(legend.position = "bottom")+
  stat_regline_equation(label.y = 17)+
  stat_cor(aes(label=..rr.label..), label.y=16.6)+
  labs (x = "Year", y = "Average Temperature in June (°C)", colour = "Temperature (°C)")