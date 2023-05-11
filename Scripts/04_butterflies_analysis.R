#___________________________----
# SET UP ----

# An analysis of the forewing size over time in Hesperia comma.

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

# convert year into date values
butterfly$year <- as.Date(as.character(butterfly$year), format = "%Y")

# check data distribution

butterfly%>%
  ggplot(aes(x=year,
             y=forewing_length))+
  geom_jitter()+
  geom_smooth(method="lm")

# data plot shows that forewing length decreased over time contradicting the hypothesis that the increase in climate temperature over time is increasing the size of butterflies