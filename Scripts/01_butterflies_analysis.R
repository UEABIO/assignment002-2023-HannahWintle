#___________________________----
# SET UP ----

# An analysis of the forewing size and temperature in Hesperia comma.

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library (lubridate) # make sure dates are processed properly

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

# check data distributions
butterfly %>%
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(aes(colour=sex))

#__________________________----

# 📊PLOT ----

# scatterplot forewing length against temperature

butterfly %>%
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(size=2, alpha=0.6, shape=21,fill="steelblue")+
  geom_smooth(method="lm", colour = "black")+
  facet_wrap(~sex)+
  labs(x = "Average Temperature in June (°C)", y = "Forewing Length (mm)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_01.png", height = 8,
       width = 10, dpi=300)

#colour blindness checker
colorBlindness::cvdPlot()