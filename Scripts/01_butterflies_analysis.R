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

#fix rain value
butterfly$rain_jun <- replace(butterfly$rain_jun, 19, "57.7")

# check data distributions
butterfly %>%
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(aes(colour=sex))

butterfly %>%
  ggplot(aes(x=rain_jun,
             y=forewing_length))+
  geom_jitter(aes(colour=sex))

butterfly %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_jitter(aes(colour=sex))+
  geom_smooth(method="lm", se=FALSE, colour = "black")

#__________________________----

# 📊PLOT ----

# scatterplot wingspan against temperature

butterfly %>%
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(aes(color=jun_mean))+
  geom_smooth(method="lm", se=FALSE, colour = "#140b34")+
  facet_wrap(~sex)+
  labs(x = "Average Temperature in June (°C)", y = "Forewing Length", colour = "Temperature (°C)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  scale_color_viridis_c(option = "inferno") +
  theme(legend.position="bottom")

# boxplot and violin plot sex against forewing length

butterfly %>%
  ggplot(aes(x=sex,
             y=forewing_length,
             fill = sex,
             colour = sex))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = c("orange1", "mediumorchid1", "mediumaquamarine"))+
  scale_colour_manual(values = c("orange1", "mediumorchid1", "mediumaquamarine"))+
  theme_classic()+
  labs (x = "Sex", y = "Forewing Length") +
  theme(legend.position = "none")