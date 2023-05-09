#___________________________----
# SET UP ----

# An analysis of the forewing size and temperature in Hesperia comma.

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
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(aes(colour=sex))

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

# boxplot and violin plot sex against forewing length

butterfly %>%
  ggplot(aes(x=sex,
             y=forewing_length,
             fill = sex,
             colour = sex))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = c("hotpink2", "skyblue2"))+
  scale_colour_manual(values = c("hotpink2", "skyblue2"))+
  theme_classic()+
  labs (x = "Sex", y = "Forewing Length (mm)") +
  theme(legend.position = "none")

# scatter plot to show changes in temperature over time

butterfly %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_point(aes(colour=jun_mean))+
  geom_smooth(method="lm", colour = "#140b34")+
  scale_color_viridis_c(option = "inferno")+
  theme_classic()+
  theme(legend.position = "bottom")+
  stat_regline_equation(label.y = 17)+
  stat_cor(aes(label=..rr.label..), label.y=16.6)+
  labs (x = "Year", y = "Average Temperature in June (°C)", colour = "Temperature (°C)")