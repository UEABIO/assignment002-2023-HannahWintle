#___________________________----
# SET UP ----

# An analysis of temperature over time.

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library (lubridate) # make sure dates are processed properly
library(ggpubr) # regression line to scatter plot
library(here)
library(kableExtra) # make tables
library(broom.helpers)
library(GGally)
library(emmeans)
library(performance)
library(patchwork)

#__________________________----

# 📂 IMPORT DATA ----

butterfly <- read_csv("Data/univoltine_butterfly.csv")

#__________________________----

# 🔍 CHECK DATA----

butterfly # call the dataframe

str(butterfly) # check structure of dataframe

#__________________________----

# 🧹 TIDY ----

# snake case all col names
butterfly <- clean_names(butterfly)

# check the new variable names
colnames(butterfly)

#create new object
butterfly_year <- select(.data=butterfly, year, jun_mean, rain_jun)

#check for duplicate rows in the data
butterfly_year %>% 
  duplicated() %>%
  sum() 
#remove duplicate rows
# Remove duplicate rows
butterfly_year <- distinct(butterfly_year)

#check for missing values
butterfly_year %>% 
  is.na() %>%
  sum()

# fix rain value
butterfly_year$rain_jun <- replace(butterfly_year$rain_jun, 14, "57.7")
butterfly_year$rain_jun <- as.numeric(butterfly_year$rain_jun)

# convert year into date values
butterfly_year$year <- as.Date(as.character(butterfly_year$year), format = "%Y")

# check data distributions
butterfly_year %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_jitter()+
  geom_smooth(method="lm")

#__________________________----

# 📊PLOT ----

# scatter plot to show changes in temperature over time

butterfly_year %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_point(aes(colour=jun_mean))+
  geom_smooth(method="lm", colour = "#140b34")+
  scale_color_gradient(low = "gold",
                       high = "darkred")+
  theme_light()+
  theme(legend.position = "bottom")+
  stat_regline_equation(label.y = 17)+
  stat_cor(aes(label=..rr.label..), label.y=16.6)+
  labs (x = "Year", y = "Average Temperature in June (°C)", colour = "Temperature (°C)")+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_03.png", height = 8,
       width = 10, dpi=300)

#colour blindness checker
colorBlindness::cvdPlot()

#_________________________----

# MODEL----

#HYPOTHESIS
#The average temperature in june is increasing over time

butterfly_ls3 <- lm(jun_mean ~ year, data = butterfly_year)

check_model(butterfly_ls3, check = "linearity")
check_model(butterfly_ls3, check = "homogeneity")
check_model(butterfly_ls3, check = "outliers")
check_model(butterfly_ls3, check = "vif") 
check_model(butterfly_ls3, check = "qq") 

MASS::boxcox(butterfly_ls3)

butterfly_sqrt <- lm(1/sqrt(jun_mean) ~ year, data = butterfly)

check_model(butterfly_sqrt, check = "linearity")
check_model(butterfly_sqrt, check = "homogeneity")
check_model(butterfly_sqrt, check = "outliers") 
check_model(butterfly_sqrt, check = "vif") 
check_model(butterfly_sqrt, check = "qq") 