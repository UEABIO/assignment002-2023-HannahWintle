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
butterfly_year$rain_jun <- replace(butterfly_year$rain_jun, 14, 57.7)
butterfly_year$rain_jun <- as.numeric(butterfly_year$rain_jun)

# check data distributions
butterfly_year %>%
  ggplot(aes(x=year,
             y=jun_mean))+
  geom_point()+
  geom_smooth(method="lm")

butterfly_year %>%
  ggplot(aes(x=year,
             y=rain_jun))+
  geom_point()+
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

# HYPOTHESIS
#The average temperature in June is increasing over time

butterfly_ls7 <- lm(jun_mean ~ year 
                    + rain_jun
                    + year:rain_jun,
                    data=butterfly_year)

check_model(butterfly_ls7, check = "linearity")
check_model(butterfly_ls7, check = "homogeneity")
check_model(butterfly_ls7, check = "outliers")
check_model(butterfly_ls7, check = "vif") 
check_model(butterfly_ls7, check = "qq") 

MASS::boxcox(butterfly_ls7) #log transformation not possible

#high collinearity
#homogeneity of variance and linearity possibly violated

#linear regression
butterfly_ls6 <- lm(jun_mean ~ year, 
                    data=butterfly_year)

summary(butterfly_ls6)

#(F=1.687, DF=1,41, p value=0.201)
#p value is greater than 0.05 therefore not statistically significant
