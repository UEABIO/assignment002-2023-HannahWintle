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

# fix rain value
butterfly$rain_jun <- replace(butterfly$rain_jun, 19, "57.7")

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

# boxplot temperature over time

butterfly %>%
  ggplot(aes(x = year, 
             y = jun_mean,
             fill = sex,
             colour = sex)) +
  geom_boxplot(alpha = 0.6, 
               width = 0.2) +
  theme(legend.position = "none") +
  theme_classic()

# time series of temperature and rainfall over time

butterfly$rain_jun <- as.numeric(butterfly$rain_jun)

butterfly %>%
  ggplot(aes(x = year)) + 
  geom_line(aes(y=jun_mean, color = "Temperature in June")) +
  geom_line(aes(y=rain_jun, color = "Rainfall in June (mL)"))+
  scale_y_continuous(name = "Temperature in June",
                     limits = c(11.8, 16.4),
                     sec.axis = sec_axis(~.*1, name="Rainfall in June (mL)")) +
  scale_x_date(date_labels = "%Y", name = "Year") +
  theme_classic()+
  theme(aspect.ratio = 2 / (1 + sqrt(6)))+
  scale_color_manual(values = c("orange2", "gray30"))