#___________________________----
# SET UP ----

# An analysis of the forewing size and temperature in Hesperia comma.

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library (lubridate) # make sure dates are processed properly
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

butterfly <- clean_names(butterfly) # snake case all col names

colnames(butterfly) # check the new variable names

butterfly %>% 
  duplicated() %>% #check for duplicate rows in the data
  sum() 

# fix sex names
butterfly$sex <- str_replace(butterfly$sex, "Maes", "Males")
butterfly$sex <- str_replace(butterfly$sex, "Female", "Females")
butterfly$sex <- str_replace(butterfly$sex, "Femaless", "Females")

# pivot data to wide format
butterfly_wide <- butterfly %>% 
  pivot_wider(names_from = sex, values_from = forewing_length)

butterfly_wide %>% 
  is.na() %>% #check for missing values
  sum()

butterfly_wide <- na.omit(butterfly_wide) #remove rows with na

#pivot data back to long format
butterfly_long <- butterfly_wide %>%
  pivot_longer(cols = Females:Males, 
               names_to = "sex",
               values_to = "forewing_length")

# check data distributions
butterfly_long %>%
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(aes(colour=sex))

#__________________________----

# 📊PLOT ----

# scatterplot forewing length against temperature

butterfly_long %>%
  ggplot(aes(x=jun_mean,
             y=forewing_length))+
  geom_jitter(size=2, alpha=0.6, shape=21,fill="steelblue")+
  geom_smooth(method="lm", colour = "black")+
  facet_wrap(~sex)+
  labs(x = "Average Temperature in June (°C)", y = "Forewing Length (mm)")+
  theme_bw()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_01.png", height = 8,
       width = 10, dpi=300)

#colour blindness checker
colorBlindness::cvdPlot()

#__________________________----

# STATISTICS ----

butterfly_long %>%
  group_by(sex) %>%
  summarise(mean=mean(forewing_length),
            sd=sd(forewing_length))

# new object
butterfly_summary <- butterfly_long %>%
  group_by(sex) %>%
  summarise(mean=mean(forewing_length),
            sd=sd(forewing_length))

# make summary plot
butterfly_summary %>%
  ggplot(aes(x=sex,
             y=mean))+
  geom_pointrange(aes(ymin=mean-sd, ymax=mean+sd)) +
  theme_bw()

# make table

butterfly_summary %>% 
  kbl(caption="Summary statistics of forewing sizes of butterflies in male and female Silver Spotter Skippers") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")

# calculate the average difference and sd in forewing length between sexes

butterfly_new_wide <- butterfly_long %>% 
  pivot_wider(names_from = sex, values_from = forewing_length) %>% 
  mutate(difference = Females - Males)

difference_summary <- butterfly_new_wide %>% 
  summarise(mean=mean(difference),
            sd=sd(difference),
            n=n())

difference_summary

#standard error of the difference
difference_summary %>% 
  mutate(se= sd/sqrt(n))

#confidence intervals
lowerCI <- 1.23-(2*0.111)

upperCI <- 1.23+(2*0.111)

lowerCI
upperCI

#_________________________----

#MODEL----

butterfly_ls1 <- lm(forewing_length ~ sex + jun_mean + rain_jun +
                      sex:jun_mean +
                      sex:rain_jun +
                      jun_mean:rain_jun, 
                    data=butterfly_long)

check_model(butterfly_ls1, check = "linearity")
check_model(butterfly_ls1, check = "homogeneity")
check_model(butterfly_ls1, check = "outliers")
check_model(butterfly_ls1, check = "vif") 
check_model(butterfly_ls1, check = "qq") 