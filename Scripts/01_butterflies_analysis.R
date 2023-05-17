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

#_________________________----

#HYPOTHESES
# The average temperature in june affects forewing length
# The amount of rainfall affects forewing length
# Changes in temperature will affect male forewing length more than female forewing length by changes in temperature

#MODEL----

butterfly_ls1 <- lm(forewing_length ~ sex + jun_mean + rain_jun +
                      sex:jun_mean +
                      sex:rain_jun +
                      jun_mean:rain_jun, 
                    data=butterfly_long)

check_model(butterfly_ls1, check = "normality")
check_model(butterfly_ls1, check = "linearity")
check_model(butterfly_ls1, check = "homogeneity")
check_model(butterfly_ls1, check = "outliers")
check_model(butterfly_ls1, check = "vif") 
check_model(butterfly_ls1, check = "qq")

MASS::boxcox(butterfly_ls1)

butterfly_ls1log <- lm(log(forewing_length) ~ sex + jun_mean + rain_jun +
                      sex:jun_mean +
                      sex:rain_jun +
                      jun_mean:rain_jun, 
                    data=butterfly_long)

check_model(butterfly_ls1log, check = "homogeneity")
check_model(butterfly_ls1log, check = "vif")

summary(butterfly_ls1)
summary(butterfly_ls1log)

# there is high collinearity
# remove one of the variables 

#drop1 can help identify the least significant predictor variable

drop1(butterfly_ls1, test = "F")
# all three interaction terms appeared under the drop1 function
# therefore they are not statistically significant
# sex:jun_mean (F=0.55, DF=1,23, p value=0.47)
# sex:rain_jun (F=0.12, DF=1,23, p value=0.66)
# jun_mean:rain_jun (F=0.17, DF=1,23, p value=0.69)
# found no evidence that temperature affects rain or that rain affects forewing length between sexes

butterfly_ls2 <- lm(forewing_length ~ sex + 
                      jun_mean +
                      jun_mean:sex,
                    data=butterfly_long)

check_model(butterfly_ls2, check = "vif")

# produced better results even though there is still high multicollinearity
# a large F value indicates that the variability between groups is larger compared with the variability within groups
# large standard error comparing temperature and sex
# untrustworthy coefficients/not representative
# multicollinearity may be biasing the regression model
# not enough data points

broom::tidy(butterfly_ls2)

butterfly_long %>% #linear model
  ggplot(aes(x=sex, 
             y=forewing_length))+
  geom_jitter(aes(fill=sex))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=14.23222, yend=13.00187), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

emmeans::emmeans(butterfly_ls2, specs = c("sex")) %>%
  kbl(caption="Summary statistics of forewing sizes of butterflies in male and female Silver Spotter Skippers") %>% 
  kable_styling(bootstrap_options = "striped", full_width = T, position = "left")