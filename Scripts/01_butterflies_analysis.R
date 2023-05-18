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
library(performance)
library(patchwork)
library(broom)
library(knitr)
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
  stat_regline_equation(label.y = 15, label.x = 12)+
  stat_cor(aes(label=..rr.label..), label.y=15.2, label.x = 12)+
  theme(axis.title = element_text(size = 14))

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_01.png", height = 8,
       width = 10, dpi=300)

#colour blindness checker
colorBlindness::cvdPlot()

#_________________________----

#HYPOTHESES
# The average temperature in june will affect forewing length
# The amount of rainfall affects forewing length

#MODEL----

butterfly_ls1 <- lm(forewing_length ~ jun_mean + sex + rain_jun +
                      jun_mean:sex +
                      jun_mean:rain_jun +
                      sex:rain_jun, 
                    data=butterfly_long)

check_model(butterfly_ls1, check = "normality")
check_model(butterfly_ls1, check = "linearity")
check_model(butterfly_ls1, check = "homogeneity")
check_model(butterfly_ls1, check = "outliers")
check_model(butterfly_ls1, check = "vif") 
check_model(butterfly_ls1, check = "qq")

# there is high collinearity

drop1(butterfly_ls1, test = "F") #drop1 can help identify the least significant predictor variable

# all three interaction terms appeared under the drop1 function
# all interaction term p values were over 0.05 therefore not statistically significant
# sex:jun_mean (F=0.55, DF=1,23, p value=0.47)
# sex:rain_jun (F=0.20, DF=1,23, p value=0.66)
# jun_mean:rain_jun (F=0.17, DF=1,23, p value=0.69)

butterfly_ls2 <- lm(forewing_length ~ jun_mean,
                    data=butterfly_long)
summary(butterfly_ls2)

# F=3.027, DF=1,28, p value= 0.093
# p value greater than 0.05. Not statistically significant
# multicollinearity may be biasing the regression model
# not enough data points?

butterfly_ls8 <- lm(forewing_length ~ rain_jun,
                    data=butterfly_long)
summary(butterfly_ls8)
#precipitation has no effect on forewing length
#F-statistic: 0.4966 on 1 and 28 DF,  p-value: 0.4868

cor_coeff <- cor(butterfly_long$jun_mean, butterfly_long$forewing_length)
print(cor_coeff)
# correlation coefficient = 0.312
# suggests a low-to-moderate positive correlation between temp and forewing length

# HYPOTHESIS
# Changes in temperature will affect male forewing length more than female forewing length

butterfly_ls3 <- lm(Males ~ jun_mean,
                    data=butterfly_wide)
summary(butterfly_ls3)
#F-statistic: 11.53 on 1 and 13 DF,  p-value: 0.004778

butterfly_ls4 <- lm(Females ~ jun_mean,
                    data=butterfly_wide)
summary(butterfly_ls4)
#F-statistic: 2.905 on 1 and 13 DF,  p-value: 0.1121
# Male value is far more statistically significant
# However high collinearity persists

#___________________________----
#TABLE----

# Extract the model summaries using broom
summary_ls2 <- tidy(butterfly_ls2)
summary_ls3 <- tidy(butterfly_ls3)
summary_ls4 <- tidy(butterfly_ls4)

# Remove intercept term from each summary table
summary_ls2 <- summary_ls2 %>% filter(term != "(Intercept)")
summary_ls3 <- summary_ls3 %>% filter(term != "(Intercept)")
summary_ls4 <- summary_ls4 %>% filter(term != "(Intercept)")

# Replace jun_mean term with custom labels in each summary table
summary_ls2$term <- "Combined"
summary_ls3$term <- "Males"
summary_ls4$term <- "Females"

# Combine the modified tables
combined_table <- bind_rows(
  `Forewing Length` = summary_ls2,
  `Males` = summary_ls3,
  `Females` = summary_ls4
)

kable(combined_table, caption = "Summary statistics of temperature affecting forewing sizes in male and female Silver Spotter Skippers") %>%
  kable_styling(bootstrap_options = "striped", full_width = TRUE, position = "left")
