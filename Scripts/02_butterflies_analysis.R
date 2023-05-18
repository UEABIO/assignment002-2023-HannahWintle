#___________________________----
# SET UP ----

# An analysis of sex and forewing length in Hesperia comma.

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
library (lubridate) # make sure dates are processed properly
install.packages("png") # install png package
library("png") # load png package
install.packages("grid") # Install grid package
library("grid") # Load grid
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

butterfly %>% 
  is.na() %>% #check for missing values
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

# boxplot and violin plot sex against forewing length

male_image <- readPNG("Images/male_butterfly.png", native=TRUE)
female_image <- readPNG("Images/female_butterfly_01.png", native=TRUE)

butterfly_long %>%
  ggplot(aes(x=sex,
             y=forewing_length,
             fill = sex,
             colour = sex))+
  geom_violin(alpha = 0.2)+
  geom_boxplot(width = 0.2,
               alpha = 0.6)+
  scale_fill_manual(values = c("hotpink2", "skyblue3"))+
  scale_colour_manual(values = c("hotpink2", "skyblue3"))+
  theme_classic()+
  labs (x = "Sex", y = "Forewing Length (mm)") +
  theme(legend.position = "none")+
  annotation_custom(rasterGrob(male_image), xmin = 1.55, xmax = 2.45, ymin = 14, ymax = 14.7)+
  annotation_custom(rasterGrob(female_image), xmin = 0.55, xmax = 1.45, ymin = 12.4, ymax = 13.2)+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))+
  ylim(12,15)

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_02.png", height = 8,
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

#Sizes of female butterflies were bigger than male butterflies, with a mean difference in forewing length of 1.23 [1.008, 1.452] mm (mean [95% CI]).

#____________________----
#MODEL----

butterfly_long %>% #linear model
  ggplot(aes(x=sex, 
             y=forewing_length))+
  geom_jitter(aes(fill=sex))+
  theme_classic()+
  geom_segment(aes(x=1, xend=2, y=14.23222, yend=13.00187), linetype="dashed")+
  stat_summary(fun.y=mean, geom="crossbar", width=0.2)

#linear regression model

butterfly_ls3 <- lm(forewing_length ~ sex, 
                    data=butterfly_long)

check_model(butterfly_ls3, check = "normality")
check_model(butterfly_ls3, check = "linearity")
check_model(butterfly_ls3, check = "homogeneity")
check_model(butterfly_ls3, check = "outliers")
check_model(butterfly_ls3, check = "vif") 
check_model(butterfly_ls3, check = "qq")

MASS::boxcox(butterfly_ls3)

#the assumption of homogeneity of variance may be violated
#this may produce:
#incorrect p values
#biased parameter estimates
#incorrect standard errors
#insufficient tests

summary(butterfly_ls3)

#F=61.3, DF=1,28, p value=1.579e-08
#p value is less than 0.05 therefore statistically significant