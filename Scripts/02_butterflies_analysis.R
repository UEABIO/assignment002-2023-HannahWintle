#___________________________----
# SET UP ----

# An analysis of sex and forewing length in Hesperia comma.

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

# boxplot and violin plot sex against forewing length

butterfly %>%
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
  theme(legend.position = "none")

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_02.png", height = 8,
       width = 10, dpi=300)

#colour blindness checker
colorBlindness::cvdPlot()