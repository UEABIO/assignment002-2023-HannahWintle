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

male_image <- readPNG("Images/male_butterfly.png", native=TRUE)
female_image <- readPNG("Images/female_butterfly_01.png", native=TRUE)

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
  theme(legend.position = "none")+
  annotation_custom(rasterGrob(male_image), xmin = 1.55, xmax = 2.45, ymin = 14, ymax = 14.7)+
  annotation_custom(rasterGrob(female_image), xmin = 0.55, xmax = 1.45, ymin = 12.4, ymax = 13.3)+
  theme(axis.text = element_text(size = 14))+
  theme(axis.title = element_text(size = 14))

#__________________________----

# OUTPUT FIGURE TO FILE ----

ggsave("Figures/butterfly_plot_02.png", height = 8,
       width = 10, dpi=300)

#colour blindness checker
colorBlindness::cvdPlot()