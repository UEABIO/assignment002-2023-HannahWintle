#___________________________----
# SET UP ---

# An analysis of humerus and femur relationship in the Titanosaurs.


#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
#__________________________----

# 📂 IMPORT DATA ----

dinosaur <- read_csv("Examples/Titanosaur.csv")


#__________________________----

# 🔍 CHECK DATA----

dinosaur # call the dataframe

str(dinosaur) # check structure of dataframe

dinosaur<- clean_names(dinosaur) # snake case all col names

#__________________________----

# 🧹 TIDY ----

# humerus is a character variable not numerical

dinosaur %>% 
  distinct(humerus)

# some NA values are coded as character "-"
# search term "tidyverse convert value to NA"

dinosaur_tidy <- dinosaur %>% 
  mutate(humerus = na_if(humerus, "-")) %>% 
  drop_na(humerus) %>% 
  mutate(humerus = as.numeric(humerus))

# check all "-" and NA values removed
dinosaur_tidy %>% 
  distinct(humerus)



#__________________________----

# 📊PLOT ----

# scatterplot humerus against femur
# one datapoint with high humerus and femur value - possible outlier?

ggplot(data = dinosaur_tidy,
       aes(x = humerus,
           y = femur))+
  geom_point()


# Find high value in case an issue later

dinosaur_tidy %>% 
  arrange(desc(femur))



#__________________________----

# MODEL----

# model indicates that 1st row of dataframe does exert leverage on the model
lsmodel1 <- lm(femur ~ humerus, data = dinosaur_tidy)
performance::check_model(lsmodel1)

# always preferable to keep data in, but boxcox indicatates transformations unlikely to help
MASS::boxcox(lsmodel1)

# removing the term does not appear to improve model fit
lsmodel2 <- lm(femur ~ humerus, data = dinosaur_tidy[-1,])
performance::check_model(lsmodel2)

# lsmodel is preferred

summary(lsmodel1)


