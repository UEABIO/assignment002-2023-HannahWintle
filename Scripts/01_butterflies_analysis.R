#___________________________----
# SET UP ----

# An analysis of the forewing size and temperature in Hesperia comma.

#__________________________----

# 📦 PACKAGES ----
library(tidyverse) # tidy data packages
library(janitor) # clean data names
#__________________________----

# 📂 IMPORT DATA ----

butterfly <- read_csv("Data/univoltine_butterfly.csv")