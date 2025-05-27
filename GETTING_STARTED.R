# Getting Setup for the R Visualization Workshop - ACCESS 2025
# Hopefully you have install R and RStudio (Obviously you have if you're seeing this)
# It is good practice if you have time to update to the latest versions of R and RStudio
# ---NOT NECESSARY BUT ENCOURAGED---
# Visit https://posit.co/download/rstudio-desktop/ to download most up to date versions

# We will be using several packages 
# Packages = add ons or additional toolboxes for coding in R
# Prior to the workshop, please install/update the following packages

install.packages('tidyverse') # Includes other packages like: ggplot2 (plotting), dplyr (data manipulation/grammar), etc.
install.packages('ggeffects') # Vizualizing 
install.packages('broom')     # Tidy model outputs
install.packages('emmeans')   # Model contrasts (just in case)

# Packages are install from the Comprehensive R Archive Network (CRAN), a repository of code!
# IF you get an ---ERROR--- while install packages the most like culprit is your version of R needs to be updated

# Once installed please library a.k.a. ---LOAD--- the packages
# This loads the package into the memory (RAM) to make it useable

library(tidyverse)
library(ggeffects)
library(broom)
library(emmeans)

# You may see a list of conflicts, this is not to worry...
# This happens when functions from different packages share the same name


# IF you get an ---ERROR--- while loading it may look like this...
# Error: package or namespace load failed for ‘somePackage’ in loadNamespace(...): there is no package called ‘xyz’

# First try reinstalling with depandancies = TRUE
# install.packages("somePackage", dependencies = TRUE) # run this code by deleting '#' prefix

# If this doesn't resolve your issue try installing the packages it is failing to load separately then re-library the package
# install.packages('<package_name>')