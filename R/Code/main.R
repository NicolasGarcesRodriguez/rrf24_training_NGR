### Reproducible Research Fundamentals -  Main R Script

# Load libraries ---- 

# Load necessary libraries
library(haven)  # for reading .dta files
library(dplyr)  # for data manipulation
library(tidyr)  # for reshaping data
library(stringr) # work with strings
library(labelled) # use labels
library(gtsummary) # tables
library(gt) # tables
library(ggplot2) #graphs
library(tidyverse) # working with tidy data
library(modelsummary) # creating summary tables
library(stargazer) # writing nice tables
library(RColorBrewer) # color palettes


# Recover environment ----
# You should restore the environment if it is your first time with the following code
# renv::restore()


# Set data path ----

# this is the second root of the project, the first root is the code whose directory 
# is already being handled by the rstudio project.


data_path <- "C:/Users/wb631291/OneDrive - WBG/Desktop/Data"

# Run the R scripts ----

source(file.path("Code/01-processing-data.R"))

source(file.path("Code/02-constructing-data.R"))

source(file.path("Code/03-analyzing-data.R"))
