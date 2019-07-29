# Data Understanding

# [0] ---- Load packages ----

library(readr)
library(tidyverse)
library(rdrop2)
library(feather)
library(httpuv)
library(sp)
library(rgdal)
library(rgeos)
library(maptools)
library(ggmap)
library(broom)
library(sf)

# [1] ---- Load Data ----

land_value <- readRDS("land_value/land_value.rds")
old_sales_data <- readRDS("sale_value/old_sales_data.rds")
new_sales_data <- readRDS("sale_value/new_sales_data.rds")

# [2] ---- Combine old sales data with new data ----


# [3] ---- Data Understanding ----