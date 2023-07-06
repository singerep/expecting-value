library(tidyverse)
library(tidycensus)
library(tigris)
library(sf)
library(ggplot2)
library(scales)
library(ineq)

census_api_key("11d46bc70e375d39b67b4b4919a0099934aecbc7")

us <- get_acs(
  geography = "us",
  year = 2021,
  survey = "acs1",
  table = c("B25093"),
  output = "wide"
)

us$B25093_016E
