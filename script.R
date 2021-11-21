# Header ------------------------------------------------------------------
# Path
rm(list = ls())
DATAROOT <-  "Documents"
    
# Options
options(scipen = 999)
options(stringsAsFactors = FALSE)
options(mc.cores = 4)

# Packages
library(DBI)
library(tidycensus)
library(readr)
library(readxl)
library(parallel)
library(openxlsx)
library(stringr)
library(lubridate)
library(rlang)
library(purrr)
library(stringdist)
library(tidyr)
library(glue)
library(ggplot2)
library(ggpmisc)
library(dplyr)
library(XML)
library(methods)
library(haven)
library(janitor)
library(dataCompareR)
library(furrr)
library(scales)
library(vctrs)
library(zoo)
library(fredr)
library(wbstats)
library(forcats)

# Analysis ---------------------------------------------------------------------
# Import the data
import <- read.csv(paste0(DATAROOT, "/data.csv")) %>% 
  mutate(month = as.Date(month)) %>% 
  # Filter closer to time of interest
  filter(month >= "2017-07-01")


# Create a versatile plotting function
line_plot <- function(DATA, MEASURE, SEGMENT, TITLE, Y_LABEL, LEGEND){
  
  # Set up the segmentation
  if (is.null(SEGMENT)) {
    plot <- ggplot()+
      geom_line(data = DATA, aes(x = month, y = get(MEASURE), color = NULL))
  } else if(is.character(SEGMENT)) {
    plot <- ggplot()+
      geom_line(data = DATA, aes(x = month, y = get(MEASURE), color = get(SEGMENT)))
  }
  
  plot <- plot +
    scale_y_continuous(
      expand = c(0, 0)
    ) +
    scale_x_date(
      date_breaks = "1 month",
      date_labels = "%b-%y",
      expand = c(0, 0)
    ) +
    theme(
      text = element_text(family = 'Helvetica', size = 14, colour = "black"),
      plot.title = element_text(hjust = 0.5, size = 14 + 4, face = "bold"),
      plot.subtitle = element_text(hjust = 0),
      plot.margin = unit(rep(1.5, 4), "cm"),
      panel.background = element_rect(fill = "white"),
      panel.border = element_blank(), 
      axis.line = element_line(),
      axis.title.y = element_text(angle = 0, vjust = 1.05, hjust = 0, colour = "black"),
      legend.position = "bottom",
      legend.text = element_text(size = 14)
    ) +
    labs(
      title = paste0(TITLE, '\n'),
      subtitle = Y_LABEL,
      y = '',
      x = '',
      colour = LEGEND,
      wrap_len = 13
    ) +
    guides(color = guide_legend(ncol = 3))
  
  return(plot)
}

# As a baseline, visualize the monthly occupancy rate
baseline <- import %>% 
  group_by(month) %>% 
  # Occupancy rate is booked nights divided by available nights
  summarize(occupancy = sum(total_booked_nights)/sum(total_available_nights))

  # We see a surge after June 2018
  base_plot <- line_plot(baseline, "occupancy", NULL, "Overall Occupancy Rate", "Occupancy Rate", "")


# Visualize the monthly occupancy rate segmented by region
by_region <- import %>% 
  group_by(city, month) %>%
  summarize(occupancy = sum(total_booked_nights)/sum(total_available_nights))

  # Los Angeles may be the causal region
  regional_plot <- line_plot(by_region, "occupancy", "city", "Occupancy Rate by City", "Occupancy Rate", "City")


# Visualize the monthly occupancy rate with and without the root cause region at the surge
counterfactual <- import %>% 
  mutate(condition = !(city == "Los Angeles" & month >= "2018-06-01")) %>%
  group_by(month) %>% 
  summarize(`With Los Angeles` = sum(total_booked_nights)/sum(total_available_nights),
            `Without Los Angeles After June 2018` = sum(total_booked_nights * as.numeric(condition))/sum(total_available_nights* as.numeric(condition))) %>% 
  pivot_longer(cols = c(`With Los Angeles`, `Without Los Angeles After June 2018`), names_to = "type", values_to = "occupancy")

  # Further confirmation about Los Angeles
  counterfactual_plot <- line_plot(counterfactual, "occupancy", "type", "Counterfactual Occupancy Rate With/Without Los Angeles", "Occupancy Rate", "")


# Within the root cause region, visualize occupancy rate by space type
los_angeles <- import %>% 
  filter(city == "Los Angeles") %>%
  group_by(month, space_type) %>% 
  summarize(occupancy = sum(total_booked_nights)/sum(total_available_nights))

  # All space types surged in this period, especially apartments and hotels
  space_type_plot <- line_plot(los_angeles, "occupancy", "space_type", "Los Angeles Occupancy Rates by Space Type", "Occupancy Rate", "Space Type")

  
# Within the root cause region, visualize contribution to bookings by space type
los_angeles_bookings <- import %>% 
  filter(city == "Los Angeles") %>%
  group_by(month, space_type) %>% 
  summarize(bookings = sum(total_booked_nights))

  # Based on bookings alone, apartments seem like the top contributor to occupancy rate and hotels do not seem significant
  los_angeles_bookings_plot <- line_plot(los_angeles_bookings, "bookings", "space_type", "Los Angeles Bookings by Space Type", "Bookings", "Space Type")


# Within the root cause region-space type, visualize contributions to bookings and availability
los_angeles_apartment <- import %>% 
  filter(city == "Los Angeles") %>%
  group_by(month, space_type) %>% 
  summarize(bookings = sum(total_booked_nights),
            availability = sum(total_available_nights)) %>% 
  group_by(month) %>% 
  mutate(`Contribution to LA Bookings` = prop.table(bookings),
         `Contribution to LA Availability` = prop.table(availability)) %>% 
  pivot_longer(cols = c(`Contribution to LA Bookings`, `Contribution to LA Availability`), names_to = "type", values_to = "rate") %>% 
  select(month, space_type, type, rate) %>% 
  filter(space_type == "Apartment")
  
  # Apartment's contribution to bookings increased and contribution to availability decreased, which would greatly affect occupancy rate
  los_angeles_apartments_plot <- line_plot(los_angeles_apartment, "rate", "type", "Apartment Spaces' Contributions to Bookings and Availability in Los Angeles", "Monthly Share", "Metric")


# Export plots
golden_ratio <- (1 + sqrt(5))/2
i <- 1
for (PLOT in list(base_plot, regional_plot, counterfactual_plot, space_type_plot, los_angeles_bookings_plot, los_angeles_apartments_plot)) {
  ggsave(PLOT, path = DATAROOT, filename = paste0("plot",i, ".pdf"), height = 8, width = 8 * golden_ratio, encoding = "ISOLatin9.enc")
  i <- i+1
}
