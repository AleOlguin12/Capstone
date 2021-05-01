
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Introduction

This package is the result of the Capstone Project of the JHU’s Coursera
specialization. It contains a few functions that process, clean and plot
in an ideal way the information contained in a dataframe with the
records of earthquakes registered in our planet, categorized by country,
number of deaths, magnitude, etc.

## Data

The mentioned dataframe is included in the package.

# Functions

## Cleaning

### eq\_clean\_data

This function takes the entire dataframe as argument, and it creates the
variable DATE as a class Date, out of the original variables: YEAR,
MONTH and DAY. It also transform the columns LATITUD and LONGITUD to
numeric class. This will be useful when we attempt to plot the
information.

### eq\_location\_clean

This function takes the original LOCATION\_NAME variable (which contains
the country, city and other relevant location information), and
eliminates the country information (which is redundant with another
original variable called COUNTRY), and transform the strings to title
cases (instead of all upcases)

## Plotting: Maps

The following functions also are for plotting information, but instead
of timelines we use a World Map. This way, we can visualize each
earthquake in the exact location it occurred.

### eq\_map

This function takes the earthquake dataframe as input (or a subset of
the dataframe), and plots a map with its information, indicating by
points the location of each specific earthquake.

### eq\_create\_label

Finally, this function creates a label to be displayed along with the
previous map. This label is a pop-up that appears when you place the
mouse on any earthquake point on the map.
