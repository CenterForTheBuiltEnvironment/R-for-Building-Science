# R for Building Science

This repo contains the materials for the ARCH 249 Module 3. 

## Week 1 April 08 - Introduction to R (Tom Parkinson, Carlos Duarte, Federico Dallo; 3 hours)

This class will introduce you to the R programming language and Jupyter Notebooks. We’ll briefly cover fundamentals like syntax, data types, functions, packages, and development environments. We’ll also walk through the philosophy of the Tidyverse collection of packages, including data visualizations with ggplot2. By the end of the class you’ll be able to write, edit and run a basic R script in Jupyter Notebooks.

### Readings:

- Choosing Python or R for Data Analysis? An Infographic
- Wickham H. & Grolemund, G. (2022) R for Data Science [Chapters 1 to 8]
- Wickham H. (2021) The tidyverse style guide
- RStudio Cheatsheets
- Awesome R Learning Resources
- Wickham, H. (2022) ggplot2: Elegant Graphics for Data Analysis
- The R Graph Gallery
 
## Week 2 April 15 - Working with survey data in R (Tom Parkinson; 3 hrs)

In this class you’ll learn how to analyse responses to survey questions. We’ll establish a workflow for importing data into R, data wrangling, producing summary statistics, and simple visualizations. Specifically, we’ll work with categorical data from the CBE Occupant Survey with a focus on strings and factors. By the end of the class you’ll be familiar with basic data wrangling and working with different data types in R.

### Readings:
- Graham, L. T., Parkinson, T., & Schiavon, S. (2021). Lessons learned from 20 years of CBE’s occupant surveys. Buildings and Cities, 2(1), 166–184.
- Pew Research Center, Writing Survey Questions
- [Optional] Leaman, A., & Bordass, B. (2001). Assessing building performance in use 4: The Probe occupant surveys and their implications. Building Research & -Information, 29(2), 129–143.
- Wickham H. & Grolemund, G. (2022) R for Data Science [Chapters 9 to 15]
- Engel, C. (2021) Data Wrangling with R 
- RStudio Cheatsheets for dplyr, tidyr, stringr, and forcats

## Week 3 April 22 - During the class will be shown how to work with date and time with R. We will use the Tidyverse programming style and deepen the Lubridate library tools to: 

1) describe a date-time object properties (e.g. format, time-zone, etc.); 
2) create a date-time object for time series analysis in R; 
3) manipulate time series data and its limitations (e.g. downscale to monthly averaging, upscaling using interpolation techniques *seasonality); 
4) predict new data using a forecasting model (e.g. ARIMA); 
5) visualization of time series data using the ggplot2 library. During the class we will use some real-world example dataset (e.g. WMO, EPlus).

### Readings:

- H. Wickham & G. Grolemund, R for Data Science. O’Reilly. https://www.tidyverse.org/learn/
- https://r4ds.had.co.nz/dates-and-times.html
- https://otexts.com/fpp3/


## Week 4 April 29 - Whole Building Simulation and Analysis in R(Carlos Duarte; 3 hrs)

In this class you will learn how to create building performance data and apply your timeseries analysis techniques learned in previous classes to help you design high performance buildings. We will use the eplusr package to import an EnergyPlus model file, modify it to create a parametric study, retrieve simulation outputs, and analyze them to make informed building design decisions.

### Readings:
- Jia, H., and A. Chong. 2021. Eplusr: A Framework for Integrating Building Energy Simulation and Data-Driven Analytics. Energy and Buildings 237 (April): 110757. https://linkinghub.elsevier.com/retrieve/pii/S0378778821000414.
- Duarte, C., P. Raftery, S. Schiavon, and F. Bauman. 2018. How High Can You Go? Determining the Highest Supply Water Temperature for High Thermal Mass Radiant - Cooling Systems in California. In Proceedings of the 4th International Conference On Building Energy & Environment 2018, 585–590. Melbourne, Australia. https://escholarship.org/uc/item/0s06q03g.

### Resources:
- EnergyPlus Input Output Reference
- eplusr user’s manual
- Annual weather files for simulation




