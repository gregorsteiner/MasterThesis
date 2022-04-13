
######## Preliminaries ########

source("AuxFunctions.R")
library(data.table)
library(usmap)
library(ggplot2)



######## Maps ########


# plot median income 
plot_usmap(data = seda.cov[year == 2018 & grade == 3,
                           .(inc50all = exp(lninc50all), fips = sedacounty)],
           values = "inc50all") +
  scale_fill_viridis_c(name = "Median Income (2018)")



# plot cumulative disasters
plot_usmap(data = fema.cum, values = "Disasters") +
  scale_fill_viridis_c(name = "Disasters")




