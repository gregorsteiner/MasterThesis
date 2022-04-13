
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


# plot test scores
plot_usmap(data = seda.comb[year == 2016 & grade == 3 & subject == "mth",
                                   .(Scores = gcs_mn_all, fips = sedacounty, subject)],
                  values = "Scores") +
  scale_fill_viridis_c(name = "Test Scores")

  

# plot cumulative disasters
plot_usmap(data = fema.cum, values = "Disasters") +
  scale_fill_viridis_c(name = "Disasters")



