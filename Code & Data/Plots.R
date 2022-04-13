
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(usmap)



######## Map of test results by state ########


dat.seda$fips <- dat.seda$sedacounty

library(ggplot2)

# plot
plot_usmap(data = dat.seda, values = "gcs_mn_all") +
  scale_fill_viridis_c(name = "Avg. Test Scores") +
  facet_grid(c("subject", "grade"))



