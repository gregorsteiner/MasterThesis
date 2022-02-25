

# get data
cps.dat <- cpsR::get_basic(
  year = 2021,
  month = 7,
  vars = c("gestfips", "gtco", "prpertyp", "pemlr", "peabsrsn")
)


# only keep adult civilian household members (prpertyp == 2)
cps.dat <- cps.dat[cps.dat$prpertyp == 2, ]

# compute sick leave share by state
sickLeaves <- aggregate(cps.dat$peabsrsn, list(cps.dat$gestfips), \(x) mean(x == 5))

# change column names
colnames(sickLeaves) <- c("stateFIPS", "SickLeaveShare")

# add state names
sickLeaves[sickLeaves$stateFIPS != 11, "state"] <- state.name
sickLeaves[sickLeaves$stateFIPS == 11, "state"] <- "District of Columbia"

# Make Map
library(ggplot2)

plot <- usmap::plot_usmap(regions = "states",
                          data = sickLeaves,
                          values = "SickLeaveShare")

plot +
  scale_fill_viridis_c(option = "turbo",
                       name = "Share of \n Sick Leaves") + 
  theme(legend.position = "right",
        plot.margin = unit(c(0,0,0,0), "cm"))






