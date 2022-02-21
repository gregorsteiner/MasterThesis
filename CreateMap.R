########## GCreate Map ##########
Dat <- read.csv("ProductivityData.csv")


library(ggplot2)

colnames(Dat)[1] <- "state"

period <- c("2020-Q2", "2020-Q3", "2021-Q2", "2021-Q3")
DatPlot <- Dat[Dat$YearQtr %in% period,]


png("Proposal/ProdMap.png")

plot <- usmap::plot_usmap(regions = "states",
                  data = DatPlot,
                  values = "Prod")
  
plot$data[!complete.cases(plot$data), c("YearQtr")] <- c("2021-Q2", "2021-Q3") 

plot +
  facet_wrap(~ YearQtr) +
  scale_fill_viridis_c(option = "turbo",
                       name = "Productivity") + 
  theme(legend.position = "right",
        plot.margin = unit(c(0,0,0,0), "cm"))

dev.off()

