
######## Preliminaries ########

source("00_AuxFunctions.R")
library(data.table)
library(usmap)
library(ggplot2)

hei <- 10 / 2.5
wid <- 16 / 2.5

dat <- setDT(readRDS("Data.RDS"))
assist <- setDT(readRDS("AssistanceData.RDS"))
assist.cov <- setDT(readRDS("AssistanceCovData.RDS"))

# color scheme
col <- c("firebrick", "cornflowerblue")

######## Summary Statistics ########

# dataset summary
dat.summary <- dat[, .(Disasters, Treatment = factor(DisasterTreat),
                       "Subject" = factor(subject, labels = c("Mathematics", "RLA")),
                       "Mean test score" = cs_mn_all,
                       "Mean test score (black students)" = cs_mn_blk,
                       "Mean test score (hispanic students)" = cs_mn_hsp,
                       "Mean test score (female students)" = cs_mn_fem,
                       "Mean test score (econ. disadv. students)" = cs_mn_ecd)]

# vtable::sumtable(dat.summary,
#                  out = "latex", file = "../TeX Files/SummaryStats.tex", anchor = "SumStats")


# boxplots for dependent variables
pdf("DepVarsBoxplot.pdf",
    width = wid, height = hei)

par(mar = c(3, 3, 1, 1))
boxplot(dat[, .("Overall" = cs_mn_all,
                "Black" = cs_mn_blk,
                "Hispanic" = cs_mn_hsp,
                "Female" = cs_mn_fem,
                "Econ. disadv." = cs_mn_ecd)],
        col = col)

dev.off()


# types of natural disasters
fema.disasters <- setDT(rfema::open_fema("DisasterDeclarationsSummaries",
                                         ask_before_call = FALSE))

# filter for time and exclude terrorism
fema.disasters <- fema.disasters[fyDeclared %in% 2009:2018 & incidentType != "Terrorist"]



vtable::sumtable(fema.disasters[, .("Disaster Type" = factor(incidentType))],
                 out = "latex", file = "../TeX Files/DisasterTypes.tex",
                 anchor = "DisasterTypes", title = "Disasters from 2009 to 2018 by type",
                 fit.page = NA)


######## Application characteristics ########

# voter share and median income by applicant status


# png("AssistanceCovDensity.png",
#     width = wid, height = 16, units = "cm", res = 1200)
# 
# col <- c(3, 4)
# 
# par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))
# invis.Map(function(x, xlab){
#   # filter by applicant status and remove nas
#   bool1 <- assist.cov$AssistanceApplicant == 1 & !is.na(x)
#   bool2 <- assist.cov$AssistanceApplicant == 0 & !is.na(x)
#   # plot
#   plot(density(x[bool2]),
#        main = "", xlab = xlab, type = "n", cex = 0.8)
#   grid()
#   
#   lines(density(x[bool1]),
#         col = col[1], lwd = 2)
#   lines(density(x[bool2]),
#         col = col[2], lwd = 2)
#   # add legend
#   legend("topright", legend = c("Applied", "Did not apply"),
#          lwd = 2, col = col)
# }, assist.cov[, .(MedInc2016, ShareDem2016, PovertyRate, SingleMother)],
# c("Median Income (2016)", "Democratic Votes (2016 Election)",
#   "Poverty Rate (2016)", "Share of Single Mothers (2016)"))
# 
# dev.off()



# Boxplots

# set applicant status as factor
assist.cov[, AssistanceApplicant := factor(AssistanceApplicant,
                                           levels = c(0, 1),
                                           labels = c("Did not apply", "Applied"))]



pdf("AssistanceCovBoxplot.pdf",
    width = wid, height = wid)

par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
invis.Map(function(x, ylab){
  # create boxplot
  boxplot(x ~ AssistanceApplicant,
          data = assist.cov, col = col,
          xlab = "", ylab = ylab)
  
}, assist.cov[, .(MedInc2016, ShareDem2016, PovertyRate, SingleMother)],
c("Median Income (2016)", "Democratic Votes (2016 Election)",
  "Poverty Rate (2016)", "Share of Single Mothers (2016)"))

dev.off()


######## Parallel Trends Plots ########

# loop over fema and storm data
invis.lapply(c("FEMA", "Storm"), function(type){
  
  # loop over both subjects
  invis.Map(function(sub, name){
    
    file <- paste0("ParTrendsPlot", name, type, ".pdf")
    pdf(file, width = 15 / 2.5, height = 20 / 2.5)
    
    
    par(mar = c(2, 4, 1, 1))
    layout(matrix(c(1:11, 11), nrow = 6, ncol = 2, byrow = TRUE), heights = c(rep(4, 5), 1))
    
    # loop over cohorts
    invis.Map(function(cohort){
      
      # filter by subject
      dat.int <- dat[subject == sub]
      
      # split into treatment and control
      if(type == "FEMA") dat.int[, Group := ifelse(TreatStart == cohort,
                                                   "Treatment", ifelse(TreatStart == 3000,
                                                                       "Control", NA))]
      else dat.int[, Group := ifelse(TreatStartStorm == cohort,
                                     "Treatment", ifelse(TreatStartStorm == 3000,
                                                         "Control", NA))]
      
      
      # add relative time
      dat.int[, RelTime := year - cohort ]
      
      # aggregate by relative time and group
      dat.int <- dat.int[, .(Mean = mean(cs_mn_all, na.rm = TRUE)),
                         by = .(RelTime, Group)]
      
      # sort by relative time to make sure the plot looks fine
      dat.int <- dat.int[order(dat.int$RelTime)]
      
      # plot
      plot(dat.int[Group == "Treatment", RelTime],
           dat.int[Group == "Treatment", Mean],
           xlab = "Years to treatment", ylab = "Mean Score", 
           main = paste0("First treated in ", cohort),
           ylim = range(dat.int$Mean), type = "n")
      
      grid()
      abline(v = 0, col = 1, lty = "dashed")
      
      lines(dat.int[Group == "Treatment", RelTime],
            dat.int[Group == "Treatment", Mean],
            col = col[1], lwd = 2)
      lines(dat.int[Group == "Control", RelTime],
            dat.int[Group == "Control", Mean],
            col = col[2], lwd = 2)
      
      
    
    }, 2009:2018)
    
    par(mai=c(0,0,0,0))
    plot.new()
    legend(x = "center", legend = c("Treatment", "Control"),
           col = col, lwd = 2, cex = 1, inset = 0, horiz = TRUE)
    
    dev.off()
    
  }, c("mth", "rla"), c("Mathematics", "RLA"))

})


######## Maps ########


# plot cumulative disasters
dat[, CumuDisasters := cumsum(Disasters), by = .(fips, grade, subject)]
fema.cum <- aggregate(list("Disasters" = as.numeric(dat$CumuDisasters)),
                      list("fips" = dat$fips), function(x) {
                        
                        res <- x[!is.na(x)][length(x[!is.na(x)])]
                        if(length(res) == 0) return(0)
                        
                        return(res)
                        
                        })

pdf("DisasterMap.pdf",
    width = wid, height = hei)

plot_usmap(data = fema.cum, values = "Disasters") +
  scale_fill_viridis_c(name = "", option = "H",
                       breaks = c(0, 5, 10, max(fema.cum$Disasters, na.rm = TRUE))) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()



# plot cumulative Storms
dat[, CumuStorms := cumsum(Storms), by = .(fips, grade, subject)]
storms.cum <- aggregate(list("Storms" = as.numeric(dat$CumuStorms)),
                        list("fips" = dat$fips), function(x) {
                          res <- x[!is.na(x)][length(x[!is.na(x)])]
                          if(length(res) == 0) return(0)
                          
                          return(res)
                        })

pdf("StormMap.pdf",
    width = wid, height = hei)

plot_usmap(data = storms.cum, values = "Storms") +
  scale_fill_viridis_c(name = "", option = "H",
                       breaks = c(0, 5, 10, 15, max(storms.cum$Storms))) +
  theme(legend.position = "right",
        legend.key.size = grid::unit(1, "cm"),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 14),
        plot.margin= grid::unit(c(0,0,0,0), "mm"))

dev.off()


# plot assistance received

dat.plot <- melt(assist[, .("Damage" = sum(totalDamage, na.rm = TRUE) + 1,
                            "Assistance" = sum(federalAssistance, na.rm = TRUE) + 1),
                        by = .(fips)],
                 id.vars = c("fips"), measure.vars = c("Damage", "Assistance"))

pdf("AssistanceMap.pdf",
    width = wid, height = hei)

plot_usmap(data = dat.plot,
           values = "value") +
  scale_fill_viridis_c(name = "", trans = "log", option = "H",
                       breaks = c(1e1, 1e3, 1e5, 1e7, 1e9)) +
  facet_grid(cols = vars(variable)) + 
  theme(legend.position = c(0.4, -0.35),
        legend.key.size = grid::unit(0.4, "cm"),
        legend.text = element_text(size = 10),
        legend.title = element_text(size = 14),
        legend.direction = "vertical",
        plot.margin = grid::unit(c(0,0,0,0), "mm"),
        strip.text.x = element_text(size = 12))
dev.off()




