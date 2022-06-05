
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


fema.disasters <- readRDS("DisasterDataNoTerrorismNoCovid.RDS")


# color scheme
col <- c("firebrick", "cornflowerblue")



######## Disaster Count ########

dat.plot <- fema.disasters[, .(.N), by = .(Year = as.numeric(format(declarationDate, "%Y")))]
dat.plot <- dat.plot[Year != 2022]

pdf("DisasterCount.pdf", width = wid, height = 7 / 2.5)

par(mar = c(2, 2, 1, 1))
plot(dat.plot$Year, dat.plot$N, type = "n",
     xlab = "", ylab = "", cex.axis = 0.85)
grid()
lines(dat.plot$Year, dat.plot$N, lwd = 2, col = col[1])

dev.off()



######## Summary Statistics ########

# boxplots for dependent variables
pdf("DepVarsBoxplot.pdf",
    width = wid, height = 10 / 2.5)

layout(matrix(c(1, 2, 3), ncol = 1, byrow = TRUE), heights = c(4, 4, 1))
par(mar = c(2, 3, 1, 1))
boxplot(dat[subject == "mth", .("Overall" = cs_mn_all,
                "Black" = cs_mn_blk,
                "Hispanic" = cs_mn_hsp,
                "Female" = cs_mn_fem,
                "Econ. disadv." = cs_mn_ecd)],
        col = col[1], cex.axis = 1)
par(mar = c(2, 3, 1, 1))
boxplot(dat[subject == "rla", .("Overall" = cs_mn_all,
                                "Black" = cs_mn_blk,
                                "Hispanic" = cs_mn_hsp,
                                "Female" = cs_mn_fem,
                                "Econ. disadv." = cs_mn_ecd)],
        col = col[2], cex.axis = 1)


par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Mathematics", "RLA"),
       fill = col, cex = 1, inset = 0, horiz = TRUE)

dev.off()


# filter for time and exclude terrorism
fema.disastersSQ <- fema.disasters[syDeclared %in% 2009:2018]



vtable::sumtable(fema.disastersSQ[, .("Disaster Type" = factor(incidentType))],
                 out = "latex", file = "../TeX Files/DisasterTypes.tex",
                 anchor = "DisasterTypes", title = "Disasters from 2009 to 2018 by type",
                 fit.page = NA)


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





# Plot ethnic shares by relative time
pdf("EthnicComposition.pdf", width = wid, height = hei)

# add third color
col3 <- c(col, 3)

par(mar = c(4, 4, 1, 1))
layout(matrix(c(1, 2, 3, 3), nrow = 2, ncol = 2, byrow = TRUE),
       heights = c(4, 1))

invis.lapply(c("FEMA", "Storms"), function(type){
  if(type == "FEMA"){
    dat.ethnic <- dat[, lapply(.SD, mean, na.rm = TRUE),
                      by = .(RelTime),
                      .SDcols = c("perhsp", "perblk", "perwht")]
  }
  if(type == "Storms"){
    dat.ethnic <- dat[, lapply(.SD, mean, na.rm = TRUE),
                      by = .(RelTime = RelTimeStorm),
                      .SDcols = c("perhsp", "perblk", "perwht")]
  }
  
  dat.ethnic <- dat.ethnic[!is.na(RelTime)][order(RelTime)]
  
  
  matplot(dat.ethnic$RelTime, dat.ethnic[, -1], type = "n",
          xlab = "Years to treatment", ylab = "Share")
  abline(v = 0, lty = "dashed")
  grid()
  matlines(dat.ethnic$RelTime, dat.ethnic[, -1],
           col = col3, lty = 1, lwd = 2)
  
})

# add legend
par(mai=c(0,0,0,0))
plot.new()
legend(x = "center", legend = c("Hispanic", "Black", "White"),
       col = col3, lwd = 2, cex = 1, inset = 0, horiz = TRUE)


dev.off()







######## Application characteristics ########


# load assistance data
fema.assistance <- readRDS("AssistanceDataRawWithFIPS.RDS")

# filter for relevant time period
fema.disasters <- fema.disasters[syDeclared %in% c(2017, 2018)]

# create id (disasterNumber + fips)
fema.disasters[, fips := as.numeric(paste0(fipsStateCode, fipsCountyCode))]
fema.disasters[, dnFips := paste0(disasterNumber, fips)]
fema.assistance[, dnFips := paste0(disasterNumber, fips)]


# create application indicator (1 if county applied, 0 else)
fema.disasters[, Applied := as.numeric(dnFips %in% fema.assistance$dnFips)]

# create table 
res <- cbind("Number of Cases" = table(fema.disasters[, .("Disaster Type" = factor(incidentType))]),
             "Applied for Assistance (in %)" = 100 * tapply(fema.disasters$Applied, fema.disasters$incidentType, mean, na.rm = TRUE))
res <- rbind(res, "Total" = c(nrow(fema.disasters), 100 * mean(fema.disasters$Applied)))

# export as .tex table
writeLines(knitr::kable(res, digits = 2, format = "latex",
                        label = "AppsByType", caption = "Share of counties that applied for federal assistance following a disaster by disaster type (schoolyears 2016/2017 and 2017/2018)",
                        booktabs = TRUE),
           "../TeX Files/ApplicationsByType.tex")

# add covariates
fema.disasters <- merge(fema.disasters, assist.cov, by = "fips", all.x = TRUE, all.y = FALSE)

fema.disasters[, Applied := factor(Applied, levels = c(0, 1), labels = c("Did not apply", "Applied"))]


pdf("AssistanceCovBoxplot.pdf",
    width = wid, height = hei + 1)

par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
invis.Map(function(x, ylab){
  # create boxplot
  boxplot(x ~ Applied,
          data = fema.disasters, col = col,
          xlab = "", ylab = ylab, cex = 0.7)
  
}, fema.disasters[, .(MedInc2016, ShareDem2016, PovertyRate, SingleMother)],
c("Median Income (2016)", "Democratic Votes (2016 Election)",
  "Poverty Rate (2016)", "Share of Single Mothers (2016)"))

dev.off()






# logistic regression for assistance covariates
dat.app$MedInclog <- log(dat.app$MedInc2016)
dat.app[, AppliedNum := ifelse(Applied == "Applied", 1, 0)]
model.logit.ass <- fixest::feglm(AppliedNum ~ ShareDem2016 + MedInclog
                                 + PovertyRate + SingleMother, data = dat.app[Disasters > 0],
                                 family = binomial("logit"))


fixest::etable(list(model.logit.ass), file = "../TeX Files/ResultsLogit.tex", replace = TRUE,
               label = "ResultsLogit", title = "Determinants of Assistance Application",
               dict = c(ShareDem2016 = "Share of democratic voters (2016)",
                        ShareDem2008 = "Share of democratic voters (2008)",
                        MedInclog = "Median Income (logs)",
                        PovertyRate = "Poverty Rate", 
                        SingleMother = "Share of single mothers",
                        AppliedNum = "Applied"))





