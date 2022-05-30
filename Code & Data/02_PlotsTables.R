
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
fema.assistance[, year := dplyr::case_when(
  # if in september to december add 1 to the year (Schoolyear x/x+1 is x+1)
  as.numeric(format(declarationDate, "%m")) %in% 9:12 ~ as.numeric(format(declarationDate, "%Y")) + 1,
  as.numeric(format(declarationDate, "%m")) %in% 1:3 ~ as.numeric(format(declarationDate, "%Y")),
  as.numeric(format(declarationDate, "%m")) %in% 4:8 ~ NA_real_
)]

fema.assistance[, fipsyear := paste0(fips, year)]

# extract years that overlap with the assistance data
dat.app <- dat[year %in% unique(fema.assistance$year)]

# create fipsyear
dat.app[, fipsyear := paste0(fips, year)]

# add application dummy
dat.app[, Applied := as.numeric(fipsyear %in% fema.assistance$fipsyear)]


# check if there are cases where they had a disaster but did not apply
boolDis <- dat.app$Disasters > 0
boolApp <- dat.app$Applied == 0

# proportion
sum(boolDis & boolApp, na.rm = TRUE) / sum(boolDis, na.rm = TRUE)

# add some covariate data
dat.app <- merge(dat.app, assist.cov, by = "fips", all.x = TRUE, all.y = FALSE)



# set applicant status as factor
dat.app[, Applied := factor(Applied, levels = c(0, 1), labels = c("Did not apply", "Applied"))]


pdf("AssistanceCovBoxplot.pdf",
    width = wid, height = hei + 1)

par(mfrow = c(2, 2), mar = c(3, 4, 1, 1))
invis.Map(function(x, ylab){
  # create boxplot
  boxplot(x ~ Applied,
          data = dat.app[Disasters > 1], col = col,
          xlab = "", ylab = ylab, cex = 0.7)
  
}, dat.app[Disasters > 1, .(MedInc2016, ShareDem2016, PovertyRate, SingleMother)],
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





