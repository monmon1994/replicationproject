# This code is to replicate the analyses and findings from my 2019
# SOSS7092 project. Code developed by Monique Bennett, University of Witwatersrand.

## Required packages ##
install.packages(c("car", "MASS"))
library(car)
library(MASS)

## Load the data ##
load("CCG_dataset.RData")
CCGdata <- table
####################
### Replication of Andonova, Hale and Roger (2017) 
####################
glm.tcg <- glm.nb(tcg ~ civil_lib + federal + epiclimate + 
                  iea_rats + gdp_pc + itrade + iso14001 +
                  ngo_number + co2_emissions, 
                data = CCGdata, method = "glm.fit")
summary(glm.tcg)

glm.tcgadj <- glm.nb(tcgadj ~ civil_lib + federal + epiclimate +
                  iea_rats + gdp_pc + itrade + iso14001 +
                  ngo_number + co2_emissions, method = "glm.fit",
                data = CCGdata)

glm.tcghi <- glm.nb(tcghi ~ federal + epiclimate + iea_rats +
                  gdp_pc + itrade + iso14001 + ngo_number +
                  co2_emissions, method = "glm.fit", 
                  data = CCGdata)

glm.tcglo <- glm.nb(tcglo ~ federal + epiclimate + iea_rats +
                  gdp_pc + itrade + iso14001 + ngo_number +
                  co2_emissions, method = "glm.fit", 
                  data = CCGdata)

####################
### Replication Diagnostics for main regressions 
####################
plot(glm.tcg, caption = NA, cex = 0.5, cex.id = 0.5)
plot(glm.tcgadj, caption = NA, cex = 0.5, cex.id = 0.5)

residualPlots(glm.tcg, cex=0.5, layout=NA, plot = FALSE)
residualPlots(glm.tcgadj, cex=0.5, layout=NA, plot = FALSE)
# set plot = TRUE to view residual plots

influencePlot(glm.tcg, pch = 1, id=list(method = "noteworthy", cex=1))
influencePlot(glm.tcgadj, pch = 1, id=list(method = "noteworthy", cex=1))

####################
### New regressions using transformed variables
####################
glm.new.tcg <- glm.nb(tcg ~ civil_lib + federal + epiclimate + 
                        iea_rats + gdp_pc + tradeopen.log + iso14001.sqrt +
                        ngo.sqrt + co2_emissions, method = "glm.fit",
                      data = CCGdata)

glm.new.tcgadj <- glm.nb(tcgadj ~ civil_lib + federal + epiclimate + 
                        iea_rats + gdp_pc + tradeopen.log + iso14001.sqrt +
                        ngo.sqrt + co2_emissions, method = "glm.fit",
                      data = CCGdata)

glm.new.tcghi <- glm.nb(tcghi ~ federal + epiclimate + 
                        iea_rats + gdp_pc + tradeopen.log + iso14001.sqrt +
                        ngo.sqrt + co2_emissions, method = "glm.fit",
                      data = CCGdata)

glm.new.tcglo <- glm.nb(tcglo ~ federal + epiclimate + 
                        iea_rats + gdp_pc + tradeopen.log + iso14001.sqrt +
                        ngo.sqrt + co2_emissions, method = "glm.fit",
                      data = CCGdata)

####################
### New regressions diagnostics
####################
plot(glm.new.tcg, caption = NA, cex = 0.5, cex.id = 0.5)
plot(glm.new.tcgadj, caption = NA, cex = 0.5, cex.id = 0.5)

residualPlots(glm.new.tcg, cex=0.5, layout=NA, plot = FALSE)
residualPlots(glm.new.tcgadj, cex=0.5, layout=NA, plot = FALSE)
# set plot = TRUE to view residual plots

influencePlot(glm.new.tcg, pch = 1, id=list(method = "noteworthy", cex=1))
influencePlot(glm.new.tcgadj, pch = 1, id=list(method = "noteworthy", cex=1))

####################
### ANOVA test 
####################
anova(glm.tcg, glm.new.tcg)

anova(glm.tcgadj, glm.new.tcgadj)

anova(glm.tcghi, glm.new.tcghi)

anova(glm.tcglo, glm.new.tcglo)

###################
### Alternative explanatory variable regressions
##################
glm.fh <- glm.nb(tcg ~ fh_total_reversed + federal + 
                      epiclimate + iea_rats + gdp_pc + 
                      tradeopen.log + iso14001.sqrt + ngo.sqrt + 
                      co2_emissions, method = "glm.fit", 
                    data = CCGdata.new)

glm.voice <- glm.nb(tcg ~ voice_acc + federal + 
                      epiclimate + iea_rats + gdp_pc + 
                      tradeopen.log + iso14001.sqrt + ngo.sqrt + 
                      co2_emissions, method = "glm.fit", 
                    data = CCGdata.new)

glm.polity <- glm.nb(tcg ~ polity + federal + 
                      epiclimate + iea_rats + gdp_pc + 
                      tradeopen.log + iso14001.sqrt + ngo.sqrt + 
                      co2_emissions, method = "glm.fit", 
                    data = CCGdata.new)
###################
### Robustness checks 
##################
glm.1a <- glm.nb(tcg ~ civil_lib + federal + iea_rats +
                   gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + co2_emissions + pollution, 
                 data = CCGdata)

glm.2a <- glm.nb(tcg ~ civil_lib + federal + iea_rats +
                   gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + co2_emissions + epi, data = CCGdata)

glm.3a <- glm.nb(tcg ~ federal + epiclimate + iea_rats + 
                   gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + co2_emissions + polity, data = CCGdata)

glm.4a <- glm.nb(tcg ~ federal + epiclimate + iea_rats +
                   gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + co2_emissions + voice_acc, 
                 data = CCGdata)

glm.5a <- glm.nb(tcg ~ federal + epiclimate + iea_rats +
                   gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + co2_emissions + pol_rights, 
                 data = CCGdata)

glm.6a <- glm.nb(tcg ~ civil_lib + federal + epiclimate
                 + iea_rats + gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + gdp, data = CCGdata)

glm.7a <- glm.nb(tcg ~ civil_lib + federal + epiclimate
                 + iea_rats + gdp_pc + tradeopen.log + 
                   iso14001.sqrt + ngo.sqrt + co2_emissions + fdi.log,
                 data = CCGdata)

glm.8a <- glm.nb(tcg ~ civil_lib + federal + epiclimate
                 + iea_rats + gdp_pc + tradeopen.log + iso14001.sqrt + 
                   ngo.sqrt + co2_emissions + greenaid.log,
                 data = CCGdata)