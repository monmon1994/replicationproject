# This code is to replicate the analyses and findings from my 2019
# SOSS7092 project. Code developed by Monique Bennett, University of Witwatersrand.

## Required packages ##
#install.packages(c("stargazer", "jtools", "wesanderson"))
library(stargazer)
library(jtools)
library(wesanderson)

## Load the data ##
load("CCG_dataset.RData")
CCGdata <- table

####################
### Table 1: Descriptive statistics of dependent variables
####################
tcg.sum <- data.frame(CCGdata$tcg, CCGdata$tcgadj,CCGdata$tcghi, CCGdata$tcglo)

names(tcg.sum) <- c("TCG", "TCG (adjusted)", "TCG high CL", "TCG low CL")

stargazer(tcg.sum, title = "Descriptive Statistics of Dependent Variables",
          type = "latex", summary.stat = c("n", "mean", "sd", "min", "max"), 
          font.size = "small", no.space = TRUE, digits = 0)

####################
### Table 2: Descriptive statistics of independent variables
####################
indep.sum <- data.frame(CCGdata$federal, CCGdata$civil_lib, CCGdata$epiclimate, 
                        CCGdata$iea_rats,CCGdata$gdp_pc, CCGdata$itrade, CCGdata$iso14001, 
                        CCGdata$ngo_number,CCGdata$co2_emissions)

names(indep.sum) <- c("Federalism", "Civil Liberties (CL)", "EPI climate", 
                      "IEA ratifications", "GDP per capita", "Trade", "ISO 14001", 
                      "INGOs", "Carbon Dioxide")

stargazer(indep.sum, title = "Descriptive Statistics of Independent Variables", 
          type = "latex", summary.stat = c("n", "mean", "sd", "min", "max"), 
          font.size = "small", no.space = TRUE, digits = 1)
####################
### Table 3: Descriptive statistics of robustness checks
####################
robust.sum <- data.frame(CCGdata$voice_acc, CCGdata$pol_rights, CCGdata$polity, 
                         CCGdata$federal, CCGdata$epi, CCGdata$pollution, CCGdata$greenaid, 
                         CCGdata$fdi, CCGdata$gdp)

names(robust.sum) <- c("Voice", "Political Rights", "Polity", "EPI", 
                       "Air pollution", "Green aid", "FDI", "GDP")

stargazer(robust.sum, title = "Descriptive Statistics of other Independent Variables", 
          type = "latex", summary.stat = c("n", "mean", "sd", "min", "max"), font.size = "small", 
          no.space = TRUE, digits = 1)

####################
### Table 4: Article Regression Results
####################
stargazer(glm.tcg, glm.tcgadj, glm.tcghi, glm.tcglo, type = "latex", 
          title = "Article Regression Results", 
          dep.var.labels = c("TCG", "TCG (Adjusted)", "TCG (High CL)", "TCG (Low CL)"), 
          model.numbers = TRUE, no.space = TRUE)

####################
### Table 5: New Regression Results
####################
stargazer(glm.new.tcg, glm.new.tcgadj, glm.new.tcghi, glm.new.tcglo, type = "latex", 
          title = "New Regression Results", 
          dep.var.labels = c("TCG", "TCG (Adjusted)", "TCG (High CL)", "TCG (Low CL)"), 
          model.numbers = TRUE, no.space = TRUE)

####################
### Table 6: Alternative Explanatory Variable
####################
stargazer(glm.new.tcg, glm.fh, glm.voice, glm.polity, type = "latex",
          title = "Alternative Explanatory Variable", model.numbers = TRUE, no.space = TRUE)

####################
### Table 7: Robustness Checks
####################
stargazer(glm.1a, glm.2a, glm.3a, glm.4a, glm.5a, glm.6a, glm.7a, glm.8a, 
          type = "latex", title = "Robustness Checks", model.numbers = TRUE, 
          no.space = TRUE)

####################
### Figure 1: Effects plot of civil liberty and TCG
####################
effect_plot(glm.new.tcg, pred = civil_lib, interval = T, 
            outcome.scale = "response", x.label = "Civil liberty", 
            y.label = "TCG")

####################
### Figure 2: Effects plot of Freedom House total score and TCG
####################
effect_plot(glm.fh, pred = fh_total_reversed, interval = T, 
            outcome.scale = "response", x.label = "Freedom House total score", 
            y.label = "TCG")

####################
### Figure 3: Top 30 instances of participation across countries
####################
onlytcg <- subset(CCGdata[, c("country_name", "iso_ccode", "tcg")])

sort(onlytcg$tcg, decreasing = TRUE)

tcg.order <- order(onlytcg$tcg, decreasing = T)

tcg.sorted <- onlytcg[tcg.order,]

top30 <- tcg.sorted[1:30,]

dotchart(top30$tcg, cex = 0.5, ylim = 2600, labels = top30$country_name, 
         col = wes_palette("BottleRocket1", 30, type = "continuous"), pch = 19) 


