# This code is to replicate the analyses and findings from my 2019
# SOSS7092 project. Code developed by Monique Bennett, Wits University.

## Required packages ##
#install.packages(c("car", "MASS", "WDI", "democracyData"))
library(car)
library(MASS)
library(WDI)
library(democracyData)

## Load the data ##
load("CCG_dataset.RData")
CCGdata <- table
####################
### Summary data  
####################
tcg.sum <- data.frame(CCGdata$tcg, CCGdata$tcgadj, # dependent variable
                      CCGdata$tcghi, CCGdata$tcglo)

summary(tcg.sum)

indep.sum <- data.frame(CCGdata$federal, CCGdata$civil_lib, 
                        CCGdata$epiclimate, CCGdata$iea_rats, 
                        CCGdata$gdp_pc, CCGdata$itrade, 
                        CCGdata$iso14001, CCGdata$ngo_number,
                        CCGdata$co2_emissions)
summary(indep.sum)

robust.sum <- data.frame(CCGdata$voice_acc, CCGdata$pol_rights, 
                        CCGdata$polity, CCGdata$epi, 
                        CCGdata$pollution, CCGdata$greenaid, 
                        CCGdata$fdi, CCGdata$gdp)

summary(robust.sum)
####################
### Number of observations
####################
length(CCGdata$country_name) # 192 countries

sum(na.omit(CCGdata$tcg)) #14449 intiatives 

####################
### Transnational Climate Governance (TCG) (Dependent Variable)
####################
summary(CCGdata$tcg)

CCGdata$tcg.sqrt <- sqrt(CCGdata$tcg) # tcg transformed by square-root

CCGdata$tcgadj <- sqrt(CCGdata$tcgadj) # tcgadj transformed by square-root

###################
### Independent variables 
###################
plot(CCGdata$civil_lib) # civil liberty score
plot(CCGdata$federal) # federal or non-federal
plot(CCGdata$epiclimate) # EPI climate policy index
plot(CCGdata$iea_rats) # IEA ratifications
plot(CCGdata$gdp_pc) # GDP per capita
plot(CCGdata$itrade) # weighted international trade variable
plot(CCGdata$iso14001) # ISO 14001 regulation
plot(CCGdata$ngo_number) # INGOs
plot(CCGdata$co2_emissions) # CO_2 emissions

###################
### Trade 
###################
trade <- WDI(country = c("AFG", "ALB", "DZA", "AND", "AGO", "ATG", "ARG", "ARM", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN", "BTN", "BOL", "BIH", "BWA", "BRA", "BRN", "BGR", "BFA", "BDI", "KHM", "CMR", "CAN", "CPV", "CAF", "TCD", "CHL", "CHN", "COL", "COM", "COD", "COG", "CRI", "CIV", "HRV", "CUB", "CYP", "CZE", "DNK", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "ETH", "FJI", "FIN", "FRA", "GAB", "GMB", "GEO", "DEU", "GHA", "GRC", "GRD", "GTM", "GIN", "GNB", "GUY", "HTI", "HND", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL", "ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN", "KIR", "PRK", "KOR", "XK",  "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", "LUX", "MKD", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MRT", "MUS", "MEX", "FSM", "MDA", "MCO", "MNG", "MNE", "MAR", "MOZ", "MMR", "NAM", "NPL", "NLD", "NZL", "NIC", "NER", "NGA", "NOR", "OMN", "PAK", "PLW", "PAN", "PNG", "PRY", "PER", "PHL", "POL", "PRT", "QAT", "ROU", "RUS", "RWA", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP", "SVK", "SVN", "SLB", "SOM", "ZAF", "ESP", "LKA", "KNA", "LCA", "VCT", "SDN", "SUR", "SWZ", "SWE", "CHE", "SYR", "TJK", "TZA", "THA", "TLS", "TGO", "TON", "TTO", "TUN","TUR", "TKM", "TUV", "UGA", "UKR", "ARE", "GBR", "USA", "URY", "UZB", "VUT", "VEN", "VNM", "YEM", "ZMB", "ZWE"), indicator = c("GDP" = "NY.GDP.MKTP.CD", "Imports" = "NE.IMP.GNFS.CD","Exports" = "NE.EXP.GNFS.CD"), start = 2012, end = 2012)

tradeopen <- (trade$Exports) + (trade$Imports) / 2 *(trade$GDP)

CCGdata$tradeopen.log <- log(tradeopen)

summary(CCGdata$tradeopen.log)

###################
### ISO 14001
###################
iso.untrans <- (CCGdata$iso14001) * 10000

CCGdata$iso14001.sqrt <- sqrt(iso.untrans)

###################
### INGOs
###################
ngo.untrans <- (CCGdata$ngo_number) * 1300

CCGdata$ngo.sqrt <- sqrt(ngo.untrans)

###################
### Robustness check: variables transformations
###################
greenaid.bfrescale <- (CCGdata$greenaid) * 100000

start <- min(greenaid.bfrescale[greenaid.bfrescale > 0], na.rm = TRUE)

CCGdata$greenaid.log <- log(greenaid.bfrescale + start)

fdi.prescale <- (CCGdata$fdi) * 1000000000

CCGdata$fdi.log <- log(fdi.prescale)

###################
### Freedom House total score
###################
fh <- download_fh(include_territories = FALSE, verbose = FALSE)

countries <-  c("AFG", "ALB", "DZA", "AND", "AGO", "ATG", "ARG", "ARM", "AUS", "AUT", "AZE", "BHS", "BHR", "BGD", "BRB", "BLR", "BEL", "BLZ", "BEN", "BTN", "BOL", "BIH", "BWA", "BRA", "BRN", "BGR", "BFA", "BDI", "KHM", "CMR", "CAN", "CPV", "CAF", "TCD", "CHL", "CHN", "COL", "COM", "COD", "COG", "CRI", "CIV", "HRV", "CUB", "CYP", "CZE", "DNK", "DJI", "DMA", "DOM", "ECU", "EGY", "SLV", "GNQ", "ERI", "EST", "ETH", "FJI", "FIN", "FRA", "GAB", "GMB", "GEO", "DEU", "GHA", "GRC", "GRD", "GTM", "GIN", "GNB", "GUY", "HTI", "HND", "HUN", "ISL", "IND", "IDN", "IRN", "IRQ", "IRL", "ISR", "ITA", "JAM", "JPN", "JOR", "KAZ", "KEN", "KIR", "PRK", "KOR", "XK",  "KWT", "KGZ", "LAO", "LVA", "LBN", "LSO", "LBR", "LBY", "LIE", "LTU", "LUX", "MKD", "MDG", "MWI", "MYS", "MDV", "MLI", "MLT", "MHL", "MRT", "MUS", "MEX", "FSM", "MDA", "MCO", "MNG", "MNE", "MAR", "MOZ", "MMR", "NAM", "NPL", "NLD", "NZL", "NIC", "NER", "NGA", "NOR", "OMN", "PAK", "PLW", "PAN", "PNG", "PRY", "PER", "PHL", "POL", "PRT", "QAT", "ROU", "RUS", "RWA", "WSM", "SMR", "STP", "SAU", "SEN", "SRB", "SYC", "SLE", "SGP", "SVK", "SVN", "SLB", "SOM", "ZAF", "ESP", "LKA", "KNA", "LCA", "VCT", "SDN", "SUR", "SWZ", "SWE", "CHE", "SYR", "TJK", "TZA", "THA", "TLS", "TGO", "TON", "TTO", "TUN","TUR", "TKM", "TUV", "UGA", "UKR", "ARE", "GBR", "USA", "URY", "UZB", "VUT", "VEN", "VNM", "YEM", "ZMB", "ZWE")

fh$isocode3 <- countrycode(fh$fh_country, "country.name", "iso3c")

new.fh <- subset(fh[, c("isocode3", "fh_total", "status", 
                        "year", "cl", "pr", "fh_total_reversed")], 
                 year == 2012)

CCGdata.new <-  merge(CCGdata, new.fh, 
                      by.x = "iso_ccode", by.y = "isocode3")

summary(CCGdata.new$fh_total_reversed)

