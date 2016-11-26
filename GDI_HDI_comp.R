library(readr)
library(ggplot2)
library(hash)
setwd("~/GitHub/GII_Correlations/")
##source for gdi, hdi, gii: http://hdr.undp.org/en/content/human-development-index-hdi
##source for pm: http://www.pewforum.org/2011/01/27/table-muslim-population-by-country/
##source for gdpPC: https://www.cia.gov/library/publications/the-world-factbook/rankorder/rawdata_2004.txt

#read in files
gdi <- read.csv("GDI.csv", stringsAsFactors = FALSE)
gii <- read.csv("gii.csv", stringsAsFactors = FALSE)
hdi <- read.csv("HDI.csv", stringsAsFactors = FALSE)
pm <- read.csv("PercentMuslim.csv", stringsAsFactors = FALSE)
gdpPC <- read.csv("gdpPerCapita.csv", stringsAsFactors = FALSE)

#change case of country names
gdi$Country <- sapply(gdi$Country, toupper)
gii$Country <- sapply(gdi$Country, toupper)
hdi$Country <- sapply(hdi$Country, toupper)
pm$Country <- sapply(pm$Country, toupper)
gdpPC$Country <- sapply(gdpPC$Country, toupper)

#check which countries dont match between files
hdiPmCountries <- sapply(pm$Country, function(y) grepl(y, hdi$Country))
hdiPmCountriesS <- apply(hdiPmCountries, 1, any)
hdiGDPCountries <- sapply(gdpPC$Country, function(y) grepl(y, hdi$Country))
hdiGDPCountriesS <- apply(hdiGDPCountries, 1, any)

#fix country names to match between all files
pmCountryConversion <- hash(c("ST. KITTS AND NEVIS", "BOSNIA-HERZEGOVINA", "ST. LUCIA", "ST. VINCENT AND THE GRENADINES", "PALESTINIAN TERRITORIES", "VIETNAM", "CAPE VERDE", "LAOS", "BURMA (MYANMAR)", "IVORY COAST", "RUSSIA", "REPUBLIC OF MACEDONIA", "BRUNEI", "GUINEA BISSAU", "SYRIA"), 
                          c("SAINT KITTS AND NEVIS", "BOSNIA AND HERZEGOVINA", "SAINT LUCIA", "SAINT VINCENT AND THE GRENADINES", "PALESTINE, STATE OF", "VIET NAM", "CABO VERDE", "LAO PEOPLE'S DEMOCRATIC REPUBLIC", "MYANMAR", "CÔTE D'IVOIRE", "RUSSIAN FEDERATION", "THE FORMER YUGOSLAV REPUBLIC OF MACEDONIA", "BRUNEI DARUSSALAM", "GUINEA-BISSAU", "SYRIAN ARAB REPUBLIC"))
pmConverted <- data.frame(Country=sapply(pm$Country, function(z) if(has.key(z, pmCountryConversion)){pmCountryConversion[[z]]}else{z}), PercentMuslim=pm$PercentMuslim, stringsAsFactors = FALSE) 
diCountryConversion <- hash(c("KOREA (REPUBLIC OF)", "KOREA (DEMOCRATIC PEOPLE'S REP. OF)", "MICRONESIA (FEDERATED STATES OF)", "CONGO (DEMOCRATIC REPUBLIC OF THE)", "CONGO", "HONG KONG, CHINA (SAR)", "IRAN (ISLAMIC REPUBLIC OF)", "MOLDOVA (REPUBLIC OF)", "TANZANIA (UNITED REPUBLIC OF)", "VENEZUELA (BOLIVARIAN REPUBLIC OF)", "BOLIVIA (PLURINATIONAL STATE OF)"), 
                            c("SOUTH KOREA", "NORTH KOREA", "FEDERATED STATES OF MICRONESIA", "CONGO", "REPUBLIC OF CONGO", "HONG KONG", "IRAN", "MOLDOVA", "TANZANIA", "VENEZUELA", "BOLIVIA"))
gdpCountryConversion <- hash(c("KOREA, SOUTH", "KOREA, NORTH", "CZECHIA", "VIETNAM", "BAHAMAS, THE", "MICRONESIA, FEDERATED STATES OF", "CONGO, DEMOCRATIC REPUBLIC OF THE", "CONGO, REPUBLIC OF THE", "BURMA", "COTE D'IVOIRE", "GAMBIA, THE", "BRUNEI", "LAOS", "RUSSIA", "MACEDONIA", "SYRIA"),
                             c("SOUTH KOREA", "NORTH KOREA", "CZECH REPUBLIC", "VIET NAM", "BAHAMAS", "FEDERATED STATES OF MICRONESIA", "CONGO", "REPUBLIC OF CONGO", "MYANMAR", "CÔTE D'IVOIRE", "GAMBIA", "BRUNEI DARUSSALAM", "LAO PEOPLE'S DEMOCRATIC REPUBLIC", "RUSSIAN FEDERATION", "THE FORMER YUGOSLAV REPUBLIC OF MACEDONIA", "SYRIAN ARAB REPUBLIC"))
hdiConverted <- data.frame(Country=sapply(hdi$Country, function(z) if(has.key(z, diCountryConversion)){diCountryConversion[[z]]}else{z}), HDIndex=hdi$HDIndex, stringsAsFactors = FALSE) 
gdiConverted <- data.frame(Country=sapply(gdi$Country, function(z) if(has.key(z, diCountryConversion)){diCountryConversion[[z]]}else{z}), GDIndex=gdi$GDIndex, stringsAsFactors = FALSE) 
giiConverted <- data.frame(Country=sapply(gii$Country, function(z) if(has.key(z, diCountryConversion)){diCountryConversion[[z]]}else{z}), GIIndex=gii$GIIndex, stringsAsFactors = FALSE) 
gdpConverted <- data.frame(Country=sapply(gdpPC$Country, function(z) if(has.key(z, gdpCountryConversion)){gdpCountryConversion[[z]]}else{z}), GDPPerCapita=gdpPC$GDPPerCapita, stringsAsFactors = FALSE) 

#recheck that conversion worked
hdiPmCountriesCon <- sapply(pmConverted$Country, function(y) grepl(y, hdiConverted$Country))
hdiPmCountriesSCon <- apply(hdiPmCountriesCon, 1, any)

#make datasheet with all data
countryData <- merge(merge(merge(giiConverted, hdiConverted), pmConverted), gdpConverted)
countryDataF <- countryData[complete.cases(countryData),]

#plot and show correlations
ggplot(countryDataF, aes(x=PercentMuslim, y=GIIndex)) + geom_point() + 
  ggtitle("Relating Muslim Population to UN Gender Inequality Index Across 155 Countries, r=.29")
cor(countryDataF$GIIndex, countryDataF$PercentMuslim)
ggplot(countryDataF, aes(x=HDIndex, y=GIIndex)) + geom_point() + 
  ggtitle("Relating UN Human Development Index to UN Gender Inequality Index Across 155 Countries, r=-.87")
cor(countryDataF$GIIndex, countryDataF$HDIndex)
ggplot(countryDataF, aes(x=GDPPerCapita, y=GIIndex)) + geom_point() + 
  ggtitle("Relating GDPPerCapita to UN Gender Inequality Index Across 155 Countries, r=.74")
cor(countryDataF$GDPPerCapita, countryDataF$HDIndex)
