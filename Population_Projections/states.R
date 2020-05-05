install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(readr)

# FIPS #
all_geocodes_v2017 <- read_excel("all-geocodes-v2017.xlsx")
all_geocodes_v2017 <- rename(all_geocodes_v2017,`Name` = `Area Name (including legal/statistical area description)`)
## get just rows for county FIPS
County_geocode <- all_geocodes_v2017[grep("County", all_geocodes_v2017$Name), ]
County_geocode <- rename(County_geocode, County = Name)
County_geocode$County <- str_replace(County_geocode$County, " County", "")
County_geocode <- select(County_geocode, -c(1,4,5,6))

# Arizona #
arizona_projected_2018_2055 <- read_excel("arizona_projected_2018_2055.xlsx")
arizona_projected_2018_2055 <- select(arizona_projected_2018_2055, -c(2,3,4,5))
# reshape table
arizona_county <- gather(arizona_projected_2018_2055, key = "County", value = "Pop_projection", `Apache County`:`Yuma County`)
arizona_county$County <- str_replace(arizona_county$County, " County", "")
# join FIPS for Arizona
state_FIPS <- read_excel("State FIPS.xlsx")
Arizona_FIPS <- filter(state_FIPS, State == "AZ")
Arizona_FIPS <- rename(Arizona_FIPS, County = Name)
arizona_county <- merge(arizona_county, Arizona_FIPS)
write.csv(arizona_county, file = "arizona_formated.csv")

# California #
California_projections_1970_2050 <- read_csv("California_projections_1970_2050.csv")
California_projections_1970_2050 <- select(California_projections_1970_2050, -c(5, 6))
CA_Grouped <- group_by(California_projections_1970_2050, fips, year)
CA <- summarise(CA_Grouped, sum = sum(pop_total))
sum(is.na(California_projections_1970_2050))
sum(is.na(CA))
CA <- filter(CA, !is.na(fips))
CA <- rename(CA, `Pop_projection` = `sum`)
CA <- rename(CA, `Year` = `year`)
CA <- rename(CA, `FIPS` = `fips`)
write.csv(CA, file = "california_formatted.csv")

# Colorado #
Colorado_projections_2000_2050 <- read_excel("Colorado_projections_2000_2050.xls")
colorado_county <- gather(Colorado_projections_2000_2050, key = "Year", value = "Pop_projection", `2000`:`2050`)
colorado_county <- rename(colorado_county, County = `REGIONS/Counties`)
# join FIPS for Colorado
Colorado_FIPS <- filter(County_geocode, `State Code (FIPS)` == "08")
col <- paste(Colorado_FIPS$`State Code (FIPS)`, Colorado_FIPS$`County Code (FIPS)`)
Colorado_FIPS$FIPS <- col
Colorado_FIPS$FIPS <- str_replace(Colorado_FIPS$FIPS, " ", "")
colorado_county <- merge(colorado_county, Colorado_FIPS)
write.csv(colorado_county, file = "colorado_formatted.csv")

# Conneticut #
Conneticut_projection_2015_2040 <- read_csv("Conneticut_projection_2015_2040.csv")
Conneticut_projection_2015_2040 <- rename(Conneticut_projection_2015_2040, County = `Geography`)
Conneticut_county <- filter(Conneticut_projection_2015_2040, `Age_Group` == "Total")
Conneticut_county <- select(Conneticut_county, -c(2,4,5,6))
Conneticut_county <- rename(Conneticut_county, Pop_projection = `Total`)
# join FIPS for Conneticut
Conneticut_FIPS <- filter(County_geocode, `State Code (FIPS)` == "09")
col <- paste(Conneticut_FIPS$`State Code (FIPS)`, Conneticut_FIPS$`County Code (FIPS)`)
Conneticut_FIPS$FIPS <- col
Conneticut_FIPS$FIPS <- str_replace(Conneticut_FIPS$FIPS, " ", "")
Conneticut_county <- merge(Conneticut_county, Conneticut_FIPS)
write.csv(Conneticut_county, file = "Conneticut_formatted.csv")

# Delaware #
Delaware_pop_projection_2010_2050 <- read_excel("Delaware_pop_projection_2010-2050.xls")
Delaware_county <- gather(Delaware_pop_projection_2010_2050, key = "Year", value = "Pop_projection", `2010`:`2050`)
# join FIPS for Delaware
Delaware_FIPS <- filter(County_geocode, `State Code (FIPS)` == "10")
col <- paste(Delaware_FIPS$`State Code (FIPS)`, Delaware_FIPS$`County Code (FIPS)`)
Delaware_FIPS$FIPS <- col
Delaware_FIPS$FIPS <- str_replace(Delaware_FIPS$FIPS, " ", "")
Delaware_county <- merge(Delaware_county, Delaware_FIPS)
write.csv(Delaware_county, file = "Delaware_formatted.csv")

# Idaho #
# by region not county skipping for now

# Illinois #
Illinois_pop_projection_2010_2025 <- read_csv("Illinois_pop_projection_2010_2025 (1).csv")
Illinois_county <- gather(Illinois_pop_projection_2010_2025, key = "Year", value = "Pop_projection", `2015`:`2025`)
Illinois_county <- rename(Illinois_county, County = `state_county`)
Illinois_county$County <- str_replace(Illinois_county$County, "Witt", "De Witt")
Illinois_county$County <- str_replace(Illinois_county$County, "Daviess", "Jo Daviess")
Illinois_county$County <- str_replace(Illinois_county$County, "Island", "Rock Island")
Illinois_county$County <- str_replace(Illinois_county$County, "Clair", "St. Clair")
# join FIPS for Illinois
Illinois_FIPS <- filter(County_geocode, `State Code (FIPS)` == "17")
col <- paste(Illinois_FIPS$`State Code (FIPS)`, Illinois_FIPS$`County Code (FIPS)`)
Illinois_FIPS$FIPS <- col
Illinois_FIPS$FIPS <- str_replace(Illinois_FIPS$FIPS, " ", "")
Illinois_county <- merge(Illinois_county, Illinois_FIPS)
write.csv(Illinois_county, file = "Illinois_formatted.csv")

# Indiana #
Indiana_pop_projections_2010_2050 <- read_excel("Indiana_pop_projections_2010_2050 (1).xlsx")
Indiana_pop_projections_2010_2050 <- rename(Indiana_pop_projections_2010_2050, Pop_projection = `Pop_projections`)
Indiana_pop_projections_2010_2050$County <- str_replace(Indiana_pop_projections_2010_2050$County, " County", "")
sum(is.na(Indiana_pop_projections_2010_2050))
# join FIPS for Indiana
Indiana_FIPS <- filter(County_geocode, `State Code (FIPS)` == "18")
col <- paste(Indiana_FIPS$`State Code (FIPS)`, Indiana_FIPS$`County Code (FIPS)`)
Indiana_FIPS$FIPS <- col
Indiana_FIPS$FIPS <- str_replace(Indiana_FIPS$FIPS, " ", "")
Indiana_county <- merge(Indiana_pop_projections_2010_2050, Indiana_FIPS)
write.csv(Indiana_county, file = "Indiana_formatted.csv")

# Iowa #
Iowa_pop_projections_2010_2040 <- read_excel("Iowa_pop_projections_2010_2040.xls")
Iowa_county <- gather(Iowa_pop_projections_2010_2040, key = "Year", value = "Pop_projection", `2000`:`2040`)
# join FIPS for Iowa
Iowa_FIPS <- filter(County_geocode, `State Code (FIPS)` == "19")
col <- paste(Iowa_FIPS$`State Code (FIPS)`, Iowa_FIPS$`County Code (FIPS)`)
Iowa_FIPS$FIPS <- col
Iowa_FIPS$FIPS <- str_replace(Iowa_FIPS$FIPS, " ", "")
Iowa_county <- merge(Iowa_county, Iowa_FIPS)
write.csv(Iowa_county, file = "Iowa_formatted.csv")

# Kansas #
Kansas_pop_projections_2014_2044 <- read_excel("Kansas_pop_projections_2014_2044 (1).xlsx")
Kansas_pop_projections_2014_2044 <- rename(Kansas_pop_projections_2014_2044, `2044` = `2044         2014-2044`)
Kansas_pop_projections_2014_2044 <- rename(Kansas_pop_projections_2014_2044, `County` = `County                    2014               2019`)
Kansas_county <- gather(Kansas_pop_projections_2014_2044, key = "Year", value = "Pop_projection", `2014`:`2044`)
# join FIPS for Kansas
Kansas_FIPS <- filter(County_geocode, `State Code (FIPS)` == "20")
col <- paste(Kansas_FIPS$`State Code (FIPS)`, Kansas_FIPS$`County Code (FIPS)`)
Kansas_FIPS$FIPS <- col
Kansas_FIPS$FIPS <- str_replace(Kansas_FIPS$FIPS, " ", "")
Kansas_county <- merge(Kansas_county, Kansas_FIPS)
write.csv(Kansas_county, file = "Kansas_formatted.csv")

# Maine #
# each county on different sheet in excel -- need to merge and add county identifier
maine_list <- list()
#for loop that calls function for each sheet in excel
for(i in seq(from=1, to=31, by=2)) {
  if (i < 10) {
    s <- sprintf("0%d", i)
  } else {
    s <- sprintf("%d", i)
  }
  county <- read_excel("Maine_pop_projection_2016_2036.xlsx", sheet = s, skip = 1)
  maine_list[[i]] <- maineFunction(county, i)
}

maineFunction <- function(maineCounty, i) {
  maineCounty <- select(maineCounty, -c(2:20))
  maineCounty <- maineCounty[-c(1:33), ]
  maineCounty <- rename(maineCounty, Pop_projection = `Total`)
  maineCounty <- rename(maineCounty, Year = `Age`)
  if (i < 10 ) {
    fip <-  sprintf("2300%d", i)
  } else {
    fip <-  sprintf("230%d", i)
  }
  list <- rep(fip,length(maineCounty$Year))
  maineCounty$County <- list
  return(maineCounty)
}

maine <- maine_list[[1]]
for(i in seq(from=3, to=31, by=2)) {
  maine <- bind_rows(maine, maine_list[[i]])
}
maine <- rename(maine, `FIPS` = `County`)
write.csv(maine, file = "Maine_formatted.csv")

# Maryland
Maryland_pop_projections <- read_excel("Maryland_pop_projections_1970_2045.xlsx")
Maryland_county <- gather(Maryland_pop_projections, key = "Year", value = "Pop_projection", `2010`:`2045`)
Maryland_county$County <- str_replace(Maryland_county$County, " County", "")
# join FIPS for Maryland
Maryland_FIPS <- filter(County_geocode, `State Code (FIPS)` == "24")
col <- paste(Maryland_FIPS$`State Code (FIPS)`, Maryland_FIPS$`County Code (FIPS)`)
Maryland_FIPS$FIPS <- col
Maryland_FIPS$FIPS <- str_replace(Maryland_FIPS$FIPS, " ", "")
Maryland_county <- merge(Maryland_county, Maryland_FIPS, all = TRUE)
Maryland_county$FIPS <- ifelse(Maryland_county$County == "Baltimore City", "24510", Maryland_county$FIPS)
Maryland_county$`State Code (FIPS)` <- ifelse(Maryland_county$County == "Baltimore City", "24", Maryland_county$`State Code (FIPS)`)
Maryland_county$`County Code (FIPS)` <- ifelse(Maryland_county$County == "Baltimore City", "510", Maryland_county$`County Code (FIPS)`)
write.csv(Maryland_county, file = "Maryland_formatted.csv")

#Massachusetts
Massachusetts_pop_projections <- read_excel("Massachusetts_pop_projections_2010_2040.xlsx", 
                                                      sheet = "County Totals")
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2010` = `Census 2010`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2015` = `Projection 2015`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2020` = `Projection 2020`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2025` = `Projection 2025`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2030` = `Projection 2030`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2035` = `Projection 2035`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `2040` = `Projection 2040`)
Massachusetts_pop_projections <- rename(Massachusetts_pop_projections, `County` = `COUNTY`)
Massachusetts_county <- gather(Massachusetts_pop_projections, key = "Year", value = "Pop_projection", `2010`:`2040`)
# join FIPS for Massachusetts
Massachusetts_FIPS <- filter(County_geocode, `State Code (FIPS)` == "25")
col <- paste(Massachusetts_FIPS$`State Code (FIPS)`, Massachusetts_FIPS$`County Code (FIPS)`)
Massachusetts_FIPS$FIPS <- col
Massachusetts_FIPS$FIPS <- str_replace(Massachusetts_FIPS$FIPS, " ", "")
Massachusetts_county <- merge(Massachusetts_county, Massachusetts_FIPS, all = TRUE)
write.csv(Massachusetts_county, file = "Massachusetts_formatted.csv")

# Michigan #
Michigan_pop_projections <- read_excel("Michigan_pop_projections_2020_2045.xlsx")
# filter for just county (4)
Michigan_county <- filter(Michigan_pop_projections, areatype == 4)
Michigan_county <- select(Michigan_county, -c(1,2,3,6,7,8,10:65))
Michigan_county <- rename(Michigan_county, Year = periodyear)
Michigan_county <- rename(Michigan_county, Pop_projection = population)
Michigan_county <- rename(Michigan_county, County = geo_name)
# join FIPS for Michigan
Michigan_FIPS <- filter(County_geocode, `State Code (FIPS)` == "26")
col <- paste(Michigan_FIPS$`State Code (FIPS)`, Michigan_FIPS$`County Code (FIPS)`)
Michigan_FIPS$FIPS <- col
Michigan_FIPS$FIPS <- str_replace(Michigan_FIPS$FIPS, " ", "")
Michigan_FIPS$County <- str_replace(Michigan_FIPS$County, "St.", "Saint")
Michigan_county <- merge(Michigan_county, Michigan_FIPS, all = TRUE)
write.csv(Michigan_county, file = "Michigan_formatted.csv")

# Minnesota #
Minnesota_pop_projections <- read_excel("Minnesota_pop_projections_2015_2050.xlsx")
Minnesota_pop_projections <- select(Minnesota_pop_projections, -c(3,4))
Minnesota_county <- gather(Minnesota_pop_projections, key = "Year", value = "Pop_projection", `2015`:`2050`)
write.csv(Minnesota_county, file = "Minnesota_formatted.csv")

# Missuri #
Missuri_pop_projections <- read_excel("Missuri_pop_projections_2000_2030.xls")
Missuri_county <- gather(Missuri_pop_projections, key = "Year", value = "Pop_projection", `2000`:`2030`)
# join FIPS for Missuri
Missuri_FIPS <- filter(County_geocode, `State Code (FIPS)` == "29")
col <- paste(Missuri_FIPS$`State Code (FIPS)`, Missuri_FIPS$`County Code (FIPS)`)
Missuri_FIPS$FIPS <- col
Missuri_FIPS$FIPS <- str_replace(Missuri_FIPS$FIPS, " ", "")
Missuri_county <- merge(Missuri_county, Missuri_FIPS, all = TRUE)
Missuri_county$FIPS <- ifelse(Missuri_county$County == "St. Louis City*", "29510", Maryland_county$FIPS)
Missuri_county$`State Code (FIPS)` <- ifelse(Missuri_county$County == "St. Louis City*", "29", Maryland_county$`State Code (FIPS)`)
Missuri_county$`County Code (FIPS)` <- ifelse(Missuri_county$County == "St. Louis City*", "510", Maryland_county$`County Code (FIPS)`)
write.csv(Missuri_county, file = "Missuri_formatted.csv")

# Montana #
MT_pop_projection <- read_csv("MT_pop_projection.csv")
MT_pop_projection <- MT_pop_projection[1:56, ]
Montana_county <- gather(MT_pop_projection, key = "Year", value = "Pop_projection", `2001 Population`:`pop2060`)
Montana_county <- select(Montana_county, -c(4,5,6,7))
Montana_county$Year <- str_replace(Montana_county$Year, " Population", "")
Montana_county <- rename(Montana_county, `County` = `NAME`)
Montana_county <- rename(Montana_county, `FIPS` = `GEOID`)
Montana_county <- select(Montana_county, -c(2))
write.csv(Montana_county, file = "Montana_formatted.csv")

# Nebraska #
# each county on different sheet
# need counter for county FIP
nebraska_list <- list()
cnty_FIP <- 1
#for loop that calls function for each sheet in excel
for(i in seq(from=1, to=93, by=1)) {
  t <- i + 9
  s <- sprintf("Table %d", t)
  county <- read_excel("Nebraska_pop_projections_2010_2050 (1)-converted.xlsx", 
                                                               sheet = s, col_names = FALSE, skip = 20)
  #county <- read_excel("Nebraska_pop_projections_2010_2050 (1).xlsx", sheet = s, col_names = FALSE)
  nebraska_list[[i]] <- nebraskaFunction(county, cnty_FIP)
  cnty_FIP <- cnty_FIP + 2
}
#county <- read_excel("Nebraska_pop_projections_2010_2050 (1).xlsx", sheet = "Table 1")
#nebraskaCounty <- gather(nebraskaCounty,key = "Year", value = "Pop_projection", `2010`:`2050`)

nebraskaFunction <- function(nebraskaCounty, i) {
  nebraskaCounty<- rename(nebraskaCounty, `2010` = `...2`)
  nebraskaCounty<- rename(nebraskaCounty, `2015` = `...3`)
  nebraskaCounty<- rename(nebraskaCounty, `2020` = `...4`)
  nebraskaCounty<- rename(nebraskaCounty, `2025` = `...5`)
  nebraskaCounty<- rename(nebraskaCounty, `2030` = `...6`)
  nebraskaCounty<- rename(nebraskaCounty, `2035` = `...7`)
  nebraskaCounty<- rename(nebraskaCounty, `2040` = `...8`)
  nebraskaCounty<- rename(nebraskaCounty, `2045` = `...9`)
  nebraskaCounty<- rename(nebraskaCounty, `2050` = `...10`)
  nebraskaCounty <- select(nebraskaCounty, -c(1, 11:14))
  nebraskaCounty <- nebraskaCounty[-c(2:9), ]
  nebraskaCounty <- gather(nebraskaCounty,key = "Year", value = "Pop_projection", `2010`:`2050`)
  # rename 2025 through 2035
  if (i < 10 ) {
    fip <- sprintf("3100%d", i)
  } else {
    fip <- sprintf("310%d", i)
  }
  list <- rep(fip,length(nebraskaCounty$Year))
  nebraskaCounty$County <- list
  return(nebraskaCounty)
}
nebraska <- nebraska_list[[1]]
for(i in seq(from=2, to=93, by=1)) {
  nebraska <- bind_rows(nebraska, nebraska_list[[i]])
}
nebraska <- rename(nebraska, `FIPS` = `County`)
nebraska <- select(nebraska, -c(1, 5:11))
view(nebraska[!complete.cases(nebraska),])
## 44 na's just from wrong conversion of pdf to excel
# fixed in excel
write.csv(nebraska, file = "Nebraska_formatted.csv")

# Nevada #
Nevada_pop_projections<- read_excel("Nevada_pop_projections_2019_2038 (2).xlsx")
nevadaCounty <- gather(Nevada_pop_projections,key = "County", value = "Pop_projection", `Carson City`:`White Pine`)
# join FIPS for Nevada
nevada_FIPS <- filter(County_geocode, `State Code (FIPS)` == "32")
col <- paste(nevada_FIPS$`State Code (FIPS)`, nevada_FIPS$`County Code (FIPS)`)
nevada_FIPS$FIPS <- col
nevada_FIPS$FIPS <- str_replace(nevada_FIPS$FIPS, " ", "")
nevadaCounty <- merge(nevadaCounty, nevada_FIPS, all = TRUE)
nevadaCounty$FIPS <- ifelse(nevadaCounty$County == "Carson City", "32510", nevadaCounty$FIPS)
nevadaCounty$`State Code (FIPS)` <- ifelse(nevadaCounty$County == "Carson City", "32", nevadaCounty$`State Code (FIPS)`)
nevadaCounty$`County Code (FIPS)` <- ifelse(nevadaCounty$County == "Carson City", "510", nevadaCounty$`County Code (FIPS)`)
write.csv(nevadaCounty, file = "Nevada_formatted.csv")

# New Hampshire #
NewHampshire_pop_projections <- read_excel("NewHampshire_pop_projections_2010_2040 (1).xlsx")
NewHampshireCounty <- NewHampshire_pop_projections[-c(1),]
NewHampshireCounty <- gather(NewHampshireCounty, key = "Year", value = "Pop_projection", `2010`:`2040`)
# join FIPS for New Hampshire
NewHampshire_FIPS <- filter(County_geocode, `State Code (FIPS)` == "33")
col <- paste(NewHampshire_FIPS$`State Code (FIPS)`, NewHampshire_FIPS$`County Code (FIPS)`)
NewHampshire_FIPS$FIPS <- col
NewHampshire_FIPS$FIPS <- str_replace(NewHampshire_FIPS$FIPS, " ", "")
NewHampshireCounty <- merge(NewHampshireCounty, NewHampshire_FIPS, all = TRUE)
write.csv(NewHampshireCounty, file = "NewHamshire_formatted.csv")

# New Jersey #
NewJersey_pop_projections <- read_excel("NewJersey_pop_projections_2014_2034 (1).xlsx", 
                                        skip = 3, n_max = 30)
NewJersey_pop_projections <- select(NewJersey_pop_projections, -c(4))
NewJerseyCounty <- NewJersey_pop_projections[complete.cases(NewJersey_pop_projections),]
NewJerseyCounty <- NewJerseyCounty[-c(1),]
NewJerseyCounty <- rename(NewJerseyCounty, `2010`= `4/1/2010`)
NewJerseyCounty <- rename(NewJerseyCounty, `2014`= `7/1/2014`)
NewJerseyCounty <- gather(NewJerseyCounty, key = "Year", value = "Pop_projection", `2010`:`2034`)
# join FIPS for New Jersey
NewJersey_FIPS <- filter(County_geocode, `State Code (FIPS)` == "34")
col <- paste(NewJersey_FIPS$`State Code (FIPS)`, NewJersey_FIPS$`County Code (FIPS)`)
NewJersey_FIPS$FIPS <- col
NewJersey_FIPS$FIPS <- str_replace(NewJersey_FIPS$FIPS, " ", "")
NewJerseyCounty <- merge(NewJerseyCounty, NewJersey_FIPS, all = TRUE)
write.csv(NewJerseyCounty, file = "NewJersey_formatted.csv")

# New Mexico #
NewMexico_pop_projections <- read_excel("NewMexico_pop_projections_2000_2040.xlsx", 
                                                  sheet = "Preliminary Projections", range = "A1:I34")
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2000`= `2000 Count`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2010`= `2010 Count`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2015`= `2015 Estimate`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2020`= `2020 Projection`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2025`= `2025 Projection`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2030`= `2030 Projection`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2035`= `2035 Projection`)
NewMexico_pop_projections <- rename(NewMexico_pop_projections, `2040`= `2040 Projection`)
NewMexicoCounty <- gather(NewMexico_pop_projections, key = "Year", value = "Pop_projection", `2000`:`2040`)
# join FIPS for New Mexico
NewMexico_FIPS <- filter(County_geocode, `State Code (FIPS)` == "35")
col <- paste(NewMexico_FIPS$`State Code (FIPS)`, NewMexico_FIPS$`County Code (FIPS)`)
NewMexico_FIPS$FIPS <- col
NewMexico_FIPS$FIPS <- str_replace(NewMexico_FIPS$FIPS, " ", "")
NewMexicoCounty <- merge(NewMexicoCounty, NewMexico_FIPS, all = TRUE)
write.csv(NewMexicoCounty, file = "NewMexico_formatted.csv")

#Each county in a different excel file - need to merge
# New York #
NewYork <- read_excel("individual_counties/county01.xlsx")
NewYork <- NewYork[c(1),]
for(i in seq(from=3, to=123, by=2)) {
  if ( i < 10 ) {
    s <- sprintf("individual_counties/county0%d.xlsx", i)
  } else {
    s <- sprintf("individual_counties/county%d.xlsx", i)
  }
  county <- read_excel(s)
  county <- county[c(1),]
  NewYork <- bind_rows(NewYork, county)
}
NewYork <- select(NewYork, -c(1,3:8))
NewYork <- gather(NewYork, key = "Year", value = "Pop_projection", `YR_2015`:`YR_2040`)
NewYork$Year <- str_replace(NewYork$Year, "YR_", "")
NewYork <- rename(NewYork, `County` = COUNTY_DESCR)
NewYork_FIPS <- filter(County_geocode, `State Code (FIPS)` == "36")
col <- paste(NewYork_FIPS$`State Code (FIPS)`, NewYork_FIPS$`County Code (FIPS)`)
NewYork_FIPS$FIPS <- col
NewYork_FIPS$FIPS <- str_replace(NewYork_FIPS$FIPS, " ", "")
NewYork_FIPS$County <- str_replace(NewYork_FIPS$County, "St. Lawrence", "St Lawrence")
NewYork <- merge(NewYork, NewYork_FIPS, all = TRUE)
write.csv(NewYork, file = "NewYork_formatted.csv")

# North Dakota #
NorthDakota_pop_projections <- read_excel("NorthDakota_pop_projections_2010_2040.xlsx")
NorthDakota_pop_projections <- NorthDakota_pop_projections[-c(1,43),]
NorthDakota_pop_projections$`North Dakota Counties` <- str_replace(NorthDakota_pop_projections$`North Dakota Counties`, " County", "")
NorthDakotaCounty <- gather(NorthDakota_pop_projections, key = "Year", value = "Pop_projection", `2010`:`2040`)
NorthDakotaCounty <- rename(NorthDakotaCounty, County = `North Dakota Counties`)
# join FIPS for North Dakota
NorthDakota_FIPS <- filter(County_geocode, `State Code (FIPS)` == "38")
col <- paste(NorthDakota_FIPS$`State Code (FIPS)`, NorthDakota_FIPS$`County Code (FIPS)`)
NorthDakota_FIPS$FIPS <- col
NorthDakota_FIPS$FIPS <- str_replace(NorthDakota_FIPS$FIPS, " ", "")
NorthDakotaCounty <- merge(NorthDakotaCounty, NorthDakota_FIPS, all = TRUE)
write.csv(NorthDakotaCounty, file = "NorthDakota_formatted.csv")

# Ohio #
Ohio_pop_projections<- read_excel("Ohio_pop_projections_2010_2040 (1).xlsx")
Ohio_pop_projections <- Ohio_pop_projections[-c(1:2, 91:94), ]
Ohio_pop_projections <- rename(Ohio_pop_projections, `County`=`...1`)
OhioCounty <- gather(Ohio_pop_projections, key = "Year", value = "Pop_projection", `2010`:`2040`)
# join FIPS for Ohio
Ohio_FIPS <- filter(County_geocode, `State Code (FIPS)` == "39")
col <- paste(Ohio_FIPS$`State Code (FIPS)`, Ohio_FIPS$`County Code (FIPS)`)
Ohio_FIPS$FIPS <- col
Ohio_FIPS$FIPS <- str_replace(Ohio_FIPS$FIPS, " ", "")
OhioCounty <- merge(OhioCounty, Ohio_FIPS, all = TRUE)
write.csv(OhioCounty, file = "Ohio_formatted.csv")

# Pennslyvania #
Pennsylvania_Projections <- read_excel("Pennsylvania_Projections_2010-2040.xlsx", 
                                                 skip = 4)
Pennsylvania_Projections <- Pennsylvania_Projections[Pennsylvania_Projections$Age == "Total", ] 
Pennsylvania_Projections <- select( Pennsylvania_Projections, -c(6,10,14,18,22,26))
Pennsylvania_Projections <- Pennsylvania_Projections[complete.cases(Pennsylvania_Projections),]
Pennsylvania_Projections <- Pennsylvania_Projections[-c(1),]
Pennsylvania_Projections$County <- str_replace(Pennsylvania_Projections$County, " County", "")
Pennsylvania_Projections$County <- str_replace(Pennsylvania_Projections$County, " Count", "")
Pennsylvania_Projections <- select( Pennsylvania_Projections, c(1,5,8,11,14,17,20,23))
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2010`=`Total...5`)
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2015`=`Total...9`)
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2020`=`Total...13`)
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2025`=`Total...17`)
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2030`=`Total...21`)
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2035`=`Total...25`)
Pennsylvania_Projections <- rename(Pennsylvania_Projections, `2040`=`Total...29`)
PennsylvaniaCounty <- gather(Pennsylvania_Projections, key = "Year", value = "Pop_projection", `2010`:`2040`)
# join FIPS for Pennsylvania
Pennsylvania_FIPS <- filter(County_geocode, `State Code (FIPS)` == "42")
col <- paste(Pennsylvania_FIPS$`State Code (FIPS)`, Pennsylvania_FIPS$`County Code (FIPS)`)
Pennsylvania_FIPS$FIPS <- col
Pennsylvania_FIPS$FIPS <- str_replace(Pennsylvania_FIPS$FIPS, " ", "")
PennsylvaniaCounty <- merge(PennsylvaniaCounty, Pennsylvania_FIPS, all = TRUE)
write.csv(PennsylvaniaCounty, file = "Pennsylvania_formatted.csv")
