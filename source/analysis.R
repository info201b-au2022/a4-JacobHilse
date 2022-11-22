library(tidyverse)

## Section 2  ---- 
#----------------------------------------------------------------------------#
#In this section is establish my main dataframe as well as some important nationwide statistics

dataMain <- read.csv("https://raw.githubusercontent.com/vera-institute/incarceration-trends/master/incarceration_trends.csv")

popStart <- dataMain %>%
  filter(year == 1990) 
popNew <- dataMain %>%
  filter(year==2018)

blackPropNew <- (100*(sum(popNew$black_jail_pop, na.rm = TRUE)/ sum(popNew$total_jail_pop, na.rm = TRUE)))
blackPropStar <- (100*(sum(popStart$black_jail_pop, na.rm = TRUE)/ sum(popStart$total_jail_pop, na.rm = TRUE)))
percTotalPop <- (100*(sum(popNew$total_jail_pop, na.rm = TRUE)/sum(popStart$total_jail_pop, na.rm = TRUE))-1)

## Section 3  ---- 
#----------------------------------------------------------------------------#
# Growth of the U.S. Prison Population

#This function takes a subset of specific columns intended for use and returns them grouped and summarized
get_year_jail_pop <- function() {
  subData <- dataMain[c(2,21)]
  tempdf <- subData %>%
    group_by(year)%>%
    summarize(sumPop = sum(total_jail_pop, na.rm = TRUE))
  return(tempdf)   
}

# This function plots the wrangled dataframe
plot_jail_pop_for_us <- function()  {
  plotdf <- get_year_jail_pop()
  barchart <- ggplot(data=plotdf, aes(x=year, y=sumPop)) + geom_bar(stat = "identity") +
    xlab("Year")+ylab("Total Incarcerated Individuals")
  return(barchart)   
} 

## Section 4  ---- 
#----------------------------------------------------------------------------#
# Growth of Prison Population by State 

#Function that filters by vector(states) and then groups by the columns intended for use
get_jail_pop_by_states <- function(states) {
    fortemp <- dataMain %>%
      filter(state %in%  states) %>%
      group_by(state,year) %>%
      summarize(stateSum = sum(total_jail_pop, na.rm=TRUE))
  return(fortemp)
}

#Function plots the wrangled dataframe
plot_jail_pop_by_states <- function(states) {
  lineplotdf <- get_jail_pop_by_states(states)
  lineReturn <- ggplot(data=lineplotdf, aes(x=year, y=stateSum, group=state)) +
    geom_line(aes(linetype=state))+
    geom_point() + xlab("Year")+ylab("Total Incarcerated Individuals by State")
  return(lineReturn)
}

## Section 5  ---- 
#----------------------------------------------------------------------------#

#function returns a dataframe comparing the proportion of incarcerated white/black individuals to total incarcerated pop
comparisonDF <- function(){
  compTemp <- dataMain[c(2,21,29)]
  compDF1 <- compTemp %>%
    filter(year >= 1990) %>%
    group_by(year)%>%
    summarize(bProp = (sum(black_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)))
    
  compTemp <- dataMain[c(2,21,32)]
  compDF2 <- compTemp %>%
    filter(year >= 1990) %>%
    group_by(year)%>%
    summarize(wProp = (sum(white_jail_pop, na.rm=TRUE)/sum(total_jail_pop, na.rm=TRUE)))
  
  compTemp <- left_join(compDF1, compDF2, by="year")
  return(compTemp)
}

#plot out my wrangled dataframe with specific visual elements
plotComparison <- function(){
  compplotdf <- comparisonDF()
  compPlot <- ggplot(data=compplotdf, aes(year)) +
    geom_line(aes(y=wProp, color="Proportion of White individuals"))+
    geom_line(aes(y=bProp, color="Proportion of Black individuals"))+
    xlab("Year")+ylab("Proportion of total Jail Population")+
    guides(color=guide_legend(title="Chart Legend"))
  
  return(compPlot)
}

## Section 6  ---- 
#----------------------------------------------------------------------------#
library(leaflet)

#pull a csv that has lat,lng associated by fips
coord <- read.csv("https://gist.githubusercontent.com/russellsamora/12be4f9f574e92413ea3f92ce1bc58e6/raw/3f18230058afd7431a5d394dab7eeb0aafd29d81/us_county_latlng.csv")
coord$fips <- coord$fips_code
coord <- coord[3:5]
#Join the lat,lng info to a new dataframe so that I can plot on leaflet map
coordMain <- left_join(dataMain, coord, by="fips")

#Wrangling function : I create a df taking specific columns (including lat,lng)
mapFrame <- function(){
  mapDf <- coordMain[c(2,3,21,29,122,123)]
  mapDf$bProp <- (mapDf$black_jail_pop / mapDf$total_jail_pop)
  retdf <- mapDf %>%
    filter(year==2018) %>%
    select(lng,lat,bProp,fips)
  
  return(retdf)
}

#retun my subsetted df into my mapping function and create a map based off the leafdf$bProp statistic
printMap <- function(){
  leafdf <- mapFrame()
  leafdf <- filter(leafdf, bProp <= 1)
  mapOne <- leaflet(data=leafdf) %>%
    addTiles() %>%
    addCircleMarkers(lat=leafdf$lat, lng=leafdf$lng, popup = leafdf$bProp, stroke=FALSE, radius=(leafdf$bProp*7), fillOpacity = 0.5)
  
  return(mapOne)
}

