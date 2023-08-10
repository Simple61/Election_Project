setwd("C:\\Users\\carys\\Desktop\\Election Project\\Federal Election Results")

library(tidyverse)
library(ggplot2)
library(tigris)
library(USAboundaries)
library(usmap)
library(maps)
library(ggmap)
library(scales)
#citation("ggmap")

county = read.csv("..\\Data\\countypres_2000-2016.csv", colClasses = c(FIPS = "character"))
poverty = read.csv("..\\Data\\Poverty.csv")
unemployment = read.csv("..\\Data\\Unemployment.csv")
education = read.csv("..\\Data\\Education.csv")
demographic = read.csv("..\\Data\\Demographic.csv")

#difference of percent of people of democrat - republican

#unemployment = rename(unemployment, year = ï..year)
#instead of the above, use the following to avoid the special character
colnames(unemployment)[1] <- "year"
unemployment$year <- as.character(unemployment$year)
county$year <- as.character(county$year)
education <- select(education, -c(4:6))
education <- rename(education, CollegeDegree = bachelor.s.degree.or.higher)
county_map <- map_data("county")
county_map$County <- county_map$subregion
county_map$State <- county_map$region
county = filter(county, party != "Other", party != "green")
coeffs <- data.frame(Year = character(), Politics = character(), Intercept = double())

MainStates <- map_data("state")

for (i in seq(2000, 2016, by=4)) {
  yr = as.character(i)
  election = filter(county, year == yr)
  democrat = filter(election, party == "democrat" )
  republican = filter(election, party == "republican")
  democrat$percentVotes = (democrat$candidatevotes / democrat$totalvotes) * 100
  republican$percentVotes = (republican$candidatevotes / republican$totalvotes) * 100
  
  democrat$diff_percent = democrat$percentVotes - republican$percentVotes

  differences <- select(democrat, state, county, FIPS, diff_percent)
  differences$county <-tolower(differences$county)
  differences$state <-tolower(differences$state)
  colnames(differences) = c("State","County", "FIPS", "diff_percent")
  election_map <- left_join(county_map, differences)

  # ggplot(election_map, aes(x = long, y = lat, group=group)) +
  #   geom_polygon(aes(fill = diff_percent)) + 
  #   scale_fill_gradient2(midpoint = 0, mid="#770077", high="#0000ff", low= "#ff0000")
  
  ggplot(election_map, aes(x = long, y = lat, group=group)) +
     geom_polygon(aes(fill = diff_percent), color="black",size=0.001) +
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="white",  size = 0.05, alpha = .3) +
    scale_fill_gradient2(midpoint = 0, mid="#770077", high="#0000ff", low= "#ff0000") +
    #scale_fill_gradient2(low="red",high="blue") +
    #coord_map("bonne", parameters = 41.6)
    coord_map("albers", lat0 = 39, lat1 = 45) + 
    labs(title=paste("Election Map", yr)) +
    theme(plot.title = element_text(color = "black", size = 12, face = "bold",hjust=0.5))
  
  ggsave(paste(yr, "election_map.png"), plot = last_plot(), path = "..\\graphs")
  
  
  republicanStates = republican %>%
    group_by(state) %>%
    summarize(sumVotes = sum(candidatevotes))

  democratStates = democrat %>%
    group_by(state) %>%
    summarize(sumVotes = sum(candidatevotes))
  if(yr == "2000") {
    democratStates = democratStates[-7, ]
    democratStates = democratStates[-46, ]
    republicanStates = republicanStates[-7, ]
    republicanStates = republicanStates[-46, ]
  }
  both = as.data.frame(cbind(state=democratStates$state,demVotes=democratStates$sumVotes, repVotes=republicanStates$sumVotes))
  both$demVotes = as.numeric(both$demVotes)
  both$repVotes = as.numeric(both$repVotes)
  both$stateDifference = abs((both$demVotes - both$repVotes) / (both$demVotes + both$repVotes))
  both$stateDifference = both$stateDifference * 100
  democratStates <- left_join(democratStates, both)
  both = select(both, state, stateDifference)
  both$stateDifference = as.numeric(both$stateDifference)
  
  #Percent of unemployment rate
  
  unemployment_yr = filter(unemployment, year == yr)
  job_rates <- select(unemployment_yr, State, County, FIPS, Unemployment_Rate)
  job_rates$County <-tolower(job_rates$County)
  unemployment_map <- left_join(county_map, job_rates)
  unemployment_map$Unemployment_Rate <- as.double(unemployment_map$Unemployment_Rate)

  # ggplot(unemployment_map, aes(x = long, y = lat, group=group)) +
  #   geom_polygon(aes(fill = Unemployment_Rate)) + 
  #   scale_fill_gradient2(midpoint = 12, mid="#00ff00", high="#0000ff", low= "#ff0000")
 
  ggplot(unemployment_map, aes(x = long, y = lat, group=group)) +
    geom_polygon(aes(fill = Unemployment_Rate), color="black",size=0.001) +
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="white",  size = 0.05, alpha = .3) +
    scale_fill_gradient2(midpoint = 12, mid="#00ff00", high="#0000ff", low= "#ff0000") +
    #scale_fill_gradient2(low="red",high="green") +
    #coord_map("bonne", parameters = 41.6)
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(title=paste("Unemployment Rate", yr)) +
    theme(plot.title = element_text(color = "black", size = 12, face = "bold",hjust=0.5))
  
    ggsave(paste(yr, "unemployment_map.png"), plot = last_plot(), path = "..\\graphs")
  
  #Correlation of difference by party and unemployment rate
  job_rates$FIPS <- as.character(job_rates$FIPS)
  Merged <- left_join(differences, job_rates)
  Merged <- filter(Merged, FIPS != '36000')
  Merged$Unemployment_Rate <- as.double(Merged$Unemployment_Rate)
  Merged$State <- as.factor(Merged$State)
  
  if(yr == "2000") {
    r = c("Washington", 0)
    r2 = c("Delaware", 0)
    both <- rbind(both, r)
    both <- rbind(both, r2)    
  }

  both <- both[order(both[,'state']), ]
  both$state <- tolower(both$state)
  
  both$Coefficient <- by(Merged, Merged$State, FUN = function(X) cor(X$diff_percent, X$Unemployment_Rate, method = "spearman"))
  both$Coefficient <- as.double(both$Coefficient) 
  Cors1 <- select(both, state, Coefficient)
  Cors1 <- filter(Cors1, state != "district of columbia")
  Cors1$State <- Cors1$state
  Cors1_map <- left_join(county_map, Cors1)
  
  
  # ggplot(Cors1_map, aes(x = long, y = lat, group=group)) +
  #   geom_polygon(aes(fill = corr)) + 
  #   scale_fill_gradient2(midpoint = 0.5, mid="#00ff00", high="#0000ff", low= "#ff0000")

    ggplot(Cors1_map, aes(x = long, y = lat, group=group)) +
    geom_polygon(aes(fill = Coefficient), color="black",size=0.001) +
    geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                  color="white",  size = 0.05, alpha = .3) +
    scale_fill_gradient2(midpoint = 0, mid="#00ff00", high="#0000ff", low= "#ff0000") +
    coord_map("albers", lat0 = 39, lat1 = 45) +
    labs(title=paste("Correlation between Unemployment and Voting Differences", yr)) +
    theme(plot.title = element_text(color = "black", size = 12, face = "bold",hjust=0.5))
  
  ggsave(paste(yr,"Cors1_map.png"), plot = last_plot(), path = "..\\graphs")
  
  politics <- c("Red", "Lean Red", "Red", "Blue", "Blue", "Blue", "Blue", "Purple", "Lean Red", "Blue", "Red", "Blue", "Lean Red", "Purple", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Lean Blue", "Blue", "Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Blue", "Blue", "Lean Red", "Red", "Purple", "Red", "Blue", "Lean Blue", "Blue", "Red", "Red", "Red", "Red", "Red", "Blue", "Blue", "Blue", "Red", "Lean Blue", "Red")
  Cors1$politics <- politics 
  
  Cors1$politics <- as.factor(Cors1$politics)
  Cors1$politics <- factor(Cors1$politics, levels = c("Blue","Lean Blue", "Purple", "Lean Red", "Red"))

  ggplot(Cors1, aes(x=politics, y=Coefficient)) + 
    geom_boxplot(fill="slateblue", alpha=100) + 
    coord_cartesian(ylim = c(-1, 1)) +
    xlab("Political Leanings of States") +
    ylab("Spearman Correlation of Unemployment and difference of voting percentage") +
    labs(title=paste("Correlation between Unemployment and Voting Differences", yr)) +
    theme(plot.title = element_text(color = "black", size = 12, face = "bold",hjust=0.5))
    
  ggsave(paste(yr, "Cors1.png"), plot = last_plot(), path = "..\\graphs")
  
  Merged <- left_join(Merged, Cors1)
  closeStates <- filter(Merged, politics != "Red")
  closeStates <- filter(closeStates, politics != "Blue")
  closeStates <- select(closeStates, -state)
  
  #purple states
  
  ohio_plot <- filter(closeStates, State == "ohio")
  ggplot(ohio_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Ohio", yr))
    
  ggsave(paste(yr, "ohio.png"), plot = last_plot(), path = "..\\graphs")
  
  florida_plot <- filter(closeStates, State == "florida")
  ggplot(florida_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Florida", yr))
  
  ggsave(paste(yr, "florida.png"), plot = last_plot(), path = "..\\graphs")
  
  iowa_plot <- filter(closeStates, State == "iowa")
  ggplot(iowa_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Iowa", yr))
  
  ggsave(paste(yr, "iowa.png"), plot = last_plot(), path = "..\\graphs")
  
 #lean blue
  
  michigan_plot <- filter(closeStates, State == "michigan")
  ggplot(michigan_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Michigan", yr))
  
  ggsave(paste(yr, "michigan.png"), plot = last_plot(), path = "..\\graphs") 
  
  pennsylvania_plot <- filter(closeStates, State == "pennsylvania")
  ggplot(pennsylvania_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Pennsylvania", yr))
  
  ggsave(paste(yr, "pennsylvania.png"), plot = last_plot(), path = "..\\graphs")
 
  wisconsin_plot <- filter(closeStates, State == "wisconsin")
  ggplot(wisconsin_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Wisconsin", yr))
  
  ggsave(paste(yr, "wisconsin.png"), plot = last_plot(), path = "..\\graphs")
   
  #lean red
  
  arizona_plot <- filter(closeStates, State == "arizona")
  ggplot(arizona_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Arizona", yr))
  
  ggsave(paste(yr, "arizona.png"), plot = last_plot(), path = "..\\graphs")
  
  georgia_plot <- filter(closeStates, State == "georgia")
  ggplot(georgia_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Georgia", yr))
  
  ggsave(paste(yr, "georgia.png"), plot = last_plot(), path = "..\\graphs")
  
  indiana_plot <- filter(closeStates, State == "indiana")
  ggplot(indiana_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Indiana", yr))
  
  ggsave(paste(yr, "indiana.png"), plot = last_plot(), path = "..\\graphs")
  
  NC_plot <- filter(closeStates, State == "north carolina")
  ggplot(NC_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("North Carolina", yr))
  
  ggsave(paste(yr, "NC.png"), plot = last_plot(), path = "..\\graphs")
  
 #blue state
   california_plot <- filter(Merged, State == "california")
  ggplot(california_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("California", yr))
  ggsave(paste(yr, "california.png"), plot = last_plot(), path = "..\\graphs")
  
  #red state
  texas_plot <- filter(Merged, State == "texas")
  ggplot(texas_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Texas", yr))
  ggsave(paste(yr, "texas.png"), plot = last_plot(), path = "..\\graphs")
  
  
  #dummy variables
  
  Merged$dumR <- ifelse(Merged$politics == "Red", 1, 0)
  Merged$dumLR <- ifelse(Merged$politics == "Lean Red", 1, 0)
  Merged$dumLB <- ifelse(Merged$politics == "Lean Blue", 1, 0)
  Merged$dumB <- ifelse(Merged$politics == "Blue", 1, 0)
  
  #all red states
  red_plot <- filter(Merged, politics == "Red")
  ggplot(red_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Red States", yr))
  ggsave(paste(yr, "red.png"), plot = last_plot(), path = "..\\graphs")
  red_coeff <- lm(red_plot$diff_percent~red_plot$Unemployment_Rate)
  
  #all lean red states
  Lred_plot <- filter(Merged, politics == "Lean Red")
  ggplot(Lred_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Lean Red States", yr))
  ggsave(paste(yr, "lean_red.png"), plot = last_plot(), path = "..\\graphs")
  Lred_coeff <- lm(Lred_plot$diff_percent~Lred_plot$Unemployment_Rate)
  
  #all purple states
  purple_plot <- filter(Merged, politics == "Purple")
  ggplot(purple_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Purple States", yr))
  ggsave(paste(yr, "purple.png"), plot = last_plot(), path = "..\\graphs")
  purple_coeff <- lm(purple_plot$diff_percent~purple_plot$Unemployment_Rate)
  
  #all lean blue states
  Lblue_plot <- filter(Merged, politics == "Lean Blue")
  ggplot(Lblue_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Lean Blue States", yr))
  ggsave(paste(yr, "lean_blue.png"), plot = last_plot(), path = "..\\graphs")
  Lblue_coeff <- lm(Lblue_plot$diff_percent~Lblue_plot$Unemployment_Rate)
  
  #all blue states
  blue_plot <- filter(Merged, politics == "Blue")
  ggplot(blue_plot, aes(x=Unemployment_Rate, y=diff_percent)) + 
    geom_point() + 
    geom_smooth(method = "lm") +
    labs(title=paste("Blue States", yr))
  ggsave(paste(yr, "blue.png"), plot = last_plot(), path = "..\\graphs")
  blue_coeff <- lm(blue_plot$diff_percent~blue_plot$Unemployment_Rate)
  
  coeffs <- rbind(coeffs, c(yr, "Red", red_coeff$coefficients))
  coeffs <- rbind(coeffs, c(yr, "Lean Red", Lred_coeff$coefficients))
  coeffs <- rbind(coeffs, c(yr, "Purple", purple_coeff$coefficients))
  coeffs <- rbind(coeffs, c(yr, "Lean Blue", Lblue_coeff$coefficients))
  coeffs <- rbind(coeffs, c(yr, "Blue", blue_coeff$coefficients))
  
  model <- lm(diff_percent ~ Unemployment_Rate + dumR + dumLR + dumLB + dumB, data= Merged)
  summary(model)
  
  #Percent of people of in poverty
    
#  poverty_rates <- select(poverty, State, County, FIPS, Percent)
#  poverty_rates$County <-tolower(poverty_rates$County)
#  poverty_map <- left_join(county_map, poverty_rates)
#  poverty_map$Percent <- as.double(county_map$Percent)

#  ggplot(poverty_map, aes(x = long, y = lat, group=group)) +
#    geom_polygon(aes(fill = Percent)) + 
#    scale_fill_gradient2(midpoint = 25, mid="#00ff00", high="#0000ff", low= "#ff0000")

  #Correlation of difference in party votes and poverty rates

#  poverty_rates$FIPS = as.character(poverty_rates$FIPS)
#  Merged <- inner_join(differences, poverty_rates)
#  Merged$Percent <- as.double(Merged$Percent)
#  Merged$State <- as.factor(Merged$State)
#  cor(Merged$diff_percent, Merged$Percent, method = "spearman")
#  corrCoeff <- by(Merged, Merged$State, FUN = function(X) cor(X$diff_percent, X$Percent, method = "spearman"))
#  Cors2 <- data.frame(State = dimnames(corrCoeff)[[1]], corr = as.vector(corrCoeff))
#  Cors2$corr = abs(Cors2$corr)
  
#  povertyCor_map <- left_join(county_map, Cors2)
  
#  ggplot(povertyCor_map, aes(x = long, y = lat, group=group)) +
#    geom_polygon(aes(fill = corr)) + 
#    scale_fill_gradient2(midpoint = 0.5, mid="#00ff00", high="#0000ff", low= "#ff0000")

  #education rates by percent of people with more than a bachelors degree (estimate for 2015-2019, not perfectly accurate for 2016)

#  edu_rates <- select(education, State, County, FIPS, CollegeDegree)
#  edu_rates$County <-tolower(edu_rates$County)
#  edu_map <- left_join(county_map, edu_rates)
#  edu_map$CollegeDegree <- as.double(county_map$CollegeDegree)
  
#  ggplot(edu_map, aes(x = long, y = lat, group=group)) +
#    geom_polygon(aes(fill = CollegeDegree)) + 
#    scale_fill_gradient2(midpoint = 40, mid="#00ff00", high="#0000ff", low= "#ff0000")
  
  #Correlation of difference in party votes and college degree rates
  
#  edu_rates$FIPS = as.character(edu_rates$FIPS)
#  Merged <- inner_join(differences, edu_rates)
#  Merged$CollegeDegree <- as.double(Merged$CollegeDegree)
#  Merged$State <- as.factor(Merged$State)
#  cor(Merged$diff_percent, Merged$CollegeDegree, method = "spearman")
#  corrCoeff <- by(Merged, Merged$State, FUN = function(X) cor(X$diff_percent, X$CollegeDegree, method = "spearman"))
#  Cors3 <- data.frame(State = dimnames(corrCoeff)[[1]], corr = as.vector(corrCoeff))
#  Cors3$corr = abs(Cors2$corr)
  
#  eduCor_map <- left_join(county_map, Cors3)
  
#  ggplot(eduCor_map, aes(x = long, y = lat, group=group)) +
#    geom_polygon(aes(fill = corr)) + 
#    scale_fill_gradient2(midpoint = 0.5, mid="#00ff00", high="#0000ff", low= "#ff0000")
}
coeffs <- rename(coeffs, Year = X.2000., Politics = X.Red., Intercept = X..54.8234914295885., Slope = X.6.77664137179767.)
