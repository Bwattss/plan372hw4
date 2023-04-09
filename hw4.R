airportdata = read.csv("airport_pairs.csv")
library(tidyverse)
library(tidycensus)

#So the line below creates a table that includes only flights to or from RDu
#and only flights with more than 10,000 passengers 
RDU = airportdata %>% filter(origin == "RDU"| dest == "RDU") %>% 
  filter(passengers > 10000) 

#This line arranges the RDU data set in descending order by passengers
RDU = arrange(RDU, desc(passengers))

census_api_key("de0e60afd7280f00d4484ec72292951906f6f381", install = T)

#this is me looking at what variables are available to me 
acs_vars = load_variables(2021, "acs5")
write_csv(acs_vars, "censusinfo.csv")

#ok so now its time to retrive the varibles 
Originvars = get_acs(geography = "cbsa", 
                  variables = c("origin population" = "B01003_001",
                                "origin Median Income" = "B07011_001"),
                  year = 2021, 
                  survey = "acs5",
                  output = "wide")
Destvars =  get_acs(geography = "cbsa", 
                    variables = c("Destination population" = "B01003_001",
                                  "Destination Median Income" = "B07011_001"),
                    year = 2021, 
                    survey = "acs5",
                    output = "wide")

Originvars = rename(Originvars, origin_cbsa = GEOID)
Destvars = rename(Destvars, dest_cbsa = GEOID)

RDU$origin_cbsa = as.character(RDU$origin_cbsa)
RDU$dest_cbsa = as.character(RDU$dest_cbsa)

#so now I will begin to join 
RDUinfo = left_join(RDU, Originvars, by = "origin_cbsa")
RDUinfo = left_join(RDUinfo, Destvars, by = "dest_cbsa")

#ok so now I need it to be from CBSA to CBSA
test = RDUinfo %>% group_by(dest_cbsa) %>% summarize(mean(distancemiles),
                      mean(passengers))

#so this table has all of the origin CBSAs      
originCBSA = RDUinfo %>% group_by(origin_cbsa_name) %>%
  summarize(passengers = mean(passengers),distancemiles = mean(distancemiles),
            population = mean(`origin populationE`),
           medianincome = mean(`origin Median IncomeE`))

#This table has all of the destination CBSAs
DestCBSA = RDUinfo %>% group_by(dest_cbsa_name) %>% 
  summarize(passengers = mean(passengers), distancemiles = mean(distancemiles), 
            population = mean(`Destination populationE`), 
            medianincome = mean(`Destination Median IncomeE`))

#ok so now I need to make a scatter plot showing origin/destination 
#population and total passangers 


ggplot(originCBSA, aes(x=passengers, y=population))+ geom_point()

ggplot(DestCBSA, aes(x=passengers, y=population)) + geom_point()


#ok so now I need to make a scatter plot showing flight distance 
#and total passsenegers


ggplot(originCBSA, aes(x=passengers, y=distancemiles))+ geom_point()

ggplot(DestCBSA, aes(x=passengers, y=distancemiles)) + geom_point()

#Now for extra credit I want to look at median income and passengers 
ggplot(originCBSA, aes(x=passengers, y=medianincome))+ geom_point()

ggplot(DestCBSA, aes(x=passengers, y=medianincome)) + geom_point()

#so now I need to do the regression with ALL of the flight data Not just RDU


#so that means I need to find the step where I filtered RDU info and 
#basically (but not exactly) undo that 

airportdata$origin_cbsa = as.character(airportdata$origin_cbsa)
airportdata$dest_cbsa = as.character(airportdata$dest_cbsa)

airportregress = airportdata

airportregress = left_join(airportregress, Originvars, by = "origin_cbsa")
airportregress = left_join(airportregress, Destvars, by = "dest_cbsa")

airportregress = rename(airportregress, originpop = 'origin populationE')
airportregress = rename(airportregress, destpop = 'Destination populationE')
airportregress = rename(airportregress, originincome = 'origin Median IncomeE')
airportregress = rename(airportregress, destincome = 'Destination Median IncomeE')

#ok so now I'I'm running my regression 
lilregress = lm(passengers ~distancemiles + originpop + originincome + destpop +
                  destincome, 
        data = airportregress )

summary(lilregress)

#So now I need to predict the passenger volumes for certain flights 
#but first I need to make a table 
AirCarolinaRoutes = tibble(
  origin=c("RDU", "PDX", "RDU", "ELP", "RDU", "TLH", "RDU", "SAN"),
  dest=c("PDX", "RDU", "ELP", "RDU", "TLH", "RDU", "SAN", "RDU"),
  originpop=c(13891801, 2493429, 13891801, 863807, 13891801, 382747, 13891801, 3296317),
  destpop=c(2493429, 13891801, 863807, 13891801, 382747, 13891801, 3296317, 13891801),
  originincome=c(41163, 40159, 41163, 25269, 41163,30443, 41163, 38792),
  destincome=c(40159, 41163, 25269, 41163,30443, 41163, 38792, 41163),
  distancemiles=c(2363, 2363, 1606, 1606, 496, 496, 2193, 2193)
)

#This is me predicting passenger volume
AirCarolinaRoutes$forecastdemand = predict(lilregress, AirCarolinaRoutes)