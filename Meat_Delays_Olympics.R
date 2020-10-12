# James Wen
# Qss 17, Rescue Assigment 

# Due Nov 14, 2017 


# Assignment Objectives/Location of Objectives -------------------------------------------------------

# use new data
# use factor 
# - in figure 2 and 3
# merge mulitple data frames 
# - in figure 1 and 2
# use group_by() and summarize()
# - in figure 1 and 2 
# use gather() or spread()
# - in figure 1 and 3
# make a fxn
# - in figure 1 and 2 
# use fxns for patter matching/regex 
# - in figure 1 and 2 
# polish figure as much as possible 

# Initial Settings --------------------------------------------------------

rm(list=ls())
library(tidyverse)
library(ggthemes)
library(readxl)
library(readtext)
library(rvest)

options(stringsAsFactors = FALSE)

# Figure 1 (Recalled Meat) ------------------------------------------------

# make fxn for .xlsx sheets 
figure1.wrangling.xlsx <- function(year, data.type, excel.sheet){
  
  data_source = "data/FSIS-Recall-Summary-"
  
  data <- read_xlsx(paste0(data_source, year, data.type), skip = 1, sheet = excel.sheet) %>% 
    mutate(period = year) %>% 
    mutate(reason = gsub("E.+", "E.Coli", `Reason for Recall`)) %>%   # so that all strains of E.coli are categorized together
    filter(`Pounds Recalled` != "Undetermined") %>% 
    mutate(Pounds = as.numeric(`Pounds Recalled`)) %>% 
    mutate(kilos = Pounds/2.2) %>%                                    # convert pounds to kilos
    select(period, reason, kilos) %>% 
    group_by(period, reason) %>% 
    summarize(total_kilos = sum(kilos)/1000) %>%                      # scale kilos to make graph look nicer 
    spread(reason, total_kilos)
   
  select(data)
  return(data)
}

# make same fxn for .xls sheets 
figure1.wrangling.xls <- function(year, data.type, excel.sheet){
  
  data_source = "data/FSIS-Recall-Summary-"
  
  data <- read_xls(paste0(data_source, year, data.type), skip = 1, sheet = excel.sheet) %>% 
    mutate(period = year) %>% 
    mutate(reason = gsub("E.+", "E.Coli", `Reason for Recall`)) %>% 
    filter(`Pounds Recalled` != "Undetermined") %>% 
    mutate(Pounds = as.numeric(`Pounds Recalled`)) %>% 
    mutate(kilos = Pounds/2.2) %>%                                
    select(period, reason, kilos) %>% 
    group_by(period, reason) %>% 
    summarize(total_kilos = sum(kilos)/1000) %>%                 
    spread(reason, total_kilos)
  
  select(data)
  return(data)
}

# implement fxns 
figure1.2014.data <- figure1.wrangling.xlsx(2014, ".xlsx", 1)
figure1.2013.data <- figure1.wrangling.xlsx(2013, ".xlsx", 2) 

figure1.2012.data <-figure1.wrangling.xls(2012, ".xls", 1)
figure1.2011.data <-figure1.wrangling.xls(2011, ".xls", 1) 

# combine data 
figure1.combine.2011.2012 <- rbind(figure1.2011.data, figure1.2012.data) 
figure1.combine.2011.2013 <- rbind(figure1.combine.2011.2012, figure1.2013.data)
figure1.total.data <- rbind(figure1.combine.2011.2013,figure1.2014.data)

# further finetuning 
figure1.data <- figure1.total.data %>% 
  select(period,
         E.Coli,
         `Listeria monocytogenes`, 
         Salmonella)      # get rid of observations that have no data for each year

# make figure 

ggplot(figure1.data) +
  geom_line(aes(x = period, y = Salmonella, color = "Salmonella"), size = 1) +
  geom_line(aes(x = period, y = `Listeria monocytogenes`, color = "Listeria monocytogenes"), size = 1) +
  geom_line(aes(x = period, y = `E.Coli`, color = "E.Coli"), size = 1) +
  geom_text(x = 2011.75, 
           y = 15500, 
           label = "Outbreak of Salmonella in ground turkey",
           size = 3,
           color = "blue") +
  geom_text(x = 2013,
            y = 6000,
            label = "Outbreak of E.coli in frozen foods/snack products",
            size = 3,
            color = "red") + 
  theme_economist_white() +
  labs(x = "Year",
       y = "Kilos Recalled (in thousands)",
       title = "Bacteria & Recalled Meat",
       subtitle = "Assessing outbreaks from 2011 - 2014",
       color = "Bacteria", 
       caption = "Data Source = Data.gov") 

# Figure 2 ----------------------------------------------------------------

# make fxn for wrangling csv sheets 

figure2.wrangling <- function(airport) {
  
  csv_file = paste0("data/United_", airport, ".csv")

  new <- read.csv(csv_file) %>% 
    rename(total_flights = arr_flights,
           delay_weather = weather_ct,    
           air_carrier_delay = carrier_ct,
           national_aviation_system_delay = nas_ct,
           delay_security = security_ct,
           arrived_late = late_aircraft_ct,
           canceled = arr_cancelled,
           diverted = arr_diverted) %>% 
    group_by(airport) %>% 
    summarize(total_flights = sum(total_flights),
              air_carrier_delay = sum(air_carrier_delay),
              delay_weather = sum(delay_weather),
              national_aviation_system_delay = sum(national_aviation_system_delay),
              delay_security = sum(delay_security),
              arrived_late = sum(arrived_late),
              canceled = sum(canceled),
              diverted = sum(diverted)) %>% 
    ungroup() %>% 
    mutate(total_delayed = sum(air_carrier_delay,
                               delay_weather,
                               national_aviation_system_delay,
                               delay_security,
                               arrived_late,
                               canceled,
                               diverted)) %>% 
    mutate(percent_ontime = (total_flights - total_delayed)/total_flights * 100) 
  select(new)
  return(new)
}

# make data sets for each airport 
DEN <- figure2.wrangling("DEN")
EWR <- figure2.wrangling("EWR")
IAD <- figure2.wrangling("IAD")
IAH <- figure2.wrangling("IAH")
LAX <- figure2.wrangling("LAX")
ORD <- figure2.wrangling("ORD")
SFO <- figure2.wrangling("SFO")

# bind them all together 
temp1 <- rbind(DEN,EWR)
temp2 <- rbind(temp1, IAD)
temp3 <- rbind(temp2, IAH)
temp4 <- rbind(temp3, LAX)
temp5 <- rbind(temp4, ORD)
final.airport.data <- rbind(temp5, SFO) 

# load sunshine data

url <- "https://en.wikipedia.org/wiki/List_of_cities_by_sunshine_duration#North_and_Central_America"

temp <- read_html(url) %>% 
  html_nodes("table") %>% # Select nodes from an HTML document
  html_table(fill = TRUE) # Parse an html table into a data frame.

sunshine.data <- temp[[8]] %>% 
  filter(City == "Denver" |
           City == "Houston" |
           City == "Chicago" |
           City == "Washington, D.C." |      
           City == "New York City" |
           City == "Los Angeles" |
           City == "San Francisco") %>% 
  rename(hours_sunshine = Year,
         airport = City) %>% 
  select(airport, hours_sunshine) %>% 
  mutate(airport = factor(airport, labels = c("ORD",          # IAD left out bc previous filter() would not pick up Washington DC 
                                        "DEN",
                                        "IAH",
                                        "LAX",
                                        "EWR",
                                        "SFO")))    

# combine airport data and sunshine data

figure2.final.data <- left_join(final.airport.data, sunshine.data, by = "airport") %>% 
  mutate(hours_sunshine = ifelse(is.na(hours_sunshine), "2,527.7", hours_sunshine), # above filter would not pick up DC, so have to manually insert value
         hours_sunshine = gsub("\\,", "", hours_sunshine),                          # remove comma before converting to numeric 
         percent_ontime = as.numeric(percent_ontime), 
         hours_sunshine = as.numeric(hours_sunshine)/360,
         airport = factor(airport, levels = c("DEN",                                  # puts them in decreasing order on graph
                                              "IAD",
                                              "IAH",
                                              "ORD",
                                              "SFO",
                                              "EWR",
                                              "LAX"))) 
         

# make figure
ggplot(figure2.final.data) +
  geom_bar(aes(x = airport, y = percent_ontime, fill = hours_sunshine),stat = "identity") +
  scale_fill_gradient(high = "light blue", low = "blue") +
  theme_fivethirtyeight() +
  labs(x = "Airport", 
       y = "Percent of Ontime Flights",
       title = "United Airlines Flight Delays by Hub",
       subtitle = "How weather and delays arn't always related",
       caption = "Data Source = transtats.bts.gov",
       fill = "Hours of Sunshine/Day") 

# Figure 3 ----------------------------------------------------------------

# load Germany Medal Data 

data.3 <- read.csv("data/olympic_game_medals_cleaned.csv") %>% 
  filter(Country == "Germany" |
           Country == "West Germany" |
           Country == "East Germany") %>% 
  select(Country, Summer_Participated, Summer_Gold, Summer_Silver, Summer_Bronze) %>% 
  gather(medal, count, 3:5) %>% 
  mutate(average = count/Summer_Participated) %>% 
  mutate(medal = factor(medal, levels = c("Summer_Bronze",
                                     "Summer_Silver",
                                     "Summer_Gold"))) %>% 
  mutate(medal = factor(medal, labels = c("Bronze",
                                          "Silver",
                                          "Gold"))) %>% 
  mutate(Country = factor(Country, levels = c("Germany",
                                            "East Germany",
                                            "West Germany")))
  
# make figure

ggplot(data.3) +
  geom_bar(aes(x = medal, y = average, fill = medal),  position = "dodge", stat = "identity") +
  facet_wrap(~Country, nrow = 3) +
  scale_fill_manual(values = c("brown", "grey", "yellow")) +
  theme_economist() + 
  labs(x = "Medal",
       y = "Average Medals Won Per Summer Game",
       title = "E. Germany & W. Germany vs. Germany in the Olympics",
       subtitle = "How East and West Germany Compared to Modern Germany in the Summer Olympics",
       caption = "Data Source = Final Exam Data Bank") +
  guides(fill = "none") 
  




