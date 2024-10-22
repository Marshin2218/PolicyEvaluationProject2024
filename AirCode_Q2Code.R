#NOTE: THERE are multiple setwd() changes in document


library(tidyverse)
library(haven)
library(dplyr)
library(skimr)
library(lubridate)
library(zoo)
library(rdhs)
library(janitor)
library(readr)
library(TSstudio)
library(scales)

#setwd for this file
#setwd("")

library(tidyverse)
library(haven)
library(dplyr)
library(skimr)
library(lubridate)
library(zoo)
library(rdhs)
library(janitor)
library(readr)
library(TSstudio)
library(kableExtra)

aqi_2013 <- read_csv("daily_aqi_by_county_2013.csv")
aqi_2014 <- read_csv("daily_aqi_by_county_2014.csv")
aqi_2015 <- read_csv("daily_aqi_by_county_2015.csv")
aqi_2016 <- read_csv("daily_aqi_by_county_2016.csv")
aqi_2017 <- read_csv("daily_aqi_by_county_2017.csv")
aqi_2018 <- read_csv("daily_aqi_by_county_2018.csv")
aqi_2019 <- read_csv("daily_aqi_by_county_2019.csv")

#annual data - read in
#setwd("")
aqi_2013_ann <- read_csv("annual_conc_by_monitor_2013.csv")
aqi_2014_ann <- read_csv("annual_conc_by_monitor_2014.csv")
aqi_2015_ann <- read_csv("annual_conc_by_monitor_2015.csv")
aqi_2016_ann <- read_csv("annual_conc_by_monitor_2016.csv")
aqi_2017_ann <- read_csv("annual_conc_by_monitor_2017.csv")
aqi_2018_ann <- read_csv("annual_conc_by_monitor_2018.csv")
aqi_2019_ann <- read_csv("annual_conc_by_monitor_2019.csv")

#aqi_sites file readin
#setwd("")
aqs_sites <- read_csv("aqs_sites.csv")


#Filter out NYC Observations
five_boroughs <- c("New York", "Kings", "Bronx", "Queens", "Richmond")

aqi_2013 <- aqi_2013 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_2014 <- aqi_2014 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_2015 <- aqi_2015 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_2016 <- aqi_2016 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_2017 <- aqi_2017 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_2018 <- aqi_2018 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_2019 <- aqi_2019 %>% filter(`State Name` == "New York" &
                                  `county Name` %in% five_boroughs) 
aqi_dta <- rbind(aqi_2013, aqi_2014, aqi_2015,
                 aqi_2016, aqi_2017, aqi_2018, aqi_2019)

aqi_dta$Date
ggplot(aqi_dta, aes(x = Date, y = AQI, group = `county Name`, color = `county Name`)) +
  geom_line() +
  facet_wrap(~`county Name`, scales = "free_y") +
  labs(title = "AQI Time Series by County",
       x = "Date",
       y = "AQI") +
  theme_minimal()

#annual data - just NY - note that their' county names has "County" capitalized compared with aqi_county. 
aqi_2013_ann <- aqi_2013_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_2014_ann <- aqi_2014_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_2015_ann <- aqi_2015_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_2016_ann <- aqi_2016_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_2017_ann <- aqi_2017_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_2018_ann <- aqi_2018_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_2019_ann <- aqi_2019_ann %>% filter(`State Name` == "New York" &
                                          `County Name` %in% five_boroughs) 
aqi_ann <- rbind(aqi_2013_ann, aqi_2014_ann, aqi_2015_ann, aqi_2016_ann,
                 aqi_2017_ann, aqi_2018_ann, aqi_2019_ann)

aqs_sites <- aqs_sites %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs)


#Creating "Defining Site" column (like in aqi_dta) for aqi_ann and aqs_sites data sets

#view(unique(aqi_dta$`Defining Site`))
#view(unique(aqi_ann$`Site Num`))
#view(unique(aqi_ann$`County Code`))
#number of digits looks fine, just got to smash them together

aqs_sites$`Defining Site` <- paste(aqs_sites$`State Code`,aqs_sites$`County Code`,aqs_sites$`Site Number`, sep="-")
aqi_ann$`Defining Site` <- paste(aqi_ann$`State Code`,aqi_ann$`County Code`,aqi_ann$`Site Num`, sep="-")

colnames(aqi_ann)[colnames(aqi_ann) == 'Site Num'] <- 'Site Number' #renaming column to match other columns


#joining aqi_ann site names to aqs
aqi_ann_sitename <- aqi_ann %>% 
  dplyr::select(`Defining Site`, `Certification Indicator`, `Local Site Name`, `Address`)

aqs_w_aqiann <- left_join(aqs_sites, aqi_ann_sitename, by = "Defining Site")

aqs_w_aqiann <- aqs_w_aqiann %>% distinct(`Defining Site`, .keep_all = T)

colnames(aqs_w_aqiann)[colnames(aqs_w_aqiann) == 'Local Site Name.y'] <- 'Local Site Name_aqiann' #renaming to indicate where name came from to see matches
colnames(aqs_w_aqiann)[colnames(aqs_w_aqiann) == 'Address.y'] <- 'Address_aqiann' #renaming to indicate where name came from to see matches

#joining site info to daily info
aqi_daily_joined <- left_join(aqi_dta, aqs_w_aqiann, by = "Defining Site")

unique(aqi_dta$`Defining Site`) #same number of unique sites (18)
unique(aqi_daily_joined$`Defining Site`)



#Looking at List of Sites
aqi_daily_joined_unique <- aqi_daily_joined %>% distinct(`Defining Site`, .keep_all = T)
#notes: Kings county doesn't have a certified meter out of all the years
#none of them are closed

#write_csv(aqi_daily_joined_unique, "UniqueSites_Info_AQI_daily.csv")



#Looking at list of sites and which parameter they have the most of
aqi_dta %>% group_by(`Defining Site`, `Defining Parameter`) %>% dplyr::summarize( n = n())

#Combining AQI daily with Unique site info *Note that we found out that not all AQI summary pollutants are calculated at each site

#setwd for this file
#setwd("")
UniqueSites_Info_AQI_daily <- read_csv("UniqueSites_Info_AQI_daily_update2.csv")
aqi_daily_joined <- left_join(aqi_daily_joined, UniqueSites_Info_AQI_daily, by = "Defining Site")

aqi_daily_joined <- aqi_daily_joined %>% mutate(uhf34code = as.factor(uhf34code),
                                                uhf34nhood = as.factor(uhf34nhood),
                                                uhf42code = as.factor(uhf42code),
                                                uhf42nhood = as.factor(uhf42nhood),
                                                locatdesc = as.factor(locatdesc)) %>% 
  rename('State Name' = 'State Name.x',
         'State Code' = 'State Code.x',
         'County Code'='County Code.x',
         'Address' = 'Address.x',
         'Local Site Name' = 'Local Site Name.x')

aqi_daily_joined <- aqi_daily_joined %>% mutate(year = year(Date),
                                                month = month(Date),
                                                mdate = mday(Date))

aqi_daily_joined <- aqi_daily_joined %>% relocate(c(year, month, mdate), .before = AQI)

AllSiteinfo <- aqi_daily_joined %>% distinct(`Defining Site`, .keep_all = T)

#aqi_daily_joinedpm <- aqi_daily_joined #%>% filter(`Defining Parameter` == "PM2.5")

#aqi_daily_joinedpm %>% group_by(`Defining Site`, year) %>% dplyr::summarize(n =n())
#36-061-0079 - has very low number of readings across the years
#36-081-0125 only has 2017 and 2018 - only one that doesn't have data for all 7 years (when it comes to PM2.5)

aqi_uniqueSitebyDate <- aqi_daily_joined %>% group_by(`Defining Site`, year) %>% dplyr::summarize(n =n())
#unique(aqi_uniqueSitebyDate$`Defining Site`)
#can see that 0067 (from Long Island) does not appear on this PM.25 list
aqi_uniqueSitebyDate
#36-061-0079 - has very low number of readings across the years
#36-081-0125 only has 2017 and 2018 - only one that doesn't have data for all 7 years (when it comes to PM2.5)
length(unique(aqi_uniqueSitebyDate$`Defining Site`))
#17 out of are 18 sites are on this list (for PM2.5), that's why there's 115 rows (7 * 17 +3 (from site 0125))
#0067



length(unique(aqi_uniqueSitebyDate$`Defining Site`))
#17 out of are 18 sites are on this list (for PM2.5), that's why there's 115 rows (7 * 17 +3 (from site 0125))
#0067
unique(aqi_daily_joined$`Defining Site`)
aqipmDateuh34f<- aqi_daily_joined%>% group_by(uhf34code, `Defining Site`, Date)
aqipmDateuh34f <- aqipmDateuh34f %>% relocate(c(uhf34code, uhf34nhood), .after = `Defining Parameter`)
aqipmDateuh42f<- aqi_daily_joined %>% group_by(uhf42code, `Defining Site`, Date)
aqipmDateuh42f <- aqipmDateuh34f %>% relocate(c(uhf42code, uhf34nhood), .after = `Defining Parameter`)



length(unique(aqi_daily_joined$uhf34code)) #11
length(unique(aqi_daily_joined$uhf42code)) #13

table(aqi_daily_joined$uhf34code, aqi_daily_joined$year)
tablist34 <- list()
years <- seq(2013,2019, by = 1)
for (i in years){
  aqidyear <- dplyr::filter(aqi_daily_joined, year == i)
  tablist34[[which(years == i)]] <- table(aqidyear$uhf34code, aqidyear$month)
}

tablist34 #see what sites have an observation in every month in every year
tablist42 <- list()
years <- seq(2013,2019, by = 1)
for (i in years){
  aqidyear <- dplyr::filter(aqi_daily_joined, year == i)
  tablist42[[which(years == i)]] <- table(aqidyear$uhf42code, aqidyear$month)
}

tablist42 #see what sites have an observation in every month in every year


#Good sites after not filtering by just PM2.5
Good34 <- c(201, 205, 211, 301, 309310, 404406, 501502, 503504, 105106107)
Good42 <- c(103, 107, 201, 205, 211, 301, 309, 406, 501, 503)

#get monthly averages per neighborhood by year, plot them
uh34pldata <- aqi_daily_joined %>% group_by(year, month, uhf34code,uhf34nhood) %>% dplyr::summarize(avgMthAQI = mean(AQI)) #figure out how to keep other columns later
uh34pldata <- uh34pldata %>% filter(`uhf34code` %in% Good34)
uh34mthaqiall <- ggplot(data = uh34pldata, mapping = aes(x = month, y = avgMthAQI, col = as.factor(uhf34code))) +
  geom_line() + geom_point()+ 
  #scale_x_continuous(labels = c('1', '2', "3", "4", "5", "6")) 
  labs(title = "Monthly AQI by Year and UHF34 Site", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uh34mthaqiall
ggsave("mnthaqiuh34.png", uh34mthaqiall) 


#get monthly averages per neighborhood by year, plot them
uh42pldata <- aqi_daily_joined %>% group_by(year, month, uhf42code,uhf42nhood) %>% dplyr::summarize(avgMthAQI = mean(AQI)) #figure out how to keep other columns later
uh42pldata <- uh42pldata %>% filter(`uhf42code` %in% Good42)
uh42mthaqiall <- ggplot(data = uh42pldata, mapping = aes(x = month, y = avgMthAQI, col = as.factor(uhf42code))) +
  geom_point()+ geom_line() +
  labs(title = "Monthly AQI by Year and UHF42 Site", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uh42mthaqiall
ggsave("mnthaqiuh42.png", uh42mthaqiall) 


#get monthly averages per neighborhood by year, plot them
uh34pldata2 <- aqi_daily_joined %>% group_by(year, month, uhf34code) %>% dplyr::summarize(avgMthAQI = mean(AQI)) #figure out how to keep other columns later
uh34pldata2_201 <- uh34pldata %>% filter(uhf34code==201)
ggplot(data = uh34pldata2_201, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_line(col = 'red') + geom_point(col = 'red')+
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)


uh34pldata2_205 <- uh34pldata %>% filter(uhf34code==205)
ggplot(data = uh34pldata2_205, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'orange')+ geom_line(col = 'orange') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)


uh34pldata2_211 <- uh34pldata %>% filter(uhf34code==211)
ggplot(data = uh34pldata2_211, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'gold')+ geom_line(col = 'gold') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)


uh34pldata2_301 <- uh34pldata %>% filter(uhf34code==301)
ggplot(data = uh34pldata2_301, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'limegreen')+ geom_line(col = 'limegreen') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)

uh34pldata2_309310 <- uh34pldata %>% filter(uhf34code==309310)
ggplot(data = uh34pldata2_309310, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'blue')+ geom_line(col = 'blue') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)


uh34pldata2_404406 <- uh34pldata %>% filter(uhf34code==404406)
ggplot(data = uh34pldata2_404406, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'purple')+ geom_line(col = 'purple') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)


uh34pldata2_501502 <- uh34pldata %>% filter(uhf34code==501502)
ggplot(data = uh34pldata2_501502, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'hotpink')+ geom_line(col = 'hotpink') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)

uh34pldata2_503504 <- uh34pldata %>% filter(uhf34code==503504)
ggplot(data = uh34pldata2_503504, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1, col = 'steelblue')) +
  geom_point(col = 'steelblue')+ geom_line(col = 'steelblue') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)

uh34pldata2_100s <- uh34pldata %>% filter(uhf34code==105106107)
ggplot(data = uh34pldata2_100s, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'brown')+ geom_line(col = 'brown') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)


#UHF42
#get monthly averages per neighborhood by year, plot them
#uh42pldata2 <- uh42pldata %>% filter(`uhf42code` %in% Good42)

uh42pldata2 <- aqi_daily_joined %>% group_by(year, month, uhf42code) %>% dplyr::summarize(avgMthAQI = mean(AQI)) %>% filter(`uhf42code` %in% Good42) 

uh42pldata2yearavg <- aqi_daily_joined %>% group_by(year,uhf42code) %>% dplyr::summarize(avgMthAQI = mean(AQI)) %>% filter(`uhf42code` %in% Good42)

uh42pldata2yearavg_meantab <- uh42pldata2yearavg %>% pivot_wider(names_from = uhf42code, names_prefix="UHF42_", values_from = avgMthAQI)
uh42pldata2yearavg_meantab <- kable(uh42pldata2yearavg_meantab)

uh42pldata2yearavgplt <- ggplot(data = uh42pldata2yearavg, mapping = aes(x = as.factor(year), y = avgMthAQI, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Monthtly AQI", x = "Year", y = "Avg Mnthly AQI")+
  facet_wrap(~uhf42code)
uh42pldata2yearavgplt
#ggsave("uh42pldata2yearavgplt.png", uh42pldata2yearavgplt)


uh42pldata2_103 <- uh42pldata %>% filter(uhf42code==103)
uhf42mthaqi103 <- ggplot(data = uh42pldata2_103, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'red')+ geom_line(col = 'red') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 103: Fordham - Bronx Park", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi103
#ggsave("uhf42mthaqi103.png", uhf42mthaqi103) 


uh42pldata2_107 <- uh42pldata %>% filter(uhf42code==107)
uhf42mthaqi107 <- ggplot(data = uh42pldata2_107, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'orange')+ geom_line(col = 'orange') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 107: Hunts Point - Mott Haven", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi107
#ggsave("uhf42mthaqi107.png", uhf42mthaqi107) 

uh42pldata2_201 <- uh42pldata %>% filter(uhf42code==201)
uhf42mthaqi201 <- ggplot(data = uh42pldata2_201, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'gold')+ geom_line(col = 'gold') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 201: Greenpoint", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi201
#ggsave("uhf42mthaqi201.png", uhf42mthaqi201)


uh42pldata2_205 <- uh42pldata %>% filter(uhf42code==205)
uhf42mthaqi205<- ggplot(data = uh42pldata2_205, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'limegreen')+ geom_line(col = 'limegreen') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 205: Sunset Park", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi205
#ggsave("uhf42mthaqi205.png", uhf42mthaqi205)


uh42pldata2_211 <- uh42pldata %>% filter(uhf42code==211)
uhf42mthaqi211 <- ggplot(data = uh42pldata2_211, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'blue')+ geom_line(col = 'blue') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 211: Williamsburg - Bushwick", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi211
#ggsave("uhf42mthaqi211.png", uhf42mthaqi211)


uh42pldata2_301 <- uh42pldata %>% filter(uhf42code==301)
uhf42mthaqi301 <- ggplot(data = uh42pldata2_301, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'purple')+ geom_line(col = 'purple') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 301: Washington Heights - Inwood", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi301
#ggsave("uhf42mthaqi301.png", uhf42mthaqi301)


uh42pldata2_309 <- uh42pldata %>% filter(uhf42code==309)
uhf42mthaqi309 <- ggplot(data = uh42pldata2_309, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'hotpink')+ geom_line(col = 'hotpink') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 309: Union Square - Lower East Side", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi309
#ggsave("uhf42mthaqi309.png", uhf42mthaqi309)


uh42pldata2_406 <- uh42pldata %>% filter(uhf42code==406)
uhf42mthaqi406 <- ggplot(data = uh42pldata2_406, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1, col = 'steelblue')) +
  geom_point(col = 'steelblue')+ geom_line(col = 'steelblue') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 406: Fresh Meadows", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi406
#ggsave("uhf42mthaqi406.png", uhf42mthaqi406)


uh42pldata2_501 <- uh42pldata %>% filter(uhf42code==501)
uhf42mthaqi501 <- ggplot(data = uh42pldata2_501, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'brown')+ geom_line(col = 'brown') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 501: Port Richmond", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi501
#ggsave("uhf42mthaqi501.png", uhf42mthaqi501)


uh42pldata2_503 <- uh42pldata %>% filter(uhf42code==503)
uhf42mthaqi503 <- ggplot(data = uh42pldata2_503, mapping = aes(x = as.factor(month), y = avgMthAQI, group = 1)) +
  geom_point(col = 'forestgreen')+ geom_line(col = 'forestgreen') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 503: Willowbrook", x = "Month", y = "Avg Mnthly AQI")+
  facet_wrap(~year)
uhf42mthaqi503
#ggsave("uhf42mthaqi503.png", uhf42mthaqi503)


#Asthma emergency room visits due to PM2.5
#setwd()
asthmaemerOG <- read_csv("NYC EH Data Portal - Asthma emergency department visits due to PM2.5 (full table).csv")
asthmaemer <- asthmaemerOG

asthmaemer <- asthmaemer %>% filter(GeoType == "UHF42")
#asthmaemer <- asthmaemer %>% filter((TimePeriod != "2005-2007") & (TimePeriod != "2009-2011"))

asthmaemer <- asthmaemer %>% filter(GeoID %in% Good42)

#just looking at 1 neighborhood
asthmaemer <- asthmaemer |> 
  pivot_longer(
    cols = starts_with("Estimated annual"), 
    names_to = "Estmgrp", 
    values_to = "values")

asthmaemerNos <- asthmaemer %>% filter(Estmgrp %in% c("Estimated annual number (age 18+)", "Estimated annual number (under age 18)"))

uhf42asth103 <- ggplot(filter(asthmaemerNos, GeoID == 103), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth103


uhf42asth107 <- ggplot(filter(asthmaemerNos, GeoID == 107), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth107
#ggsave("uhf42asth107.png", uhf42asth107)


uhf42asth201<- ggplot(filter(asthmaemerNos, GeoID == 201), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth201
#ggsave("uhf42asth201.png", uhf42asth201)


uhf42asth205 <- ggplot(filter(asthmaemerNos, GeoID == 205), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth205
#ggsave("uhf42asth205.png", uhf42asth205)


uhf42asth211 <- ggplot(filter(asthmaemerNos, GeoID == 211), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth211
#ggsave("uhf42asth211.png", uhf42asth211)


uhf42asth301 <- ggplot(filter(asthmaemerNos, GeoID == 301), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth301
#ggsave("uhf42asth301.png", uhf42asth301)


uhf42asth309 <- ggplot(filter(asthmaemerNos, GeoID == 309), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth309
#ggsave("uhf42asth309.png", uhf42asth309)


uhf42asth406 <- ggplot(filter(asthmaemerNos, GeoID == 406), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth406
#ggsave("uhf42asth406.png", uhf42asth406)


uhf42asth501 <- ggplot(filter(asthmaemerNos, GeoID == 501), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth501
#ggsave("uhf42asth501.png", uhf42asth501)


uhf42asth503 <- ggplot(filter(asthmaemerNos, GeoID == 503), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma ER from PM2.5", x = "", y = "Value")
uhf42asth503
#ggsave("uhf42asth503.png", uhf42asth503)


# cardiovascular observations due to PM2.5
#setwd("")
cardihospOG <- read_csv("NYC EH Data Portal - Cardiovascular hospitalizations due to PM2.5 (age 40+) (full table).csv")
cardihosp <- cardihospOG

cardihosp <- cardihosp %>% filter(GeoType == "UHF42")
#cardihosp <- cardihosp %>% filter((TimePeriod != "2005-2007") & (TimePeriod != "2009-2011"))

cardihosp <- cardihosp %>% filter(GeoID %in% Good42)

cardihosp <- cardihosp |> 
  pivot_longer(
    cols = starts_with("Estimated annual"), 
    names_to = "Estmgrp", 
    values_to = "values") #only has annual rate

uhf42card103 <- ggplot(filter(cardihosp, GeoID == 103), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card103
#ggsave("uhf42card107.png", uhf42card103)

uhf42card107 <- ggplot(filter(cardihosp, GeoID == 107), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card107
#ggsave("uhf42card107.png", uhf42card107)

uhf42card201 <- ggplot(filter(cardihosp, GeoID == 201), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card201
#ggsave("uhf42card201.png", uhf42card201)

uhf42card205 <- ggplot(filter(cardihosp, GeoID == 205), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card205
#ggsave("uhf42card205.png", uhf42card205)

uhf42card211 <- ggplot(filter(cardihosp, GeoID == 211), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 211: Williamsburg - Bushwick, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card211
#ggsave("uhf42card211.png", uhf42card211)

uhf42card301 <- ggplot(filter(cardihosp, GeoID == 301), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card301
#ggsave("uhf42card301.png", uhf42card301)

uhf42card309 <- ggplot(filter(cardihosp, GeoID == 309), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card309
#ggsave("uhf42card309.png", uhf42card309)

uhf42card406 <- ggplot(filter(cardihosp, GeoID == 406), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card406
#ggsave("uhf42card406.png", uhf42card406)

uhf42card501 <- ggplot(filter(cardihosp, GeoID == 501), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card501
#ggsave("uhf42card501.png", uhf42card501)

uhf42card503 <- ggplot(filter(cardihosp, GeoID == 503), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Cardvasc Hosptl. from PM2.5", x = "", y = "Value")
uhf42card503
#ggsave("uhf42card503.png", uhf42card503)



#respiratory analysis pm2.5
#setwd("")
resphospOG <- read_csv("NYC EH Data Portal - Respiratory hospitalizations due to PM2.5 (age 20+) (full table).csv")
resphosp <- resphospOG

resphosp <- resphosp %>% filter(GeoType == "UHF42")
#resphosp <- resphosp %>% filter((TimePeriod != "2005-2007") & (TimePeriod != "2009-2011"))

resphosp <- resphosp %>% filter(GeoID %in% Good42)

#just looking at 1 neighborhood
resphosp <- resphosp |> 
  pivot_longer(
    cols = starts_with("Estimated annual"), 
    names_to = "Estmgrp", 
    values_to = "values")

uhf42resp103 <- ggplot(filter(resphosp, GeoID == 103), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp103
#ggsave("uhf42resp103.png", uhf42resp103)

uhf42resp107 <- ggplot(filter(resphosp, GeoID == 107), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp107
#ggsave("uhf42resp107.png", uhf42resp107)

uhf42resp201 <- ggplot(filter(resphosp, GeoID == 201), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp201
#ggsave("uhf42resp201.png", uhf42resp201)

uhf42resp205 <- ggplot(filter(resphosp, GeoID == 205), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp205
#ggsave("uhf42resp205.png", uhf42resp205)

uhf42resp211 <- ggplot(filter(resphosp, GeoID == 211), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp211
#ggsave("uhf42resp211.png", uhf42resp211)

uhf42resp301 <- ggplot(filter(resphosp, GeoID == 301), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp301
#ggsave("uhf42resp301.png", uhf42resp301)

uhf42resp309 <- ggplot(filter(resphosp, GeoID == 309), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp309
#ggsave("uhf42resp309.png", uhf42resp309)

uhf42resp406 <- ggplot(filter(resphosp, GeoID == 406), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp406
#ggsave("uhf42resp406.png", uhf42resp406)

uhf42resp501 <- ggplot(filter(resphosp, GeoID == 501), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp501
#ggsave("uhf42resp501.png", uhf42resp501)

uhf42resp503<- ggplot(filter(resphosp, GeoID == 503), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Resp Hosp from PM2.5", x = "", y = "Value")
uhf42resp503


#Deaths to PM2.5
#setwd(")
deathsOG <- read_csv("NYC EH Data Portal - Deaths due to PM2.5 (full table).csv")
deaths <- deathsOG

deaths <- deaths %>% filter(GeoType == "UHF42")

deaths <- deaths %>% filter(GeoID %in% Good42)

#just looking at 1 neighborhood
deaths <- deaths |> 
  pivot_longer(
    cols = starts_with("Estimated annual"), 
    names_to = "Estmgrp", 
    values_to = "values")

uhf42deat103<-ggplot(filter(deaths, GeoID == 103), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Deaths from PM2.5", x = "", y = "Value")
uhf42deat103
#ggsave("uhf42deat103.png", uhf42deat103)

uhf42deat107<-ggplot(filter(deaths, GeoID == 107), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Deaths from PM2.5", x = "", y = "Value")
uhf42deat107
#ggsave("uhf42deat107.png", uhf42deat107)

uhf42deat201<-ggplot(filter(deaths, GeoID == 201), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Deaths from PM2.5", x = "", y = "Value")
uhf42deat201
#ggsave("uhf42deat201.png", uhf42deat201)

uhf42deat205<-ggplot(filter(deaths, GeoID == 205), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Deaths from PM2.5", x = "", y = "Value")
uhf42deat205
#ggsave("uhf42deat205.png", uhf42deat205)

uhf42deat211<-ggplot(filter(deaths, GeoID == 211), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Deaths from PM2.5", x = "", y = "Value")
uhf42deat211
#ggsave("uhf42deat211.png", uhf42deat211)

uhf42deat301<-ggplot(filter(deaths, GeoID == 301), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Deaths from PM2.5", x = "", y = "Value")
uhf42deat301
#ggsave("uhf42deat301.png", uhf42deat301)

uhf42deat309<-ggplot(filter(deaths, GeoID == 309), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Deaths from PM2.5", x = "", y = "Value")
uhf42deat309
#ggsave("uhf42deat309.png", uhf42deat309)

uhf42deat406<-ggplot(filter(deaths, GeoID == 406), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Deaths from PM2.5", x = "", y = "Value")
uhf42deat406
#ggsave("uhf42deat406.png", uhf42deat406)

uhf42deat501<-ggplot(filter(deaths, GeoID == 501), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Deaths from PM2.5", x = "", y = "Value")
uhf42deat501
#ggsave("uhf42deat501.png", uhf42deat501)

uhf42deat503<-ggplot(filter(deaths, GeoID == 503), mapping = aes(x = Estmgrp, y = values, fill = TimePeriod)) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Deaths from PM2.5", x = "", y = "Value")
uhf42deat503



#ASthma Emergency Department visits (4 and under)
#setwd("")
asthmaemer4undOG<- read_csv("NYC EH Data Portal - Asthma emergency department visits (age 4 and under) (filtered).csv")
asthmaemer4und <- asthmaemer4undOG

asthmaemer4und <- asthmaemer4und %>% filter(GeoTypeDesc == "UHF 42")
#deaths <- deaths %>% filter((TimePeriod != "2005-2007") & (TimePeriod != "2009-2011"))

asthmaemer4und <- asthmaemer4und %>% filter(GeoID %in% Good42) %>% arrange(TimePeriod)

#just looking at 1 neighborhood
asthmaemer4und <- asthmaemer4und |> 
  pivot_longer(
    cols = c("Estimated annual rate per 10,000", "Number"), 
    names_to = "Estimate", 
    values_to = "values")

uhf42asthmaemer4und103<-ggplot(filter(asthmaemer4und, GeoID == 103), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und103
#ggsave("uhf42asthmaemer4un103.png", uhf42asthmaemer4und103)

uhf42asthmaemer4und107<-ggplot(filter(asthmaemer4und, GeoID == 107), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und107
#ggsave("uhf42asthmaemer4und107.png", uhf42asthmaemer4und107)

uhf42asthmaemer4und201<-ggplot(filter(asthmaemer4und, GeoID == 201), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und201
#ggsave("uhf42asthmaemer4und201.png", uhf42asthmaemer4und201)

uhf42asthmaemer4und205<-ggplot(filter(asthmaemer4und, GeoID == 205), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und205
#ggsave("uhf42asthmaemer4und205.png", uhf42asthmaemer4und205)

uhf42asthmaemer4und211<-ggplot(filter(asthmaemer4und, GeoID == 211), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und211
#ggsave("uhf42asthmaemer4und211.png", uhf42asthmaemer4und211)

uhf42asthmaemer4und301<-ggplot(filter(asthmaemer4und, GeoID == 301), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und301
#ggsave("uhf42asthmaemer4und301.png", uhf42asthmaemer4und301)

uhf42asthmaemer4und309<-ggplot(filter(asthmaemer4und, GeoID == 309), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und309
#ggsave("uhf42asthmaemer4und309.png", uhf42asthmaemer4und309)

uhf42asthmaemer4und406<-ggplot(filter(asthmaemer4und, GeoID == 406), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und406
#ggsave("uhf42asthmaemer4und406.png", uhf42asthmaemer4und406)

uhf42asthmaemer4und501<-ggplot(filter(asthmaemer4und, GeoID == 501), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und501
#ggsave("uhf42asthmaemer4und501.png", uhf42asthmaemer4und501)

uhf42asthmaemer4und503<-ggplot(filter(asthmaemer4und, GeoID == 503), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma ER >=4", x = "", y = "Value")
uhf42asthmaemer4und503
#ggsave("uhf42asthmaemer4und503.png", uhf42asthmaemer4und503)


#Asthma Emergency Department visits (5-17)
#setwd(")
asthmaemer5_17OG<- read_csv("NYC EH Data Portal - Asthma emergency department visits (age 5 to 17) (filtered).csv")
asthmaemer5_17 <- asthmaemer5_17OG

asthmaemer5_17 <- asthmaemer5_17 %>% filter(GeoTypeDesc == "UHF 42")

asthmaemer5_17 <- asthmaemer5_17 %>% filter(GeoID %in% Good42) %>% arrange(TimePeriod)

asthmaemer5_17 <- asthmaemer5_17 |> 
  pivot_longer(
    cols = c("Estimated annual rate per 10,000", "Number"), 
    names_to = "Estimate", 
    values_to = "values")

uhf42asthmaemer5_17103<-ggplot(filter(asthmaemer5_17, GeoID == 103), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17103
#ggsave("uhf42asthmaemer5_17103.png", uhf42asthmaemer5_17103)

uhf42asthmaemer5_17107<-ggplot(filter(asthmaemer5_17, GeoID == 107), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17107
#ggsave("uhf42asthmaemer5_17107.png", uhf42asthmaemer5_17107)

uhf42asthmaemer5_17201<-ggplot(filter(asthmaemer5_17, GeoID == 201), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17201
#ggsave("uhf42asthmaemer5_17201.png", uhf42asthmaemer5_17201)

uhf42asthmaemer5_17205<-ggplot(filter(asthmaemer5_17, GeoID == 205), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17205
#ggsave("uhf42asthmaemer5_17205.png", uhf42asthmaemer5_17205)

uhf42asthmaemer5_17211<-ggplot(filter(asthmaemer5_17, GeoID == 211), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17211
#ggsave("uhf42asthmaemer5_17211.png", uhf42asthmaemer5_17211)

uhf42asthmaemer5_17301<-ggplot(filter(asthmaemer5_17, GeoID == 301), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17301
#ggsave("uhf42asthmaemer5_17301.png", uhf42asthmaemer5_17301)

uhf42asthmaemer5_17309<-ggplot(filter(asthmaemer5_17, GeoID == 309), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17309
#ggsave("uhf42asthmaemer5_17309.png", uhf42asthmaemer5_17309)

uhf42asthmaemer5_17406<-ggplot(filter(asthmaemer5_17, GeoID == 406), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17406
#ggsave("uhf42asthmaemer5_17406.png", uhf42asthmaemer5_17406)

uhf42asthmaemer5_17501<-ggplot(filter(asthmaemer5_17, GeoID == 501), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17501
#ggsave("uhf42asthmaemer5_17501.png", uhf42asthmaemer5_17501)

uhf42asthmaemer5_17503<-ggplot(filter(asthmaemer5_17, GeoID == 503), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma ER 5-17", x = "", y = "Value")
uhf42asthmaemer5_17503
#ggsave("uhf42asthmaemer5_17503.png", uhf42asthmaemer5_17503)


#Asthma Emergency Department visits (18+)

#setwd("")
asthmaemer18adOG<- read_csv("NYC EH Data Portal - Asthma emergency department visits (adults) (filtered).csv")
asthmaemer18ad <- asthmaemer18adOG

asthmaemer18ad <- asthmaemer18ad %>% filter(GeoTypeDesc == "UHF 42")

asthmaemer18ad <- asthmaemer18ad %>% filter(GeoID %in% Good42) %>% arrange(desc(TimePeriod))

#just looking at 1 neighborhood
asthmaemer18ad <- asthmaemer18ad |> 
  pivot_longer(
    cols = c("Estimated annual rate per 10,000", "Number"), 
    names_to = "Estimate", 
    values_to = "values")

asthmaemer18ad$values <-as.numeric(gsub('[,]', '',asthmaemer18ad$values))

uhf42asthmaemer18ad103<-ggplot(filter(asthmaemer18ad, GeoID == 103), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad103
#ggsave("uhf42asthmaemer18ad103.png", uhf42asthmaemer18ad103)

uhf42asthmaemer18ad107<-ggplot(filter(asthmaemer18ad, GeoID == 107), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad107
#ggsave("uhf42asthmaemer18ad107.png", uhf42asthmaemer18ad107)

uhf42asthmaemer18ad201<-ggplot(filter(asthmaemer18ad, GeoID == 201), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad201
#ggsave("uhf42asthmaemer18ad201.png", uhf42asthmaemer18ad201)

uhf42asthmaemer18ad205<-ggplot(filter(asthmaemer18ad, GeoID == 205), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad205
#ggsave("uhf42asthmaemer18ad205.png", uhf42asthmaemer18ad205)

uhf42asthmaemer18ad211<-ggplot(filter(asthmaemer18ad, GeoID == 211), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad211
#ggsave("uhf42asthmaemer18ad211.png", uhf42asthmaemer18ad211)

uhf42asthmaemer18ad301<-ggplot(filter(asthmaemer18ad, GeoID == 301), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad301
#ggsave("uhf42asthmaemer18ad301.png", uhf42asthmaemer18ad301)

uhf42asthmaemer18ad309<-ggplot(filter(asthmaemer18ad, GeoID == 309), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad309
#ggsave("uhf42asthmaemer18ad309.png", uhf42asthmaemer18ad309)

uhf42asthmaemer18ad406<-ggplot(filter(asthmaemer18ad, GeoID == 406), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad406
#ggsave("uhf42asthmaemer18ad406.png", uhf42asthmaemer18ad406)

uhf42asthmaemer18ad501<-ggplot(filter(asthmaemer18ad, GeoID == 501), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad501
#ggsave("uhf42asthmaemer18ad501.png", uhf42asthmaemer18ad501)

uhf42asthmaemer18ad503<-ggplot(filter(asthmaemer18ad, GeoID == 503), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma ER 18+", x = "", y = "Value")
uhf42asthmaemer18ad503
#ggsave("uhf42asthmaemer18ad503.png", uhf42asthmaemer18ad503)

#Asthma hospitalizations (4 and under)
#setwd("")

asthmahosp4undOG<- read_csv("NYC EH Data Portal - Asthma hospitalizations (age 4 and under) (filtered).csv")
asthmahosp4und <- asthmahosp4undOG

asthmahosp4und <- asthmahosp4und %>% filter(GeoTypeDesc == "UHF 42")

asthmahosp4und <- asthmahosp4und %>% filter(GeoID %in% Good42) %>% arrange(desc(TimePeriod))

#just looking at 1 neighborhood
asthmahosp4und <- asthmahosp4und |> 
  pivot_longer(
    cols = c("Estimated annual rate per 10,000", "Number"), 
    names_to = "Estimate", 
    values_to = "values")

asthmahosp4und$values <-as.numeric(gsub('[*]', '',asthmahosp4und$values))

uhf42asthmahosp4und103<-ggplot(filter(asthmahosp4und, GeoID == 103), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und103
#ggsave("uhf42asthmahosp4und103.png", uhf42asthmahosp4und103)

uhf42asthmahosp4und107<-ggplot(filter(asthmahosp4und, GeoID == 107), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und107
#ggsave("uhf42asthmahosp4und107.png", uhf42asthmahosp4und107)

uhf42asthmahosp4und201<-ggplot(filter(asthmahosp4und, GeoID == 201), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und201
#ggsave("uhf42asthmahosp4und201.png", uhf42asthmahosp4und201)

uhf42asthmahosp4und205<-ggplot(filter(asthmahosp4und, GeoID == 205), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und205
#ggsave("uhf42asthmahosp4und205.png", uhf42asthmahosp4und205)

uhf42asthmahosp4und211<-ggplot(filter(asthmahosp4und, GeoID == 211), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und211
#ggsave("uhf42asthmahosp4und211.png", uhf42asthmahosp4und211)

uhf42asthmahosp4und301<-ggplot(filter(asthmahosp4und, GeoID == 301), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und301
#ggsave("uhf42asthmahosp4und301.png", uhf42asthmahosp4und301)

uhf42asthmahosp4und309<-ggplot(filter(asthmahosp4und, GeoID == 309), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und309
#ggsave("uhf42asthmahosp4und309.png", uhf42asthmahosp4und309)

uhf42asthmahosp4und406<-ggplot(filter(asthmahosp4und, GeoID == 406), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und406
#ggsave("uhf42asthmahosp4und406.png", uhf42asthmahosp4und406)

uhf42asthmahosp4und501<-ggplot(filter(asthmahosp4und, GeoID == 501), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und501
#ggsave("uhf42asthmahosp4und501.png", uhf42asthmahosp4und501)

uhf42asthmahosp4und503<-ggplot(filter(asthmahosp4und, GeoID == 503), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma Hosptlz. >=4", x = "", y = "Value")
uhf42asthmahosp4und503
#ggsave("uhf42asthmahosp4und503.png", uhf42asthmahosp4und503)


#Asthma hospitalizations (5 and 17)
#setwd("")

asthmahosp5_17OG<- read_csv("NYC EH Data Portal - Asthma hospitalizations (age 5 to 17) (filtered).csv")
asthmahosp5_17 <- asthmahosp5_17OG

asthmahosp5_17 <- asthmahosp5_17 %>% filter(GeoTypeDesc == "UHF 42")
#deaths <- deaths %>% filter((TimePeriod != "2005-2007") & (TimePeriod != "2009-2011"))

asthmahosp5_17 <- asthmahosp5_17 %>% filter(GeoID %in% Good42) %>% arrange(TimePeriod)

#just looking at 1 neighborhood
asthmahosp5_17 <- asthmahosp5_17 |> 
  pivot_longer(
    cols = c("Estimated annual rate per 10,000", "Number"), 
    names_to = "Estimate", 
    values_to = "values")

asthmahosp5_17$values <-as.numeric(gsub('[*,]', '',asthmahosp5_17$values))

uhf42asthmahosp5_17103<-ggplot(filter(asthmahosp5_17, GeoID == 103), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17103
#ggsave("uhf42asthmahosp5_17103.png", uhf42asthmahosp5_17103)

uhf42asthmahosp5_17107<-ggplot(filter(asthmahosp5_17, GeoID == 107), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17107
#ggsave("uhf42asthmahosp5_17107.png", uhf42asthmahosp5_17107)

uhf42asthmahosp5_17201<-ggplot(filter(asthmahosp5_17, GeoID == 201), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17201
#ggsave("uhf42asthmahosp5_17201.png", uhf42asthmahosp5_17201)

uhf42asthmahosp5_17205<-ggplot(filter(asthmahosp5_17, GeoID == 205), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17205
#ggsave("uhf42asthmahosp5_17205.png", uhf42asthmahosp5_17205)

uhf42asthmahosp5_17211<-ggplot(filter(asthmahosp5_17, GeoID == 211), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17211
#ggsave("uhf42asthmahosp5_17211.png", uhf42asthmahosp5_17211)

uhf42asthmahosp5_17301<-ggplot(filter(asthmahosp5_17, GeoID == 301), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17301 
#ggsave("uhf42asthmahosp5_17301.png", uhf42asthmahosp5_17301)

uhf42asthmahosp5_17309<-ggplot(filter(asthmahosp5_17, GeoID == 309), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17309
#ggsave("uhf42asthmahosp5_17309.png", uhf42asthmahosp5_17309)

uhf42asthmahosp5_17406<-ggplot(filter(asthmahosp5_17, GeoID == 406), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17406
#ggsave("uhf42asthmahosp5_17406.png", uhf42asthmahosp5_17406)

uhf42asthmahosp5_17501<-ggplot(filter(asthmahosp5_17, GeoID == 501), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17501
#ggsave("uhf42asthmahosp5_17501.png", uhf42asthmahosp5_17501)

uhf42asthmahosp5_17503<-ggplot(filter(asthmahosp5_17, GeoID == 503), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma Hosptlz. 5-17", x = "", y = "Value")
uhf42asthmahosp5_17503
#ggsave("uhf42asthmahosp5_17503.png", uhf42asthmahosp5_17503)


#Asthma hospitalizations (18+)
#setwd("")

asthmahosp18adOG<- read_csv("NYC EH Data Portal - Asthma hospitalizations (adults) (filtered).csv")
asthmahosp18ad <- asthmahosp18adOG

asthmahosp18ad <- asthmahosp18ad %>% filter(GeoTypeDesc == "UHF 42")

asthmahosp18ad <- asthmahosp18ad %>% filter(GeoID %in% Good42) %>% arrange(desc(TimePeriod))

#just looking at 1 neighborhood
asthmahosp18ad <- asthmahosp18ad |> 
  pivot_longer(
    cols = c("Estimated annual rate per 10,000", "Number"), 
    names_to = "Estimate", 
    values_to = "values")

asthmahosp18ad$values <-as.numeric(gsub('[,]', '',asthmahosp18ad$values))

uhf42asthmahosp18ad103<-ggplot(filter(asthmahosp18ad, GeoID == 103), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 103: Fordham - Bronx Park, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad103
#ggsave("uhf42asthmahosp18ad103.png", uhf42asthmahosp18ad103)

uhf42asthmahosp18ad107<-ggplot(filter(asthmahosp18ad, GeoID == 107), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 107: Hunts Point - Mott Haven, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad107
#ggsave("uhf42asthmahosp18ad107.png", uhf42asthmahosp18ad107)

uhf42asthmahosp18ad201<-ggplot(filter(asthmahosp18ad, GeoID == 201), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 201: Greenpoint, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad201
#ggsave("uhf42asthmahosp18ad201.png", uhf42asthmahosp18ad201)

uhf42asthmahosp18ad205<-ggplot(filter(asthmahosp18ad, GeoID == 205), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 205: Sunset Park, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad205
#ggsave("uhf42asthmahosp18ad205.png", uhf42asthmahosp18ad205)

uhf42asthmahosp18ad211<-ggplot(filter(asthmahosp18ad, GeoID == 211), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) + 
  labs(title = "UHF42 211: Williamsburg - Bushwick, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad211
#ggsave("uhf42asthmahosp18ad211.png", uhf42asthmahosp18ad211)

uhf42asthmahosp18ad301<-ggplot(filter(asthmahosp18ad, GeoID == 301), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 301: Washington Heights - Inwood, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad301
#ggsave("uhf42asthmahosp18ad301.png", uhf42asthmahosp18ad301)

uhf42asthmahosp18ad309<-ggplot(filter(asthmahosp18ad, GeoID == 309), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 309: Union Square - Lower East Side, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad309
#ggsave("uhf42asthmahosp18ad309.png", uhf42asthmahosp18ad309)

uhf42asthmahosp18ad406<-ggplot(filter(asthmahosp18ad, GeoID == 406), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 406: Fresh Meadows, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad406
#ggsave("uhf42asthmahosp18ad406.png", uhf42asthmahosp18ad406)

uhf42asthmahosp18ad501<-ggplot(filter(asthmahosp18ad, GeoID == 501), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 501: Port Richmond, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad501
#ggsave("uhf42asthmahosp18ad501.png", uhf42asthmahosp18ad501)

uhf42asthmahosp18ad503<-ggplot(filter(asthmahosp18ad, GeoID == 503), mapping = aes(x = Estimate, y = values, fill = as.factor(TimePeriod))) +
  geom_col(position = "dodge")+
  scale_x_discrete(labels = label_wrap(10)) +
  labs(title = "UHF42 503: Willowbrook, Asthma Hosptlz. 18+", x = "", y = "Value")
uhf42asthmahosp18ad503
#ggsave("uhf42asthmahosp18ad503.png", uhf42asthmahosp18ad503)


# PM2.5analysis

#reading in data
#setwd("")
pm25_2013 <- read_csv("daily_88101_2013.csv")
pm25_2014 <- read_csv("daily_88101_2014.csv")
pm25_2015 <- read_csv("daily_88101_2015.csv")
pm25_2016 <- read_csv("daily_88101_2016.csv")
pm25_2017 <- read_csv("daily_88101_2017.csv")
pm25_2018 <- read_csv("daily_88101_2018.csv")
pm25_2019 <- read_csv("daily_88101_2019.csv")

pm25_2013 <- pm25_2013 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_2014 <- pm25_2014 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_2015 <- pm25_2015 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_2016 <- pm25_2016 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_2017 <- pm25_2017 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_2018 <- pm25_2018 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_2019 <- pm25_2019 %>% filter(`State Name` == "New York" &
                                    `County Name` %in% five_boroughs) 
pm25_data <- rbind(pm25_2013, pm25_2014, pm25_2015,
                   pm25_2016, pm25_2017, pm25_2018, pm25_2019)


#joining site info datasets
pm25_data$`Defining Site` <- paste(pm25_data$`State Code`,pm25_data$`County Code`,pm25_data$`Site Num`, sep="-")
colnames(pm25_data)[colnames(pm25_data) == 'Site Num'] <- 'Site Number'

pm25_daily_joined <- left_join(pm25_data, aqs_sites, by = "Defining Site")
pm25_daily_joined <- pm25_daily_joined %>% rename('State Name' = 'State Name.x',
                                                  'Site Number' = 'Site Number.x',
                                                  'State Code' = 'State Code.x',
                                                  'County Code'='County Code.x',
                                                  'Address' = 'Address.x',
                                                  'Local Site Name' = 'Local Site Name.x',
                                                  'Latitude' = 'Latitude.x',
                                                  'Longitude' = 'Longitude.x',
                                                  'Datum'='Datum.x',
                                                  'Address' = 'Address.x',
                                                  'County Name' ='County Name.x',
                                                  'CBSA Name' = 'CBSA Name.x')

pm25_daily_joined <- left_join(pm25_daily_joined, UniqueSites_Info_AQI_daily, by = "Defining Site")

pm25_daily_joined <- pm25_daily_joined %>% mutate(uhf34code = as.factor(uhf34code),
                                                  uhf34nhood = as.factor(uhf34nhood),
                                                  uhf42code = as.factor(uhf42code),
                                                  uhf42nhood = as.factor(uhf42nhood),
                                                  locatdesc = as.factor(locatdesc))

pm25_daily_joined <- pm25_daily_joined %>% mutate(year = year(`Date Local`),
                                                  month = month(`Date Local`),
                                                  mdate = mday(`Date Local`))

pm25_daily_joined <- pm25_daily_joined %>% relocate(c(year, month, mdate), .before = `Units of Measure`)

table(pm25_daily_joined$`Sample Duration`)


pm25_daily_joined24h <- filter(pm25_daily_joined, `Sample Duration` == "24 HOUR")

pm25_daily_joined24h <- pm25_daily_joined24h %>% group_by(year, month, mdate, `Defining Site`) %>% mutate(meanPOC = mean(`Arithmetic Mean`)) %>% ungroup %>% relocate(meanPOC, .after = `Arithmetic Mean`)

pm25_daily_joined24h <- arrange(pm25_daily_joined24h, `Date Local`) 

pm25_daily_joined24h <- pm25_daily_joined24h %>% 
  distinct(year, month, mdate, `Defining Site` ,.keep_all = T)



#get monthly averages per neighborhood by year, plot them
uh34pldatapm <- pm25_daily_joined24h %>% group_by(year, month, uhf34code) %>% dplyr::summarize(avgMthPM2.5 = mean(`meanPOC`))
uh34pldatapm <- uh34pldatapm %>% filter(`uhf34code` %in% Good34)
uh34mthpmall <- ggplot(data = uh34pldatapm, mapping = aes(x = month, y = avgMthPM2.5, col = as.factor(uhf34code))) +
  geom_point()+ geom_line() +
  facet_wrap(~year)

uh34mthpmall
#ggsave("mnthpmuh34.png", uh34mthpmall) 

uh42pldatapm <- pm25_daily_joined24h %>% group_by(year, month, uhf42code) %>% dplyr::summarize(avgMthPM2.5 = mean(`meanPOC`))
uh42pldatapm <- uh42pldatapm %>% filter(`uhf42code` %in% Good42)
uh42mthpmall <- ggplot(data = uh42pldatapm, mapping = aes(x = month, y = avgMthPM2.5, col = as.factor(uhf42code))) +
  geom_point()+ geom_line() +
  facet_wrap(~year)

uh42mthpmall
#ggsave("mnthpmuh42.png", uh42mthpmall) 



#next, see if any of the other parameters are for sites not accounted for

pm25_daily_joined24hBLK <- filter(pm25_daily_joined, `Sample Duration` == "24-HR BLK AVG")


pm25_daily_joined24hBLK <- pm25_daily_joined24hBLK %>% group_by(year, month, mdate, `Defining Site`) %>% mutate(meanPOC = mean(`Arithmetic Mean`)) %>% ungroup

pm25_daily_joined24hBLK <- arrange(pm25_daily_joined24hBLK, `Date Local`) #only has 4S. Interesting. Maybe they just got this data point to compare to the 24HR/ FRM data

pm25_daily_joined24hBLK <- pm25_daily_joined24hBLK %>% 
  distinct(year, month, mdate, `Defining Site` ,.keep_all = T)



#next, see if any of the other parameters are for sites not accounted for

pm25_daily_joined1hr <- filter(pm25_daily_joined, `Sample Duration` == "1 HOUR")

pm25_daily_joined1hr <- pm25_daily_joined1hr %>% group_by(year, month, mdate, `Defining Site`) %>% mutate(meanPOC = mean(`Arithmetic Mean`)) %>% ungroup

pm25_daily_joined1hr <- arrange(pm25_daily_joined1hr, `Date Local`) #only has 4S. Interesting. Maybe they just got this data point to compare to the 24HR/ FRM data

pm25_daily_joined1hr <- pm25_daily_joined1hr %>% 
  distinct(year, month, mdate, `Defining Site` ,.keep_all = T)



unique(pm25_daily_joined24hBLK$`Defining Site`)
unique(pm25_daily_joined1hr$`Defining Site`)


#Getting yearly averages
uh42pm25_data2yearavg <- pm25_daily_joined24h %>% group_by(year,uhf42code) %>% dplyr::summarize(avgMthAQI = mean(AQI)) %>% filter(`uhf42code` %in% Good42)

uh42pm25_data2yearavg_meantab <- uh42pm25_data2yearavg %>% 
  pivot_wider(names_from = uhf42code, names_prefix="UHF42_", values_from = avgMthAQI)
uh42pm25_data2yearavg_meantab <- kable(uh42pm25_data2yearavg_meantab)

uh42pm25_data2yearavgplt <- ggplot(data = uh42pm25_data2yearavg, mapping = aes(x = as.factor(year), y = avgMthAQI, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Yearly PM2.5 Avg", x = "Year", y = "Avg Yearly PM2.5")+
  facet_wrap(~uhf42code)
uh42pm25_data2yearavgplt
#ggsave("uh42pm25_data2yearavgplt.png", uh42pm25_data2yearavgplt)


#UHF42
#get monthly averages per neighborhood by year, plot them
uh42pmdata <- pm25_daily_joined24h %>% group_by(year, month, uhf42code) %>% dplyr::summarize(avgMthPM.5 = mean(meanPOC)) #figure out how to keep other columns later
uh42pmdata2_103 <- uh42pmdata %>% filter(uhf42code==103)
uhf42mthpm2103 <- ggplot(data = uh42pmdata2_103, mapping = aes(x = as.factor(month), y = avgMthPM.5, group = 1)) +
  geom_point(col = 'red')+ geom_line(col = 'red') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 103: Fordham - Bronx Park, PM2.5", x = "Month", y = "Avg Mnthly PM2.5")+
  facet_wrap(~year)

uhf42mthpm2103
#ggsave("uhf42mthpm2103.png", uhf42mthpm2103) 

uh42pmdata2_107 <- uh42pmdata %>% filter(uhf42code==107)
uhf42mthpm2107 <- ggplot(data = uh42pmdata2_107, mapping = aes(x = as.factor(month), y = avgMthPM.5, group = 1)) +
  geom_point(col = 'orange')+ geom_line(col = 'orange') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 107: Hunts Point - Mott Haven, PM2.5", x = "Month", y = "Avg Mnthly PM2.5")+
  facet_wrap(~year)
uhf42mthpm2107
#ggsave("uhf42mthpm2107.png", uhf42mthpm2107) 

uh42pmdata2_201 <- uh42pmdata %>% filter(uhf42code==201)
uhf42mthpm2201 <- ggplot(data = uh42pmdata2_201, mapping = aes(x = as.factor(month), y = avgMthPM.5, group = 1)) +
  geom_point(col = 'gold')+ geom_line(col = 'gold') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 201: Greenpoint, PM2.5", x = "Month", y = "Avg Mnthly PM2.5")+
  facet_wrap(~year)
uhf42mthpm2201
#ggsave("uhf42mthpm2201.png", uhf42mthpm2201)

uh42pmdata2_309 <- uh42pmdata %>% filter(uhf42code==309)
uhf42mthpm2309 <- ggplot(data = uh42pmdata2_309, mapping = aes(x = as.factor(month), y = avgMthPM.5, group = 1)) +
  geom_point(col = 'hotpink')+ geom_line(col = 'hotpink') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 309: Union Square - Lower East Side, PM2.5", x = "Month", y = "Avg Mnthly PM2.5")+
  facet_wrap(~year)
uhf42mthpm2309
#ggsave("uhf42mthpm2309.png", uhf42mthpm2309)

uh42pmdata2_406 <- uh42pmdata %>% filter(uhf42code==406)
uhf42mthpm2406 <- ggplot(data = uh42pmdata2_406, mapping = aes(x = as.factor(month), y = avgMthPM.5, group = 1, col = 'steelblue')) +
  geom_point(col = 'steelblue')+ geom_line(col = 'steelblue') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 406: Fresh Meadows, PM2.5", x = "Month", y = "Avg Mnthly PM2.5")+
  facet_wrap(~year)
uhf42mthpm2406
#ggsave("uhf42mthpm2406.png", uhf42mthpm2406)

uh42pmdata2_501 <- uh42pmdata %>% filter(uhf42code==501)
uhf42mthpm2501 <- ggplot(data = uh42pmdata2_501, mapping = aes(x = as.factor(month), y = avgMthPM.5, group = 1)) +
  geom_point(col = 'brown')+ geom_line(col = 'brown') +
  scale_x_discrete(labels = c(
    "JAN", "","MAR","","MAY", "","JUL","","SEPT","","NOV",""))+
  labs(title = "UHF42 501: Port Richmond, PM2.5", x = "Month", y = "Avg Mnthly PM2.5")+
  facet_wrap(~year)
uhf42mthpm2501


# Birth Outcomes

#setwd(")
LivebirthOG<- read.csv("Live Births.csv", header = F)
LivebirthMomsOG<- read.csv("Live Births to Mothers on Medicaid.csv", header = F)
lowbwOG <- read.csv("Low Birthweight Babies.csv", header = F)
PretermbirhOG <- read.csv("Preterm Births.csv", header = F)
infmortOG <- read.csv("Infant Mortality.csv", header = F)

infmort <- infmortOG %>% filter(V2 %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019))
infmort <- infmort %>% mutate(Geodsc = case_when(V1 == "Manhattan"| V1 == "Queens" | V1 == "Bronx" | V1 == "Brooklyn"| V1 == "Staten Island" ~ "borough",
                                                 TRUE ~ "community"))

infmort <- infmort %>% filter(Geodsc == "community")

infmort <- infmort %>% mutate(uhf42code = case_when(V1 == "Hunts Point" | V1 == "Mott Haven" ~ "107",
                                                    V1 == "Morrisania" ~ "106",
                                                    V1 == "Sunset Park" ~ "205",
                                                    V1 == "Lower East Side" ~ "309",
                                                    V1 == "Washington Heights" ~ "301",
                                                    V1 == "East Harlem" ~ "303",
                                                    V1 == "Fresh Meadows/Briarwood" ~ "406",
                                                    V1 == "Williamsburg/Greenpoint" ~ "201",
                                                    V1 == "Bushwick" ~ "211",
                                                    TRUE ~ "No UHF"))


infmort <- infmort %>% filter(uhf42code != "No UHF")
infmort <-infmort %>%  dplyr::select(V1, V2,V4, Geodsc,uhf42code )
infmort <- as_tibble(infmort)
infmort <- infmort %>% mutate(V2 = as.numeric(V2),
                              V4 = as.numeric(V4))
infmort <- infmort %>% rename(year = V2,
                              rate = V4)

infmort <- infmort %>% group_by(year,uhf42code) %>% dplyr::summarize(rate = mean(rate))

infmortplt <- ggplot(data = infmort, mapping = aes(x = as.factor(year), y = rate, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Infant Mortality", x = "Year", y = "Infant Mortality Rate")+
  facet_wrap(~uhf42code)
infmortplt



preterm <- PretermbirhOG %>% filter(V2 %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019))
preterm <- preterm %>% mutate(Geodsc = case_when(V1 == "Manhattan"| V1 == "Queens" | V1 == "Bronx" | V1 == "Brooklyn"| V1 == "Staten Island" ~ "borough",
                                                 TRUE ~ "community"))

preterm <- preterm %>% filter(Geodsc == "community")

preterm <- preterm %>% mutate(uhf42code = case_when(V1 == "Hunts Point" | V1 == "Mott Haven" ~ "107",
                                                    V1 == "Morrisania" ~ "106",
                                                    V1 == "Sunset Park" ~ "205",
                                                    V1 == "Lower East Side" ~ "309",
                                                    V1 == "Washington Heights" ~ "301",
                                                    V1 == "East Harlem" ~ "303",
                                                    V1 == "Fresh Meadows/Briarwood" ~ "406",
                                                    V1 == "Williamsburg/Greenpoint" ~ "201",
                                                    V1 == "Bushwick" ~ "211",
                                                    TRUE ~ "No UHF"))


preterm <- preterm %>% filter(uhf42code != "No UHF")
preterm <-preterm %>%  dplyr::select(V1, V2,V4, Geodsc,uhf42code )
preterm <- as_tibble(preterm)
preterm <- preterm %>% mutate(V2 = as.numeric(V2),
                              V4 = as.numeric(V4))
preterm <- preterm %>% rename(year = V2,
                              rate = V4)

preterm <- preterm %>% group_by(year,uhf42code) %>% dplyr::summarize(rate = mean(rate))

pretermplt <- ggplot(data = preterm, mapping = aes(x = as.factor(year), y = rate, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Preterm Births", x = "Year", y = "% of Births Preterm")+
  facet_wrap(~uhf42code)
pretermplt



lowbw <- lowbwOG %>% filter(V2 %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019))
lowbw <- lowbw %>% mutate(Geodsc = case_when(V1 == "Manhattan"| V1 == "Queens" | V1 == "Bronx" | V1 == "Brooklyn"| V1 == "Staten Island" ~ "borough",
                                             TRUE ~ "community"))

lowbw <- lowbw %>% filter(Geodsc == "community")

lowbw <- lowbw %>% mutate(uhf42code = case_when(V1 == "Hunts Point" | V1 == "Mott Haven" ~ "107",
                                                V1 == "Morrisania" ~ "106",
                                                V1 == "Sunset Park" ~ "205",
                                                V1 == "Lower East Side" ~ "309",
                                                V1 == "Washington Heights" ~ "301",
                                                V1 == "East Harlem" ~ "303",
                                                V1 == "Fresh Meadows/Briarwood" ~ "406",
                                                V1 == "Williamsburg/Greenpoint" ~ "201",
                                                V1 == "Bushwick" ~ "211",
                                                TRUE ~ "No UHF"))


lowbw <- lowbw %>% filter(uhf42code != "No UHF")
lowbw <-lowbw %>%  dplyr::select(V1, V2,V4, Geodsc,uhf42code )
lowbw <- as_tibble(lowbw)
lowbw <- lowbw %>% mutate(V2 = as.numeric(V2),
                          V4 = as.numeric(V4))
lowbw <- lowbw %>% rename(year = V2,
                          rate = V4)

lowbw <- lowbw %>% group_by(year,uhf42code) %>% dplyr::summarize(rate = mean(rate))

lowbwplt <- ggplot(data = lowbw, mapping = aes(x = as.factor(year), y = rate, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Low Weight Births", x = "Year", y = "% of Low Weight Births")+
  facet_wrap(~uhf42code)
lowbwplt



livebirth <- LivebirthOG %>% filter(V2 %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019))
livebirth <- livebirth %>% mutate(Geodsc = case_when(V1 == "Manhattan"| V1 == "Queens" | V1 == "Bronx" | V1 == "Brooklyn"| V1 == "Staten Island" ~ "borough",
                                                     TRUE ~ "community"))

livebirth <- livebirth %>% filter(Geodsc == "community")

livebirth <- livebirth %>% mutate(uhf42code = case_when(V1 == "Hunts Point" | V1 == "Mott Haven" ~ "107",
                                                        V1 == "Morrisania" ~ "106",
                                                        V1 == "Sunset Park" ~ "205",
                                                        V1 == "Lower East Side" ~ "309",
                                                        V1 == "Washington Heights" ~ "301",
                                                        V1 == "East Harlem" ~ "303",
                                                        V1 == "Fresh Meadows/Briarwood" ~ "406",
                                                        V1 == "Williamsburg/Greenpoint" ~ "201",
                                                        V1 == "Bushwick" ~ "211",
                                                        TRUE ~ "No UHF"))

livebirth <- livebirth %>% filter(uhf42code != "No UHF")
livebirth <-livebirth %>%  dplyr::select(V1, V2, V3, V4, Geodsc,uhf42code )
livebirth <- as_tibble(livebirth)
livebirth <- livebirth %>% mutate(V2 = as.numeric(V2),
                                  V4 = as.numeric(V4))
livebirth <- livebirth %>% rename(year = V2,
                                  value = V4)

livebirthnum <- livebirth %>% filter(V3 == "Number") %>%  group_by(year,uhf42code) %>% dplyr::summarize(Number = mean(value))

livebirthrate <- livebirth %>% filter(V3 == "Rate") %>%  group_by(year,uhf42code) %>% dplyr::summarize(Rate = mean(value))


livebirtratehplt <- ggplot(data = livebirthrate, mapping = aes(x = as.factor(year), y = Rate, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Live Births Rates", x = "Year", y = "% of Live Births")+
  facet_wrap(~uhf42code)
livebirtratehplt

livebirtrnumhplt <- ggplot(data = livebirthnum, mapping = aes(x = as.factor(year), y = Number, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Live Births", x = "Year", y = "Number of Live Births")+
  facet_wrap(~uhf42code)
livebirtrnumhplt



LivebirthMom <- LivebirthMomsOG %>% filter(V2 %in% c(2013, 2014, 2015, 2016, 2017, 2018, 2019))
LivebirthMom <- LivebirthMom %>% mutate(Geodsc = case_when(V1 == "Manhattan"| V1 == "Queens" | V1 == "Bronx" | V1 == "Brooklyn"| V1 == "Staten Island" ~ "borough",
                                                           TRUE ~ "community"))

LivebirthMom <- LivebirthMom %>% filter(Geodsc == "community")

LivebirthMom <- LivebirthMom %>% mutate(uhf42code = case_when(V1 == "Hunts Point" | V1 == "Mott Haven" ~ "107",
                                                              V1 == "Morrisania" ~ "106",
                                                              V1 == "Sunset Park" ~ "205",
                                                              V1 == "Lower East Side" ~ "309",
                                                              V1 == "Washington Heights" ~ "301",
                                                              V1 == "East Harlem" ~ "303",
                                                              V1 == "Fresh Meadows/Briarwood" ~ "406",
                                                              V1 == "Williamsburg/Greenpoint" ~ "201",
                                                              V1 == "Bushwick" ~ "211",
                                                              TRUE ~ "No UHF"))

LivebirthMom <- LivebirthMom %>% filter(uhf42code != "No UHF")
LivebirthMom <-LivebirthMom %>%  dplyr::select(V1, V2, V3, V4, Geodsc,uhf42code )
LivebirthMom <- as_tibble(LivebirthMom)
LivebirthMom <- LivebirthMom %>% mutate(V2 = as.numeric(V2),
                                        V4 = as.numeric(V4))
LivebirthMom <- LivebirthMom %>% rename(year = V2,
                                        rate = V4)

LivebirthMom <- LivebirthMom %>%  group_by(year,uhf42code) %>% dplyr::summarize(rate = mean(rate))

LivebirthMomplt <- ggplot(data = LivebirthMom, mapping = aes(x = as.factor(year), y = rate, group = 1, color = uhf42code)) +
  geom_point()+ geom_line() +
  scale_x_discrete(labels = c(
    "2013", "","2015","","2017", "","2019"))+
  labs(title = "UHF42 Sites - Live Births for Mom's on Medicaid", x = "Year", y = "% of Births - Live")+
  facet_wrap(~uhf42code)
LivebirthMomplt


