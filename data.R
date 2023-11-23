# https://www.r-bloggers.com/2015/01/using-rvest-to-scrape-an-html-table/

#install.packages('rvest')
#install.packages('Hmisc')
#install.packages('tidyverse')
#install.packages('devtools')
#devtools::install_github("twitter/AnomalyDetection")
library(Hmisc)
library(rvest)
library(tidyverse)
library(AnomalyDetection)

`%notin` <- Negate(`%in%`)

##### get the GVA data
years <- as.character(2014:2023)
pages <- 0:32 # works if there's less than 775 shootings in a year (31 pages*25 events)
url <- 'https://www.gunviolencearchive.org/reports/mass-shooting'
urls <- paste0(url, "?page=", pages)

urls.all <- NA
for (file in urls) {
  tmp.url <- paste0(file, "&year=", years)
  urls.all <- c(tmp.url, urls.all)
}

urls.all <- urls.all[!is.na(urls.all)]

get.gva <- function(site) {
  site %>%
    read_html()%>%
    html_nodes(xpath = '//*[@id="content"]/div/div/div') %>%
    html_table()
}

results <- sapply(urls.all, get.gva)

gva <- data.frame()
for (urls in results) {
  tmp.gva <- data.frame(urls)
  gva <- rbind(tmp.gva, gva)
}

gva$date <- as.Date(gva$Incident.Date, format = "%B %d, %Y")
gva$total.victims <- gva$Victims.Killed + gva$Victims.Injured
gva$day <- weekdays(gva$date)
gva$year <- substr(gva$date, 0, 4)
gva <- subset(gva, select = c("Incident.ID","date","day","year","City.Or.County","State","Victims.Killed",
                              "Victims.Injured","total.victims"))
names(gva) <- c("event", "date", "dow", "year", "city", "state", "killed", "injured", "total")
gva <- gva[!duplicated(gva), ]

gva.sub <- subset(gva, gva$date >= '2019-01-01')

sum.year <- gva %>%
  group_by(year) %>%
  summarise(events = n())

# anomaly detection
## max_anoms = Max anomalies that S-H-ESD will detect as a percentage of the data
## alpha = level of statistical significance with which to accept or reject anomalies
# original study breakpoints: 8 killed, 13 injured, or 15 total
# total
ad.total <- AnomalyDetectionVec(gva$total, max_anoms = 0.01, 
                                        direction = "pos", alpha = 0.01, period = 2, 
                                        only_last = F, threshold = 'None', e_value = F, 
                                        longterm_period = NULL, plot = T, y_log = F, 
                                        xlabel = "Mass Shootings", ylabel = "Victim Count", 
                                        title = "Anomalies: Total Casualty Counts")
counts.ad.total <- ad.total$anoms

# killed
ad.killed <- AnomalyDetectionVec(gva$killed, max_anoms = 0.01, 
                                         direction = "pos", alpha = 0.01, period = 2, 
                                         only_last = F, threshold = 'None', e_value = F, 
                                         longterm_period = NULL, plot = T, y_log = F, 
                                         xlabel = "Mass Shootings", ylabel = "Victim Count", 
                                         title = "Anomalies: Killed Counts")
counts.ad.killed <- ad.killed$anoms

# wounded
ad.wounded <- AnomalyDetectionVec(gva$injured, max_anoms = 0.01, 
                                          direction = "pos", alpha = 0.01, period = 2, 
                                          only_last = F, threshold = 'None', e_value = F, 
                                          longterm_period = NULL, plot = T, y_log = F, 
                                          xlabel = "Mass Shootings", ylabel = "Victim Count", 
                                          title = "Anomalies: Injured Counts")
counts.ad.injured <- ad.wounded$anoms

ad <- data.frame(Anomaly = c("Total", "Killed", "Wounded"),
                 Value = c(min(counts.ad.total$anoms), 
                           min(counts.ad.killed$anoms), 
                               min(counts.ad.injured$anoms)))

##### get the MJ data
mj <- read.csv("https://docs.google.com/spreadsheets/d/e/2PACX-1vQBEbQoWMn_P81DuwmlQC0_jr2sJDzkkC0mvF6WLcM53ZYXi8RMfUlunvP1B5W0jRrJvH-wc-WGjDB1/pub?gid=0&single=true&output=csv", stringsAsFactors = FALSE)

mj$temp.month <- substr(mj$date, 0, 2)
mj$temp.month <- gsub("/", "", mj$temp.month)
mj$temp.day <- substr(mj$date, (nchar(mj$temp.month)+2), (nchar(mj$temp.month)+3))
mj$temp.day <- gsub("/", "", mj$temp.day)
mj$temp.year <- substr(mj$date, nchar(mj$date)-1, nchar(mj$date))
mj$temp.date <- paste(mj$temp.month, mj$temp.day, mj$temp.year, sep = "/")
mj$date <- as.Date(mj$temp.date, format = "%m/%d/%y")
mj <- separate(data = mj, col = location, into = c("city", "state"), sep = ", ")
mj$day <- weekdays(mj$date)
mj <- subset(mj, select = c("case","date","day","year","city","state","fatalities","injured","total_victims"))
names(mj) <- c("event", "date", "dow", "year", "city", "state", "killed", "injured", "total")

mj.sub <- subset(mj, mj$date < '1998-03-24')

##### get the dissertation data
dissertation <- read.csv("https://raw.githubusercontent.com/matthew-danna/black-swan-mass-shootings/main/blackswans%20events%201998%202018.csv",
                         stringsAsFactors = FALSE)

dissertation$temp.month <- substr(dissertation$date, 0, 2)
dissertation$temp.month <- gsub("/", "", dissertation$temp.month)
dissertation$temp.day <- substr(dissertation$date, (nchar(dissertation$temp.month)+2), (nchar(dissertation$temp.month)+3))
dissertation$temp.day <- gsub("/", "", dissertation$temp.day)
dissertation$temp.year <- substr(dissertation$date, nchar(dissertation$date)-1, nchar(dissertation$date))
dissertation$temp.date <- paste(dissertation$temp.month, dissertation$temp.day, dissertation$temp.year, sep = "/")
dissertation$date <- as.Date(dissertation$temp.date, format = "%m/%d/%y")
dissertation$dow <- weekdays(dissertation$date)
dissertation <- dissertation[c(1,9,40,6,18,20,10:12)]
names(dissertation) <- c("event", "date", "dow", "year", "city", "state", "killed", "injured", "total")

# merge GVA, MJ, and dissertation data
events <- rbind(gva.sub, mj.sub, dissertation)
events$total <- as.numeric(events$total)
events$killed <- as.numeric(events$killed)
events$injured <- as.numeric(events$injured)

events.ad.tot <- subset(events, events$total >= min(counts.ad.total$anoms))
events.ad.killed <- subset(events, events$killed >= min(counts.ad.killed$anoms))
events.ad.injured <- subset(events, events$injured >= min(counts.ad.injured$anoms))

events.ad <- rbind(events.ad.tot, events.ad.killed, events.ad.injured)
events.ad <- events.ad[!duplicated(events.ad), ]

new.events <- subset(events.ad$event, events.ad$date > '2018-11-07')
incident <- 'https://www.gunviolencearchive.org/incident/'
incidents <- paste0(incident, new.events)

tmp <- 'https://www.gunviolencearchive.org/incident/2242153'

tmp <- read_html('https://www.gunviolencearchive.org/incident/2242153') %>%
  html_nodes(xpath = '/html/body/section/div/div/div/div[2]/div/div[3]')

get.incidents <- function(site) {
  site %>%
    read_html()%>%
    html_nodes(xpath = '//*[@id="content"]/div/div/div') %>%
    html_table()
}

##### qualitative review for excluding mutual perpetrator events
events.valid <- events.ad %>%
  filter(event != '879953', event != '1142789', event != '803054', event != '611479', event != '604233', 
         event != '545525',
         event != '390526', event != '192851' , event != '1600787', event != '1635750', event != '1741838', event != '1758966',
         event != '1799307', event != '2012765', event != '2019622', event != '2032220', event != '2137198', event != '2237593',
         event != '2242153', event != '2257739', event != '2269082', event != '2269100', event != '2301494', event != '2449653')

# original study start: 3/24/1998


'/html/body/section/div/div/div/div[2]/div/div[3]'
