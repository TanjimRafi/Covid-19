# Loading files from different directory
source("utils.R")

# Reading and refreshing data from GitHub
# This function download the data only once
DownloadTheCovidData <- function(){
        download.file(                                                                    # Go to https://github.com/CSSEGISandData/COVID-19/
                url = "https://github.com/CSSEGISandData/COVID-19/archive/master.zip"     # Click on the green "Code" button
                , destfile = "data/covid19JH.zip"                                         # Copy the link address of "Download Zip" button to get the download link
        ) 
        data_path <- "COVID-19-master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_"
        
        unzip(
                zipfile = "data/covid19JH.zip"
                , files = paste0(data_path , c("confirmed_global.csv","deaths_global.csv","recovered_global.csv"))
                , exdir = "data"
                , junkpaths = T
        )
}

# We need to update the data according to time
UpdateMyData <- function(){
        
        # Download data from Jhons Hopkins if the data is older than 24 hour
        T_refresh = 24       # hours
        
        if(!dir.exists("data")){
                dir.create("data")
                DownloadTheCovidData()
        }
        
        else if((!file.exists("data/covid19JH.zip"))|| (as.double(Sys.time() - file.info("data/covid19JH.zip")$atime , units = "hours")) > T_refresh){
                DownloadTheCovidData()
        }
}

# Update with the start of the app
UpdateMyData()

# CSV data
data_confirmed <- read.csv("data/time_series_covid19_confirmed_global.csv")
data_deceased <- read.csv("data/time_series_covid19_deaths_global.csv")
data_recovered <- read.csv("data/time_series_covid19_recovered_global.csv")

# Latest data
n <- names(data_confirmed)[ncol(data_confirmed)] 
t <- trimws(gsub("\\X"," ", n))

current_date <- as.Date(t , format = "%m.%d.%y")
changed_date <- file_info("data/covid19JH.zip")$change_time

# Evolution data by country
data_confirmed_sub <- data_confirmed %>%
        pivot_longer(names_to = "date", cols = 5:ncol(data_confirmed)) %>%
        group_by(Province.State , Country.Region , date , Lat , Long) %>%
        summarise("confirmed" = sum(value, na.rm = T))
data_confirmed_sub$date <- as.Date(trimws(gsub("\\X"," ", data_confirmed_sub$date)) , format = "%m.%d.%y")


data_deceased_sub <- data_deceased %>%
        pivot_longer(names_to = "date", cols = 5:ncol(data_deceased)) %>%
        group_by(Province.State, Country.Region , date , Lat , Long) %>%
        summarise("deceased" = sum(value, na.rm = T))
data_deceased_sub$date <- as.Date(trimws(gsub("\\X"," ", data_deceased_sub$date)) , format = "%m.%d.%y")

data_evolution <- data_confirmed_sub %>%
        full_join(data_deceased_sub) %>%
        ungroup() %>%
        arrange(date) %>%
        group_by(Province.State , Country.Region , Lat , Long) %>%
        mutate(
                recovered = lag(confirmed, 14, default = 0) - deceased,
                recovered = ifelse(recovered > 0, recovered, 0),
                active = confirmed - recovered - deceased
        ) %>%
        pivot_longer(names_to = "var", cols = c(confirmed, recovered, deceased, active)) %>%
        ungroup()

# Calculating new cases
data_evolution <- data_evolution %>%
        group_by(Province.State , Country.Region) %>%
        mutate(value_new = value - lag(value, 4, default = 0)) %>%
        ungroup()

population <- wb(country = "countries_only", indicator = "SP.POP.TOTL", startdate = 2018, enddate = 2020)
n <- length(unique(population$country))
odd <- c()
for(i in 1:n){
        odd[i] <- 2*i - 1
}

population <- population[odd , ]
population <- population %>%
        select(country, value) %>%
        rename(population = value)

countryNamesPop <- c("Brunei Darussalam", "Congo, Dem. Rep.", "Congo, Rep.", "Czech Republic",
                     "Egypt, Arab Rep.", "Iran, Islamic Rep.", "Korea, Rep.", "St. Lucia", "West Bank and Gaza", "Russian Federation",
                     "Slovak Republic", "United States", "St. Vincent and the Grenadines", "Venezuela, RB")
countryNamesDat <- c("Brunei", "Congo (Kinshasa)", "Congo (Brazzaville)", "Czechia", "Egypt", "Iran", "Korea, South",
                     "Saint Lucia", "occupied Palestinian territory", "Russia", "Slovakia", "US", "Saint Vincent and the Grenadines", "Venezuela")

population[which(population$country %in% countryNamesPop), "country"] <- countryNamesDat

# Data from Wikipedia
noDataCountries <- data.frame(
        country    = c("Cruise Ship", "Guadeloupe", "Guernsey", "Holy See", "Jersey", "Martinique", "Reunion", "Taiwan*"),
        population = c(3700, 395700, 63026, 800, 106800, 376480, 859959, 23780452)
)
population <- bind_rows(population , noDataCountries)

data_evolution <- data_evolution %>% left_join(population , by = c("Country.Region" = "country"))

good <- complete.cases(data_evolution)
data_evolution <- data_evolution[good , ]

UpToDate <- function(inputDate){
        data_evolution[which(data_evolution$date == inputDate) , ] %>%
                distinct() %>%
                pivot_wider(id_cols = c("Province.State","Country.Region","date","Lat","Long","population") , names_from = var , values_from = value) %>%
                filter(confirmed > 0 | recovered > 0 | deceased > 0 | active > 0)
}

data_latest <- UpToDate(max(data_evolution$date))

