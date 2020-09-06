function(input , output){
        
        # ---- Loading libraries ----
        library("shiny")
        library("shinydashboard")
        library("tidyverse")
        library("leaflet")
        library("plotly")
        library("DT")
        library("fs")
        library("wbstats")
        
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
        
        ################
        # Overview tab #
        ################
        
        sumData <- function(date) {
                if (date >= min(data_evolution$date)) {
                        data <- UpToDate(date) %>% summarise(
                                confirmed = sum(confirmed, na.rm = T),
                                recovered = sum(recovered, na.rm = T),
                                deceased  = sum(deceased, na.rm = T),
                                countries = n_distinct(Country.Region)
                        )
                        return(data)
                }
                return(NULL)
        }
        
        key_figures <- reactive({
                data <- sumData(max(data_evolution$date))
                data_yesterday <- sumData(max(data_evolution$date) - 1)
                
                data_new <- list(
                        new_confirmed = (data$confirmed - data_yesterday$confirmed) / data_yesterday$confirmed * 100,
                        new_recovered = (data$recovered - data_yesterday$recovered) / data_yesterday$recovered * 100,
                        new_deceased  = (data$deceased - data_yesterday$deceased) / data_yesterday$deceased * 100,
                        new_countries = data$countries - data_yesterday$countries
                )
                
                keyFigures <- list(
                        "confirmed" = HTML(paste(format(data$confirmed, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_confirmed))),
                        "recovered" = HTML(paste(format(data$recovered, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_recovered))),
                        "deceased"  = HTML(paste(format(data$deceased, big.mark = " "), sprintf("<h4>(%+.1f %%)</h4>", data_new$new_deceased))),
                        "countries" = HTML(paste(format(data$countries, big.mark = " "), "/ 195", sprintf("<h4>(%+d)</h4>", data_new$new_countries)))
                )
                return(keyFigures)
        })
        
        output$valueBox_confirmed <- renderValueBox({
                valueBox(
                        key_figures()$confirmed,
                        subtitle = "Total Confirmed Cases",
                        icon     = icon("procedures"),
                        color    = "red",
                        width    = NULL
                )
        })
        
        output$valueBox_recovered <- renderValueBox({
                valueBox(
                        key_figures()$recovered,
                        subtitle = "Total Estimated Recoveries",
                        icon     = icon("heartbeat"),
                        color    = "green"
                )
        })
        
        output$valueBox_deceased <- renderValueBox({
                valueBox(
                        key_figures()$deceased,
                        subtitle = "Total Deceased",
                        icon     = icon("skull"),
                        color    = "purple"
                )
        })
        
        output$valueBox_countries <- renderValueBox({
                valueBox(
                        key_figures()$countries,
                        subtitle = "Total Affected Countries",
                        icon     = icon("location-arrow"),
                        color    = "yellow"
                )
        })
        
        output$valueBox_confirmed_ban <- renderValueBox({
                valueBox(
                        value    = data_evolution %>% filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "confirmed") %>% select(value), 
                        subtitle = "Total Confirmed Cases",
                        icon     = icon("procedures"),
                        color    = "red",
                        width    = NULL
                )
        })
        
        output$valueBox_recovered_ban <- renderValueBox({
                valueBox(
                        value = data_evolution %>% filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "recovered") %>% select(value),
                        subtitle = "Total Estimated Recoveries",
                        icon     = icon("heartbeat"),
                        color    = "green"
                )
        })
        
        output$valueBox_deceased_ban <- renderValueBox({
                valueBox(
                        value = data_evolution %>% filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "deceased") %>% select(value),
                        subtitle = "Total Deceased",
                        icon     = icon("skull"),
                        color    = "purple"
                )
        })
        
        output$box_keyFigures <- renderUI(box(
                title = paste0("Key Figures (", strftime(max(data_evolution$date), format = "%d.%m.%Y"), ")"),
                div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z")),
                width = 12
        ))
        
        ##################
        # Data table tab #
        ##################
        
        getFullTableData <- function(groupBy) {
                padding_left <- max(str_length(data_evolution$value_new), na.rm = TRUE)
                data1 <- data_evolution %>%
                        filter(date == current_date) %>%
                        pivot_wider(names_from = var, values_from = c(value, value_new)) %>%
                        select(-date, -Lat, -Long) %>%
                        add_row(
                                "Province.State"      = "World",
                                "Country.Region"      = "World",
                                "population"          = 7800000000,
                                "value_confirmed"     = sum(.$value_confirmed, na.rm = T),
                                "value_new_confirmed" = sum(.$value_new_confirmed, na.rm = T),
                                "value_recovered"     = sum(.$value_recovered, na.rm = T),
                                "value_new_recovered" = sum(.$value_new_recovered, na.rm = T),
                                "value_deceased"      = sum(.$value_deceased, na.rm = T),
                                "value_new_deceased"  = sum(.$value_new_deceased, na.rm = T),
                                "value_active"        = sum(.$value_active, na.rm = T),
                                "value_new_active"    = sum(.$value_new_active, na.rm = T)
                        ) %>%
                        group_by(!!sym(groupBy), population) %>%
                        summarise(
                                confirmed_total     = sum(value_confirmed, na.rm = T),
                                confirmed_new       = sum(value_new_confirmed, na.rm = T),
                                recovered_total     = sum(value_recovered, na.rm = T),
                                recovered_new       = sum(value_new_recovered, na.rm = T),
                                deceased_total      = sum(value_deceased, na.rm = T),
                                deceased_new        = sum(value_new_deceased, na.rm = T),
                                active_total        = sum(value_active, na.rm = T),
                                active_new          = sum(value_new_active, na.rm = T)
                        ) %>%
                        as.data.frame()
                
        }
        
        output$fullTable <- renderDataTable({
                data <- getFullTableData("Country.Region")
                columNames <- c(
                        "Country",
                        "Population",
                        "Total Confirmed",
                        "New Confirmed",
                        "Total Estimated Recoveries",
                        "New Estimated Recoveries",
                        "Total Deceased",
                        "New Deceased",
                        "Total Active",
                        "New Active")
                DT::datatable(
                        data,
                        rownames  = FALSE,
                        colnames  = columNames,
                        escape    = FALSE,
                        selection = "none",
                        extensions = "FixedHeader",
                        options   = list(paging = FALSE
                                         , scrollCollapse = TRUE
                                         , fixedHeader = TRUE)
                )
        })
        
        ############
        # Plot tab #
        ############
        
        output$case_evolution <- renderPlotly({
                data <- data_evolution %>%
                        group_by(date, var) %>%
                        summarise("value" = sum(value, na.rm = T)) %>%
                        as.data.frame()
                
                p <- plot_ly(
                        data,
                        x     = ~date,
                        y     = ~value,
                        name  = sapply(data$var, capFirst),
                        color = ~var,
                        type  = 'scatter',
                        mode  = 'lines') %>%
                        layout(
                                yaxis = list(title = "Number of Cases"),
                                xaxis = list(title = "Date")
                        )
                if (input$logY) {
                        p <- layout(p, yaxis = list(type = "log"))
                }
                return(p)
        })
        
        getDataByCountry <- function(countries, normalizeByPopulation) {
                req(countries)
                data_confirmed <- data_evolution %>%
                        select(Country.Region, date, var, value, population) %>%
                        filter(Country.Region %in% countries &
                                       var == "confirmed" &
                                       value > 0) %>%
                        group_by(Country.Region, date, population) %>%
                        summarise("Confirmed" = sum(value, na.rm = T)) %>%
                        arrange(date)
                if (nrow(data_confirmed) > 0) {
                        data_confirmed <- data_confirmed %>%
                                mutate(Confirmed = if_else(normalizeByPopulation, round(Confirmed / population * 100000, 2), Confirmed))
                }
                data_confirmed <- data_confirmed %>% as.data.frame()
                
                data_recovered <- data_evolution %>%
                        select(Country.Region, date, var, value, population) %>%
                        filter(Country.Region %in% countries &
                                       var == "recovered" &
                                       value > 0) %>%
                        group_by(Country.Region, date, population) %>%
                        summarise("Estimated Recoveries" = sum(value, na.rm = T)) %>%
                        arrange(date)
                if (nrow(data_recovered) > 0) {
                        data_recovered <- data_recovered %>%
                                mutate(EstimatedRecoveries = if_else(normalizeByPopulation, round(EstimatedRecoveries / population * 100000, 2), EstimatedRecoveries))
                }
                data_recovered <- data_recovered %>% as.data.frame()
                
                data_deceased <- data_evolution %>%
                        select(Country.Region, date, var, value, population) %>%
                        filter(Country.Region %in% countries &
                                       var == "deceased" &
                                       value > 0) %>%
                        group_by(Country.Region, date, population) %>%
                        summarise("Deceased" = sum(value, na.rm = T)) %>%
                        arrange(date)
                if (nrow(data_deceased) > 0) {
                        data_deceased <- data_deceased %>%
                                mutate(Deceased = if_else(normalizeByPopulation, round(Deceased / population * 100000, 2), Deceased))
                }
                data_deceased <- data_deceased %>% as.data.frame()
                
                return(list(
                        "confirmed" = data_confirmed,
                        "recovered" = data_recovered,
                        "deceased"  = data_deceased
                ))
        }
        
        output$case_evolution_new <- renderPlotly({
                req(input$selectize_casesByCountries_new)
                data <- data_evolution %>%
                        mutate(var = sapply(var, capFirst)) %>%
                        filter(if (input$selectize_casesByCountries_new == "All") TRUE else Country.Region %in% input$selectize_casesByCountries_new) %>%
                        group_by(date, var, Country.Region) %>%
                        summarise(new_cases = sum(value_new, na.rm = T))
                
                if (input$selectize_casesByCountries_new == "All") {
                        data <- data %>%
                                group_by(date, var) %>%
                                summarise(new_cases = sum(new_cases, na.rm = T))
                }
                
                p <- plot_ly(data = data, x = ~date, y = ~new_cases, color = ~var, type = 'bar') %>%
                        layout(
                                yaxis = list(title = "Number of Cases"),
                                xaxis = list(title = "Date")
                        )
        })
}
