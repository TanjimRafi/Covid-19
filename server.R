function(input , output){
        
        ################
        # Overview tab #
        ################
        sumData <- function(date) {
                if (date >= min(data_evolution$date)) {
                        data <- UpToDate(date) %>% summarise(
                                confirmed = sum(confirmed, na.rm = T),
                                recovered = sum(recovered, na.rm = T),
                                deceased  = sum(deceased, na.rm = T),
                                active = confirmed - (recovered + deceased)
                        )
                        return(data)
                }
                return(NULL)
        }
        
        output$valueBox_confirmed <- renderText({
                s <- sumData(max(data_evolution$date)) 
                confirmed <- s$confirmed
                confirmed
        })
        
        output$valueBox_recovered <- renderText({
                s <- sumData(max(data_evolution$date)) 
                recovered <- s$recovered
                recovered
        })
        
        output$valueBox_deceased <- renderText({
                s <- sumData(max(data_evolution$date)) 
                deceased <- s$deceased
                deceased
        })
        
        output$valueBox_active <- renderText({
                s <- sumData(max(data_evolution$date)) 
                active <- s$active
                active
        })
        
        output$valueBox_confirmed_ban <- renderText({
                d <- data_evolution %>% 
                        filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "confirmed") %>% 
                        select(value)
                confirmed_ban <- d$value
                confirmed_ban
        })
        
        output$valueBox_recovered_ban <- renderText({
                d <- data_evolution %>% 
                        filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "recovered") %>% 
                        select(value)
                recovered_ban <- d$value
                recovered_ban
        })
        
        output$valueBox_deceased_ban <- renderText({
                d <- data_evolution %>% 
                        filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "deceased") %>% 
                        select(value)
                deceased_ban <- d$value
                deceased_ban
        })
        
        output$valueBox_active_ban <- renderText({
                d <- data_evolution %>% 
                        filter(date == max(data_evolution$date) & Country.Region == "Bangladesh" & var == "active") %>% 
                        select(value)
                active_ban <- d$value
                active_ban
        })
        
        # Summary data
        
        summariseData <- function(df, groupBy) {
                df %>%
                        group_by(!!sym(groupBy)) %>%
                        summarise(
                                "Confirmed"            = sum(confirmed, na.rm = T),
                                "Estimated Recoveries" = sum(recovered, na.rm = T),
                                "Deceased"             = sum(deceased, na.rm = T),
                                "Active"               = sum(active, na.rm = T)
                        ) %>%
                        as.data.frame()
        }
        
        getSummaryDT <- function(data, groupBy, selectable = FALSE) {
                datatable(
                        na.omit(summariseData(data, groupBy)),
                        rownames  = FALSE,
                        options   = list(
                                order          = list(1, "desc"),
                                scrollX        = TRUE,
                                scrollY        = "37vh",
                                scrollCollapse = T,
                                dom            = 'ft',
                                paging         = FALSE
                        ),
                        selection = ifelse(selectable, "single", "none")
                )
        }
        
        output$summaryDT <- renderDataTable({
                getSummaryDT(UpToDate(current_date), "Country.Region" , selectable = TRUE)
        })
        
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
