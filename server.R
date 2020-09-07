source("main.R" , local = TRUE)

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
