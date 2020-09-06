source("ui_about.R", local = TRUE)

fluidPage(
        title = "Covid-19 Analysis",
        
        navbarPage(
                title = "COVID-19 Simulator",
                collapsible = TRUE,
                fluid = TRUE,
                
                # Overview tab
                
                tabPanel(
                        title = "Overview",
                        icon = icon("eye"),
                        fluidRow(
                                column(width = 12,
                                       h3(" Worldwide Covid-19 situation -"),
                                       valueBoxOutput(outputId = "valueBox_confirmed" , width = 6),
                                       valueBoxOutput(outputId = "valueBox_recovered" , width = 6)
                                ),
                                
                        ),
                        fluidRow(
                                column(width = 12,
                                       valueBoxOutput(outputId = "valueBox_deceased"  , width = 6),
                                       valueBoxOutput(outputId = "valueBox_countries" , width = 6)
                                )
                        ),
                        fluidRow(
                                column(width = 12,
                                       h3(" Bangladesh Covid-19 situation -"),
                                       valueBoxOutput(outputId = "valueBox_confirmed_ban" , width = 4),
                                       valueBoxOutput(outputId = "valueBox_recovered_ban" , width = 4),
                                       valueBoxOutput(outputId = "valueBox_deceased_ban"  , width = 4),
                                ),
                        ),
                        fluidRow(
                                valueBoxOutput(
                                        outputId = "box_keyFigures",
                                        width = 12
                                )
                        )
                ),
                
                # Data table tab
                
                tabPanel(
                        title = "Data Table",
                        icon = icon("database"),
                        fluidRow(
                                dataTableOutput(
                                        outputId = "fullTable"
                                )
                        )
                ),
                
                # Plot tab
                
                tabPanel(
                        title = "Plots",
                        icon = icon("chart-bar"),
                        fluidRow(
                                h3(" Worldwide evolution of cases since outbreak -"),
                                plotlyOutput(
                                        outputId = "case_evolution"
                                ),
                                checkboxInput(
                                        inputId = "logY",
                                        label = "Logarithmic Y-axis"
                                )
                        ),
                        fluidRow(
                                h3("Cases on daily basis - "),
                                selectizeInput(
                                        inputId  = "selectize_casesByCountries_new",
                                        label    = "Select Country:",
                                        choices  = c("All", unique(data_evolution$Country.Region)),
                                        selected = "All"
                                        
                                ),
                                HTML("Note: Active cases are calculated as <i>Confirmed - (Estimated Recoveries + Deceased)</i>. Therefore, 
          new active cases can be negative for some days, if on this day there were more new estimated recoveries + deceased cases than there 
          were new confirmed cases."),
                                plotlyOutput(
                                        outputId = "case_evolution_new"
                                )
                        )
                ),
                
                # About tab
                
                tabPanel(
                        title = "About",
                        page_about,
                        value = "page_about",
                        icon = icon("info-circle")
                )
        )
)