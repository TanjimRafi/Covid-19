source("global.R" , local = TRUE)

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
                                       h3("Covid-19 Situation (Worldwide)"),
                                       valueBox(
                                               value = "valueBox_confirmed" 
                                               , subtitle = "Total Confirmed Cases"
                                               , icon = "procedures"
                                               , color = "red"),
                                       valueBox(value = "valueBox_recovered" 
                                                , subtitle = "Total Estimated Recoveries"
                                                , icon = "heartbeat"
                                                , color = "green"),
                                       valueBox(value = "valueBox_deceased"  
                                                , subtitle = "Total Deceased"
                                                , icon = "skull"
                                                , color = "purple"),
                                       valueBox(value = "valueBox_active"  
                                                , subtitle = "Active Cases"
                                                , icon = "file-medical"
                                                , color = "teal")
                                )
                        ),
                        fluidRow(
                                column(width = 12,
                                       h3("Covid-19 Situation (Bangladesh)"),
                                       valueBox(value = "valueBox_confirmed_ban" 
                                                , subtitle = "Total Confirmed Cases"
                                                , icon = "procedures"
                                                , color = "red"),
                                       valueBox(value = "valueBox_recovered_ban" 
                                                , subtitle = "Total Estimated Recoveries"
                                                , icon = "heartbeat"
                                                , color = "green"),
                                       valueBox(value = "valueBox_deceased_ban"  
                                                , subtitle = "Total Deceased"
                                                , icon = "skull"
                                                , color = "purple"),
                                       valueBox(value = "valueBox_active_ban"  
                                                , subtitle = "Active Cases"
                                                , icon = "file-medical"
                                                , color = "teal")
                                )
                        ),
                        fluidRow(
                                column(
                                        width = 12,
                                        paste0("Key Figures (", strftime(max(data_evolution$date), format = "%d.%m.%Y"), ")"),
                                        div("Last updated: ", strftime(changed_date, format = "%d.%m.%Y - %R %Z"))
                                )
                        ),
                        fluidRow(
                                column(
                                        width = 12,
                                        h3("Summary Datatable"),
                                        dataTableOutput(
                                                outputId = "summaryDT"
                                        ),
                                        br(),
                                        br()
                                )
                        )
                ),
                
                # Data table tab
                
                tabPanel(
                        title = "Data Table",
                        icon = icon("database"),
                        fluidRow(
                                h2("Complete Datatable"),
                                dataTableOutput(
                                        outputId = "fullTable"
                                )
                        ),
                        br()
                ),
                
                # Plot tab
                
                tabPanel(
                        title = "Plots",
                        icon = icon("chart-bar"),
                        fluidRow(
                                HTML("Please wait for few seconds while loading!"),
                                br(),
                                h2(" Worldwide Evolution of Cases since Outbreak"),
                                HTML("Advice: Please click on 'Compare data on hover' to get a better understanding!"),
                                plotlyOutput(
                                        outputId = "case_evolution"
                                ),
                                checkboxInput(
                                        inputId = "logY",
                                        label = "Logarithmic Y-axis"
                                )
                        ),
                        fluidRow(
                                h2("Cases on Daily Basis"),
                                selectizeInput(
                                        inputId  = "selectize_casesByCountries_new",
                                        label    = "Select Country:",
                                        choices  = c("All", unique(data_evolution$Country.Region)),
                                        selected = "All"
                                        
                                ),
                                HTML("Note: Active cases are calculated as <i>Confirmed - (Estimated Recoveries + Deceased)</i>. Therefore, 
          new active cases can be negative for some days, if on this day there were more new estimated recoveries + deceased cases than there 
          were new confirmed cases."),
                                br(),
                                HTML("Advice: Please click on 'Compare data on hover' to get a better understanding!"),
                                plotlyOutput(
                                        outputId = "case_evolution_new"
                                ),
                                br(),
                                br()
                        )
                ),
                
                # About Tab
                tabPanel(
                        title = "About",
                        icon = icon("info-circle"),
                        fluidRow(
                                column(
                                        h2("About This Project"),
                                        width = 12,
                                        h4("This app demonstrates some recent news about the COVID-19 pandemic.
                                            This app is a simulator, that reads from Jhons Hopkins University(JHU) dataset 
                                            and shows some data related to mortality , recovery , infected etc...")
                                ),
                                column(
                                        h2("Covid-19 Social Distancing"),
                                        width = 12,
                                        h4("Please stay safe and respect social distancing, which can be tough on people and
                                           could disrupt the social and economic loop of life.")
                                )
                        ),
                        fluidRow(
                                column(
                                        h2("Disclaimer"),
                                        width = 12,
                                        h4("This app is for educational purposes only. Opinions or points of view expressed 
                                           in this app represent the view of creator only, that relies on public data from JHU. 
                                           Nothing in this app constitutes legal advice.")
                                ),
                                column(
                                        h2("Reference"),
                                        width = 12,
                                        h4("Data Scource"),
                                        helpText(a(href = "https://www.github.com/CSSEGISandData/", target = "_blank", "JHONS HOPKINS WHITING SCHOOL of ENGINEERING")),
                                        h4("Tanjim Rafi"),
                                        helpText(a(href = "https://www.facebook.com/rafi.tanjim/", target = "_blank", "Facebook")),
                                        h4("View Code"),
                                        helpText(a(href = "https://github.com/TanjimRafi/Covid-19", target = "_blank", "GitHub"))
                                )
                        )
                )
        )
)