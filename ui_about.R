body_about <- dashboardBody(
        fluidRow(
                box(
                        title = h1("About this project", style = "padding-left: 18px"),
                        background = "aqua",
                        column(
                                width = 12,
                                height = 15,
                                h3("This dashboard demonstrates some recent news about the Corona virus pandemic.
                                    This app is a simulator , that reads from Jhons Hopkins University(JHU) dataset and shows 
                                    some data related to mortality , recovery , infected etc..."),
                                tags$br()
                                
                        )
                ),
                box(
                        title = h1("Disclaimer", style = "padding-left: 18px"),
                        background = "teal",
                        column(
                                width = 12,
                                height = 15,
                                h3("This app is for educational purposes only. Opinions or points of view expressed 
                                   in this app represent the view of creator only, that relies on public data from
                                   JHU. Nothing in this app constitutes legal advice."),
                                br(),
                                br()
                                
                        )
                )
        ),
        fluidRow(
                box(
                        title = h1("Covid-19 social distancing", style = "padding-left: 18px"),
                        background = "orange",
                        column(
                                width = 12,
                                height = 15,
                                h3("Please stay safe and respect social distancing, which can be tough on people and
                                   could disrupt the social and economic loop of life."),
                                tags$br()
                                
                        )
                ),
                box(
                        title = h1("Creator", style = "padding-left: 18px"),
                        background = "lime",
                        column(
                                width = 12,
                                height = 15,
                                h3("Tanjim Rafi"),
                                helpText(a(href = "https://www.facebook.com/rafi.tanjim/", target = "_blank", "Facebook")),
                                br(),
                                br()
                                
                        )
                )
        )
)

page_about <- dashboardPage(
        header = dashboardHeader(disable = TRUE),
        sidebar = dashboardSidebar(disable = TRUE),
        body = body_about,
        title = "About"
)
