dashboardPage(
    skin = "black",
    dashboardHeader(title = "Violence in Indonesia"),
    dashboardSidebar(
        sidebarMenu(
            menuItem(
                text = "Overview",
                tabName = "overview",
                icon = icon("gears")
                
            ),
            menuItem(
                text = "Rank Violence",
                tabName = "profile",
                icon = icon("chart-area")
                
            ),
            menuItem(
                text = "Distribution Map",
                tabName = "persebaran",
                icon = icon("map")
                
            ),
            menuItem(
                text = "Data",
                tabName = "data",
                icon = icon("cubes")
                
            )
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(
                tabName = "overview",
                h2(tags$b("Data Protests & Violence in Indonesia 2015-2019")),
                fluidRow(
                    box(
                        title = "Overview",
                        width = 12, 
                        
                        
                        valueBoxOutput(
                            outputId = "event", width = 3 
                        ),
                        
                        valueBoxOutput(
                            outputId = "fatalities", width = 3 
                        ),
                        
                        valueBoxOutput(
                            outputId = "province", width = 3 
                        ),
                        
                        valueBoxOutput(
                            outputId = "city", width = 3 
                        )
                    ) 
                ),
                fluidRow(
                    box(
                            width = 4,
                            height = 460,
                            title = "Filter",
                            solidHeader = T,
                            status = 'danger',
                            
                            pickerInput(
                                "district",
                                "Choose Violence Type",
                                choices = as.character(unique(df$event_type)),
                                selected = as.character(unique(df$event_type)),
                                options = list(`actions-box` = TRUE),
                                multiple = T
                            ),
                            
                            dateRangeInput(
                                "date",
                                strong("Date range"),
                                start = min(df$event_date),
                                end = max(df$event_date),
                                min = min(df$event_date),
                                max = max(df$event_date)
                            ),
                            
                            radioButtons(
                                "SelectBy",
                                label = strong("Select Date"),
                                choices = list(
                                    "By Year" = 'year',
                                    "By Month" = 'month',
                                    "Ratio Change by Year" = 'ratio'
                                ),
                                selected = 'year'
                            ),
                            
                            checkboxInput(
                                "Byother",
                                label = strong("Select by Date"),
                                value = TRUE
                            )
                            
                        ),
                        box(
                            width = 8,
                            title = 'Violence Rate by Time',
                            status = 'danger',
                            solidHeader = T,
                            plotlyOutput('date_line'),
                            
                        )
                    
                ),
                fluidRow(
                    
                        
                    
                        box(width = 4,
                            title = ("Average by Range Date"),
                            status = 'danger',
                            solidHeader = T,
                            
                            
                            valueBoxOutput(
                                outputId = "avg_fatal", width = 12
                                
                                
                            )
                            ),
                        
                        box(
                            width = 8,
                            title = 'The Amount of All Violence Data by Date Range',
                            status = 'danger',
                            solidHeader = T,
                            plotlyOutput('grafik2'),
                           
                        )
                    
                )
                
                
                
                
            ),
            tabItem(
                tabName = "persebaran",
                
                fluidRow(
                    box(
                        height = 280,
                        width = 4,
                        status = 'danger',
                        selectInput(
                            
                            choices = selectprovince,
                            label = "Provinsi",
                            selected = df,
                            inputId = "inputmap1"
                        )
                        
                    ),
                   
                    box(
                        
                        # title = "Map of the spread of violence in Indonesia",
                        h2(tags$b("Map of the spread of violence in Indonesia")),
                        width = 12,
                        status = 'danger',
                        high = "100%",
                        leafletOutput("map")
                    )

                )
            ),
            tabItem(
                tabName = "data",
                h2(tags$b("Data Protests & Violence in Indonesia 2015-2019")),
                DT::dataTableOutput("data")
                    ),
            
            tabItem(tabName = 'profile',
                    fluidRow(
                        
                        
                        box(
                            width = 5,
                            height = 460,
                            title = 'Rank Violence',
                            solidHeader = T,
                            status = 'danger',
                            selectInput(
                                "kekerasan1",
                                h4("Violence Type"),
                                choices = as.character(unique(df$event_type)),
                                selected = as.character(unique(df$event_type))[1]
                            ),
                            
                            sliderInput(
                                "year", 
                                label = h4("Year Range"), 
                                min = min(df$year), 
                                max = max(df$year), 
                                value = c(2015,2019)
                            ),
                            
                            sliderInput(
                                "max",
                                h4("Maximum Rank of City"),
                                min = 1,
                                max = 50,
                                value = 10,
                                step = 1
                            )
                            
                        ),
                        
                        box(
                            width = 7,
                            title = 'Word Cloud City',
                            solidHeader = T,
                            status = 'danger',
                            plotOutput('word')
                        )
                        
                    ),
                    fluidRow(
                        
                           
                            box(
                                width = 12,
                                title = 'Graph of Violence Type',
                                solidHeader = T,
                                status = 'danger',
                                plotlyOutput('hist')
                            )
                        
                    ))
            )
            
            
            
        )
        
    )
    


