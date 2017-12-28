library(shiny)

shinyUI(fluidPage(
    titlePanel("BackTesting Platform, ver 1.1"),
    sidebarLayout(
        sidebarPanel(
            
            fileInput('file', 'Only CSV file is accepted !',
                      accept=c('text/csv', 
                               'text/comma-separated-values,text/plain', 
                               '.csv','application/vnd.ms-excel',
                                'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet')),
            helpText("Please upload a file with the following three columns in order: (Date, Close_Price, Position)."),
            helpText('The benchmark output indicates the simple buy and hold strategy.'),
            helpText("The picture below is an example of input file:"),
            img(src='shiny_test_preview.png', width =220, height = 150, 
                style="display: block; margin-left: auto; margin-right: auto;"),
            helpText("Put a check in the 'Header' box if the first row of your data is the title of each column."),
            checkboxInput('header', 'Header', TRUE),
            sliderInput('date','Date', min = as.Date("2000-01-01"), max = Sys.Date(), value = c(as.Date("2000-01-01"),Sys.Date())),
            selectInput("trade_type", "Trading type",
                        c("Asset_Trading","Option_Trading")),
            uiOutput("ui"),
            uiOutput("ui2"),
            tags$hr("Published by Fuh-Hwa Trust, 2015")
        ),
        mainPanel(
            textOutput('text'),
            plotOutput('plot'),
            tableOutput('table'),
            tags$style(type="text/css",
                       ".shiny-output-error { visibility: hidden; }",
                       ".shiny-output-error:before { visibility: hidden; }"
            )
        )
    )
))

