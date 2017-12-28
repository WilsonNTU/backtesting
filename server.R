library(shiny)
library(quantmod)
library(PerformanceAnalytics)
source("helper.R")

shinyServer(function(input, output) {
    
    dataInput <- reactive({
        inFile <- input$file
        if (is.null(inFile))
            # return(NULL)
            return(read.csv('shiny_test.csv', header=input$header, stringsAsFactors = F))
        read.csv(inFile$datapath, header=input$header, stringsAsFactors = F)
    })
    

    price <- reactive({
        xts(dataInput()[,2],as.Date(dataInput()[,1]))
    })
    
    position <- reactive({
        as.vector(dataInput()[,3])
    })
    
    from_to <- reactive({
        c(input$date[1], input$date[2])
    })
    
    return_table <- reactive({
        if(is.null(dataInput()))
            return(NULL)
        # 1 for asset trading, 2 for option trading
        trade <- ifelse(input$trade_type=="Asset_Trading",1,ifelse(input$trade_type=="Option_Trading",2,return()))
        p <- price()
        pos <- position()
        from <- from_to()[1]
        # from <- ifelse(from<min(index(p)), min(index(p)), from)
        to <- from_to()[2]
        # to <- ifelse(to > max(index(p)), max(index(p)),to)
        pos <- pos[which(index(p)>=from & index(p)<= to)]
        p <- p[paste0(as.character(from),"/",as.character(to))]
        
        cost <- input$option_cost
        exp <- input$expiry_days

        bmk_return <- dailyReturn(p)
        my_return <- NULL
        if(trade == 1){
            my_return <- pos * bmk_return
        }else if(trade ==2){
            my_return <- opt_return(p, pos , cost , exp)
        }
        
        names(bmk_return) <- "benchmark"
        names(my_return) <- "my_strategy"
        return(cbind(my_return, bmk_return))
    })
    
    output$plot <- renderPlot({
        if(is.null(dataInput()))
            return(NULL)
        
        
        charts.PerformanceSummary(return_table(), colorset = c("darkblue","black"))
    })
    
    output$table <- renderTable({
        if(is.null(dataInput()))
            return(NULL)
        bmk_table <- Performance(return_table()[,"benchmark"])
        strat_table <- Performance(return_table()[,"my_strategy"])
        the_row_name <- names(bmk_table)
        table <- data.frame(cbind(bmk_table, strat_table), row.names = the_row_name)
        colnames(table) <- c("Benchmark", "My Strategy")
        return(table)
    }, align = 'c', rownames = TRUE)
    
    
    output$ui <- renderUI({
        if (is.null(input$trade_type))
            return(NULL)
        
        # Depending on input$input_type, we'll generate a different
        # UI component and send it to the client.
        switch(input$trade_type,
               "Asset_Trading" =  NULL,
               "Option_Trading" = numericInput("option_cost","option_cost",value=0.005, step = 0.001, min = 0)
               
        )
       
        
    })
    output$ui2 <- renderUI({
        if (is.null(input$trade_type))
            return(NULL)
        if (input$trade_type == "Option_Trading"){
            "Option_Trading" = numericInput("expiry_days","expiry_days",value=7, step = 1, min = 0)
        }
    })
    
    output$text <- renderText({
        if (is.null(input$file))
            "CAUTION: now displaying an example file. Please upload your own file for backtesting."
    })
#     
#     output$dynamic_value <- renderPrint({
#         str(input$dynamic)
#     })
})