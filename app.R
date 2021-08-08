#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)
# devtools::install_github('tystan/deltacomp')
library(deltacomp)

source("r/funcs.R")


ui <- dashboardPage(
    skin = "purple",
    dashboardHeader(title = "deltacomp online"),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            # https://icons.getbootstrap.com/: icon("cog", lib = "glyphicon")
            # https://fontawesome.com/icons?d=gallery&m=free:
            # "chart-bar", "chart-area", "code-branch", "codepen", 
            # "expand-arrows-alt", "th-list", "id-card", "users"
            menuItem("Data input", tabName = "datainput", icon = icon("file-import")),
            menuItem("Model options", tabName = "varselect", icon = icon("list-alt")),
            menuItem("Results", tabName = "results", icon = icon("chart-bar")),
            menuItem("Results log", tabName = "log", icon = icon("th-list")),
            menuItem("Personalised output", tabName = "output", icon = icon("sliders-h"))
        )
    ),
    dashboardBody(
        tabItems(
            # First tab content
            tabItem(tabName = "datainput",
                h2("Data input"), 
                box(
                    title = "CSV Upload", width = NULL,
                    fluidRow(column(12, fileInput("csv_file", "Choose TXT/CSV File:", accept = "text/csv"))),
                    fluidRow(
                        column(4, radioButtons("sep_radio", "Separator:", c("Comma" = ",", "Semicolon" = ";", "Tab" = "\t"))),
                        column(4, checkboxGroupInput("quote_checkbox", "Quotes:", c("Single" = "'", "Double" = "\""), selected = c("\""))),
                        column(4, checkboxGroupInput("header_checkbox", "Header:", c("True" = "TRUE"), selected = "TRUE"))
                    )
                ),
                box(
                    title = "Factor variables", width = NULL,
                    fluidRow(
                        column(6, selectInput("fac_vars_to_convert", label = "Numeric variables to convert to factors:", c(), multiple = TRUE)),
                        column(6, selectInput("fac_vars", label = "Current factor variables:", c(), multiple = TRUE))
                    )
                    # actionButton("conv_factors", label = "Convert selected numerical variables to factors")
                ),
                box(
                    title = "Data preview", width = NULL,
                    tableOutput("printme")
                )
            ),
            
            # Second tab content
            tabItem(tabName = "varselect",
                    h2("Model Options"),
                    box(title = "Variable Selection", width = NULL,
                        fluidRow(
                            column(4, selectInput("y", label = "Outcome variable:", c())),
                            column(4, selectInput("comp_vars", "Compositional variables:", c(), multiple = TRUE)),
                            column(4, selectInput("co_vars", "Covariates:", c(), multiple = TRUE))
                        )
                    ),
                    box(title = "Re-allocation options", width = NULL,
                        radioButtons("realloc_radio", "Method:", c("prop-realloc" = "prop-realloc", "one-v-one" = "one-v-one")),
                        uiOutput("delta_slider")
                        # sliderInput("deltas", "Deltas to re-allocate:", min = 0, max = 30, value = 15)
                    ),
                    actionButton("do_analysis", label = "Run analysis (see 'Results' tab)") #,
                    #verbatimTextOutput("vars_sel")
            ),
            
            # third tab content
            tabItem(tabName = "results",
                    h2("Compositional data analysis results"),
                    box(title = "Compositional re-allocaiton plot", width = NULL, plotOutput("res_plot")),
                    box(title = "Compositional re-allocaiton table", width = NULL, tableOutput("res_df"))
            ),
            
            # fourth tab content
            tabItem(tabName = "log",
                    h2("Log for results code"),
                    verbatimTextOutput("res_log")
            ),
            
            # 5th tab content
            tabItem(tabName = "output",
                    h2("Sliders etc to be added")
            )
        )
    )
)


server <- function(input, output, session) {
    
    reac_vals <- reactiveValues(data = NULL, sink_file = NULL)
    
    csv_file_reac <- reactive({ input$csv_file })
    
    data <- observe({
        # If missing input, return to avoid error later in function
        if(is.null(csv_file_reac()))
            return()
        
        df <-
            read.csv(
                csv_file_reac()$datapath,
                sep = input$sep_radio,
                quote = input$quote_checkbox,
                header = !is.null(input$header_checkbox) 
            )
                    
        col_list <- guess_columns(df)
        num_cols <- col_list$nums
        fac_cols <- col_list$facs
        
        n_num <- length(num_cols)
        
        
        updateSelectInput(session, "fac_vars", choices = fac_cols, selected = fac_cols)
        updateSelectInput(session, "fac_vars_to_convert", choices = num_cols, selected = NA)
        
        reac_vals$data <- df

    })
    
    # curr_dat <- reactive({
    # 
    #     up_dat <- data_updated()
    #     if (is.null(up_dat)) {
    #         init_dat <- data()
    #         if (is.null(init_dat)) {
    #             return()
    #         } else {
    #             return(init_dat)
    #         }
    #     } else {
    #         return(up_dat)
    #     }
    # 
    # })
    
    observe({ 
        
        convert_vars <- input$fac_vars_to_convert
        if (is.null(convert_vars)) {
            return()
        } 

        df <- reac_vals$data
        for (j in 1:length(convert_vars)) {
            df[[convert_vars[j]]] <- factor(df[[convert_vars[j]]])
        }

        col_list <- guess_columns(df)
        num_cols <- col_list$nums
        fac_cols <- col_list$facs
        updateSelectInput(session, "fac_vars", choices = fac_cols, selected = fac_cols)
        updateSelectInput(session, "fac_vars_to_convert", choices = num_cols, selected = NA)
        
        reac_vals$data <- df
        
        
    })
    
    output$printme <- renderTable({
        
        df <- reac_vals$data
        
        col_list <- guess_columns(df)
        num_cols <- col_list$nums
        fac_cols <- col_list$facs
        
        n_num <- length(num_cols)
        
        updateSelectInput(session, "y", choices = num_cols, selected = num_cols[1])
        updateSelectInput(session, "comp_vars", choices = num_cols, selected =  num_cols[2:n_num])
        updateSelectInput(session, "co_vars", choices = c(num_cols, fac_cols), selected = fac_cols)
        
        return(head(df, 10))
        
    })

    
    # output$vars_sel <- renderText({
    #     
    #     paste(
    #         "### selected outcome variable:\n",
    #         unlist(input$y), "\n\n",
    #         "### selected compositional variables:\n",
    #         paste(unlist(input$comp_vars), collapse = ", "), "\n\n",
    #         "### selected covariates:\n",
    #         paste(unlist(input$co_vars), collapse = ", "), "\n",
    #         sep = "", collapse = ""
    #     )
    #     
    # }, sep = "")
    
    output$delta_slider <- renderUI({
        sliderInput("delta_max", "Compositional substitutions", 0, 30, 15)
    })
    
    res_df <- eventReactive(input$do_analysis, { 
        
        comps_str <- paste0("c('", paste(input$comp_vars, collapse = "', '"), "')")
        covars_str <- "NULL"
        if (!is.null(input$co_vars))
            covars_str <- paste0("c('", paste(input$co_vars, collapse = "', '"), "')")
        
        sink_fl <- tempfile("sink_temp")
        sink(sink_fl)
        cat(
            "############################################ INPUT ############################################\n\n",
            "# Note you can copy and paste the below code in R to reproduce the output seen on this webpage\n\n",
            "# You need the following prerequisites to make sure the code works correctly:\n",
            "# (1) R installed on your computer\n",
            "# (2) the deltacomp package installed in R (see github.com/tystan/deltacomp for instrucitons)\n",
            "# (3) the csv '", input$csv_file$name, "' is in your working directory of R\n\n",
            "library(deltacomp)\n",
            "### The below function creates the output seen in the 'Results' tab\n",
            "predict_delta_comps(\n",
            "    dataf = '", input$csv_file$name, "',\n",
            "    y = '", input$y, "',\n",
            "    comps = ", comps_str, ",\n",
            "    covars = ", covars_str, ",\n",
            "    deltas = seq(-15, 15, by = 5) / (24 * 60),\n",
            "    comparisons = '", input$realloc_radio, "',\n",
            "    alpha = 0.05\n",
            ")\n\n\n",
            "############################################ OUTPUT ###########################################\n\n",
            sep = ""
        )
        
        res_df_ <-
            predict_delta_comps(
                dataf = reac_vals$data,
                y = input$y,
                comps = input$comp_vars,
                covars = input$co_vars,
                deltas = seq(-15, 15, by = 5) / (24 * 60),
                comparisons = input$realloc_radio,
                alpha = 0.05
            )
        sink()
        reac_vals$sink_file <- sink_fl
        
        return(res_df_)
        
    })
    
    observe({ 
        
        res_df()
        output$res_log <- renderText({ 
            paste(readLines(reac_vals$sink_file), collapse = "\n")
        })
        
        
    })
    
    output$res_df <- renderTable({
        df <- res_df()
        df$delta <- 24 * 60 * df$delta
        df[order(df$`comp+`, df$`comp-`, df$delta), ]
    })
    
    output$res_plot <- renderPlot({
        plot_delta_comp(res_df(), comp_total = 24 * 60, units_lab = "min")
    })
    
    
    
}



shinyApp(ui, server)













