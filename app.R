library(shiny)
library(shinydashboard)
library(googledrive)
library(googlesheets4)

options(
  # whenever there is one account token found, use the cached token
  gargle_oauth_email = TRUE,
  # specify auth tokens should be stored in a hidden directory ".secrets"
  gargle_oauth_cache = "~/.secrets"
)

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(
    collapsed = FALSE, 
    div(htmlOutput("welcome"), style = "padding: 20px"),
    sidebarMenu(
      menuItem("View Tables", tabName = "view_table", icon = icon("search")),
      menuItem("Create Tables", tabName = "create_table", icon = icon("plus-square")),
      menuItem("Update Tables", tabName = "update_table", icon = icon("exchange-alt")),
      menuItem("Insert Entries", tabName = "insert_value", icon = icon("edit")),
      menuItem("Delete Tables", tabName = "del_table", icon = icon("trash-alt")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))
    )
  ),
  dashboardBody( 
    tabItems(
    tabItem(tabName = "view_table", uiOutput("tab1UI")),
    tabItem(tabName = "del_table", uiOutput("tab2UI")),
    tabItem(tabName = "update_table", uiOutput("tab3UI")),
    tabItem(tabName = "create_table", uiOutput("tab4UI")),
    tabItem(tabName = "insert_value", uiOutput("tab5UI")),
    tabItem(tabName = "about", uiOutput("tab6UI"))
    )
  )
)

server <- function(input, output) {
  
  output$tab1UI <- renderUI({
    box(width = NULL, status = "primary",
        sidebarLayout(
          sidebarPanel(
            box(width = 12,
                collapsible = TRUE,
                div(style = "height: 15px; background-color: white;"),
                title = "Database Info:",
                p("")),
            selectInput(inputId = "sel_table_1",
                        label = "Tables in Database",
                        choices = dbListTables(db),
                        selected = "custs"),
            textOutput(outputId = "tab_intro"),
            tags$head(tags$style("#tab_intro{font-size: 15px;font-style: italic;}"))
          ),
          mainPanel(
            h4(strong("Table Preview")),
            dataTableOutput(outputId = "sel_table_view")
          )
        )
    )
  })
  
  #update table
  output$tab3UI <- renderUI({
    fluidPage(
      fluidRow(
        box(width = 12, collapsible = TRUE, title = "Note:", "")
      ),
      fluidRow(
        box(title = "Rename Table", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel(
              textInput(),
              actionButton())
        ),
        box(title = "Rename Column", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        ),
        box(title = "Add Column", width = 4, solidHeader = TRUE, status = "primary",
            selectInput(),
            wellPanel()
        )
      )
    )
  })
  
  
  #create table
  output$tab4UI <- renderUI({
    box(width = NULL, status = "primary",
        textInput(inputId = "table_name", label = "Table name"),
        numericInput(inputId = "ncols", label = "Number of columns", 1, min = 1),
        uiOutput(outputId = "cols"),
        actionButton(inputId = "create_table", label = "Create table", class = "btn-info", style = "")
    )
  })
  
  output$cols <- renderUI({
    req(input$ncols >= 1)
    cols <- vector("list", input$ncols)
    for (i in seq_len(input$ncols)) {
      cols[[i]] <- box(
        title = paste("Column", i), width = 6, solidHeader = TRUE, status = "primary",
        textInput(inputId = paste0("colName", i), label = "Column name"),
        selectInput(inputId = paste0("colType", i), label = "Column type", 
                    choices = c("NUMERIC", "VARCHAR(255)","BOOLEAN","DATE")
        )
      )
    }
    cols
  })
  
  
  observeEvent(input$create_table, {
  # gather all the colnames into a list
  col_names_list = list()
  for (i in seq_len(input$ncols)) {
    col_names_list <- c(col_names_list,input[[paste0("colName", i)]])
  }
 # updateSelectInput(session, "sel_table_1", choices = dbListTables(db))
  showModal(modalDialog(
    title = "Success",
    "The table has been successfully created.",
    footer = modalButton("OK"), easyClose = TRUE ) )
  })

}

shinyApp(ui, server)