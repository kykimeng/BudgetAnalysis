library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(readr)
library(dplyr)

shinyUI(
  dashboardPage(skin = 'black',
    dashboardHeader(title = "Budget Analysis"), 
    dashboardSidebar(
      sidebarMenu(id = 'menu',
        menuItem(text = "Expenses by Category", tabName = "sum"),
        menuItem(text = "Expenses by Month", tabName = 'tot'),
        menuItem(text = "Income and Saving", tabName = "saving"),
        menuItem(text = "Debt", tabName = "debt")
      ), 
      tags$hr(),
      conditionalPanel(
        condition = "input.menu == 'sum'",
        box(width = 12, background = 'blue', 
            selectInput('cat', "Select Category", multiple = T, 
                        choices = c()), 
            radioButtons('mode', NULL, choices = c("Total", "Average")), 
            dateRangeInput('dr_filter', "Select Date Range")
        )
      ),
      conditionalPanel(
        condition = "input.menu == 'tot'",
        box(width = 12, background = 'blue', 
            selectInput('mon', "Select Month", choices = c()),
            checkboxGroupInput('cat_filter', "Select Categories", choices = c()))
      ),
      conditionalPanel(
        condition = "input.menu == 'saving'",
        box(width = 12, background = 'blue', 
            selectInput('mon_saving', "Select Month", choices = c()))
      ),
      conditionalPanel(
        condition = "input.menu == 'debt'",
        box(width = 12, background = 'blue', 
            selectInput('mon_debt', "Select Month", choices = c()))
      )
      ), 
    dashboardBody(
      tabItems(
        tabItem(
          'sum', 
          # fluidRow(tableOutput('year_total')),
          fluidRow(valueBoxOutput('overspend_cat')),
          fluidRow(box(width = 12, solidHeader = T, title = "Monthly Total", status = 'primary',
                       plotOutput('p_mon', height = '500px'))#,
                   # box(width = 6, solidHeader = T, title = "Monthly Average", status = 'primary', 
                   #   plotOutput('p_mon_avg')
                   # )
                       ), 
          fluidRow(
            box(width = 12, solidHeader = T, title = "All Transactions", status = 'primary',
                dataTableOutput('all')
                )
          )
        ),
        tabItem(
          'tot',
          valueBoxOutput('overspend_mon'),
          box(width = 12, solidHeader = T, title = "Spending by Category", status = "primary", 
              plotOutput('p_by_mon', height = '500px')), 
          box(width = 12, solidHeader = T, title = "All Transactions", status = "primary", 
              dataTableOutput('all_mon'))
        ),
        tabItem(
          'saving',
          fluidRow(column(6, valueBoxOutput('net_saving')), 
                   column(6, valueBoxOutput('net_brokerage'))),
          box(width = 12, solidHeader = T, title = "Income and Saving", status = "primary", 
              plotOutput('p_saving', height = "500px")),
          box(width = 12, solidHeader = T, title = "Transactions", status = "primary", 
              dataTableOutput('all_saving'))
        ),
        tabItem(
          'debt', 
          valueBoxOutput("net_debt"),
          box(width = 12, solidHeader = T, title = "Debt", status = "primary",
              plotOutput('p_debt', height = "500px")),
          box(width = 12, solidHeader = T, title = "Transactions", status = "primary", 
              dataTableOutput("all_debt"))
        )
      )
    )
  )
)