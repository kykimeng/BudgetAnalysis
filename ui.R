library(data.table)
library(ggplot2)
library(shiny)
library(shinydashboard)

shinyUI(
  dashboardPage(skin = 'black',
    dashboardHeader(title = "Budget Analysis"), 
    dashboardSidebar(
      sidebarMenu(id = 'menu',
        menuItem(text = "Stats by Category", tabName = "sum"),
        menuItem(text = "Stats by Month", tabName = 'tot')
      ), 
      tags$hr(),
      conditionalPanel(
        condition = "input.menu == 'sum'",
        box(width = 12, background = 'blue', 
            selectInput('cat', "Select Category", multiple = T, 
                        choices = c())
        )
      ),
      conditionalPanel(
        condition = "input.menu == 'tot'",
        box(width = 12, background = 'blue', 
            selectInput('mon', "Select Month", choices = c()))
      )
      ), 
    dashboardBody(
      tabItems(
        tabItem(
          'sum', 
          fluidRow(tableOutput('year_total')),
          fluidRow(box(width = 6, solidHeader = T, title = "Monthly Total", status = 'primary',
                       plotOutput('p_mon')),
                   box(width = 6, solidHeader = T, title = "Monthly Average", status = 'primary', 
                     plotOutput('p_mon_avg')
                   )
                       ), 
          fluidRow(
            box(width = 12, solidHeader = T, title = "All Transactions", status = 'primary',
                dataTableOutput('all')
                )
          )
        ),
        tabItem(
          'tot',
          box(width = 12, solidHeader = T, title = "Spending by Category", status = "primary", 
              plotOutput('p_by_mon', height = '600px')), 
          box(width = 12, solidHeader = T, title = "All Transactions", status = "primary", 
              dataTableOutput('all_mon'))
        )
      )
    )
  )
)