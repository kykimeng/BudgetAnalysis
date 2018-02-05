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
        menuItem(text = "Stats by Category", tabName = "sum"),
        menuItem(text = "Stats by Month", tabName = 'tot')
      ), 
      tags$hr(),
      conditionalPanel(
        condition = "input.menu == 'sum'",
        box(width = 12, background = 'blue', 
            selectInput('cat', "Select Category", multiple = T, 
                        choices = c()), 
            radioButtons('mode', NULL, choices = c("Total", "Average"))
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
        )
      )
    )
  )
)