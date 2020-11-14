library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)

shinyUI(# Define page
    dashboardPage(
        # Define page header
        dashboardHeader(title = "coviz-mn: data on covid-19 in MN"),
        # Define sidebar
        dashboardSidebar(
            # Sidebar Menu
            sidebarMenu(
                menuItem("Daily Report", tabName = "dailyReport"),
                menuItem("About This Tracker", tabName = "about")
            )
        ),
        # Define body
        dashboardBody(
            tabItems(
                #############################################
                ################ State Level ################
                #############################################
                tabItem(
                    "dailyReport",
                    # Top summary row
                    fluidRow(
                        valueBoxOutput("newCases", width = 3),
                        valueBoxOutput("totalCases", width = 3),
                        valueBoxOutput("newDeaths", width = 3),
                        valueBoxOutput("totalDeaths", width = 3)
                    ),
                    # Plots of cases & deaths
                    fluidRow(
                        box(
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("casesDaily")
                        ),
                        box(
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("deathsDaily")
                        )
                    ),
                    # Plots of cases & deaths
                    fluidRow(
                        box(
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("newCasesDaily")
                        ),
                        box(
                            solidHeader = T,
                            width = 6,
                            collapsible = T,
                            plotlyOutput("newDeathsDaily")
                        )
                    )
                ),
                # About Page
                tabItem("about",
                        h1("About the COVID-19 Tracker"),
                        h2("Disclaimer"),
                        p("This tool should not be used to make any kind of medical decision(s). This is simply a visualization of COVID-19 data from the sources indicated below. Feel free to use this as a tracker or way to stay informed on spread, but DO NOT use this regarding any health or medical concerns."),
                        h2("Data Sources"),
                        p("Data utilized in this tracker comes from the New York Times which can be found here : https://raw.githubusercontent.com/nytimes/covid-19-data/master"),
                        p("State population estimates are as of 2019. These populations may differ from current actual populations but are used as the closest estimates.")
                        
                )
            ))
    ))