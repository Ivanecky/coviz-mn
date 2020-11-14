# Load libraries
library(shiny)
library(shinydashboard)
library(ggplot2)
library(tidymodels)
library(dplyr)
library(plotly)
library(RCurl)
library(rvest)
library(XML)

suppressWarnings(source("mndh_agg_data.R"))

shinyServer(function(input, output) {
    ###########################################################################################
    ############################### Daily Report ##############################################
    ###########################################################################################
    ################
    ### Value Boxes
    ################
    ### SUMMARY ROW
    # New cases
    output$newCases <- renderValueBox({
        valueBox(getNewCases(), "New Cases")
    })
    
    # Total cases
    output$totalCases <- renderValueBox({
        valueBox(getTotalCases(), "Total Cases")
    })
    
    # New deaths
    output$newDeaths <- renderValueBox({
        valueBox(getNewDeaths(), "New Deaths")
    })
    
    # Total deaths
    output$totalDeaths <- renderValueBox({
        valueBox(getTotalDeaths(), "Total Deaths")
    })
    
    ### PLOT ROW (1)
    # Total cases
    output$casesDaily <- renderPlotly({
        # Create the plot
        p <- ggplot(conf_cases_test_type, aes(rpt_d, cases_total)) +
            geom_bar(stat = 'identity', color = "blue", fill = "lightblue") +
            labs(x = "Report Date", y = "Total Cases") + 
            ggtitle("Total Cases by Date")
        # Render the plot
        ggplotly(p)
    })
    
    # Total deaths
    output$deathsDaily <- renderPlotly({
        # Create the plot
        p <- ggplot(deaths_daily, aes(rpt_d, deaths_total)) +
            geom_bar(stat = 'identity', color = "red", fill = "red") +
            labs(x = "Report Date", y = "Total Deaths") + 
            ggtitle("Total Deaths by Date")
        # Render the plot
        ggplotly(p)
    })
    
    ### PLOT ROW (2)
    # Total cases
    output$newCasesDaily <- renderPlotly({
        # Create the plot
        p <- ggplot(conf_cases_test_type, aes(rpt_d, total_new_cases)) +
            geom_bar(stat = 'identity', color = "blue", fill = "lightblue") +
            labs(x = "Specimen Collection Date", y = "New Cases") + 
            ggtitle("New Cases by Date", subtitle = "Includes PCR & antigen (probable) positive tests")
        # Render the plot
        ggplotly(p)
    })
    
    # Total deaths
    output$newDeathsDaily <- renderPlotly({
        # Create the plot
        p <- ggplot(deaths_daily, aes(rpt_d, deaths_new)) +
            geom_bar(stat = 'identity', color = "red", fill = "red") +
            labs(x = "Report Date", y = "New Deaths") + 
            ggtitle("New Deaths by Date")
        # Render the plot
        ggplotly(p)
    })
    
})