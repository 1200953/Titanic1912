library(shiny)
library(flexdashboard)
library(shinydashboard)
library(highcharter)
library(data.table)
library(ggthemes)
library(data.table)
library(datasets)
library(ggplot2)
library(dplyr)
library(datasets)
library(plotly)
library(corrplot)
library(caret)
library(randomForest)


shinyUI(
  dashboardPage( skin = "green",
    dashboardHeader(title = "Titanic 1912"),
    dashboardSidebar(
      sidebarMenu(
        menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
        menuItem("Survival Rate by category", tabName = "charts",icon = icon("centercode")),
        menuItem("Digging Further",tabName = "scatter",icon = icon("bity")),
        menuItem("Correlation & Importance",tabName="corrplot",icon = icon("cannabis")),
        menuItem("Raw Data", tabName = "rawdata",icon = icon("atom"))
      )
      
    ),
    dashboardBody( 
      fluidRow(
      tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "./custom.css")
      ),
      #first tab
      tabItems(
        tabItem(tabName = "dashboard",
                  box(
                    title = "Facts", solidHeader = TRUE, status = "success",
                    valueBoxOutput("vbox1"),
                    valueBoxOutput("vbox2"),
                    valueBoxOutput("vbox3"),
                    width = 10
                  ),
                column(4,
                       box(
                         title = "About", solidHeader = TRUE, status = "success",
                         tags$img(src='giphy.gif',height=150),length=150,width=350,background = "black",align="right",
                         h4("When the RMS Titanic set sail on her maiden voyage from Southampton to NYC,
                            on April 10, 1912, she was addressed as the perfect passenger liner which was
                            a marvel of sate-of-the-art techniques that capture world's eyes."))
                       ),
                box(width = 900,title = "Scatter Plot",
                    solidHeader = TRUE, status = "success",
                    plotlyOutput("pscatter2",width = 800)
                )
      ),
      #second tab
      tabItem(tabName = "charts",
                box(
                  title = "Emulator", solidHeader = TRUE, status = "success",
                  fluidRow(sidebarPanel(
                    
                        selectInput("c", label =h3("Passenger:"), list("1st Class Passenger" = "First","2nd Class Passenger" = "Second", "3rd Class Passenger" = "Third")),
                        radioButtons("s", label = h3("Sex:"),
                                     choices = list("Male" = "male", "Female" = "female"), 
                                     selected = "female"),
                        radioButtons("a", label = h3("Age:"),
                                     choices = list("Child" = "Child", "Adult" = "Adult"),
                                     selected = "Child")
                    ),
                    
                      valueBoxOutput("vbox4",width = 5),
                      h4("Generalized Linear Model"),
                      p("The survival rate is estimated based on GLM."),
                    
                    box(
                      gaugeOutput("gau"),
                      p("This emulator allows us to take insights into the suvivial rate category
                        by category via a logistic model. As we can see, there is hugh difference 
                        in age group, gender, and passenger class when it comes to survival rate.")
                    )
                    )
                  ),
                box(width = 700,
                    title = "Charts", solidHeader = TRUE, status = "success",
                    fluidRow(sidebarPanel(width = 5,
                                          selectInput(
                                            inputId = "category", "Please select the category",
                                            choices = c("Pclass", "Gender", "Age"
                                            ),
                                            selected = "Pclass"
                                          )
                    )),
                    highchartOutput("hcontainer",width = 600)
                )
              ),
        #third tab
      tabItem(tabName = "scatter",
              column(8,
                     box(width = 800,title = "Scatter Plot",
                         solidHeader = TRUE, status = "success",
                         plotlyOutput("pscatter",width = 800),
                         column(
                           8,
                           box(width = 250,
                               h4("This plot deliver a general figure of survival status of passengers; One data point
                              represents one recorded passenger. There are three passenger classes on RMS titanic
                              and the age range start from 1 to 80"),
                               h4("You can switch between passenger class to see the distribution of survived and perished
                              passenfer over different age groups")
                         )
                         )
                     ),
                     box(width = 1000,title = "Histogram",
                         solidHeader = TRUE, status = "success",
                         plotlyOutput("histo",width = 1000,height = 800)
                     )
              )
              ),
      tabItem(tabName = "corrplot",
              box(title = "Correlation Matrix",
                  solidHeader = TRUE, status = "success",
                  column(10,plotOutput("corrp"))
                  ),
              box(title = "Importance",
                  solidHeader = TRUE, status = "success",
                  column(10,plotlyOutput("pim"))
              ),
              box(title = "Explanation",
                  solidHeader = TRUE, status = "success",
                  column(10,
                         h2("Correlation Matrix"),
                         h4("The correlation matrix shows the Pearson correlation coefficient based
                            on standard deviations and covariance between different variables.
                            Generally speaking, the higher the absolute value of coeficient, 
                            the more relevant these two variables are."),
                         h2("Bar Graph"),
                         h4("The bar chart show the rank of importance of different variables, the
                            random forest modeling is implemented to rank these attributes. As we
                            can see on the chart, the rank order is sex -> fare ->pclass -> age based
                            on this data model.")
                         ))
              ),
      tabItem(tabName = "rawdata",
              box(width = 1800,title = "Raw Data Table",
                  solidHeader = TRUE, status = "success",
                  column(12,
                         dataTableOutput("rtable") 
                  )
                  )
              )
          )
        )
      )
    )
  )
