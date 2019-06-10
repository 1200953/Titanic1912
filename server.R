library(ggthemes)
library(data.table)
library(datasets)
library(shinydashboard)
library(ggplot2)
library(highcharter)
library(dplyr)
library(shiny)
library(datasets)
library(plotly)
library(corrplot)
library(caret)
library(randomForest)

#read data into R
titanic <- read.csv("titanic.csv")
lifeboats <- read.csv("Lifeboats.csv")

#data manipulation
ttable <- data.table(titanic)
ltable <- data.table(lifeboats[c(2:4)])
#set join key for both tables
setkey(ttable,boat)
setkey(ltable,boat)
#titanic left outer join lifeboat table
Result <- merge(ttable,ltable,all.x = TRUE)
#select necessary variables
relevant_data <- subset(Result, select = c(survived, pclass, sex, age, fare, boat, launch, side)) 
im_data <- subset(Result, select = c(survived, pclass, sex, age, fare))
r_data <- relevant_data[,1:5]
#change the domain to more human readable name
relevant_data$pclass[relevant_data$pclass=="1"] <- 'First'
relevant_data$pclass[relevant_data$pclass=="2"] <- 'Second'
relevant_data$pclass[relevant_data$pclass=="3"] <- 'Third'

re_data <- relevant_data[,1:5]
re_data$age <- as.numeric(re_data$age)
#Remove rows that contain NA
re_data<-re_data[complete.cases(re_data), ]
re_data$age[re_data$age>=18.0] <- 'Adult'
re_data$age[re_data$age!="Adult"] <- 'Child'
re_data$sex[re_data$sex=='1'] <- 'Female'
re_data$sex[re_data$sex=='2'] <- 'Male'
re_data <- re_data %>%
  group_by(age,pclass,sex,survived) %>%
  summarize(freq = n())
tit <- as.data.frame(re_data)
#implement generlized linear model
tit_glm <- glm(survived ~ pclass + sex + age, binomial, tit, tit$freq)
#predict the survival rate using GLM
pred_tit <- function(pclass, sex, age ){
  inputdata <- c(pclass, sex, age)
  pred_data <- as.data.frame(t(inputdata))
  colnames(pred_data) <- c("pclass", "sex", "age")
  surv_prob <- round(predict(tit_glm,pred_data , type = "response" ),4)*100
  return(surv_prob)
}

shinyServer(function(input,output){
  #render output for value box
  output$vbox1 <- renderValueBox(valueBox("31.6 %", "Total Survival Rate", icon = icon("heartbeat"), color = "red"))
  output$vbox2 <- renderValueBox(valueBox("-2 C", "Water Temperature", icon = icon("thermometer-empty"), color = "blue"))
  output$vbox3 <- renderValueBox(valueBox("2222", "People on board", icon = icon("ship"), color = "maroon"))
  output$vbox4 <- renderValueBox(valueBox({pred_tit(input$c,input$s, input$a)}, "Survival Rate(%)", icon = icon("heartbeat"), color = "red"))
  #render output for gauge
  output$gau <- renderGauge({gauge(as.integer({pred_tit(input$c,input$s, input$a)}), min = 0, max = 100, symbol = '%', gaugeSectors(
    success = c(70, 100), warning = c(30, 69), danger = c(0, 29)
  ))})
  
  #render output for data table
  output$rtable <- renderDataTable({data = Result
                      return(data)
                    },
                    options = 
                      list(pageLength = 10,width = '100px')
                    )
  #render output for bar graph of importance ranks
  output$pim <- renderPlotly({
    re_data1<-im_data[complete.cases(im_data), ]
    rf_model<-randomForest(survived~age+sex+pclass+fare,data = re_data1)
    #prediction
    rf_pred<-predict(rf_model,re_data1)
    # Get importance
    importance    <- importance(rf_model)
    varImportance <- data.frame(Variables = row.names(importance), 
                                Importance = round(importance[,],2))
    # Create a rank variable based on importance
    rankImportance <- varImportance %>%
      mutate(Rank = paste0('#',dense_rank(desc(Importance))))
    #Use ggplot2 to visualize the relative importance of variables
    # Use ggplot2 to visualize the relative importance of variables
    pimpotant <- ggplot(rankImportance, aes(x = reorder(Variables, Importance), 
                                            y = Importance, fill = Importance)) +
      geom_bar(stat='identity') + 
      geom_text(aes(x = Variables, y = 0.5, label = Rank),
                hjust=0, vjust=0.55, size = 4, colour = 'green') +
      labs(x = 'Variables') +
      coord_flip() + 
      theme_few()
    pp <- ggplotly(pimpotant)
    pp
  }) 
  #render output for scatter plot
  output$pscatter <- renderPlotly({
    relevant_data1<-relevant_data
    relevant_data1$survived[relevant_data1$survived==1] <- 'survived'
    relevant_data1$survived[relevant_data1$survived==0] <- 'perished'
    agevsSur <- ggplot(relevant_data1, aes(x = age, y = sex, frame = pclass  )) + 
                                   geom_jitter(aes(colour = factor(survived))) + 
                                   theme_gray() +
                                   scale_colour_solarized()+
                                   scale_fill_discrete(name = "survived") + 
                                   scale_x_continuous(name="age",limits=c(1, 80)) +
                                   theme(legend.title = element_blank())+
                                   labs(x = "Age", y = "Gender", title = "Gender vs Age by different passenger class")
                                  
                                 #print the plot
                                  agevsSur<-ggplotly(agevsSur) %>% animation_opts(transition = 500,easing = "elastic", redraw = FALSE) %>% animation_slider(
                                    currentvalue = list(prefix = "Passenger Class: ", font = list(color="green"))
                                  )
                                  agevsSur
                                 })
  #render the output for scatter plot
  output$pscatter2 <- renderPlotly({
    relevant_data1<-relevant_data
    relevant_data1$survived[relevant_data1$survived==1] <- 'survived'
    relevant_data1$survived[relevant_data1$survived==0] <- 'perished'
    agevsSur <- ggplot(relevant_data1, aes(x = age, y = sex, frame = pclass  )) + 
      geom_jitter(aes(colour = factor(survived))) + 
      theme_gray() +
      scale_colour_solarized()+
      scale_fill_discrete(name = "survived") + 
      scale_x_continuous(name="age",limits=c(1, 80)) +
      theme(legend.title = element_blank())+
      labs(x = "Age", y = "Gender", title = "Gender vs Age by different passenger class")
    
    #print the plot
    agevsSur<-ggplotly(agevsSur) %>% animation_opts(transition = 500,easing = "elastic", redraw = FALSE) %>% animation_slider(
      currentvalue = list(prefix = "Passenger Class: ", font = list(color="green"))
    )
    agevsSur
  })
  #render the output for histogram
  output$histo <- renderPlotly({
    relevant_data$survived[relevant_data$survived==1] <- 'survived'
    relevant_data$survived[relevant_data$survived==0] <- 'perished'
    histo<-ggplot(relevant_data, aes(age, fill = factor(survived))) + 
      geom_histogram(bins=30) + 
      theme_economist() +
      scale_fill_brewer(palette = 'Dark2') +  # Color of fill.
      scale_color_brewer(palette = 'Dark2') +
      xlab("Age") +
      facet_wrap(vars(sex,pclass)) +
      scale_fill_discrete(name = "survived") + 
      ggtitle("Number of survivors over Age")
      hist <- ggplotly(histo)
      hist
  })
  #render the graph for the combined chart
  output$hcontainer <- renderHighchart({
    #charts for passenger class
    if(input$category == "Pclass")
    {
      #distribution for passengers by class
      tmp <- relevant_data %>% group_by(pclass) %>% tally() %>% mutate(rate = round((n /sum(n))*100,digits = 2))
      #survival rate for passengers by class
      pclass <- relevant_data %>% filter(!(pclass=="")) %>%  group_by(pclass) %>% summarize(survived = round(mean(survived)*100,digits = 2))
      
      chart <- highchart() %>% 
        hc_add_series_labels_values(pclass$pclass, pclass$survived,type = "column", name = "Survival rate") %>%
        hc_add_series_labels_values(tmp$pclass, tmp$rate, name = "Percentage",type = "pie") %>%
        hc_plotOptions(
          #specify the options for bar chart
          column = list(
            showInLegend = FALSE,
            colorByPoint = TRUE
          ),
          #specify the options for pie chart
          pie = list(
            showInLegend = TRUE,
            colorByPoint = TRUE, center = c('80%', '30%'),
            size = 120, dataLabels = list(enabled = TRUE)
          )) %>%
        hc_title(
          text = "Bar Chart: Survival rate of passengers by class"
        ) %>%
        hc_subtitle(text = "Pie Chart: Distribution of passengers on board by class") %>%
        hc_xAxis(categories = pclass$pclass) %>% 
        hc_xAxis(title = list(text="Passenger Cabin Class")) %>%
        hc_yAxis(title = list(text="Survival Rate"), labels = list(format = "{value}%"), max = 100) %>%
        hc_chart(style = list(fontFamily = "Futura",
                              fontWeight = "bold"))%>%
        hc_legend(align = "right", 
                  verticalAlign = "top") %>% 
        hc_add_theme(hc_theme_elementary())
    }
    #charts for gender
    if(input$category == "Gender") 
    {
      #table for gender
      gender <- relevant_data %>%  group_by(sex) %>% summarize(survived = round(mean(survived)*100,digits = 2))
      tmp <- relevant_data %>% group_by(sex) %>% tally() %>% mutate(rate = round((n /sum(n))*100,digits = 2))
      chart <- highchart() %>%
        hc_add_series_labels_values(gender$sex, gender$survived,type = "column", name = "Survival rate") %>%
        hc_add_series_labels_values(tmp$sex, tmp$rate, name = "Percentage",type = "pie") %>%
        hc_plotOptions(
          column = list(
            showInLegend = FALSE,
            colorByPoint = TRUE
          ),
          pie = list(
            showInLegend = TRUE,
            colorByPoint = TRUE, center = c('80%', '30%'),
            size = 120, dataLabels = list(enabled = TRUE)
          )) %>%
        hc_title(
          text = "Bar Chart: Survival rate of passengers by gender"
        ) %>%
        hc_subtitle(text = "Pie Chart: Distribution of passengers on board by gender") %>%
        hc_xAxis(categories = gender$sex) %>%
        hc_xAxis(title = list(text="Gender")) %>%
        hc_yAxis(title = list(text="Survival Rate"), labels = list(format = "{value}%"), max = 100) %>%
        hc_chart(style = list(fontFamily = "Futura",
                              fontWeight = "bold"))%>%
        hc_legend(align = "right", 
                  verticalAlign = "top")%>% 
        hc_add_theme(hc_theme_ft())
    }
    #charts for differnt age group
    if(input$category == "Age") 
    {
      #table for age
      age <- relevant_data %>% filter(age >= 1) %>% group_by(age) %>% summarize(survived = round(mean(survived)*100,digits = 2))
      Age <- cut(age$age, c(seq(0, 80, by = 10), Inf), include.lowest = TRUE)
      age <- aggregate(survived ~ Age, age, mean)
      
      tmp <- relevant_data %>% filter(age >= 1) %>% group_by(age) %>% tally() %>% mutate(rate = round((n*100 /sum(n)),digits = 2))
      age_cut <- cut(tmp$age, c(seq(0, 80, by = 10), Inf), include.lowest = TRUE)
      tmp <- aggregate(rate ~ age_cut, tmp, sum)
      
      chart <- highchart() %>%
        hc_add_series_labels_values(age$Age, age$survived,type = "column", name = "Survival rate") %>%
        hc_add_series_labels_values(tmp$age_cut, tmp$rate, name = "Percentage",type = "pie") %>%
        hc_plotOptions(
          column = list(
            showInLegend = FALSE,
            colorByPoint = TRUE
          ),
          pie = list(
            showInLegend = TRUE,
            colorByPoint = TRUE, center = c('30%', '30%'),
            size = 100, dataLabels = list(enabled = TRUE)
          )) %>%
        hc_title(
          text = "Bar Chart: Survival rate of passengers by age group"
        ) %>%
        hc_subtitle(text = "Pie Chart: Distribution of passengers on board by age") %>%
        hc_xAxis(categories = age$Age) %>%
        hc_xAxis(title = list(text="Age")) %>%
        hc_yAxis(title = list(text="Survival Rate"), labels = list(format = "{value}%"), max = 100) %>%
        hc_chart(style = list(fontFamily = "Futura",
                              fontWeight = "bold"))%>%
        hc_legend(align = "right", 
                  verticalAlign = "top")%>% 
        hc_add_theme(hc_theme_flat())
    }
    #print the plot
    chart })
  #render ouput for the correlation matrix
  output$corrp <- renderPlot({
    r_data$sex <- as.numeric(r_data$sex)
    r_data$survived <- as.numeric(r_data$survived)
    r_data$fare <- as.numeric(r_data$fare)
    r_data$pclass <- as.numeric(r_data$pclass)
    r_data$age <- as.numeric(r_data$age)
    
    corr_data<-cor(r_data, use = 'complete.obs')
    cor_p<-corrplot.mixed(corr = corr_data, tl.col = "black", upper = "ellipse")
  })
})