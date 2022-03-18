#  --------------------------------------------------------------------------------------------------------
#                                             LIBRARIES
#  --------------------------------------------------------------------------------------------------------
library(DT)
library(leaflet)
library(shinycssloaders)
library(PerformanceAnalytics)
library(dplyr)
library(shiny)
library(plotly)
library(shinydashboard)
library(shinythemes)
library(dashboardthemes)
library(lattice)
library(caTools)
library(knitr)
library(ggplot2)
library(cowplot)
library(reshape2)
library(funModeling) 
library(tidyverse) 
library(Hmisc)
library(dlookr)
library(lattice)
library(corrplot)
library(ggcorrplot)
library(naniar)
library(skimr)
library(dplyr)
library(datasets)
library(ggpubr)
library(readr)
library(gridExtra)
library(RColorBrewer)
library(caret)
library(viridis)
library(data.table)
library(googleVis)
library(grid)

#  --------------------------------------------------------------------------------------------------------
#                                              READING THE FILES
#  --------------------------------------------------------------------------------------------------------
title <- tags$img(src='logo.png',height='50', width='46',"SUGAR RUSH")
Datafinal <- read_csv("diabetes.csv")
View(Datafinal)

# Define UI for application that draws a histogram
# ---------------------------------------------------------------------------------------------------------
#                                                USER INTERFACE
# ---------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = title),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon('home')),
      menuItem(("Trends"),tabName = "trends",icon=icon('map')),
      menuItem(("Bivariate Associations"),tabName = "bivariate",icon=icon('map')),
      menuItem(("Logistic Regression"),tabName = "regression",icon=icon('map')),
      menuItem("Raw data",tabName = "table",icon = icon('table'))
    )
  ),
  
  dashboardBody(
    #custom css
    tags$head(
      tags$style(
        HTML(" #compare_state_option,#compare_year_option ,.compare-county-wrapper { display:flex; margin-bottom:-10px;}
    				#compare_state_option > div, #compare_year_option > div, .compare-county-wrapper > div > div {padding-right: 15px;}
    				.shiny-output-error:before,.shiny-output-error{visibility: hidden !important;}
    				.compare-class > .tabbable > .nav-tabs {margin-bottom: 20px !important;}
    				.box.box-solid.box-primary {border: 1px solid #dd4b39 !important;}
    				.box.box-solid.box-primary>.box-header { background: #dd4b39 !important; background-color: #dd4b39 !important; }
    				.sidebar-menu>li {font-size:17px;}")
      )
    ),
    #--------------------------------------------RAW DATA TAB---------------------------------------------------------
    tabItems(
      tabItem(tabName = "table",
              tags$h3('Download Data'),
              downloadButton("downloadData"),
              br(),
              br(),
              tableOutput("tableData")),
      
      # ---------------------------------------------HOME TAB-------------------------------------------------------------
      
      tabItem(tabName = "home",
              tags$img(src="home_img.png",height=300, width='100%'),
              br(),
              fluidRow(
                column(width=7,tags$h2(width=5,"Video Of the Day")),
                column(width=5,tags$h2(width=5,"Top News"))),
              
              fluidRow(
                box(width=6,HTML('<iframe width="560" height="315" src="https://www.youtube.com/embed/HvVBuunaeE0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
                box(width=5,height = 460,
                    HTML('<html>
                            <head>
                            <style>
                            table {
                              font-family: arial, sans-serif;
                              border-collapse: collapse;
                              width: 100%;
                            }
                            
                            td, th {
                              border: 1px solid #dddddd;
                              text-align: left;
                              padding: 8px;
                            }
                            
                            tr:nth-child(even) {
                              background-color: #dddddd;
                            }
                            p{
                              font-size: 19px;
                            }
                            h3,h2{
                              font-weight:bold;
                            }
                            
                            #imagesize  
                            {  
                            width:135px;  
                            height:127px;  
                            } 
                            </style>
                            </head>
                            <body>
                                <table border="0" width="100%">
                                    <col style="width:30%">
	                                  <col style="width:70%">
	                                  
	                                  <tr>
	                                      <td><img src="news1.png" id="imagesize"></td>
	                                      <td>Women with diabetes have more to manage. Stay on track by checking your blood sugar often, eating healthy food, and being active so you can be your healthiest and feel your best. 
                                  <a href="https://www.cdc.gov/diabetes/library/features/diabetes-and-women.html">Read More</a></td>
                                    </tr>
                                    
                                    <tr>
                                        <td><img src="news2.png" id="imagesize"></td>
                                        <td>Diabetes is a group of metabolic diseases in which a person has high blood sugar due to problems processing or producing insulin. Diabetes can affect people of any age, race, or sex. It can affect people with any lifestyle.
                                <a href="https://www.healthline.com/health/diabetes/symptoms-in-women">Read more<a/></td>
                                    </tr>
                                    
                                    <tr>
                                        <td><img src="news3.png" id="imagesize"></td>
                                        <td>Diabetes can cause serious health problems, including heart attack or stroke, blindness, problems during pregnancy, and kidney failure. About 15 million women in the United States have diabetes, or about 1 in every 9 adult women
                                <a href="https://www.womenshealth.gov/a-z-topics/diabetes">Read more</a></td>
                                    </tr>
                                </table>
                            </body>
                            </html>'))),
              fluidRow(
                column(width=7,tags$h2(width=5,"FAQ"))
              ),
              fluidRow(
                column(width=10,
                       tags$h3("Q. What is BMI?"),
                       tags$p(width=7,"BMI or Body Mass Index is the proportion of a person’s height to their weight 
                                       i.e., the measure of body fat based on the person’s weight in relation to height. 
                                       The standard value is 18.5-25 for normal BMI. BMI less than 19 signifies a very skinny 
                                       and unhealthy person. A BMI of more that 25-30 signifies a person is overweight. 
                                       BMI above the range of 30-40 is considered to be very dangerous. 
                                       A person can be classified as obese if the BMI is more than 30."),
                       tags$h3("Q. How is BMI calculated?"),
                       tags$p(width=7,"Body Mass Index is a simple calculation using a person's height and weight. 
                                       The formula is BMI = kg/m2 where kg is a person's weight in kilograms and m2 
                                       is their height in meters squared."),
                       tags$h3("Q. What is Diabetes pedigree function?"),
                       tags$p(width=7,"A pedigree is a genetic representation of a family tree that diagrams the inheritance 
                                       of a trait or disease though several generations. The pedigree shows the relationships 
                                       between family members and indicates which individuals express or silently carry the trait
                                       in question.Diabetes pedigree function is a function which scores likelihood of diabetes 
                                       based on family history. "))
                #tags$h3("Q. How do you calculate diabetes pedigree??"),
                #tags$p(width=7,"Diabetes pedigree function (DPF or x7) ")),
                
                #column(width=5,tags$img(src="AQImeter.png", height=230, width="80%", hspace="90"))
              ),
              br(),
              br(),
              fluidRow(
                column(width=7,tags$h2(width=5,"About Us"))
              ),
              fluidRow(
                column(width=5,tags$img(src="logo.png", height=400, width="85%", hspace="90")),
                column(width=7,tags$div(HTML("<p>Women have always played an anchoring role when it comes to their families in India. 
                                                                  More often than less, this has led to them prioritising their families’ health over their own.
                                                                  Traditionally, diabetes has not been spoken in the context of women. But secondary studies have 
                                                                  shown that there has been a rise in incidence of diabetes among women.<br>
                                                                  <strong>LACK OF MANAGEMENT:</strong><br>
 
                                                                  <strong>60%</strong> of Indian women are aware of diabetes linked to pregnancy(gestational diabetes),<br>
                                                                  <strong>44%</strong> of women are cognizant of prediabetes,<br>
                                                                  <strong>70%</strong> of women acknowledge that regular meals and regular exercise are important for a healthy body,<br>
                                                                  <strong>60%</strong> of them confessed to skipping meals often.</p>")))
              )
      ),
      # -------------------------------------------------BIVARIATE ASSOCIATIONS TAB------------------------------------------------------------------ 
      tabItem(tabName = "bivariate",
              fluidRow(
                column(12,
                       box(title = "Bivariate Associations",solidHeader = TRUE, status = "primary",width = 1000,
                           tabsetPanel(
                             tabPanel(HTML("<span style = 'font-size:17px'>Pregnancies with Age</span>"), plotOutput(height = 650,width= 1100, "PregnanciesWithAge")),
                             tabPanel(HTML("<span style = 'font-size:17px'>BMI with SkinThickness</span>"),plotOutput(height = 650,width= 1100,"BMIWithSkinThickness")),
                             tabPanel(HTML("<span style = 'font-size:17px'>Insulin with Glucose</span>"),plotOutput(height = 650,"InsulinWithGlucose")),
                             tabPanel(HTML("<span style = 'font-size:17px'>BMI with BP</span>"),plotOutput(height = 650,width= 1100,"BMIWithBP")),
                             tabPanel(HTML("<span style = 'font-size:17px'>BP with Age</span>"), plotOutput(height = 650,width= 1100, "BPWithAge")),
                             tabPanel(HTML("<span style = 'font-size:17px'>DPF with SkinThickness</span>"),plotOutput(height = 650,width= 1100,"DPFWithSkinThickness")),
                             tabPanel(HTML("<span style = 'font-size:17px'>Pregnancies with Glucose</span>"),plotOutput(height = 650,width=1100,"PregnanciesWithGlucose")),
                             tabPanel(HTML("<span style = 'font-size:17px'>DPF with Insulin</span>"),plotOutput(height = 650,width= 1100,"DPFWithInsulin")),
                           ))))
      ),
      # -------------------------------------------------LOGISTIC REGRESSION TAB------------------------------------------------------------------ 
      tabItem(tabName = "regression",
              fluidRow(
                column(12,
                box(title = "Logistic Regression",solidHeader = TRUE, status = "primary",width = 1000,
                    tabsetPanel(
                           tabPanel(HTML("<span style = 'font-size:20px'>Pregnancies</span>"), plotOutput(height = 690,width= 1100, "Pregnancies")),
                           tabPanel(HTML("<span style = 'font-size:20px'>Glucose</span>"),plotOutput(height = 690,width= 1100,"Glucose")),
                           tabPanel(HTML("<span style = 'font-size:20px'>BP</span>"),plotOutput(height = 690,"BP")),
                           tabPanel(HTML("<span style = 'font-size:20px'>Skin Thickness</span>"),plotOutput(height = 690,width= 1100,"SkinThickness")),
                           tabPanel(HTML("<span style = 'font-size:20px'>Insulin</span>"),plotOutput(height = 690, width= 1100,"Insulin")),
                           tabPanel(HTML("<span style = 'font-size:20px'>BMI</span>"),plotOutput(height = 690,width= 1100, "BMI")),
                           tabPanel(HTML("<span style = 'font-size:20px'>Diabetes Pedigree Function</span>"),plotOutput(height = 690,width= 1100, "DPF")),
                           tabPanel(HTML("<span style = 'font-size:20px'>Age</span>"),plotOutput(height = 690, width= 1100,"Age")),
                    ))))
      ),
      
# ---------------------------------------------TRENDS TAB-------------------------------------------------------------

tabItem(tabName = "trends",
        fluidRow(
          column(12,
                 box(title = "Correlation Matrix", solidHeader = TRUE, status = "primary", width = 1000,
                     tabsetPanel(
                       tabPanel(HTML("<span style = 'font-size:20px'>Correlation Coefficients</span>"), withSpinner(plotOutput("corrcoeff",height = 700, width= 1100))),
                       tabPanel(HTML("<span style = 'font-size:20px'>Correlation Scatterplot</span>"), withSpinner(plotOutput("corrscatt",height = 700, width = 1100))),
                       tabPanel(HTML("<span style = 'font-size:20px'>Heat Map</span>"),withSpinner(plotOutput("heatmap",height = 700, width = 1100)))
                       
                     )
                 )	
                 
        ))
)
)
)
)
      
# Define server logic required to draw a histogram
# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  # --------------------------------------------------------CORRELATION MATRIX ----------------------------------------------------------    
  
  output$corrcoeff <- renderPlot({
    mydata2 <- Datafinal #%>%filter(Year==input$years, City==input$Cities)
    #mydata<-mydata2[,c(3:11)]
    mydata2.rcorr = rcorr(as.matrix(mydata2))
    mydata2.coeff = mydata2.rcorr$r
    corrplot(mydata2.coeff,method="number")
  })
  
  # ------------------------------------------------------SCATTERPLOT CORRELATION-------------------------------------------------------    
  
  output$corrscatt <- renderPlot({
    mydata2 <- Datafinal #%>%filter(Year==input$years, City==input$Cities)
    #mydata<-mydata2[,c(3:11)]
    chart.Correlation(mydata2, histogram=TRUE, pch=19)
    
  })
  # -----------------------------------------------------------HEAT MAP-----------------------------------------------------------------    
  
  output$heatmap <- renderPlot({
    mydata2 <- Datafinal # %>%filter(Outcome==input$Outcome , Pregnancies==input$Pregnancies, Glucose == input$Glucose, BloodPressure == input$BloodPressure,
                                   #SkinThickness == input$SkinThickness, Insulin == input$Insulin, BMI== input$BMI, DiabetesPedigreeFunction == input$DiabetesPedigreeFunction, Age == input$Age)
    #mydata<-mydata2[,c(3:11)]
    mydata2.rcorr = rcorr(as.matrix(mydata2))
    mydata2.coeff = mydata2.rcorr$r
    palette = colorRampPalette(c("pink", "white", "red")) (20)
    heatmap(x = mydata2.coeff, col = palette, symm = TRUE)
  })
  #------------------------------------------------------------BIVARIATE ASSOCIATIONS TAB------------------------------------------------------------
  output$PregnanciesWithAge <-renderPlot({
    p1 <- ggplot(Datafinal, aes(x = Age, y = Pregnancies)) +
      geom_point(aes(color=Outcome)) + 
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of Pregnancies with Age Vs Diabetes")
    
    gridExtra::grid.arrange(p1, ncol = 1)
  })
  
  output$BMIWithSkinThickness<-renderPlot({
    p4 <- ggplot(Datafinal,aes(x=BMI,y=SkinThickness))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of BMI with Skin Thickness Vs Diabetes")
    
    gridExtra::grid.arrange(p4, ncol = 1)
 
  })
  
  output$BMIWithBP<-renderPlot({
    p3 <- ggplot(Datafinal,aes(x=BMI,y=BloodPressure))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of BMI with BP Vs Diabetes")
    gridExtra::grid.arrange(p3, ncol = 1)
  })
  
  output$InsulinWithGlucose<-renderPlot({
    p2 <- ggplot(Datafinal,aes(x=Insulin,y=Glucose))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of Insulin with Glucose Vs Diabetes")
    
    gridExtra::grid.arrange( p2, ncol = 1)
  })
  
  output$BPWithAge<-renderPlot({
    p6 <- ggplot(Datafinal,aes(x=BloodPressure,y=Age))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of BP with Age Vs Diabetes")
    
    gridExtra::grid.arrange(p6, ncol = 1)
  })
  
  output$DPFWithSkinThickness<-renderPlot({
    p8 <- ggplot(Datafinal,aes(x=DiabetesPedigreeFunction,y=SkinThickness))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of DPF with SkinThickness Vs Diabetes")
    
    gridExtra::grid.arrange(p8, ncol = 1)
    
  })
  
  output$PregnanciesWithGlucose<-renderPlot({
    p7 <- ggplot(Datafinal,aes(x=Pregnancies,y=Glucose))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of Pregnancies with Glucose Vs Diabetes")
    
    gridExtra::grid.arrange(p7, ncol = 1)
  })
  
  output$DPFWithInsulin<-renderPlot({
    p5 <- ggplot(Datafinal,aes(x=DiabetesPedigreeFunction,y=Insulin))+
      geom_point(aes(color=Outcome))+
      theme(legend.position = "right",text = element_text(size = 15)) #+
      #ggtitle("Relationship of DPF with Insulin Vs Diabetes")
    
    gridExtra::grid.arrange(p5, ncol = 1)
    
  })
  #------------------------------------------------------------LOGISTIC REGRESSION TAB------------------------------------------------------------
  
    output$Pregnancies <-renderPlot({
      Outcome<-Datafinal$Outcome
      Pregnancies<-Datafinal$Pregnancies
      Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
      Datafinal$Outcome<-as.factor(Outcome)
      str(Datafinal)
      logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$Pregnancies, data=Datafinal, binomial(link =logit)) 
      summary(logistic)
      ll.null<-logistic$null.deviance/-2
      ll.proposed<-logistic$deviance/-2
      (ll.null-ll.proposed)/ll.null
      1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
      predicted.data<-data.frame(Datafinal=logistic$fitted.values,Pregnancies=Datafinal$Pregnancies)
      predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
      predicted.data$rank<-1:nrow(predicted.data)
      View(predicted.data)
      ggplot(data=predicted.data,aes(Pregnancies,Datafinal))+  
        geom_point(alpha=12,shape=4,stroke=2)+ #aes(color = "hd") inside geom_point if label needed
        theme(legend.position = "right",text = element_text(size = 15))+
        xlab("Pregnancies")+
        ylab("Outcome")
    })

  output$Glucose<-renderPlot({
    Outcome<-Datafinal$Outcome
    Glucose<-Datafinal$Glucose
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$Glucose, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,Glucose=Datafinal$Glucose)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(Glucose,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("Glucose")+
      ylab("Outcome")

  })

  output$BP<-renderPlot({
    Outcome<-Datafinal$Outcome
    BP<-Datafinal$BloodPressure
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$BloodPressure, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,BloodPressure=Datafinal$BloodPressure)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(BloodPressure,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("Blood Pressure")+
      ylab("Outcome")
  })

  output$SkinThickness<-renderPlot({
    Outcome<-Datafinal$Outcome
    SkinThickness<-Datafinal$SkinThickness
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$SkinThickness, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,SkinThickness=Datafinal$SkinThickness)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(SkinThickness,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("Skin Thickness")+
      ylab("Outcome")
  })

  output$Insulin<-renderPlot({
    Outcome<-Datafinal$Outcome
    Insulin<-Datafinal$Insulin
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$Insulin, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,Insulin=Datafinal$Insulin)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(Insulin,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("Insulin")+
      ylab("Outcome")
  })

  output$BMI<-renderPlot({
    Outcome<-Datafinal$Outcome
    BMI<-Datafinal$BMI
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$BMI, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,BMI=Datafinal$BMI)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(BMI,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("BMI")+
      ylab("Outcome")
  })

  output$DPF<-renderPlot({
    Outcome<-Datafinal$Outcome
    DiabetesPedigreeFunction<-Datafinal$DiabetesPedigreeFunction
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$DiabetesPedigreeFunction, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,DiabetesPedigreeFunction=Datafinal$DiabetesPedigreeFunction)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(DiabetesPedigreeFunction,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("Diabetes Pedigree Function")+
      ylab("Outcome")
  })

  output$Age<-renderPlot({
    Outcome<-Datafinal$Outcome
    Age<-Datafinal$Age
    Datafinal$Outcome<-ifelse(test=Datafinal$Outcome == 1, yes="Diabetic", no ="Non-Diabetic")
    Datafinal$Outcome<-as.factor(Outcome)
    str(Datafinal)
    logistic<-glm(as.factor(Datafinal$Outcome) ~ Datafinal$Age, data=Datafinal, binomial(link =logit)) 
    summary(logistic)
    ll.null<-logistic$null.deviance/-2
    ll.proposed<-logistic$deviance/-2
    (ll.null-ll.proposed)/ll.null
    1-pchisq(2*(ll.proposed-ll.null),df=(length(logistic$coefficients)-1))
    predicted.data<-data.frame(Datafinal=logistic$fitted.values,Age=Datafinal$Age)
    predicted.data <- predicted.data[order(predicted.data$Datafinal,decreasing = FALSE),]
    predicted.data$rank<-1:nrow(predicted.data)
    View(predicted.data)
    ggplot(data=predicted.data,aes(Age,Datafinal))+  
      geom_point(alpha=12,shape=4,stroke=2)+
      theme(legend.position = "right",text = element_text(size = 15))+
      xlab("Age")+
      ylab("Outcome")
  })

#------------------------------------------------------------RAW DATA------------------------------------------------------------
output$downloadData <- downloadHandler(
  filename=function(){
    paste("Diabetes","csv", sep = '.')
  },
  content=function(file){
    write.csv(Datafinal,file)
  }
)

output$tableData <- renderTable(
  head(Datafinal,200),width = "100%"
)
}
# Run the application 
# ------------------------------------------------------------RUNNING THE PROJECT-------------------------------------------------- 
shinyApp(ui = ui, server = server)
