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
Datafinal <- read.csv("diabetes.csv")
View(Datafinal)
# Define UI for application that draws a histogram
# ---------------------------------------------------------------------------------------------------------
#                                                USER INTERFACE
# ---------------------------------------------------------------------------------------------------------

ui <- dashboardPage(
  skin = 'black',
  dashboardHeader(title = "SUGAR RUSH"),
  dashboardSidebar(
    sidebarMenu(
      menuItem(strong("Home"), tabName = "home", icon = icon('home')),
      menuItem(strong("Raw Data"),tabName = "table",icon = icon('table')),
      menuItem(strong("Data Exploration"), tabName = "data", icon = icon('th')),
      menuItem(strong("Trends"),tabName = "trends",icon=icon('chart-line')),
      menuItem(strong("Bivariate Associations"),tabName = "bivariate",icon=icon('bar-chart-o')),
      menuItem(h4(strong("Logistic Regression"))),
      menuItem(("Graphs"),tabName = "regression",icon=icon('chart-line')),
      menuItem(("Prediction"),tabName = "predict",icon=icon('list-alt'))
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
              tags$img(src="sugar_rush.png",height=800, width='100%'),
              br(),
              br(),
              br(),
              fluidRow(
                column(width=7,tags$h2(width=5,"Video Of the Day")),
                column(width=5,tags$h2(width=5,"Top News"))),
              
              fluidRow(
                box(width=6,HTML('<iframe width="560" height="430" src="https://www.youtube.com/embed/HvVBuunaeE0" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>')),
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
                column(width=7,tags$h2(width=5,"About the Project"))
              ),
              fluidRow(
                #column(width=5,tags$img(src="logo.png", height=400, width="85%", hspace="90")),
                column(align = "justify",width=12,tags$div(HTML("<p>Due to increasing incidence rate of diabetes and prediabetes, it is a pressing issue in the health care industry to rightly identify the 
                                                                  factors that contribute to the occurrence of Diabetes in people, more so, in Women. Women have always played an anchoring role when it comes to their families in India. 
                                                                  More often than less, this has led to them prioritising their families’ health over their own.
                                                                  Traditionally, diabetes has not been spoken in the context of women. But secondary studies have 
                                                                  shown that there has been a rise in incidence of diabetes among women.</br></br>
 
                                                                  <p><strong>60%</strong>of Indian women are aware of diabetes linked to pregnancy(gestational diabetes),<br>
                                                                  <strong>44%</strong> of women are cognizant of prediabetes,<br>
                                                                  <strong>70%</strong> of women acknowledge that regular meals and regular exercise are important for a healthy body,<br>
                                                                  <strong>60%</strong> of them confessed to skipping meals often.</p>")))
              ),
              br(),
              br(),
              fluidRow(
                column(width=7,tags$h2(width=5,"FAQ"))
              ),
              fluidRow(
                column(width=12,
                       tags$h3("Q. What is BMI?"),
                       tags$p(align = "justify",width=12,"BMI or Body Mass Index is the proportion of a person’s height to their weight 
                                       i.e., the measure of body fat based on the person’s weight in relation to height. 
                                       The standard value is 18.5-25 for normal BMI. BMI less than 19 signifies a very skinny 
                                       and unhealthy person. A BMI of more that 25-30 signifies a person is overweight. 
                                       BMI above the range of 30-40 is considered to be very dangerous. 
                                       A person can be classified as obese if the BMI is more than 30."),
                       tags$h3("Q. How is BMI calculated?"),
                       tags$p(align = "justify",width=12,"Body Mass Index is a simple calculation using a person's height and weight. 
                                       The formula is BMI = kg/m2 where kg is a person's weight in kilograms and m2 
                                       is their height in meters squared."),
                       tags$h3("Q. What is Diabetes Pedigree Function?"),
                       tags$p(align = "justify",width=12,"A pedigree is a genetic representation of a family tree that diagrams the inheritance 
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
                column(width=7,tags$h2(width=5,"About the Outliers")),
                column(width=12,tags$p(align = "justify",width=12,HTML("The squad of 6 worked together to brainstorm and get to the conclusions. 
                                                    As the name suggests “THE OUTLIERS”, all of us having different backgrounds 
                                                    helped us in exchanging ideas.")))
                
              ),
      
              br(),
              br(),
              fluidRow(align = "centre",
                column(width=3,tags$img(src="yana.png", height=300, width="70%", hspace="50")),
                column(width=3,tags$img(src="arya.png", height=300, width="70%", hspace="150")),
                column(width=3,tags$img(src="mahika.png", height=300, width="70%", hspace="250"))),
              br(),
              br(),
              fluidRow(align = "centre",
                       column(width=3,tags$img(src="ashly.png", height=300, width="70%", hspace="50")),
                       column(width=3,tags$img(src="hendrick.png", height=300, width="70%", hspace="150")),
                       column(width=3,tags$img(src="purva.png", height=300, width="70%", hspace="250"))
              ),
              br(),
              br(),
              br(),
              fluidRow(
                column(width=5,tags$img(src="logo.png", height=400, width="85%", hspace="350"))
              )
              
      ),
      #--------------------------------------------------- DATA EXPLORATION TAB--------------------------------------------------------------------
      
      tabItem(tabName = "data",
              fluidRow(column(12,
                              box(title="Response Variable",solidHeader = TRUE,status = "primary",width = '1100px',
                                    column(width=7,tags$h3(width=5,"Outcome")),
                                           column(10,plotOutput("outcome_graph",height='300px',width = '900px')),
                                           column(width=12,tags$h4(width=8,"The dataset has 268 women that were diagnosed with Diabetes and 500 
                                                                   women that didn’t have Diabetes. The sample has a high occurrence (34%) of positive records of Diabetes."))))),
              fluidRow(column(12,
                      box(title="Predictor Variables",solidHeader = TRUE,status = "primary",width = '1100px',
                          column(width=7,tags$h3(width=5,HTML("1. Pregnancies</br></br>"))),
                          column(10,plotOutput("pregnancy1_graph",height='300px',width = '900px')),
                          #column(10,plotOutput("pregnancy2_graph",height='300px',width = '900px')),
                          column(width=12,tags$h4(width=8,HTML("It’s evident that women who have been diagnosed with Diabetes have had more pregnancies 
                                                  than women who were not (in this dataset). However, from the density plot, there’s no clear relationship
                                                  between the number of pregnancies and the occurrence of Diabetes.</br></br></br></br></br>"))),
                          
      fluidRow(column(width=7,tags$h3(width=5,HTML("2. Glucose</br></br>"))),
               column(10,plotOutput("glucose1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("glucose2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("There’s a clear difference in the amount of Glucose present in the women who have been diagnosed 
                                       with Diabetes and those who haven’t. While the density plot indicates an overlap in the levels of glucose 
                                       in both categories of women, the plots show that Glucose could be a good indicator of the response.</br></br></br></br></br>")))),
      fluidRow(column(width=7,tags$h3(width=5,HTML("3. Blood Pressure</br></br>"))),
               column(10,plotOutput("bp1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("bp2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("No clear difference is seen in the two categories of women who have and don’t have Diabetes. 
                                       This shows that Blood Pressure might not be a good predictor of the response variable.</br></br></br></br></br>")))),
      fluidRow(column(width=7,tags$h3(width=5,HTML("4. Skin Thickness</br></br>"))),
               column(10,plotOutput("sk1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("sk2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("No clear difference can be seen in the two categories of women who have and don’t have Diabetes. 
               This shows that Skin Thickness might not be a good predictor of the response variable.</br></br></br></br></br>")))),
      
      fluidRow(column(width=7,tags$h3(width=5,HTML("5. Insulin</br></br>"))),
               column(10,plotOutput("in1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("in2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("Again, no clear difference can be observed between the categories of women in the data,
               indicating Insulin may not be a good predictor of the response variable.</br></br></br></br></br>")))),
      
      fluidRow(column(width=7,tags$h3(width=5,HTML("6. BMI</br></br>"))),
               column(10,plotOutput("bmi1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("bmi2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("The plots show that all the women who had Diabetes had a BMI greater than 25, which is above the normal levels.
                                       On the other hand, women who did not have Diabetes had a BMI ranging from 18 to 60.</br></br></br></br></br>")))),
      
      fluidRow(column(width=7,tags$h3(width=5,HTML("7. DiabetesPedigreeFunction</br></br>"))),
               column(10,plotOutput("dpf1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("dpf2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("No clear difference can be seen in the two categories of women who have and don’t have Diabetes.
               This shows that DPF might not be a good predictor of the response variable.</br></br></br></br></br>")))),
      
      fluidRow(column(width=7,tags$h3(width=5,HTML("8. Age</br></br>"))),
               column(10,plotOutput("age1_graph",height='300px',width = '900px')),
               #column(10,plotOutput("age2_graph",height='300px',width = '900px')),
               column(width=12,tags$h4(width=8,HTML("No clear distinction is seen in the distribution of the ‘Age’ variable for women that have Diabetes versus those who don’t.</br></br></br></br></br>"))))
      
      )))),
    
      # -------------------------------------------------BIVARIATE ASSOCIATIONS TAB------------------------------------------------------------------ 
   
      tabItem(tabName = "bivariate",
              fluidRow(column(12,
              box(title="Bivariate Associations",solidHeader = TRUE, status = "primary",width = '1100px',
                  div(style="font-size:'20px',display:inline-block",
                      selectInput("var1","Select the first independent variable",
                                  choices = c('Pregnancies','Glucose','BloodPressure','SkinThickness','Insulin','BMI','DiabetesPedigreeFunction','Age'),
                                  selected = 'Pregnancies')),
                  div(style="font-size:'20px',display:inline-block",
                      selectInput("var2","Select the second independent variable",
                                  choices=c('Pregnancies','Glucose','BloodPressure','SkinThickness','Insulin','BMI','DiabetesPedigreeFunction','Age'),
                                  selected = 'Age')),
                  
                  plotOutput("scat",height='580px',width = '1100px'))))
              
      ),
      # -------------------------------------------------LOGISTIC REGRESSION TAB------------------------------------------------------------------ 
      # -------------------------------------------------GRAPHS TAB------------------------------------------------------------------ 
      
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
                    ))))),
# -------------------------------------------------PREDICTION TAB------------------------------------------------------------------ 
tabItem(tabName = "predict",
        fluidRow(
          column(12,
                 box(title = "Logistic Regression",solidHeader = TRUE, status = "primary",width = 1000,
                    numericInput(inputId = "preg",label="Enter the number of pregnancies",value="0"),
                    numericInput(inputId = "gluc",label="Enter Glucose level",value="0"),
                    numericInput(inputId = "st",label="Enter Skin Thickness",value="0"),
                    numericInput(inputId = "ins",label="Enter Insulin level",value="0"),
                    numericInput(inputId = "bmi",label="Enter BMI",value="0"),
                    numericInput(inputId = "dpf",label="Enter Diabetes Pedigree Function Value",value="0")
    
      )),
      column(12,box(title="Outcome",solidHeader=TRUE,status = "primary",width = 1000,height=400,textOutput(outputId = "outcome"))))
),


      
# ---------------------------------------------TRENDS TAB-------------------------------------------------------------

 tabItem(tabName = "trends",
#         
#         
#         tabName = "heatmap",
#         fluidRow(
#           mainPanel(
#             # width=100,
#             
#             plotlyOutput("heat",height='980px',width = '1565px')
#           )
#         )
        fluidRow(
          column(12,
                 box(title = "Correlation Matrix", solidHeader = TRUE, status = "primary", width = 1000,
                     tabsetPanel(
                       tabPanel(HTML("<span style = 'font-size:20px'>Correlation Coefficients</span>"), withSpinner(plotOutput("corrcoeff",height = 700, width= 1100))),
                       #tabPanel(HTML("<span style = 'font-size:20px'>Correlation Scatterplot</span>"), withSpinner(plotOutput("corrscatt",height = 700, width = 1100))),
                       tabPanel(HTML("<span style = 'font-size:20px'>Heat Map</span>"),withSpinner(plotlyOutput("heatmap",height = "700px", width = "900")))

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
  
  #------------------------------------------------------------DATA EXPLORATION TAB------------------------------------------------------------
  
  output$outcome_graph <- renderPlot({ 
    #data$Outcome<-factor(Datafinal$Outcome)
  
  # 1. Outcome
  ggplot(Datafinal,aes(Outcome,fill = factor(Outcome))) +
    geom_bar() + 
    ggtitle("Distribution of Outcome variable")+ 
      theme(
      plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5))
  
  })
  output$pregnancy1_graph <- renderPlot({
  p1 <- ggplot(Datafinal, aes(x = Outcome, y = Pregnancies,fill = factor(Outcome))) +
    geom_boxplot() +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    ggtitle("Number of pregnancies Vs Diabetes")
  #gridExtra::grid.arrange(p1,ncol = 2)
  
  #})
  
  #output$pregnancy2_graph <- renderPlot({
  p2 <- ggplot(Datafinal, aes(x = Pregnancies, fill = factor(Outcome))) + #color = Outcome
    geom_density(alpha = 0.8) +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    labs(x = "Pregnancies", y = "Density", title = "Density plot of Pregnancies")
  
  gridExtra::grid.arrange(p1,p2, ncol = 2)
  })
  
  output$glucose1_graph <- renderPlot({
  p3 <- ggplot(Datafinal, aes(x = Glucose, fill = factor(Outcome))) + #color = Outcome
    geom_density(alpha = 0.8) +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    labs(x = "Glucose", y = "Density", title = "Density plot of glucose")
  #gridExtra::grid.arrange(p3, ncol = 2)
  
  #})
  
  #output$glucose2_graph <- renderPlot({
  p4 <- ggplot(Datafinal, aes(x = Outcome, y = Glucose,fill = factor(Outcome))) +
    geom_boxplot() +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    ggtitle("Variation of glucose in women Vs Diabetes")
  
  gridExtra::grid.arrange(p4,p3, ncol = 2)
  })
  
  output$bp1_graph <- renderPlot({
  p5 <- ggplot(Datafinal, aes(x = BloodPressure, fill = factor(Outcome))) +
    geom_density(alpha = 0.8) +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    labs(x = "Blood pressure", y = "Density", title = "Density plot of Blood pressure")
  #gridExtra::grid.arrange(p5, ncol = 2)
  
  #})
 
  #output$bp2_graph <- renderPlot({
  p6 <- ggplot(Datafinal, aes(x = Outcome, y = BloodPressure,fill = factor(Outcome))) +
    geom_boxplot() +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    ggtitle("Variation of blood pressure in women Vs Diabetes")
  
  gridExtra::grid.arrange(p6,p5, ncol = 2)
  })
  
  output$sk1_graph <- renderPlot({
  p7 <- ggplot(Datafinal, aes(x = SkinThickness , fill = factor(Outcome))) +
    geom_density(alpha = 0.8) +
    xlim(0,75)+
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    labs(x = "Skin thickness", y = "Density", title = "Density plot of skin thickness")
  # gridExtra::grid.arrange(p7 ,ncol = 2)})
  # 
  # output$sk2_graph <- renderPlot({
  p8 <- ggplot(Datafinal, aes(x = Outcome, y = SkinThickness ,fill =factor(Outcome))) +
    geom_boxplot() +
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    ggtitle("Variation of skin thickness Vs Diabetes")
  
  gridExtra::grid.arrange(p8,p7, ncol = 2)})
  
  output$in1_graph <- renderPlot({
  p9 <- ggplot(Datafinal, aes(x = Outcome, y = Insulin,fill = factor(Outcome))) +
    #ylim(0,0.1) +
    geom_boxplot() + 
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    ggtitle("Variation of Insulin content Vs Diabetes")
 
  
  p10<-ggplot(Datafinal, aes(x = Insulin, fill = factor(Outcome))) + #color = Outcome
    geom_density(alpha = 0.8) +
    xlim(0,300)+
    theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
    labs(x = "Insulin", y = "Density", title = "Density plot of Insulin")
  
  gridExtra::grid.arrange(p9,p10, ncol = 2)})
  
  output$bmi1_graph <- renderPlot({
    p11 <- ggplot(Datafinal, aes(x = BMI, fill = factor(Outcome))) + #color = Outcome
      geom_density(alpha = 0.8) +
      xlim(0,100)+
      theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
      labs(x = "BMI", y = "Density", title = "Density plot of BMI")
 
    p12 <- ggplot(Datafinal, aes(x = Outcome, y = BMI,fill = factor(Outcome))) +
      geom_boxplot(binwidth = 5) +
      theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
      ggtitle("Variation of BMI of women Vs Diabetes")
    gridExtra::grid.arrange(p12,p11, ncol = 2)})

  output$dpf1_graph <- renderPlot({
    p14 <- ggplot(Datafinal, aes(x = Outcome, y = DiabetesPedigreeFunction,fill = factor(Outcome))) +
      geom_boxplot() +
      theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
      ggtitle("Variation of DPF of women Vs Diabetes")
  #   gridExtra::grid.arrange(p14, ncol = 2)})
  #   
  # output$dpf2_graph <- renderPlot({
    p13 <- ggplot(Datafinal, aes(x = DiabetesPedigreeFunction, fill = factor(Outcome))) + #color = Outcome
      geom_density(alpha = 0.8) +
      xlim(0,3)+
      theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
      labs(x = "DiabetesPedigreeFunction", y = "Density", title = "Density plot of Diabetes Pedigree Function")
    
    gridExtra::grid.arrange(p14,p13, ncol = 2)})
    
  output$age1_graph <- renderPlot({
    p15 <- ggplot(Datafinal, aes(x = Age, fill = factor(Outcome))) + #color = Outcome
      geom_density(alpha = 0.8) +
      xlim(0,100)+
      theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
      labs(x = "Age", y = "Density", title = "Density plot of Age")
    
    p16 <- ggplot(Datafinal, aes(x = Outcome, y = Age,fill = factor(Outcome))) +
      geom_boxplot() +
      theme(legend.position = "bottom",plot.title = element_text(color="black", size=16, face="bold.italic",hjust=0.5)) +
      ggtitle("Variation of Age of women Vs Diabetes")
    gridExtra::grid.arrange(p16,p15, ncol = 2)}) 
  
    
  # --------------------------------------------------------CORRELATION MATRIX ----------------------------------------------------------    
  
  output$corrcoeff <- renderPlot({
    mydata2 <- Datafinal #%>%filter(Year==input$years, City==input$Cities)
    #mydata<-mydata2[,c(3:11)]
    mydata2.rcorr = rcorr(as.matrix(mydata2))
    mydata2.coeff = mydata2.rcorr$r
    corrplot(mydata2.coeff,method="number")
  })
  
  # ------------------------------------------------------SCATTERPLOT CORRELATION-------------------------------------------------------    
  
  # output$corrscatt <- renderPlot({
  #   mydata2 <- Datafinal #%>%filter(Year==input$years, City==input$Cities)
  #   #mydata<-mydata2[,c(3:11)]
  #   chart.Correlation(mydata2, histogram=TRUE, pch=19)
  #   
  # })
  # -----------------------------------------------------------HEAT MAP-----------------------------------------------------------------    
  
  output$heatmap <- renderPlotly({
    num_var=sapply(Datafinal,is.numeric)
    data_matrix <- data.matrix(Datafinal[num_var])
    cormat <- round(cor(data_matrix),2)
    melted_cormat <- melt(cormat)
    get_lower_tri<-function(cormat){
      cormat[upper.tri(cormat)] <- NA
      return(cormat)
    }
    get_upper_tri <- function(cormat){
      cormat[lower.tri(cormat)]<- NA
      return(cormat)
    }
    upper_tri <- get_upper_tri(cormat)
    reorder_cormat <- function(cormat){
      # Use correlation between variables as distance
      dd <- as.dist((1-cormat)/2)
      hc <- hclust(dd)
      cormat <-cormat[hc$order, hc$order]}
    # Reorder the correlation matrix
    cormat <- reorder_cormat(cormat)
    upper_tri <- get_upper_tri(cormat)
    # Melt the correlation matrix
    melted_cormat <- melt(upper_tri, na.rm = TRUE)
    # Create a ggheatmap
    ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
      geom_tile(color = "white")+
      scale_fill_gradient2(low = "white", high = "red", mid = "pink", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           #name="Pearson\nCorrelation"
                           ) +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                       size = 16, hjust = 1))+
      coord_fixed()
    ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 3) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text=element_text(size=18),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.8),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 2,
                                   title.position = "top", title.hjust = 0.5))+theme(plot.margin=unit(c(0.2,0.2,0.1,0.2),"cm"))
    
    
  })
  
  
  
  
  # output$heatmap <- renderPlot({
  #   mydata2 <- Datafinal # %>%filter(Outcome==input$Outcome , Pregnancies==input$Pregnancies, Glucose == input$Glucose, BloodPressure == input$BloodPressure,
  #                                  #SkinThickness == input$SkinThickness, Insulin == input$Insulin, BMI== input$BMI, DiabetesPedigreeFunction == input$DiabetesPedigreeFunction, Age == input$Age)
  #   #mydata<-mydata2[,c(3:11)]
  #   mydata2.rcorr = rcorr(as.matrix(mydata2))
  #   mydata2.coeff = mydata2.rcorr$r
  #   palette = colorRampPalette(c("blue", "white", "navyblue")) (20)
  #   heatmap(x = mydata2.coeff, col = palette, symm = TRUE)
  # })
  #------------------------------------------------------------BIVARIATE ASSOCIATIONS TAB------------------------------------------------------------
  output$scat <-renderPlot({
    ggplot(data=Datafinal,aes(x=get(input$var1),y=get(input$var2)))+geom_point(aes(color = Outcome))+labs(x=input$var1,y=input$var2)+
      theme(text=element_text(size=20),
            axis.title.x = element_text(color="black", size=20, face="bold"),
            axis.title.y = element_text(color="black", size=20,face="bold")) #+theme(plot.margin=unit(c(1,1,1.5,1.5),"cm")
  })
  

  #------------------------------------------------------------LOGISTIC REGRESSION TAB------------------------------------------------------------
  #------------------------------------------------------------GRAPHS TAB------------------------------------------------------------
  
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
  #------------------------------------------------------------PREDICTION TAB------------------------------------------------------------
  output$outcome<-renderText({
    result= -9.22 + 0.13* input$preg + 0.03* input$gluc + 0.04* input$st + 0.005 * input$ins + 0.05* input$bmi + 0.80*input$dpf
    if(result>0.5)"Patient is Diabetic"
    else"Patient is Non-Diabetic"
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
shinyApp(ui=ui, server=server)
