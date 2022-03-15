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
      menuItem(("Heat Map"),tabName = "heatmap",icon=icon('map')),
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
      # ---------------------------------------------HEATMAP TAB-------------------------------------------------------------
      tabItem(
        tabName = "heatmap",
        fluidRow(
          mainPanel(
            # width=100,

            plotlyOutput("heat",height='980px',width = '1565px')
          )
        )
      )
    )
  )
)

      
      
      
      
# Define server logic required to draw a histogram
# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {

# -----------------------------------------------------------HEAT MAP-----------------------------------------------------------------    

output$heatmap <- renderPlot({
  mydata2 <- Datafinal %>%filter(Outcome==input$Outcome , Pregnancies==input$Pregnancies)
  #mydata<-mydata2[,c(0:10)]
  mydata2.rcorr = rcorr(as.matrix(mydata2))
  mydata2.coeff = mydata2.rcorr$r
  palette = colorRampPalette(c("green", "white", "red")) (20)
  heatmap(x = mydata2.coeff, col = palette, symm = TRUE)
})

# output$heatmap <- renderPlot({
#   heatmap(x = Datafinal, symm = TRUE)
# })
  
  # data1<- reactive({
  #   if(is.null(input$Datafinal)){
  #     return(NULL)
  #   }
  # })
  # 
  # output$heat <- renderPlotly({
  #   num_var=sapply(data1(),is.numeric)
  #   data_matrix <- data.matrix(data1()[num_var])
  #   cormat <- round(cor(data_matrix),2)
  #   melted_cormat <- melt(cormat)
  #   get_lower_tri<-function(cormat){
  #     cormat[upper.tri(cormat)] <- NA
  #     return(cormat)
  #   }
  #   get_upper_tri <- function(cormat){
  #     cormat[lower.tri(cormat)]<- NA
  #     return(cormat)
  #   }
  #   upper_tri <- get_upper_tri(cormat)
  #   reorder_cormat <- function(cormat){
  #     # Use correlation between variables as distance
  #     dd <- as.dist((1-cormat)/2)
  #     hc <- hclust(dd)
  #     cormat <-cormat[hc$order, hc$order]}
  #   # Reorder the correlation matrix
  #   cormat <- reorder_cormat(cormat)
  #   upper_tri <- get_upper_tri(cormat)
  #   # Melt the correlation matrix
  #   melted_cormat <- melt(upper_tri, na.rm = TRUE)
  #   # Create a ggheatmap
  #   ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  #     geom_tile(color = "white")+
  #     scale_fill_gradient2(low = "#32305e", high = "#6f388c", mid = "#c5c5eb", 
  #                          midpoint = 0, limit = c(-1,1), space = "Lab", 
  #                          name="Pearson\nCorrelation") +
  #     theme_minimal()+ # minimal theme
  #     theme(axis.text.x = element_text(angle = 45, vjust = 1, 
  #                                      size = 20, hjust = 1))+
  #     coord_fixed()
  #   ggheatmap + 
  #     geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  #     theme(
  #       axis.title.x = element_blank(),
  #       axis.title.y = element_blank(),
  #       text=element_text(size=21),
  #       panel.grid.major = element_blank(),
  #       panel.border = element_blank(),
  #       panel.background = element_blank(),
  #       axis.ticks = element_blank(),
  #       legend.justification = c(1, 0),
  #       legend.position = c(0.6, 0.7),
  #       legend.direction = "horizontal")+
  #     guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
  #                                  title.position = "top", title.hjust = 0.5))+theme(plot.margin=unit(c(1,1,1.5,0.5),"cm"))
  #   
  #   
  # })
  # 
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
