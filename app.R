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
#Datafinal= read.csv("Users/user/Documents/Arya/St. Joseph's- MSc. Big Data Analytics/Bhogle's Project/diabetes.csv")
#Datafinal <- read_csv("diabetes.csv")
Datafinal <- read_csv("diabetes.csv")
View(Datafinal)
#Datafinal$Date = as.Date(Datafinal$Date,"%d-%m-%Y")
#Datafinal <- mutate(Datafinal, Year = format(Date,"%Y"))
#Year<-unique(Datafinal$Year)
#City<-unique(Datafinal$City)


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
      menuItem(("Line graph"), tabName = "Line_graph",icon = icon('chart-line')),
      menuItem(("Pollutant trends"), tabName = "year_data", icon = icon('th')),
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
      
      
      
      
      
      # -------------------------------------------------MAP AND BAR GRAPH TAB------------------------------------------------------------------ 
      tabItem(
        h2="Heatmap",
        tabName = "heatmap",
        fluidRow(
          mainPanel(
            # width=100,
            
            plotlyOutput("heat",height='980px',width = '1565px')
          )
        )
      ),
      # -----------------------------------------------------------CORRELATION MATRIX TAB--------------------------------------------------    
      # tabItem(tabName = "year_data",
      #         fluidRow(column(4,selectInput("Cities", ("Choose a City:"),City,selected = 'Delhi')),
#                        column(8,selectInput("years",("Choose a Year:"),Year,selected="2017"))),
#               fluidRow(
#                 column(6,
#                        box(title = "Correlation matrix", solidHeader = TRUE, status = "primary", width = 12,
#                            tabsetPanel(
#                              tabPanel("correlation coefficients", withSpinner(plotOutput("corrcoeff",height = 475))),
#                              tabPanel("correlated scatterplots", withSpinner(plotOutput("corrscatt",height = 475))),
#                              tabPanel("Heat map", withSpinner(plotOutput("heatmap",height = 475)))
#                              
#                            )
#                        )	
#                        
#                 ),
#                 column(6,
#                        box(title = "Precautions table", solidHeader = TRUE, status = "primary", width = 15,
#                            tabsetPanel(
#                              tabPanel("PM2.5", withSpinner(dataTableOutput("pm2_5",height = 475))),
#                              tabPanel("PM10", withSpinner(dataTableOutput("pm10",height = 475))),
#                              tabPanel("NO2", withSpinner(dataTableOutput("no2",height = 475))),
#                              tabPanel("CO", withSpinner(dataTableOutput("co",height = 475))),
#                              tabPanel("SO2", withSpinner(dataTableOutput("so2",height = 475))),
#                              tabPanel("O3", withSpinner(dataTableOutput("o3",height =475))),
#                              tabPanel("NO", withSpinner(dataTableOutput("no",height = 475))),
#                              tabPanel("NH3", withSpinner(dataTableOutput("nh3",height = 475)))
#                            )
#                            
#                        ))
#               ))
#     ),
#     
#     
#   )
# )

# -------------------------------------------------------------------------------------------------------------------------------------
#                                                   SERVER
# -------------------------------------------------------------------------------------------------------------------------------------

server <- function(input, output) {
  
  
  # ------------------------------------------------------TAB2------------------------------------------------------------ 
  # ---------------------------------------------- MAP AND BAR CHARTS --------------------------------------------------
  
  # output$AQI<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=AQI, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  
  # ----------------------------------------------------MAP FOR AQI--------------------------------------------------    
  # 
  # output$map<-renderLeaflet({
  #   
  #   # filtering the data according to the date selected
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   
  #   # mutating the data with the popup info for easy access.
  #   Day<-Day %>% 
  #     mutate(popup_Info=paste("City: ",City,"</br>","AQI: ",AQI,"</br>","Condition: ",AQI_Bucket))
  #   
  #   # gradient based on the AQI level
  #   colour<-c("green","red")
  #   
  #   # creating a pallet out of it
  #   pal<-colorFactor(colour,Datafinal$AQI)
  #   
  #   # sending the data to the leaflet map to be rendered
  #   # the markers are provided the pallet colour
  #   leaflet() %>% 
  #     addTiles() %>% 
  #     addCircleMarkers(data=Day, lat=~Latitude, lng =~Longitude, 
  #                      radius = 20, popup = ~popup_Info, color = ~pal(AQI))
  #   
  #   
  # })
  
  # ----------------------------------------------------BAR GRAPHS FOR AQI--------------------------------------------------    
  
  # output$PM2_5<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=PM2.5, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$PM10<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=PM10, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$NO<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=NO, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$NO2<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=NO2, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$NH3<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=NH3, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$CO<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=CO, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$SO2<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=SO2, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # output$O3<-renderPlot({
  #   Day <- filter(Datafinal,Datafinal$Date == input$select_date)
  #   df_base <- ggplot(data=Day, aes(x=City, y=O3, fill=AQI_Bucket))
  #   df_base + geom_bar(stat = "identity") + scale_fill_brewer(palette = "Oranges")
  # })
  # 
  # ------------------------------------------- TAB3 ---------------------------------------------------------------------     
  # ---------------------------------------- LINE GRAPHS ------------------------------------------------------------- 
  # 
  # output$plots <- renderPlot({
  #   Datafinal$Date <- as.Date(Datafinal$Date)
  #   week_new <- Datafinal[,c(1:10)]
  #   week_new <- filter(week_new,between(Date, as.Date(input$start_date), as.Date(input$end_date)))
  #   week_city <- filter(week_new,City==input$Cities1)
  #   
  #   plot(week_city$CO,type="b",lwd=2,
  #        xaxt="n",ylim=c(0,500),col="blue",
  #        xlab="Date",ylab="values",
  #        main = input$Cities1)
  #   
  #   axis(1,at=1:length(week_city$Date),labels=week_city$Date)
  #   lines(week_city$NO2,col="red",type="b",lwd=2)
  #   lines(week_city$NH3,col="orange",type="b",lwd=2)
  #   lines(week_city$NO,col="purple",type="b",lwd=2)
  #   lines(week_city$O3,col="grey",type="b",lwd=2)
  #   lines(week_city$PM2.5,col="green",type = "b",lwd=2)
  #   lines(week_city$PM10,col="brown",type = "b",lwd=2)
  #   lines(week_city$SO2,col="violet",type = "b",lwd=2)
  #   
  #   
  #   
  #   legend("topright",legend=c("CO","NO2","NH3","NO","O3","PM2.5","PM10","SO2
  #                            "),
  #          lty=5,lwd=4,pch=10,col=c("blue","red","orange","purple","grey","green","brown","violet"),
  #          ncol=2,bty="n",cex=0.8,
  #          text.col=c("blue","red","orange","purple","grey","green","brown","violet")
  #   )
  # })
  
  # ----------------------------------------------------------- TAB4 -----------------------------------------------------------------------  
  # --------------------------------------------------------CORRELATION MATRIX ----------------------------------------------------------    
  
  #output$corrcoeff <- renderPlot({
    #mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    #mydata<-mydata2[,c(3:11)]
    #mydata.rcorr = rcorr(as.matrix(mydata))
    #mydata.coeff = mydata.rcorr$r
    #corrplot(mydata.coeff,method="number")
  #})
  
  # ------------------------------------------------------SCATTERPLOT CORRELATION-------------------------------------------------------    
  
  #output$corrscatt <- renderPlot({
    #mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    #mydata<-mydata2[,c(3:11)]
    #chart.Correlation(mydata, histogram=TRUE, pch=19)
    
  #})
  
  # -----------------------------------------------------------HEAT MAP-----------------------------------------------------------------    
  
  #output$heatmap <- renderPlot({
    #mydata2 <- Datafinal %>%filter(Year==input$years, City==input$Cities)
    #mydata<-mydata2[,c(3:11)]
    #mydata.rcorr = rcorr(as.matrix(mydata))
    #mydata.coeff = mydata.rcorr$r
    #palette = colorRampPalette(c("green", "white", "red")) (20)
    #heatmap(x = mydata.coeff, col = palette, symm = TRUE)
  #})
  
  
  output$heat <- renderPlotly({
    num_var=sapply(data1(),is.numeric)
    data_matrix <- data.matrix(data1()[num_var])
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
      scale_fill_gradient2(low = "#32305e", high = "#6f388c", mid = "#c5c5eb", 
                           midpoint = 0, limit = c(-1,1), space = "Lab", 
                           name="Pearson\nCorrelation") +
      theme_minimal()+ # minimal theme
      theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                       size = 20, hjust = 1))+
      coord_fixed()
    ggheatmap + 
      geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
      theme(
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text=element_text(size=21),
        panel.grid.major = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.ticks = element_blank(),
        legend.justification = c(1, 0),
        legend.position = c(0.6, 0.7),
        legend.direction = "horizontal")+
      guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                   title.position = "top", title.hjust = 0.5))+theme(plot.margin=unit(c(1,1,1.5,0.5),"cm"))
    
    
  })
  
  
  # ----------------------------------------------------TABLES FOR POLLUTANT PRECAUTIONS-------------------------------------------------- 
  
  # reading csv file containing precautions from pollutants
  # Poltab= read.csv("https://raw.githubusercontent.com/prathibha13/R-shiny-project-AQI/main/pollutants%20table.csv")
  
  # Table showing PM2.5 cautions
  # pm2_5data<-Poltab[,c(1:3)]
  # output$pm2_5 <- DT::renderDataTable(
  #   DT::datatable({ 
  #     pm2_5data
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  # 
  # Table showing PM10 cautions
  # pm10data<- Poltab[,c(4:6)]
  # output$pm10 <- DT::renderDataTable(
  #   DT::datatable({ 
  #     pm10data
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   ))
  # 
  # # Table showing NO2 cautions
  # no2data<-Poltab[,c(7:9)]
  # output$no2 <- DT::renderDataTable(
  #   DT::datatable({ 
  #     no2data
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  # 
  # # Table showing CO cautions
  # codata<-Poltab[,c(10:12)]
  # output$co <- DT::renderDataTable(
  #   DT::datatable({ 
  #     codata
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  
  # Table showing SO2 cautions
  # so2data<-Poltab[,c(13:15)]
  # output$so2 <- DT::renderDataTable(
  #   DT::datatable({ 
  #     so2data
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  
  # Table showing O3 cautions
  # o3data<-Poltab[,c(16:18)]
  # output$o3 <- DT::renderDataTable(
  #   DT::datatable({ 
  #     o3data
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  
  # Table showing NO cautions
  # nodata<-Poltab[,c(19:21)]
  # output$no <- DT::renderDataTable(
  #   DT::datatable({ 
  #     nodata
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  # 
  # Table showing NH3 cautions
  # nh3data<-Poltab[,c(22:24)]
  # output$nh3 <- DT::renderDataTable(
  #   DT::datatable({ 
  #     nh3data
  #   }, 
  #   options = list(searching = FALSE, pageLength = 10, lengthChange = FALSE, order = list(list(1, 'desc'))
  #   ), rownames = FALSE 
  #   
  #   ))
  # 
  #------------------------------------------------------------TAB5------------------------------------------------------------
  #------------------------------------------------------------RAW DATA------------------------------------------------------------
  output$downloadData <- downloadHandler(
    filename=function(){
      paste("DayData","csv", sep = '.')
    },
    content=function(file){
      write.csv(Datafinal,file)
    }
  )
  
  output$tableData <- renderTable(
    head(Datafinal,200),width = "100%"
  )
  
}
# ------------------------------------------------------------RUNNING THE PROJECT--------------------------------------------------     
shinyApp(ui = ui, server = server)




















