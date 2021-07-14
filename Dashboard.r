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

ui<-dashboardPage(
    dashboardHeader(title = "Heart Disease Prediction",titleWidth = 300),
    dashboardSidebar( width = 300,
      
        sidebarMenu(
        fileInput("upload","Upload the Dataset"),
        menuItem(h4(strong("Dataset")),tabName = "dataset"),
        menuItem(h4(strong("Exploratory Data Analysis"))),
        menuSubItem("Pre-processing",tabName = "process"),
        menuSubItem("Univariate Plots",tabName = "visual"),
        menuSubItem("Proportion",tabName="comb"),
        menuSubItem("Bivariate Plots",tabName = "bivar"),
        menuSubItem("Categorical Bivariate Plots",tabName = "bivar1" ),
        menuSubItem("Heat Map",tabName = "heatmap" ),
        menuItem(h4(strong("Logistic Regression"))),
        menuSubItem("Model",tabName = "model" )
    )
    ),
    dashboardBody(
        
        shinyDashboardThemes(
            theme = "purple_gradient"
        ),
        tabItems(
            tabItem(
                h2("Our Dataset"),
                tabName = "dataset",
                fluidRow(
                mainPanel(width = 12,
                          tabsetPanel(
                              type = "tabs",
                              tabPanel("Dataset",DT::dataTableOutput("data")),
                              tabPanel("Summary",verbatimTextOutput("summary"),
                                       tags$head(tags$style(HTML("
                            #printout {
                              font-size: 20px;
                            }
                            ")))),
                              tabPanel("Structure",verbatimTextOutput("str")),
                              
                              tabPanel("Describe",DT::dataTableOutput("describe"))
                              
                          )
                )
                )
                
            ),
            tabItem(
                h2=("Visualization"),
                tabName="visual",
                
                div(style="display:inline-block",
                selectInput("chart","Select the type of Chart",
                            choices=c("","Barplot","Histogram"),
                            selected = 'Barplot')),
                div(style="display:inline-block",
                selectInput("attribute","Select the attribute",
                            choices = c("","male","totChol","education","currentSmoker",
                                        "cigsPerDay","BPMeds","prevalentStroke",
                                        "prevalentHyp","diabetes","TenYearCHD","age",
                                        "sysBP","diaBP","glucose","BMI","heartRate"),
                            selected = 'male')),
                
                plotlyOutput("ploting",height='800px',width = '1550px')
                
            ),
            tabItem(
                h2=('pre-processing'),
                tabName = 'process',

                
                mainPanel(
                           tabsetPanel(
                             type="tabs",
                               #column(width = 100,
                             tabPanel("Missing Value Treatment", 
                                      
                                      fluidRow(box(img(src='miss_1.png', align = "centre"), width = 11)),
                                      fluidRow(column( width = 12,
                                               box(img(src='miss_2.png', align = "left", width='645px', height='400px'), width = 6),
                                      box(img(src='miss_3.png', align = "left", width='645px', height='400px'), width = 6)
                                      ))),
                             tabPanel("Outlier Treatment",
                                      
                                      fluidRow(box(img(src='out_1.png', align = "centre"), width = 11)),
                                      fluidRow(column( width = 12,
                                                       box(img(src='out_2.png', align = "left", width='645px', height='400px'), width = 6),
                                                       box(img(src='out_3.png', align = "left", width='645px', height='400px'), width = 6)
                                      )))
                           )
                           , width = 100
                           
                )          
                ),
            
            tabItem(
                h2="Combining different variables",
                tabName="comb",
               fluidRow( 
                div(style="display:inline-block",
                selectInput("plots","Select the type of Chart",
                            choices=c("","Histogram","Barplot","Boxplot"),
                            selected = 'Barplot')),
                div(style="display:inline-block",
                selectInput("var","Select the attribute",
                            choices = c("","male","diaBP","totChol","education","currentSmoker",
                                        "cigsPerDay","BPMeds","prevalentStroke",
                                        "prevalentHyp","diabetes","TenYearCHD","age",
                                        "sysBP","glucose","BMI","heartRate"),
                            selected = 'male')),
                div(style="display:inline-block",
                numericInput("binwid","Binwidth",step = 0.1, min = 0, value = 1)),
            ),
            
            plotOutput("combine",height='800px',width = '1550px')
                
            ),
            tabItem(
                h2="Bivariate Data Visualization",
                tabName = "bivar",
                div(style="display:inline-block",
                selectInput("var1","Select the variable to be displayed on X axis",
                            choices = c("age","cigsPerDay","sysBP","diaBP","glucose","BMI","heartRate","totChol"),
                            selected = 'sysBP')),
                div(style="display:inline-block",
                selectInput("var2","Select the variable to be displayed on Y axis",
                            choices=c("age","cigsPerDay","sysBP","diaBP","glucose","BMI","heartRate","totChol"),
                            selected = 'diaBP')),
                
                plotlyOutput("scat",height='800px',width = '1550px')
                
               
            ),
            tabItem(
                h2="Bivariate under Categorical Variables",
                tabName  ="bivar1",
                div(style="display:inline-block",
                selectInput("var3","Select the variable to be displayed on X axis",
                            choices = c("age","sysBP","diaBP","glucose","BMI","heartRate","totChol"),
                            selected = 'BMI')),
                div(style="display:inline-block",
                selectInput("var4","Select the variable to be displayed on Y axis",
                            choices=c("age","sysBP","diaBP","glucose","BMI","heartRate","totChol"),
                            selected = 'cigsPerDay')),
                div(style="display:inline-block",
                selectInput("var5","Select the categorical variable",
                            choices=c("","male","education","currentSmoker",
                                      "cigsPerDay","BPMeds","prevalentStroke",
                                      "prevalentHyp","diabetes","TenYearCHD"),
                            selected = 'male')),
                
                plotlyOutput("scat2",height='800px',width = '1600px')
                
                           
                ),
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
            tabItem(
              h2=('Logistic Regression '),
              tabName = 'model',
              mainPanel(
                width=100,
                fluidRow(
                  column(width=12,
                         box(h3("Class Imbalance",tableOutput("table1"))),
                         box(h3("Up Sampling",tableOutput("table2"))),
                  )
                ),
                fluidRow(
                  column(width=12,
                         box(h3("Model Before Elimination"),verbatimTextOutput("model1")),
                         box(h3("Model after elimination"),verbatimTextOutput("model2")),
                  )
                ),
                fluidRow(
                  column(width=12,
                         box(h3('AUC-ROC Curve'),img(src='auc_2.png',width='550px', height='550px')),
                         fluidRow(box(h3("Confusion Matrix"),tableOutput("mat2")),
                                  box(h3("recall",tableOutput("re2"))))
                  )
                )
              )
            ) 
            
        )
    )
)

server<-function(input,output){
    data1<- reactive({
        if(is.null(input$upload)){
            return(NULL)
        }
      #fetch data
        read.csv(input$upload$datapath,header = TRUE,sep=",")
    })
    
    
    #### OUR DATASET ####
    output$data<-DT::renderDataTable({
        DT::datatable(data1(),
                      options=list(scrollX=TRUE,
                                   pageLength = 30)
        )
    })
    
    output$summary<-renderPrint({
        summary(data1())
    })
    
    output$describe <-DT::renderDataTable({
        DT::datatable(view(describe(data1())),
                      options = list(scrollX = TRUE)
        )
    })
    output$str<-renderPrint({
        str(data1())
    })
    
    ### Visualization ####
    
    output$ploting <- renderPlotly({
        if (input$chart=="Barplot"){
            vec<-c("male","education","currentSmoker","cigsPerDay","BPMeds","prevalentStroke",
                   "prevalentHyp","diabetes","TenYearCHD")
            if (input$attribute %in%  vec) {
                ggplot(data=data1(),aes(as.factor(get(input$attribute))))+geom_bar(fill="#79b0e0",col="black",width=0.4,position=position_dodge(0.9))+
                labs(x=input$attribute)+
                 geom_text(stat="count",aes(label = after_stat(count)),vjust=0)+
                theme(
                   text=element_text(size=20),
                   axis.title.x = element_text(color="black", size=20, face="bold"),
                   axis.title.y = element_text(color="black", size=20,face="bold")
                 )
                
            }

        }
        else if (input$chart=="Histogram"){
            vec<-c("age","sysBP","diaBP","glucose","BMI","heartRate","totChol")
            if (input$attribute %in%  vec) {
                ggplot(data=data1(),aes(x=get(input$attribute)))+
                    geom_histogram(aes(y=..density..),fill="#8790e8",col="black")+
                labs(x=input$attribute)+
                    geom_density(alpha=.2,fill="red")+theme(text=element_text(size=20),
                      axis.title.x = element_text(color="black", size=20, face="bold"),
                      axis.title.y = element_text(color="black", size=20,face="bold"))
            }
            
        }
       
    })
    output$info <-renderText({
        paste0("x=",input$plot_click$x,
               "\ny=",input$plot_click$y)
    })
    ########### Combining ########
    output$combine <- renderPlot({
        if (input$plots=="Barplot"){
            vec<-c("male","education","cigsPerDay","currentSmoker","BPMeds","prevalentStroke",
                   "prevalentHyp","diabetes")
            TenYrCHD<-as.factor(data1()$TenYearCHD)
            if (input$var %in%  vec) {
                a<-ggplot(data=data1(),aes(x=get(input$var),fill = TenYrCHD))+geom_bar()+scale_fill_manual(values = c("#a3a3e3","#84499c"))+
                  labs(x=input$var)+theme(text=element_text(size=28),
                    axis.title.x = element_text(color="black", size=20, face="bold"),
                    axis.title.y = element_text(color="black", size=20,face="bold"))
                
                b<-ggplot(data=data1(),aes(x=get(input$var),fill = TenYrCHD))+geom_bar(position = "fill")+scale_fill_manual(values = c("#a3a3e3","#84499c"))+
                  labs(x=input$var)+theme(text=element_text(size=30),
                    axis.title.x = element_text(color="black", size=20, face="bold"),
                    axis.title.y = element_text(color="black", size=20,face="bold"))
               
                 
                ggarrange(a,b,ncol = 2, nrow = 1)
            }   
        }
        else if (input$plots=="Histogram"){
            vec<-c("age","sysBP","diaBP","glucose","BMI","heartRate","totChol")
            TenYrCHD<-as.factor(data1()$TenYearCHD)
            if (input$var %in%  vec) {
                a<-ggplot(data=data1(),aes(x=get(input$var),fill =TenYrCHD))+
                  geom_histogram(binwidth = input$binwid)+labs(x=input$var)+scale_fill_manual(values = c("#a3a3e3","#84499c"))+theme(text=element_text(size=28),
                    axis.title.x = element_text(color="black", size=20, face="bold"),
                    axis.title.y = element_text(color="black", size=20,face="bold"))
                b<-ggplot(data=data1(),aes(x=get(input$var),fill = TenYrCHD))+
                  geom_histogram(position = "fill",binwidth =input$binwid )+scale_fill_manual(values = c("#a3a3e3","#84499c"))+
                  labs(x=input$var)+theme(text=element_text(size=30),
                    axis.title.x = element_text(color="black", size=20, face="bold"),
                    axis.title.y = element_text(color="black", size=20,face="bold"))
                ggarrange(a,b,ncol = 2, nrow = 1)
        } 
        }
        else if (input$plots=="Boxplot"){
            vec<-c("age","cigsPerDay","sysBP","diaBP","glucose","BMI","heartRate","totChol")
            TenYrCHD<-as.factor(data1()$TenYearCHD)
            if (input$var %in%  vec) {
                ggplot(data=data1(),aes(x=get(input$var),fill= TenYrCHD))+geom_boxplot()+labs(x=input$var)+theme(text=element_text(size=28),
                  axis.title.x = element_text(color="black", size=20, face="bold"),
                  axis.title.y = element_text(color="black", size=20,face="bold"))+theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))+scale_fill_manual(values = c("#a3a3e3","#84499c"))
            }    
        }
    })
    
    ######## BIVARIATE ########
    output$scat <-renderPlotly({
                ggplot(data=data1(),aes(x=get(input$var1),y=get(input$var2)))+geom_point(color = "#33386b")+labs(x=input$var1,y=input$var2)+
        theme(text=element_text(size=20),
                  axis.title.x = element_text(color="black", size=20, face="bold"),
                  axis.title.y = element_text(color="black", size=20,face="bold"))+theme(plot.margin=unit(c(1,1,1.5,1.5),"cm"))
            })
    
    ########## BIVARIATE1 ######
    output$scat2 <-renderPlotly({
        ggplot(data=data1(),aes(x=get(input$var3),y=get(input$var4)))+geom_point(color = "#33386b")+
            geom_smooth(method = "lm",se=F,color = "#84499c", size=2)+labs(x=input$var3,y=input$var4)+theme(text=element_text(size=20),
              axis.title.x = element_text(color="black", size=20, face="bold"),
              axis.title.y = element_text(color="black", size=20,face="bold"))+theme(plot.margin=unit(c(1,1,1,3),"cm"))+
            facet_grid(cols= vars(get(input$var5)))
    })
 
    ######## Heatmap #######
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
    
    ######## Logistic Regression ##########
    ########## Model ##########
    output$table1 <-renderTable({
      
      table(data1()$TenYearCHD)
    })
    
    
    output$table2 <-renderTable({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
 
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      table(up_train$Class)
    })
    
    output$model1 <- renderPrint({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      setnames(up_train, "Class", "TenYearCHD")
      framinghamLog = glm(TenYearCHD ~ ., data = up_train, family=binomial)
      print(summary(framinghamLog))
    })
    
    output$mat1 <- renderTable({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
      
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      setnames(up_train, "Class", "TenYearCHD")
      framinghamLog = glm(TenYearCHD ~ ., data = up_train, family=binomial)
      predictTest = predict(framinghamLog, type="response", newdata=test)
      table(test$TenYearCHD, predictTest > 0.4)
      
    })
    output$re1 <-renderTable({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
      
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      setnames(up_train, "Class", "TenYearCHD")
      framinghamLog = glm(TenYearCHD ~ ., data = up_train, family=binomial)
      predictTest = predict(framinghamLog, type="response", newdata=test)
      predictTest <- ifelse(predictTest > 0.4,1,0)
      predictTest<- as.factor(predictTest)
      test$TenYearCHD<-as.factor(test$TenYearCHD)
      recall <- sensitivity(predictTest, test$TenYearCHD, positive="1")
      print(recall)
    })
    
    output$model2 <-renderPrint({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
      
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      setnames(up_train, "Class", "TenYearCHD")
      framinghamLog2=glm(TenYearCHD ~ .-education-currentSmoker-heartRate-glucose-prevalentStroke, data = up_train, family=binomial)
      print(summary(framinghamLog2))
    })
    
    output$mat2 <- renderGvis({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
      
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      setnames(up_train, "Class", "TenYearCHD")
      framinghamLog2=glm(TenYearCHD ~ .-education-currentSmoker-heartRate-glucose-prevalentStroke, data = up_train, family=binomial)
      predictTest2 = predict(framinghamLog2, type="response", newdata=test)
      predictTest2=as.data.frame(predictTest2)
      predictTest2 <- ifelse(predictTest2 > 0.4,1,0)
      predictTest2<- as.factor(predictTest2)
      test$TenYearCHD<-as.factor(test$TenYearCHD)
      gvisTable(as.data.frame.matrix(table(test$TenYearCHD,predictTest2)),
                options=list(width="500px", height="300px"))
      
      
    })
    
    output$re2 <-renderTable({
      set.seed(1000)
      split = sample.split(data1()$TenYearCHD, SplitRatio = 0.75)
      train = subset(data1(), split==TRUE)
      test = subset(data1(), split==FALSE)
      
      options(scipen=999)  # prevents printing scientific notations.
      z<-train%>%
        select(male,education,currentSmoker,
               cigsPerDay,BPMeds,prevalentStroke,
               prevalentHyp,diabetes,age,
               sysBP,glucose,BMI,heartRate,totChol)
      train$TenYearCHD<-as.factor(train$TenYearCHD)
      up_train <- upSample(x = z,y = train$TenYearCHD)
      setnames(up_train, "Class", "TenYearCHD")
      framinghamLog2=glm(TenYearCHD ~ .-education-currentSmoker-heartRate-glucose-prevalentStroke, data = up_train, family=binomial)
      predictTest2 = predict(framinghamLog2, type="response", newdata=test)
      predictTest2 <- ifelse(predictTest2 > 0.4,1,0)
      predictTest2<- as.factor(predictTest2)
      test$TenYearCHD<-as.factor(test$TenYearCHD)
      recall <- sensitivity(predictTest2, test$TenYearCHD, positive="1")
      print(recall)
    })
       
}


shinyApp(ui,server)

