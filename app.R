# Install required packages if not installed
if (!require(shiny)) {
  install.packages("shiny")
}
if (!require(DT)) {
  install.packages("DT")
}
if (!require(tidyverse)) {
  install.packages("tidyverse")
}

# Load necessary libraries
library(shiny)
library(DT)  # For interactive tables
library(tidyverse)

# Define UI
ui <- fluidPage(
  titlePanel("Student Assessment Viewer"),
  
  # Common inputs
  sidebarLayout(
    sidebarPanel(
      # Input: Choose a CSV file
      fileInput("file", "Choose CSV File", accept = c(".csv")),
      
      # Input: Select student
      selectInput("student", "Select Student", NULL),
      
      # Input: Select skill
      selectInput("skill_name", "Select Skill Name", NULL),
      
      # Input: Select skill level
      # Checkbox group for selecting skill levels
      checkboxGroupInput("skill_levels", "Select Skill Levels:",
                         choices = NULL),
      radioButtons("level_combined","Do you want to see combined results?",choices=c("Yes","NO"),
                   selected = "NO",inline=TRUE ),
      # Action button to trigger data loading
      actionButton("load_data_btn", "Load Data")
    ),
    
    mainPanel(
      # Logo placeholder
      tags$div(
        style = "background-image: url('logo.png'); background-size: cover; height: 100px; width: 100%;",
        ""
      ),
      
      # Tabset panel with three tabs
      tabsetPanel(
        tabPanel("View Data",
                 mainPanel(
                   # Output: DataTable for selected student
                   DTOutput("table")
                 )
        ),
        
        tabPanel("Analysis",
                 # Sub-tabset panel with two sub-tabs
                 tabsetPanel(
                   tabPanel("Scores in All Assessments",
                            radioButtons("assessment_tabs", "Select Display:",
                                         choices = c("Table", "Graph"),
                                         selected = "Table"),
                            # Output: Scores of assessments
                            dataTableOutput("assessment_scores"),
                            # Conditional plot based on radio button selection
                            conditionalPanel(
                              condition = "input.assessment_tabs == 'Graph'",
                              tabPanel("Graph",
                                       plotOutput("assessment_plot")
                              )
                            )
                   ),
                   
                   tabPanel("Skill Details",
                            tags$style(HTML("
                .checkbox {
                  display: inline-block;
                  margin-right: 10px;
                }
              ")),
                            # Radio buttons to select between table and graph
                            radioButtons("skills_tabs", "Select Display:",
                                         choices = c("Table", "Graph"),
                                         selected = "Table"),
                            
                            # Output: Assessment details (number of questions, total score)
                            dataTableOutput("assessment_details"),
                            # Conditional plot based on radio button selection
                            conditionalPanel(
                              condition = "input.skills_tabs == 'Graph'",
                              tabPanel("Graph",
                                       plotOutput("skill_details_plot")
                              )
                            )
                            
                            # Checkbox group for selecting skill levels
                            # Populated dynamically in server
                   ),
                 ),
        ),
        
        # Third tab for Time Series Results
        tabPanel("Time Series Results",
                 # Time Wise Analysis
                 tabsetPanel(
                   tabPanel("Skill Details",
                 plotOutput("skill_time")
        ))),
        tabPanel("Comparison",
                 tabsetPanel(
                   tabPanel("Assesment TimeLine",
                            radioButtons("assesment_timeline", "Select Display:",
                                         choices = c("Table", "Graph"),
                                         selected = "Table", inline=TRUE),
                            dataTableOutput("assesment_timeline_table"),
                            conditionalPanel(
                              condition = "input.assesment_timeline == 'Graph'",
                              plotOutput("assesment_timeline_plot")
                              
                            )
                            
                   ),
                   tabPanel("Skill Comparison",
                 radioButtons("skill_comparison", "Select Display:",
                              choices = c("Table", "Graph"),
                              selected = "Table", inline=TRUE),
                 dataTableOutput("skill_comparison_table"),
                 conditionalPanel(
                   condition = "input.skill_comparison == 'Graph'",
                   plotOutput("skill_comparison_plot")

                 )
                   )
        )
        )
      )
    )
  )
)
# Define server
server <- function(input, output, session) {
  # Reactive values to store data
  data <- reactiveVal(NULL)
  
  # Load data when the "Load Data" button is clicked
  observeEvent(input$load_data_btn, {
    req(input$file)
    data(read.csv(input$file$datapath, check.names = FALSE))  # Prevent R from modifying column names
    
    # Update student names in the selectInput
    updateSelectInput(session, "student", "Select Student", choices = unique(data()$Student_Name))
    
    # Update skill level choices for checkbox group
    updateCheckboxGroupInput(session, "skill_levels", "Select Skill Levels:",
                             choices = unique(data()$level))
    updateSelectInput(session,"skill_name","Select Skill Name", choices = unique(data()$Skill_Name))
  })
  
  # Render the interactive table for the selected student
  output$table <- renderDT({
    req(input$student)
    student_data <- data()[data()$Student_Name == input$student, ]
    datatable(student_data, editable = TRUE)
  })
  
  # Display column names
  output$col_names_text <- renderText({
    req(data())
    paste("Column Names: ", paste(names(data()), collapse = ", "))
  })
  
  # Calculate and display scores of assessments
  output$assessment_scores <- renderDataTable({
    req(input$student)
    student_data <- data()[data()$Student_Name == input$student, ]
    assessment_scores <- student_data %>%
      group_by(Assesment_Name) %>%
      summarise(Total_Score = sum(Score))
    assessment_scores
  })
  
  # Calculate and display assessment details (number of questions, total score)
  output$assessment_details <- renderDataTable({
    req(input$student)
    student_data <- data()[data()$Student_Name == input$student, ]
    
    # Filter by selected skill levels
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      student_data <- student_data %>% filter(level %in% input$skill_levels)
    }
    
    assessment_details <- student_data %>%
      group_by(Skill_Name) %>%
      summarise(
        No_Questions = n(),
        Total_Score = sum(Score)
      )
    assessment_details
  })
  
  # Conditional plot based on radio button selection
  output$assessment_plot <- renderPlot({
    req(input$student)
    student_data <- data()[data()$Student_Name == input$student, ] %>%
      group_by(Assesment_Name) %>%
      summarise(Total_Score = sum(Score)) %>%
      ungroup()
    
    ggplot(student_data, aes(x = Assesment_Name, y = Total_Score)) +
      geom_bar(stat="identity")
  })
  output$skill_details_plot <- renderPlot({
    req(input$student)
    student_data <- data()[data()$Student_Name == input$student, ]
    
    # Filter by selected skill levels
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      student_data <- student_data %>% filter(level %in% input$skill_levels)
    }
    skill_details <- student_data %>%
      group_by(Skill_Name,level) %>%
      summarise(Total_Score = sum(Score))
    # Plot skill details summarized by levels
    ggplot(skill_details, aes(x = Skill_Name, fill = as.factor(level),y=Total_Score)) +
      geom_bar(position = position_dodge(width = 0.8), stat = "identity") +
      labs(title = "Skill Details Summarized by Levels")
  })
  output$skill_time <-renderPlot({
    req(input$student)
    student_data <- data()[data()$Student_Name == input$student, ]
    
    if (!is.null(input$skill_name) && length(input$skill_name) > 0) {
      student_data <- student_data %>% filter(Skill_Name %in% input$skill_name)
    }
    if (input$level_combined =="NO"){
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      student_data <- student_data %>% filter(level %in% input$skill_levels)
    }
    student_data<-student_data %>% group_by(date,level) %>% summarise(Total_score=sum(Score))
    ggplot(student_data ,aes(x=date,group=as.factor(level),y=Total_score,col=as.factor(level)))+geom_line()}
    else{
      student_data<-student_data %>% group_by(date) %>% summarise(Total_score=sum(Score))
      ggplot(student_data ,aes(x=date,group=1,y=Total_score,))+geom_line()
    }
  })
  output$skill_comparison_table<-renderDataTable({
    req(input$student)
    overall_data<-data()[data()$Skill_Name == input$skill_name, ]
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      overall_data <- overall_data %>% filter(level %in% input$skill_levels)
    }
    overall_data <-overall_data %>% group_by(Assesment_Name) %>% summarise ("mean"=mean(Score,na.rm=T),"std"=sd(Score,na.rm=T)) %>% ungroup()
    student_data <- data()[data()$Student_Name == input$student, ]
    student_data <- student_data[student_data$Skill_Name == input$skill_name, ]
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      student_data <- student_data %>% filter(level %in% input$skill_levels)
    }
    student_data <- student_data %>% group_by(Assesment_Name) %>% summarise("Mean_Score"=mean(Score),"var"=sd(Score)) %>% ungroup()
    student_data <- left_join(student_data, overall_data,by="Assesment_Name")
    student_data <- student_data %>% mutate ("z_score"=(Mean_Score-mean)/std) %>% mutate(Percentile=round(pnorm(z_score) * 100, 2))
    student_data <- student_data %>% select(c(Assesment_Name,Percentile))
    student_data <- student_data %>% filter(Percentile >0)
    student_data
  })
  output$skill_comparison_plot<-renderPlot({
    req(input$student)
    if (input$level_combined =="Yes"){
    overall_data<-data()[data()$Skill_Name == input$skill_name, ]
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      overall_data <- overall_data %>% filter(level %in% input$skill_levels)
    }
    overall_data <-overall_data %>% group_by(Assesment_Name) %>% summarise ("mean"=mean(Score,na.rm=T),"std"=sd(Score,na.rm=T)) %>% ungroup()
    student_data <- data()[data()$Student_Name == input$student, ]
    student_data <- student_data[student_data$Skill_Name == input$skill_name, ]
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      student_data <- student_data %>% filter(level %in% input$skill_levels)
    }
    student_data <- student_data %>% group_by(Assesment_Name) %>% summarise("Mean_Score"=mean(Score),"var"=sd(Score)) %>% ungroup()
    student_data <- left_join(student_data, overall_data,by="Assesment_Name")
    student_data <- student_data %>% mutate ("z_score"=(Mean_Score-mean)/std) %>% mutate(Percentile=round(pnorm(z_score) * 100, 2))
    student_data <- student_data %>% select(c(Assesment_Name,Percentile))
    student_data <- student_data %>% filter(Percentile >0)
    ggplot(student_data ,aes(x=Assesment_Name,y=Percentile))+geom_bar(stat="identity")}
    else{
      overall_data<-data()[data()$Skill_Name == input$skill_name, ]
      if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
        overall_data <- overall_data %>% filter(level %in% input$skill_levels)
      }
      overall_data <-overall_data %>% group_by(Assesment_Name,level) %>% summarise ("mean"=mean(Score,na.rm=T),"std"=sd(Score,na.rm=T)) %>% ungroup()
      student_data <- data()[data()$Student_Name == input$student, ]
      student_data <- student_data[student_data$Skill_Name == input$skill_name, ]
      if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
        student_data <- student_data %>% filter(level %in% input$skill_levels)
      }
      student_data <- student_data %>% group_by(Assesment_Name,level) %>% summarise("Mean_Score"=mean(Score),"var"=sd(Score)) %>% ungroup()
      student_data <- left_join(student_data, overall_data,by=c("Assesment_Name","level"))
      student_data <- student_data %>% mutate ("z_score"=(Mean_Score-mean)/std) %>% mutate(Percentile=round(pnorm(z_score) * 100, 2))
      student_data <- student_data %>% select(c(Assesment_Name,level,Percentile))
      student_data <- student_data %>% filter(Percentile >0)
      ggplot(student_data ,aes(x=Assesment_Name,fill=as.factor(level),y=Percentile))+geom_bar(position="dodge",stat="identity")}
  })
  output$assesment_timeline_table<-renderDataTable({
    req(input$student)
    overall_data<-data()
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      overall_data <- overall_data %>% filter(level %in% input$skill_levels)
    }
    overall_data <-overall_data %>% group_by(date,Assesment_Name) %>% summarise ("mean"=mean(Score,na.rm=T),"std"=sd(Score,na.rm=T)) %>% ungroup()
    student_data <- data()[data()$Student_Name == input$student, ]
    if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
      student_data <- student_data %>% filter(level %in% input$skill_levels)
    }
    student_data <- student_data %>% group_by(date,Assesment_Name) %>% summarise("Mean_Score"=mean(Score),"var"=sd(Score)) %>% ungroup()
    student_data <- left_join(student_data, overall_data,by=c("date","Assesment_Name"))
    student_data <- student_data %>% mutate ("z_score"=(Mean_Score-mean)/std) %>% mutate(Percentile=round(pnorm(z_score) * 100, 2))
    student_data <- student_data %>% select(c(Assesment_Name,Percentile))
    student_data <- student_data %>% filter(Percentile >0)
    student_data
  })
  output$assesment_timeline_plot<-renderPlot({
    req(input$student)
    if (input$level_combined =="Yes"){
      overall_data<-data()
      if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
        overall_data <- overall_data %>% filter(level %in% input$skill_levels)
      }
      overall_data <-overall_data %>% group_by(date,Assesment_Name) %>% summarise ("mean"=mean(Score,na.rm=T),"std"=sd(Score,na.rm=T)) %>% ungroup()
      student_data <- data()[data()$Student_Name == input$student, ]
      if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
        student_data <- student_data %>% filter(level %in% input$skill_levels)
      }
      student_data <- student_data %>% group_by(date,Assesment_Name) %>% summarise("Mean_Score"=mean(Score),"var"=sd(Score)) %>% ungroup()
      student_data <- left_join(student_data, overall_data,by=c("date","Assesment_Name"))
      student_data <- student_data %>% mutate ("z_score"=(Mean_Score-mean)/std) %>% mutate(Percentile=round(pnorm(z_score) * 100, 2))
      student_data <- student_data %>% select(c(date,Assesment_Name,Percentile))
      student_data <- student_data %>% filter(Percentile >0)
      ggplot(student_data ,aes(x=reorder(Assesment_Name,date),group=1,y=Percentile))+geom_line()}
    else{
      overall_data<-data()
      if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
        overall_data <- overall_data %>% filter(level %in% input$skill_levels)
      }
      overall_data <-overall_data %>% group_by(date,Assesment_Name,level) %>% summarise ("mean"=mean(Score,na.rm=T),"std"=sd(Score,na.rm=T),.groups='drop') %>% ungroup()
      student_data <- data()[data()$Student_Name == input$student, ]
      if (!is.null(input$skill_levels) && length(input$skill_levels) > 0) {
        student_data <- student_data %>% filter(level %in% input$skill_levels)
      }
      student_data <- student_data %>% group_by(date,Assesment_Name,level) %>% summarise("Mean_Score"=mean(Score),"var"=sd(Score),.groups='drop') %>% ungroup()
      student_data <- left_join(student_data, overall_data,by=c("date","Assesment_Name","level"))
      student_data <- student_data %>% mutate ("z_score"=(Mean_Score-mean)/std) %>% mutate(Percentile=round(pnorm(z_score) * 100, 2))
      student_data <- student_data %>% select(c(date,Assesment_Name,level,Percentile))
      student_data <- student_data %>% filter(Percentile >0)
      student_data$date <- as.Date(student_data$date,format="%d-%m-%Y")  
      ggplot(student_data ,aes(x=reorder(Assesment_Name,date),group=as.factor(level),col=as.factor(level),y=Percentile))+geom_line()+geom_point()}
  })
}

# Run the Shiny app
shinyApp(ui, server)