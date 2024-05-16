library(shiny)
library(ggplot2)
library(plotly)

ui <- navbarPage("Heart Stroke Data Analysis",
      
      tabPanel("Heart Disease Prevalence by Work Type and Smoking Status",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("smokingStatusFilter", "Select Smoking Status:", 
                                      choices = unique(data$'Smoking Status'), 
                                      selected = unique(data$'Smoking Status'))
                 ),
                 mainPanel(
                   plotOutput("heartDiseaseWorkTypePlot")
                 )
               )
      ),
      
      tabPanel("Glucose Level by Dietary Habits",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("dietFilter", "Select Diets:", 
                                      choices = unique(data$'Dietary Habits'), 
                                      selected = unique(data$'Dietary Habits'))
                 ),
                 mainPanel(
                   plotOutput("glucoseDietPlot")
                 )
               )
      ),
      
      tabPanel("Symptom Occurrence by Age Group in Stroke Patients",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("ageRangeSymptom", "Select Age Range for Stroke Patients:", 
                               min = min(data$Age), max = max(data$Age), 
                               value = c(min(data$Age), max(data$Age)))
                 ),
                 mainPanel(
                   plotOutput("symptomAgePlot")
                 )
               )
      ),
      
      tabPanel("Stress Levels Distribution by Work Type",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("workTypeFilter", "Select Work Type:", 
                               choices = unique(data$`Work Type`))
                 ),
                 mainPanel(
                   plotOutput("stressWorkTypePlot")
                 )
               )
      ),
      
      tabPanel("3D Scatter Plot: Stroke Risk by Glucose Level, BMI, and Age",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("age3DRange", "Select Age Range:", 
                               min = min(data$Age), max = max(data$Age), 
                               value = c(min(data$Age), max(data$Age)))
                 ),
                 mainPanel(
                   plotlyOutput("scatter3DPlot")
                 )
               )
      ),
      
      tabPanel("Stress Level Distribution Across Age Groups",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("ageGroupWidth", "Age Group Width:", 
                               min = 5, max = 20, value = 10)
                 ),
                 mainPanel(
                   plotOutput("stressAgeGroupPlot")
                 )
               )
      ),
      
      tabPanel("Alcohol Intake and Family History of Stroke",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("ageRangeAlcohol", "Select Age Range:", 
                               min = min(data$Age), max = max(data$Age), 
                               value = c(min(data$Age), max(data$Age))),
                   radioButtons("familyStrokeHistory", "Family History of Stroke:",
                                choices = c("Yes", "No"),
                                selected = "Yes")
                 ),
                 mainPanel(
                   plotOutput("alcoholFamilyHistoryPlot")
                 )
               )
      ),
      
      tabPanel("Average Glucose Level Distribution",
               sidebarLayout(
                 sidebarPanel(
                   sliderInput("binWidth", "Select Bin Width:", 
                               min = 1, max = 10, value = 5),
                   checkboxGroupInput("genderFilter", "Select Gender:", 
                                      choices = c("Male", "Female"),
                                      selected = c("Male", "Female"))
                 ),
                 mainPanel(
                   plotOutput("glucoseLevelDistributionPlot")
                 )
               )
      ),
      
      tabPanel("BMI by Physical Activity and Age Group",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("physicalActivitySelection", "Physical Activity Level:",
                               choices = unique(data$`Physical Activity`)),
                   checkboxGroupInput("ageGroupFilter", "Select Age Groups:", 
                                      choices = c("0-30", "31-60", "60+"),
                                      selected = c("0-30", "31-60", "60+"))
                 ),
                 mainPanel(
                   plotOutput("bmiPhysicalActivityPlot")
                 )
               )
      ),
      
      tabPanel("Stroke Occurrences by Residence Type",
                 mainPanel(
                   plotOutput("StrokeResidence")
                 )
      ),
   
      tabPanel("Marital status and the incidence of strokes",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("marriedStatusFilter", "Select Martial Status:", 
                                      choices = unique(data$'Marital Status'), 
                                      selected = unique(data$'Marital Status'))
                 ),
                 mainPanel(
                   plotOutput("marriedStatusStroke")
                 )
               )
      ),
      
      tabPanel("Relationship between Work Type and Age by Diagnosis",
                 mainPanel(
                   plotOutput("WorkAgeDiagnosisPlot")
                 )
      ),
      tabPanel("Age Distribution by Stroke Diagnosis",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("diagnosisStatusFilter", "Select Diagnosis Status:", 
                                      choices = unique(data$'Diagnosis'), 
                                      selected = unique(data$'Diagnosis'))
                 ),
                 mainPanel(
                   plotOutput("AgeStrokeDiagnosis")
                 )
               )
      )
      
)
