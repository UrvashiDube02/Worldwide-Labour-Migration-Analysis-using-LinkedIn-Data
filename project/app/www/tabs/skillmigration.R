categories = unique(skillMigration$skill_group_category)

skillTab <- tabPanel("Skill Migration",
         
         sidebarPanel(
           width = 3,
           selectInput(
             inputId = "selectSkillAttribute",
             label = "Pick a Skill Group Category",
             choices = categories
           ),
           uiOutput("skillGroupSelect"),
           
           actionButton(
             inputId = "reset",
             label = "Reset Data",
             icon = icon("refresh"),
             width = "100%"
           ),
           
         ),
         
         
         mainPanel(
           fluidRow(
           
                  plotlyOutput(
                    "histogram",
                    width = "100%",
                    height = "300px"
                  )      
         ),
         
         HTML("<br><br>"),
         
         fluidRow(
           column(6,
                  plotlyOutput(
                    "rf11",
                    width = "100%",
                    height = "300px"
                  )) ,
           
           column(6,
                  plotlyOutput(
                    "rf1",
                    width = "100%",
                    height = "300px"
                  )) 
         ),
         
         )
)