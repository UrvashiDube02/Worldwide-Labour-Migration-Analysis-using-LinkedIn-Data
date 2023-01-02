choices = unique(countryMigration$base_country_name)

predictiontab <- tabPanel(
  "Prediction",
  
  HTML("<br><br><br>"),
  
  fluidRow(
    column(12,align="center",
      fluidRow(
          selectizeInput(
            inputId = "search2",
            label = "Please select a base Country",
            multiple = FALSE,
            choices = c("Search Bar" = "", choices),
            options = list(
              create = FALSE,
              placeholder = "Enter country name",
              maxItems = '1',
              onDropdownOpen = I("function($dropdown) {if (!this.lastQuery.length) {this.close(); this.settings.openOnFocus = false;}}"),
              onType = I("function (str) {if (str === \"\") {this.close();}}")
            )
          ),
        
        
      ) 
    
    )
  ),
  
  
  HTML("<br><br>"),
  
  fluidRow(
    column(12,
           align="center",
           h3("Country Migration Prediction"),
           plotlyOutput("rf31"),
    ),
  ),
  
  HTML("<br><br>"),
  
  fluidRow(
    column(12,
      align="center",
      h3("Industry Migration Prediction"),
      plotlyOutput("rf32")
    ),
  ),
  
  HTML("<br><br>"),
  
  fluidRow(
    column(12,
           align="center",
           h3("Skill Migration Prediction"),
           plotlyOutput("rf33")
    ),
  ),
  
  HTML("<br><br>"),
  
  
)


