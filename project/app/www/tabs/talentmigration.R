choices = unique(countryMigration$base_country_name)

talentTab <- tabPanel(
  "Talent Migration",
  
  HTML("<br><br><br>"),
  
  fluidRow(
    column(12,align="center",
      fluidRow(
          selectizeInput(
            inputId = "search1",
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
  
  leafletOutput("map"),
  
  HTML("<br><br>"),
  
  fluidRow(
    column(6,
           align="center",
           plotlyOutput("rf22"),
    ),
    column(6,
           align="center",
           plotlyOutput("rf2"),
    ),
  ),
  
  
  HTML("<br><br>"),
  
  fluidRow(
    column(6,
      align="center",
      tableOutput("countrygain")
    ),
    column(6,
      align="center",
      tableOutput("countryloss")
    ),
  ),
  
  HTML("<br><br>"),
  
  fluidRow(
    column(6,
           align="center",
           tableOutput("industrygain")
    ),
    column(6,
           align="center",
           tableOutput("industryloss")
    ),
  ),
  
  HTML("<br><br>"),
  
  fluidRow(
    column(6,
           align="center",
           tableOutput("skillgain")
    ),
    column(6,
           align="center",
           tableOutput("skillloss")
    ),
  )
  
)


