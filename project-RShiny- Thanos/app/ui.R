source("www/tabs/skillmigration.R")
source("www/tabs/talentmigration.R")
source("www/tabs/home.R")
source("www/tabs/prediction.R")

ui <- fluidPage(theme = shinytheme("slate"),
  # titlePanel("Worldwide Labour Migration Analysis using LinkedIn Data", ),
  
  tabsetPanel(
    homeTab,
    
    skillTab,
    
    talentTab,
    
    predictiontab,
    
    tabPanel("About",
             mainPanel(
               img(src='./bg2.png', align = "center", width = "100%", height = "100%"),
               width = 12,
             )
    )
  ),
  # setBackgroundImage(
  #   src = "tabs/1.png"
  # ),
  )
