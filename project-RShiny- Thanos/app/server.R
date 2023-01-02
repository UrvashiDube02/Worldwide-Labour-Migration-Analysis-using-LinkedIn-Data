library(tidyverse)
server <- function(input, output, session) {
  
  dataValues <- reactiveValues(tbl = NULL, obsList = NULL, obs1=NULL, skillCategory = NULL, skillGroups = NULL)
  
  observeEvent(input$selectSkillAttribute, {
    dataValues$skillCategory <- input$selectSkillAttribute
    dataValues$skillGroups <- skillMigration %>% filter(skillMigration$skill_group_category == dataValues$skillCategory)
    output$skillGroupSelect <- renderUI({
      selectInput(
        inputId = "selectSkillGroup",
        label = "Pick a Skill Group",
        choices = unique(dataValues$skillGroups[,"skill_group_name"])
      )
    })
  })
  
  observeEvent(input$selectSkillGroup, {
    output$histogram <- renderPlotly({
      shiny::validate(need(input$selectSkillGroup,""))
      
      a = skillMigrationClean %>% 
        filter(skillMigrationClean$skill_group_category == dataValues$skillCategory & skillMigrationClean$skill_group_name == input$selectSkillGroup)
      b=ggplot(a,aes(x=skill_group_name,y=value,fill = as.factor(year))) + 
        geom_bar(position = "dodge",stat="identity") + 
        coord_flip()
      ggplotly(b)
    })
  })
  
  observeEvent(input$search1,{
    print(input$search1[1])
    if(!is.na(input$search1) & input$search1 != ""){
      base_country = input$search1
      
      base_country_lat <- countryMigration %>%  filter(base_country_name %in% base_country) 
      base_country_lat <- base_country_lat$base_lat[1]
      
      base_country_lon <- countryMigration %>%  filter(base_country_name %in% base_country) 
      base_country_lon <- base_country_lon$base_long[1]
      
      positive <- talent_graph %>% filter(base_country_name %in% base_country) %>% filter(value > 0)
      names1 <- summarise_at(group_by(positive,target_country_name), vars(value), funs(mean(.,na.rm=TRUE))) %>% arrange(desc(value))
      names1<-arrange(names1,desc(value))[1:5,] %>% subset(select = target_country_name)
      positive <- positive %>% filter(positive$target_country_name %in% names1$target_country_name)
      
      negative <- talent_graph %>% filter(base_country_name %in% input$search1) %>% filter(value < 0)
      names2 <- summarise_at(group_by(negative,target_country_name), vars(value), funs(mean(.,na.rm=TRUE))) %>% arrange(desc(value))
      names2<-arrange(names2,value)[1:5,] %>% subset(select = target_country_name)
      negative <- negative %>% filter(negative$target_country_name %in% names2$target_country_name)
      
      Country_positive <- summarise_at(group_by(positive, target_country_name), vars(value), funs(mean(.,na.rm=TRUE)))
      Country_negative <- summarise_at(group_by(negative, target_country_name), vars(value), funs(mean(.,na.rm=TRUE)))
      
      industry_table <- subset(industryMigration, select = c(industry_name, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019))
      
      industry_table$industry_Value <- round((industry_table$net_per_10K_2017 + industry_table$net_per_10K_2018 + industry_table$net_per_10K_2019)/3, digits = 2)
      
      industry_table <- subset(industry_table, select = c(industry_name, industry_Value))
      
      industry_table <- aggregate(industry_table$industry_Value, by=list(industry_name=industry_table$industry_name), FUN=min)
      industry_table <- industry_table %>%  rename("industry_Value" = "x")
      
      industry_table <- filter(industry_table, industry_Value > -440)
      positive_industry <- industry_table %>% filter(industry_Value > 40)
      
      positive_industry <- positive_industry[order(positive_industry$industry_Value),][1:5,]
      positive_industry <- na.omit(positive_industry)
      
      negative_industry <- industry_table %>% filter(industry_Value < 0) 
      negative_industry <- negative_industry[order(negative_industry$industry_Value),][1:5,]
      negative_industry <- na.omit(negative_industry)
      
      skill_table <- subset(skillMigration, select = c(skill_group_name, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019))
      skill_table$skill_Value <- round((skill_table$net_per_10K_2017 + skill_table$net_per_10K_2018 + skill_table$net_per_10K_2019)/3, digits = 2)
      skill_table <- subset(skill_table, select = c(skill_group_name, skill_Value))
      skill_table <- skill_table %>%  filter(skill_Value > -400)
      skill_table <- aggregate(skill_table$skill_Value, by=list(skill_name=skill_table$skill_group_name), FUN=min)
      skill_table <- skill_table %>%  rename("skill_Value" = "x")
      
      positive_skill <- skill_table %>% filter(skill_Value > 20) 
      positive_skill <- positive_skill[order(positive_skill$skill_Value),][1:5,]
      positive_skill <- na.omit(positive_skill)
      
      negative_skill <- skill_table %>% filter(skill_Value < 0) 
      negative_skill <- negative_skill[order(negative_skill$skill_Value),][1:5,]
      negative_skill <- na.omit(negative_skill)
      
      
      output$map <- renderLeaflet({
        leaflet(data = mapStates,height = "100%") %>% addTiles() %>% addProviderTiles("Esri.WorldImagery") %>% 
          addPolygons(fillColor = topo.colors(10, alpha = NULL), stroke = FALSE) %>% 
          fitBounds(~-100,-60,~60,70) %>%
          setView(19.08, 60.25, zoom = 2) %>% 
          addLayersControl(
            position = "bottomright",
            overlayGroups = c("Positive", "Negative"),
            options = layersControlOptions(collapsed = FALSE)) %>% 
          addAwesomeMarkers(lng=base_country_lon, lat=base_country_lat, popup=base_country, icon=makeAwesomeIcon( markerColor = 'black') ) %>% 
          addCircleMarkers(data = positive, lat = ~ target_lat, lng = ~ target_long, weight = 1, radius = 7, 
                           fillOpacity = 0.2, color = "#89fc00", group = "Positive",
                           label = sprintf("<strong>%s</strong><br/>Talent Migration Rate: <br/>%g", positive$target_country_name, positive$value) %>% lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px", direction = "auto")) %>% 
          addCircleMarkers(data = negative, lat = ~ target_lat, lng = ~ target_long, weight = 1, radius = 7, 
                           fillOpacity = 0.2, color = "red", group = "Negative",
                           label = sprintf("<strong>%s</strong><br/>Talent Migration Rate: <br/>%g", negative$target_country_name, negative$value) %>% lapply(htmltools::HTML),
                           labelOptions = labelOptions(
                             style = list("font-weight" = "normal", padding = "3px 8px"),
                             textsize = "15px", direction = "auto"))
      })
      
      
      output$countrygain <- renderTable(
        {Country_positive}
      )
      output$countryloss <- renderTable(
        {Country_negative}
      )
      output$industrygain <- renderTable(
        {negative_industry}
      )
      output$industryloss <- renderTable(
        {positive_industry}
      )
      output$skillgain <- renderTable(
        {positive_skill}
      )
      output$skillloss <- renderTable(
        {negative_skill}
      )
    }
  })
  
  
  ############################################################################
  
  # ######### skill migration 2019 ######
  # df3 <- read.csv("www/data/skill_migration.csv")
  # df3 <- aggregate(df3$net_per_10K_2019, by=list(Region = df3$wb_region), FUN=mean)
  # df3 <- df3 %>%  rename("net_per_10K_2019" = "x")
  # df3$Group<-ifelse(df3$net_per_10K_2019>0,"A","B")
  # 
  
  
  ######## skill migration 2023 #######

  country_wise_net_per_year_rf_skill = skillMigration

  country_wise_net_per_year_rf_skill <- country_wise_net_per_year_rf_skill %>% group_by(country_name, wb_region) %>% summarise(net_per_10K_2015 = sum(net_per_10K_2015), net_per_10K_2016 = sum(net_per_10K_2016), net_per_10K_2017 = sum(net_per_10K_2017), net_per_10K_2018 = sum(net_per_10K_2018), net_per_10K_2019 = sum(net_per_10K_2019))

  #head(country_wise_net_per_year_rf_skill)

  skillmigrationmodel <- readRDS("www/skillmigration.rds")

  colnames(country_wise_net_per_year_rf_skill)[4] = "net_per_10K_2015"
  colnames(country_wise_net_per_year_rf_skill)[5] = "net_per_10K_2016"
  colnames(country_wise_net_per_year_rf_skill)[6] = "net_per_10K_2017"
  colnames(country_wise_net_per_year_rf_skill)[7] = "net_per_10K_2018"

  #head(country_wise_net_per_year_rf_skill)

  country_wise_net_per_year_rf_skill$net_per_10K_2023 = predict(skillmigrationmodel, newdata = country_wise_net_per_year_rf_skill %>% select(4, 5, 6, 7))

  #head(country_wise_net_per_year_rf_skill)

  colnames(country_wise_net_per_year_rf_skill)[4] = "net_per_10K_2016"
  colnames(country_wise_net_per_year_rf_skill)[5] = "net_per_10K_2017"
  colnames(country_wise_net_per_year_rf_skill)[6] = "net_per_10K_2018"
  colnames(country_wise_net_per_year_rf_skill)[7] = "net_per_10K_2019"

  #head(country_wise_net_per_year_rf_skill)

  df3 <- country_wise_net_per_year_rf_skill
  df3 <- aggregate(df3$net_per_10K_2023, by=list(Region = df3$wb_region), FUN=mean)
  df3 <- df3 %>%  rename("net_per_10K_2023" = "x")
  df3$Group<-ifelse(df3$net_per_10K_2023>0,"A","B")
  
  
  output$rf1 <- renderPlotly(
    ggplotly(
      ggplotly(ggplot(df3,aes(x=Region,y=net_per_10K_2023,fill=Group))+geom_bar(stat="identity") + coord_flip() + 
                 scale_fill_manual(values=c("#A7C5EB", "#152D35"))
      )
    )
    
  )
  
  output$rf11 <- renderPlotly(
    ggplotly(
      ggplotly(ggplot(df3,aes(x=Region,y=net_per_10K_2023,fill=Group))+geom_bar(stat="identity") + coord_flip() + 
                 scale_fill_manual(values=c("#A7C5EB", "#152D35")) + ylab("net_per_10K_2019")
      )
    )
    
  )
  
  
  
  ###### country migration 2019 ###################
  df <- countryMigration
  df <- aggregate(df$net_per_10K_2019, by=list(base_country_wb_region = df$base_country_wb_region), FUN=mean)
  df <- df %>%  rename("net_per_10K_2019" = "x")
  df$Group<-ifelse(df$net_per_10K_2019>0,"A","B")
  
  output$rf22 <- renderPlotly(
    ggplotly(ggplot(df,aes(x=base_country_wb_region,y=net_per_10K_2019,fill=Group))+geom_bar(stat="identity") + 
               coord_flip() + scale_fill_manual(values=c("#A7C5EB", "#152D35")) )
               
  )
  
  
  ###### country migration 2023 #################
  
  #df = read.csv("www/data/country_migration.csv")
  country_wise_net_per_year_lr <- countryMigration %>% group_by(base_country_name, base_lat, base_long, base_country_wb_region) %>% summarise(net_per_10K_2015 = sum(net_per_10K_2015), net_per_10K_2016 = sum(net_per_10K_2016), net_per_10K_2017 = sum(net_per_10K_2017), net_per_10K_2018 = sum(net_per_10K_2018), net_per_10K_2019 = sum(net_per_10K_2019))
  # head(country_wise_net_per_year_lr)
  
  country_wise_net_per_year_rf <- country_wise_net_per_year_lr
  countrymigrationmodel <- readRDS("www/countrymigration.rds")
  
  #head(country_wise_net_per_year_rf)
  
  colnames(country_wise_net_per_year_rf)[6] = "net_per_10K_2015"
  colnames(country_wise_net_per_year_rf)[7] = "net_per_10K_2016"
  colnames(country_wise_net_per_year_rf)[8] = "net_per_10K_2017"
  colnames(country_wise_net_per_year_rf)[9] = "net_per_10K_2018"
  
  # head(country_wise_net_per_year_rf)
  
  country_wise_net_per_year_rf$net_per_10K_2023 = predict(countrymigrationmodel, newdata = country_wise_net_per_year_rf %>% select(6, 7, 8, 9))
  
  # head(country_wise_net_per_year_rf)
  
  colnames(country_wise_net_per_year_rf)[6] = "net_per_10K_2016"
  colnames(country_wise_net_per_year_rf)[7] = "net_per_10K_2017"
  colnames(country_wise_net_per_year_rf)[8] = "net_per_10K_2018"
  colnames(country_wise_net_per_year_rf)[9] = "net_per_10K_2019"
  for_predict <- country_wise_net_per_year_rf
  #head(country_wise_net_per_year_rf)
  
  
  #################
  
  country_wise_net_per_year_rf <- aggregate(country_wise_net_per_year_rf$net_per_10K_2023, by=list(base_country_wb_region = country_wise_net_per_year_rf$base_country_wb_region), FUN=mean)
  country_wise_net_per_year_rf <- country_wise_net_per_year_rf %>%  rename("net_per_10K_2023" = "x")
  
  country_wise_net_per_year_rf$Group<-ifelse(country_wise_net_per_year_rf$net_per_10K_2023>0,"A","B")
  
  
  output$rf2 <- renderPlotly(
    ggplotly(ggplot(country_wise_net_per_year_rf,aes(x=base_country_wb_region,y=net_per_10K_2023,fill=Group))+
               geom_bar(stat="identity") + coord_flip() + scale_fill_manual(values=c("#A7C5EB", "#152D35")))
  )
  
  
  
  ##########################################################################################
  #Industry Migration
  
  
  country_wise_net_per_year_rf_industry = industryMigration
  
  country_wise_net_per_year_rf_industry <- country_wise_net_per_year_rf_industry %>% group_by(country_name) %>% summarise(net_per_10K_2015 = sum(net_per_10K_2015), net_per_10K_2016 = sum(net_per_10K_2016), net_per_10K_2017 = sum(net_per_10K_2017), net_per_10K_2018 = sum(net_per_10K_2018), net_per_10K_2019 = sum(net_per_10K_2019))
  #head(country_wise_net_per_year_rf_industry)
  
  #head(country_wise_net_per_year_rf_industry)
  industrymigrationmodel <- readRDS("www/industrymigration.rds")
  
  colnames(country_wise_net_per_year_rf_industry)[3] = "net_per_10K_2015"
  colnames(country_wise_net_per_year_rf_industry)[4] = "net_per_10K_2016"
  colnames(country_wise_net_per_year_rf_industry)[5] = "net_per_10K_2017"
  colnames(country_wise_net_per_year_rf_industry)[6] = "net_per_10K_2018"
  
  #head(country_wise_net_per_year_rf_industry)
  
  country_wise_net_per_year_rf_industry$net_per_10K_2023 = predict(industrymigrationmodel, newdata = country_wise_net_per_year_rf_industry %>% select(3, 4, 5, 6))
  
  #head(country_wise_net_per_year_rf_industry)
  
  colnames(country_wise_net_per_year_rf_industry)[3] = "net_per_10K_2016"
  colnames(country_wise_net_per_year_rf_industry)[4] = "net_per_10K_2017"
  colnames(country_wise_net_per_year_rf_industry)[5] = "net_per_10K_2018"
  colnames(country_wise_net_per_year_rf_industry)[6] = "net_per_10K_2019"
  
  #head(country_wise_net_per_year_rf_industry)
  
  
  ####################################################################################################
  
  observeEvent(input$search2,{
    if(!is.na(input$search2) & input$search2 != ""){
      country_wise_net_rf_country <- for_predict %>% filter(base_country_name %in% input$search2)
      country_wise_net_rf_country <- country_wise_net_rf_country %>% gather(Years, Net_Per, c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))
      
      output$rf31 <- renderPlotly(
        ggplotly(ggplot(country_wise_net_rf_country, aes(x = Years, y = Net_Per, group = 1)) + geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")))
        
      )
      
      
      country_wise_net_rf_country_industry <- country_wise_net_per_year_rf_industry %>%filter(country_name %in% input$search2)
      country_wise_net_rf_country_industry <- country_wise_net_rf_country_industry %>% gather("Years", "Net_Per", c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))
      
      output$rf32 <- renderPlotly(
        ggplot(country_wise_net_rf_country_industry, aes(x = Years, y = Net_Per, group = 1)) + geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      )
      
      
      country_wise_net_rf_country_skill <- country_wise_net_per_year_rf_skill %>%filter(country_name %in% input$search2)
      
      country_wise_net_rf_country_skill <- country_wise_net_rf_country_skill %>% gather("Years", "Net_Per", c(net_per_10K_2016, net_per_10K_2017, net_per_10K_2018, net_per_10K_2019, net_per_10K_2023))
      
      output$rf33 <- renderPlotly(
        ggplot(country_wise_net_rf_country_skill, aes(x = Years, y = Net_Per, group = 1)) + 
          geom_line(color="red") + geom_point() + theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
      )
    }})
  
  
  
  
  
  
  
  observeEvent(input$reset, {
    values$tbl <- NULL
    output$obs1 <- NULL
  })
  
  output$aaa <- renderPrint({
    values$obs1
  })
}