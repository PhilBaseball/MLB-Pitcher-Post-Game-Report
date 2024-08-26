#install.packages("shiny")
library(shiny)

library(tidyverse)
#install.packages("dplyr")
library(dplyr)

#install.packages("DT")
library(DT)

#install.packages("ggplot2")
library(ggplot2)
library(baseballr)
#install.packages('tidyverse')
library(tidyverse)

## MLB ##

#Choose Date #
x <- map_df(.x = seq.Date(as.Date('2024-08-24'), 
                          as.Date('2024-08-24'), 
                          'day'), 
            ~get_game_pks_mlb(date = .x, 
                              level_ids = c(1))
)


safe_mlb <- safely(get_pbp_mlb)

# filter the game files for only those games that were completed and pull the game_pk as a numeric vector
Game <- map(.x = x %>%
            filter(status.codedGameState == "F") %>% 
            pull(game_pk), 
          ~safe_mlb(game_pk = .x)) %>%
  map('result') %>%
  bind_rows()

Game<- Game %>% filter(matchup.pitcher.fullName == "Bowden Francis")

Game$Pitcher<- Game$matchup.pitcher.fullName 
Game$Date<- Game$game_date
Game<- subset(Game, !is.na(details.type.code))
Game$PlayResult<- ifelse(Game$details.isInPlay == TRUE & Game$result.event == "Single" , "Single",
                  ifelse(Game$details.isInPlay == TRUE & Game$result.event == "Double" , "Double",
                  ifelse(Game$details.isInPlay == TRUE & Game$result.event == "Triple" , "Triple",
                  ifelse(Game$details.isInPlay == TRUE & Game$result.event == "Home Run" , "Home Run",
                  ifelse(Game$details.isInPlay == TRUE & Game$result.event %in% c("Groundout", "Double Play","Lineout","Pop Out","Flyout","Grounded Into DP","Field Error","Forceout","Bunt Pop Out","Fielders Choice Out") , "Out",
                  ifelse(Game$details.isInPlay == TRUE & Game$result.event == "Sac Fly" , "Sacrifice",
                  ifelse(Game$details.call.description %in% c("Swinging Strike" , "Called Strike", "Swinging Strike (Blocked)","Foul Tip") & Game$details.isOut == TRUE,"Strikeout",
                  #ifelse(Game$last.pitch.of.ab == "true" & Game$result.event == "Strikeout" , "Strikeout",
                  #ifelse(Game$result.event == "Walk" & Game$last.pitch.of.ab == "true", "Walk" ,NA
                  ifelse(Game$result.event == "Walk" & Game$count.balls.start == 4, "Walk" ,NA))))))))

Game$Outs<- ifelse(Game$details.isOut == TRUE & Game$result.event %in% c("Groundout","Lineout","Pop Out","Flyout","Forceout","Bunt Pop Out","Fielders Choice Out","Sac Fly", "Strikeout"), 1 ,
            ifelse(Game$details.isOut == TRUE & Game$result.event %in% c("Double Play" , "Grounded Into DP" ), 2 ,
            ifelse(Game$details.isInPlay == TRUE & Game$result.event %in% c("Single", "Double", "Triple", "Home Run", "Field Error"),0, NA
                   )))

Game$HardHit<- ifelse(Game$details.isInPlay == TRUE & Game$hitData.launchSpeed > 95 , 1, 0)

#Game$Count<- paste(Game$count.balls.end , Game$count.strikes.end, sep = "-")
Left <- -8.5/12
Right <- 8.5/12
Bottom <- 18.29/12
Top <- 44.08/12

# This is to Make a 3x3 Strike Zone (Vertical and Horizontal Lines in Zone)
Width <- (Right - Left) / 3
Height <- (Top - Bottom) / 3
#Data<-  df %>% filter(matchup.pitcher.fullName == "Brady Singer")

ui <- fluidPage(
  column(10, offset = 1,
         titlePanel("Pitcher Post-Game Report"),
         hr(style="border-color: black;"), 
         fluidRow(
           column(2, selectInput(inputId = "PitcherInput", label = "Select Pitcher", choices = sort(unique(Game$Pitcher)))),
           column(2, selectInput(inputId = "GameInput", label = "Select Game", choices = "")),
           column(2, selectInput(inputId = "SideInput", label = "Select Batter Handeness", choices = c("All" , sort(unique(Game$matchup.splits.pitcher))))),
           
         ),
         hr(style="border-color: black;"),
         wellPanel(style = "background: white; border-color:black; border-width:2px",
                   fluidRow(
                     #column(2, img(src = "C:/RStudioProjects/AthleteLab/mlb.png", height = "100%", width = "100%", 
                      #             style = "display: block; margin-left: auto; margin-right: auto;", height = 150, width = 150)),
                     column(4, h2(strong(textOutput("selected_pitcher"))), hr(style="border-color: black;"), style = "padding-right:0px;"),
                     column(6, h2("Post-Game Report"), hr(style="border-color: black;"), h2(textOutput("selected_game")),  h2(textOutput("selected_Side")), align = "right", style = "padding-left:0px;")),
                   hr(style="border-color: black;"), 
                   fluidRow(column(10, offset = 1, h3(strong("Game Stats")), dataTableOutput("Stats"), align = "center")
                   ), br(), br(),br(), br(),
                  
                   fluidRow(column(10, offset = 1, h3(strong("Pitch Metrics")), dataTableOutput("pitcher_metrics"), align = "center")
                   ), br(), br(),br(), br(), 
                   fluidRow(
                     column(plotOutput("pitch_movement_plot"), align = "center" , width = 6, height = 24),
                     column(plotOutput("pitch_release"), align = "center" , width = 6, height = 24)),
                   br(), br(),br(),br(),br(),br(),br(), br(),
                   fluidRow(
                     column(10, offset = 1 ,h3(strong("Pitcher Summary Table")), dataTableOutput("pitcher_summary_table"), align = "center")),
                   fluidRow(column( plotOutput("pitch_velocity_plot"), align = "center", width = 6, height = 24),
                            column( plotOutput("pitch_usage_plot"), align = "center", width = 6, height = 24)),
                  br(),br(),br(),br(),br(), br(),
                   fluidRow(
                     column( plotOutput("pitch_location_plot2"), align = "center", width = 6, height = 24),
                     column( plotOutput("pitch_location_Situation"), align = "center", width = 6, height = 24),
                     
                   )  ,br(),br(),br(),br(),br(),br(),
                  fluidRow(
                    column( plotOutput("pitch_location_Player"), align = "center", width = 12, height = 48),
                    
                  )
                  ,br(), br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),br(),
         ), 
         p(em("If the contents of this page appear distorted, please decrease your web browser zoom to 80% or 90%."))
  )
)


server <- function(input, output, session) {
  
  observeEvent(input$PitcherInput, 
               updateSelectInput(session, inputId = "GameInput", label = "Select Game", 
                                 choices = sort(unique(Game$Date[Game$Pitcher == input$PitcherInput]))))
  
  output$selected_pitcher <- renderText({paste(input$PitcherInput)})
  
  output$selected_game <- renderText({paste(input$GameInput)})
  
  output$selected_Side <- renderText({paste(input$SideInput)})
  
  
  output$Stats <- renderDataTable({
    if (input$SideInput != "All"){
    table <- Game %>%
      filter(Pitcher == input$PitcherInput, Date == input$GameInput, matchup.splits.pitcher == input$SideInput) %>%
      summarise("Pitches" = n(),
                'IP' = round((sum(Outs, na.rm = T))/3, 1),
                'BF' = n_distinct(about.inning, matchup.batter.fullName, about.atBatIndex),
                'K' = sum(PlayResult =="Strikeout" , na.rm = T),
                'BB' = sum(PlayResult =="Walk", na.rm = T),
                #'HBP' = sum(result.event == "Hit By Pitch", na.rm = T),
                'BIP' = sum(details.isInPlay == TRUE, na.rm = T) ,
                'H' = sum(PlayResult %in% c('Single','Double','Triple','Home Run'), na.rm = T),
                'XBH' = sum(PlayResult %in% c('Double','Triple','Home Run'), na.rm = T),
                #'ER' = sum( result.rbi[details.description == "In play, run(s)"], na.rm = TRUE),
                "HardHit %" = round(sum(HardHit, na.rm = T) / sum(details.isInPlay == TRUE, na.rm = T) * 100, 1) ,
                "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                "Whiff" = sum(details.description %in% c("Swinging Strike", "Swinging Strike (Blocked)" ), na.rm = TRUE),
                "CSW%" = round(sum(details.description %in% c("Swinging Strike", "Swinging Strike (Blocked)","Called Strike" ))/ n() , 3)*100

      ) } else {
        
        table <- Game %>%
          filter(Pitcher == input$PitcherInput, Date == input$GameInput) %>%
          summarise("Pitches" = n(),
                    'IP' = round(sum(Outs,na.rm = T)/3, 1),
                    'BF' = n_distinct(about.inning, matchup.batter.fullName, about.atBatIndex),
                    'K' = sum(PlayResult =="Strikeout" , na.rm = T),
                    "BB" = sum(PlayResult =="Walk", na.rm = T),
                    #'HBP' = sum(result.event == "Hit By Pitch", na.rm = T),
                    'BIP' = sum(details.isInPlay == TRUE, na.rm = T) ,
                    'H' = sum(PlayResult %in% c('Single','Double','Triple','Home Run'), na.rm = T),
                    'XBH' = sum(PlayResult %in% c('Double','Triple','Home Run'), na.rm = T),
                    #'ER' = sum( result.rbi[details.description == "In play, run(s)"], na.rm = TRUE),
                    "HardHit %" = round(sum(HardHit, na.rm = T) / sum(details.isInPlay == TRUE, na.rm = T) * 100, 1) ,
                    "GB%" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                    "Whiff" = sum(details.description %in% c("Swinging Strike", "Swinging Strike (Blocked)" ), na.rm = TRUE),
                    "CSW%" = round(sum(details.description %in% c("Swinging Strike", "Swinging Strike (Blocked)","Called Strike" ))/ n() , 3)*100
                    
          ) 
        
      }
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
    
  })
  
  
 
  
  output$pitcher_summary_table <- renderDataTable({
    
    if (input$SideInput != "All"){
    table <- Game %>%
      filter(Pitcher == input$PitcherInput, Date == input$GameInput, matchup.splits.pitcher == input$SideInput) %>%
      group_by('Pitch' = details.type.description) %>%
                       dplyr::summarize('No' = n(),
                                        'Usage' = n(),
                                        'Usage %' = n(),
                                        'AVG Exit Velo' = round(mean(hitData.launchSpeed, na.rm = TRUE),1),
                                        "GB_pct" = round(100* sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T),0),
                                        "AVG" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run"))/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),3),
                                        "SLUG" = round((sum(PlayResult == "Single", na.rm=T) + 2* sum(PlayResult == "Double", na.rm=T)+ 3* sum(PlayResult == "Triple", na.rm=T) + 4* sum(PlayResult == "Home Run", na.rm=T) )/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout", "Out" )),3),
                                        # "XSLUG" = round(sum(XSLUG, na.rm=T)/sum(AB, na.rm = T),3),
                                       # "HardHit_pct" = round(sum(HardHit, na.rm = T)/sum(details.call.description == "InPlay", na.rm = T),3)*100,
                                        "Swing_pct" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                 "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),2)*100,
                                        'Whiff_pct' = round(sum(details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))/
                                                              sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                   "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),3)*100,
                                      #  "Chase_pct" = round(mean(Chase, na.rm = TRUE),2)*100,
                                        "Strike_pct" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip", "InPlay", "Called Strike"))/n(),3)*100,
                                        "CSW_pct" = round(sum(details.call.description %in% c("Swinging Strike", "Swinging Strike (Blocked)","Called Strike")) / n() , 3)
                                        
      )  %>%
        mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
        dplyr::select(Pitch,No,'Usage %', Whiff_pct, CSW_pct,Strike_pct, AVG, SLUG,'AVG Exit Velo', "GB_pct" )
      
    
    table <- table[order(table$'No',decreasing = TRUE),]
    
    } else{
      table <- Game %>%
        filter(Pitcher == input$PitcherInput, Date == input$GameInput) %>%
        group_by('Pitch' = details.type.description) %>%
        dplyr::summarize('No' = n(),
                         'Usage' = n(),
                         'Usage %' = n(),
                         'AVG Exit Velo' = round(mean(hitData.launchSpeed, na.rm = TRUE),1),
                         "GB_pct" = round(sum(hitData.trajectory == "ground_ball" , na.rm = T) / sum(hitData.trajectory %in% c("line_drive","fly_ball","ground_ball","popup","bunt_popup"), na.rm = T)*100,0),
                         "AVG" = round(sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run"))/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout","Out" )),3),
                         "SLUG" = round((sum(PlayResult == "Single", na.rm=T) + 2* sum(PlayResult == "Double", na.rm=T)+ 3* sum(PlayResult == "Triple", na.rm=T) + 4* sum(PlayResult == "Home Run", na.rm=T) )/sum(PlayResult %in% c("Single", "Double", "Triple", "Home Run","Strikeout", "Out" )),3),
                         # "XSLUG" = round(sum(XSLUG, na.rm=T)/sum(AB, na.rm = T),3),
                         # "HardHit_pct" = round(sum(HardHit, na.rm = T)/sum(details.call.description == "InPlay", na.rm = T),3)*100,
                         "Swing_pct" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                 "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),2)*100,
                         'Whiff_pct' = round(sum(details.call.description %in% c("Swinging Strike","Swinging Strike (Blocked)"))/
                                               sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip",
                                                                                   "In play, out(s)","In play, no out","In play, run(s)","Swinging Strike (Blocked)")),3)*100,
                         #  "Chase_pct" = round(mean(Chase, na.rm = TRUE),2)*100,
                         "Strike_pct" = round(sum(details.call.description %in% c("Swinging Strike", "Foul", "Foul Tip", "InPlay", "Called Strike"))/n(),3)*100,
                         "CSW_pct" = round(sum(details.call.description %in% c("Swinging Strike", "Swinging Strike (Blocked)","Called Strike")) / n() , 3)
                         
        )  %>%
        mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>%
        dplyr::select(Pitch,No,'Usage %', Whiff_pct, CSW_pct,Strike_pct, AVG, SLUG,'AVG Exit Velo', "GB_pct" )
      
      
      table <- table[order(table$'No',decreasing = TRUE),]
      
      
    }
    
    
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
 
    
  
  output$pitcher_metrics <- renderDataTable({
    
    if (input$SideInput != "All"){
    table <- Game %>%
      filter(Pitcher == input$PitcherInput, Date == input$GameInput, matchup.splits.pitcher == input$SideInput) %>%
      group_by('Pitch' = details.type.description) %>%
      dplyr::summarize('No.' = n(),
                       'Usage' = n(),
                       'Usage %' = n(),
                       'Velo' = round(mean(pitchData.startSpeed, na.rm = TRUE),1),
                       'VeloMax' = round(max(pitchData.startSpeed, na.rm = TRUE),1),
                       'Spin' = round(mean(pitchData.breaks.spinRate, na.rm = TRUE),0),
                       'Vert' = round(mean(pitchData.breaks.breakVerticalInduced, na.rm = TRUE),1),
                       'Horz' = round(mean(pitchData.breaks.breakHorizontal, na.rm = TRUE),1),
                       'RelHt' = round(mean(pitchData.coordinates.z0, na.rm = TRUE),1),
                       'RelSide' = round(mean(pitchData.coordinates.x0, na.rm = TRUE),1),
                       'Ext' = round(mean(pitchData.extension, na.rm = TRUE),1)
      ) %>%
      mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>% dplyr::select(-Usage)

    
    table <- table[order(table$'No.',decreasing = TRUE),]
    } else{
      table <- Game %>%
        filter(Pitcher == input$PitcherInput, Date == input$GameInput) %>%
        group_by('Pitch' = details.type.description) %>%
        dplyr::summarize('No.' = n(),
                         'Usage' = n(),
                         'Usage %' = n(),
                         'Velo' = round(mean(pitchData.startSpeed, na.rm = TRUE),1),
                         'VeloMax' = round(max(pitchData.startSpeed, na.rm = TRUE),1),
                         'Spin' = round(mean(pitchData.breaks.spinRate, na.rm = TRUE),0),
                         'Vert' = round(mean(pitchData.breaks.breakVerticalInduced, na.rm = TRUE),1),
                         'Horz' = round(mean(pitchData.breaks.breakHorizontal, na.rm = TRUE),1),
                         'RelHt' = round(mean(pitchData.coordinates.z0, na.rm = TRUE),1),
                         'RelSide' = round(mean(pitchData.coordinates.x0, na.rm = TRUE),1),
                         'Ext' = round(mean(pitchData.extension, na.rm = TRUE),1)
        ) %>%
        mutate(`Usage %` = round(`Usage %`/sum(`Usage %`),3)*100) %>% dplyr::select(-Usage)
      
      
      table <- table[order(table$'No.',decreasing = TRUE),]
    }
    
    tableFilter <- reactive({table})
    datatable(tableFilter(), options = list(dom = 't', columnDefs = list(list(targets = 0, visible = FALSE)))) %>%
      formatStyle(c(1,2), `border-left` = "solid 1px") %>% formatStyle(c(2,5,7), `border-right` = "solid 1px")
  })
  
  
  output$pitch_movement_plot <- renderPlot({
    
    if (input$SideInput != "All") {
    dataFilter <- reactive({
      Game %>%
        filter(Pitcher == input$PitcherInput, Date == input$GameInput, matchup.splits.pitcher == input$SideInput)
    })
    ggplot(data = dataFilter(),
           aes(x = pitchData.breaks.breakHorizontal, y = pitchData.breaks.breakVerticalInduced, color = details.type.code)) +
      labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
      xlim(-25, 25) + ylim(-25, 25) +
      geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
      geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
      geom_point(size =4, alpha = .75) +
      # we manually set the pitch colors so that they are uniform across each plot and tables
      scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
      theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
      theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
    
    
    } 
    else {
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput, Date == input$GameInput)
      })
      ggplot(data = dataFilter(),
             aes(x = pitchData.breaks.breakHorizontal, y = pitchData.breaks.breakVerticalInduced, color = details.type.code)) +
        labs(title = "Pitch Movement" ,color = "",x = "Horizontal Break (in.)", y = "Induced Vertical Break (in.)" ) + 
        xlim(-25, 25) + ylim(-25, 25) +
        geom_segment(aes(x = 0, y = -25, xend = 0, yend = 25), size = 1, color = "grey55") +
        geom_segment(aes(x = -25, y = 0, xend = 25, yend = 0), size = 1, color = "grey55") +
        geom_point(size =4, alpha = .75) +
        # we manually set the pitch colors so that they are uniform across each plot and tables
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5)) +
        theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
      
      
    }
  }, width = 550, height = 550)
  
  
  output$pitch_release <- renderPlot({
    
    if (input$SideInput != "All") {
      
    dataFilter <- reactive({
      Game %>%
        filter(Pitcher == input$PitcherInput, Date == input$GameInput, matchup.splits.pitcher == input$SideInput )
    })
    ggplot(data = dataFilter(),
           aes(x = pitchData.coordinates.x0 * -1, y = pitchData.coordinates.z0, color = details.type.code)) +
      xlim(-5,5) + ylim(0,10) + labs(color = "", title = "Pitch Release") +
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      geom_point(aes(colour = factor(details.type.code)), size = 4) +
      scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
      theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
      theme(legend.text = element_text(size = 12), axis.title = element_blank())
 
    } else{
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput, Date == input$GameInput)
      })
      ggplot(data = dataFilter(),
             aes(x = pitchData.coordinates.x0 * -1, y = pitchData.coordinates.z0, color = details.type.code)) +
        xlim(-5,5) + ylim(0,10) + labs(color = "", title = "Pitch Release") +
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        geom_point(aes(colour = factor(details.type.code)), size = 4) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        theme_bw() + theme(plot.title = element_text(size = 14, face = "bold", hjust = 0.5)) +
        theme(legend.text = element_text(size = 12), axis.title = element_blank())
      
    }
    
     }, width = 550, height = 550)
  
  
  
  
  
  output$pitch_velocity_plot <- renderPlot({
    
    pvp_game <- Game %>%
      group_by(Pitcher, details.type.code,about.inning, Date) %>%
      summarise(Avg = mean(pitchData.startSpeed, na.rm = TRUE), Max = max(pitchData.startSpeed, na.rm = T), min = min(pitchData.startSpeed, na.rm = T)) %>%
      arrange(about.inning, desc(Max)) 
    
    #length(unique(pvp_game$Inning))
    
    dataFilter <- reactive({
      pvp_game %>%
        filter(Pitcher == input$PitcherInput, Date == input$GameInput) %>%
        group_by(details.type.code) %>%
        mutate(Inning = about.inning)
    })
    if(length(unique(pvp_game$about.inning)) >1) {
      ggplot(data = dataFilter(), aes(x = about.inning, y = Avg, color = details.type.code) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$about.inning)) +
        geom_line() +
        scale_x_continuous(labels = as.numeric(pvp_game$about.inning), breaks = pvp_game$about.inning) +
        # xlim(min(velo_inn$about.inning),max(velo_inn$about.inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        labs(title = "Velo by Inning (Not Splits Reactive)", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by about.inning") +
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
      
    } else{
      
      ggplot(data = dataFilter(), aes(x = about.inning, y = Avg, color = details.type.code) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$about.inning)) +
        #  geom_line() +
        scale_x_continuous(labels = as.numeric(pvp_game$about.inning), breaks = pvp_game$about.inning) +
        # xlim(min(velo_inn$about.inning),max(velo_inn$about.inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        labs(title = "Velo by Inning (Not Splits Reactive)", x = "Inning", y = "Pitch Velocity (MPH)", color = " " ) + #, title = "Pitch Velocity by about.inning") +
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
    }
  }, width = 450, height = 450)
  
  
  
  output$pitch_usage_plot <- renderPlot({
    
    pv_game <- Game %>%
      group_by(Pitcher, Date,  about.inning, details.type.code) %>%
      dplyr::summarize('No.' = n(),
                       'Usage' = n(),
                       "Usage_pct"= n(),
                       
      ) %>%
      mutate(`Usage_pct` = round(`Usage_pct`/sum(`Usage_pct`),3)*100) %>% dplyr::select(-Usage) %>%
      arrange(about.inning) 
    

    dataFilter <- reactive({
      pv_game %>%
        filter(Pitcher == input$PitcherInput, Date == input$GameInput) %>%
        group_by(details.type.code) #%>%
        #mutate(Inning = about.inning)
    })
    if(length(unique(pv_game$about.inning)) >1) {
      ggplot(data = dataFilter(), aes(x = about.inning, y = Usage_pct, color = details.type.code) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$about.inning)) +
        geom_line() +
        scale_x_continuous(labels = as.numeric(pv_game$about.inning), breaks = pv_game$about.inning) +
        # xlim(min(velo_inn$about.inning),max(velo_inn$about.inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        labs(title = "Usage % By Inning (Not Splits Reactive) ", x = "Inning", y = "Usage (%)", color = " " ) + #, title = "Pitch Velocity by about.inning") +
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
      
    } else{
      
      ggplot(data = dataFilter(), aes(x = about.inning, y = Usage_pct, color = details.type.code) ) +
        geom_point( size = 2, alpha = .75) +
        #   scale_x_date(date_labels = "%b %d %Y", breaks = unique(velo_inn$about.inning)) +
        #  geom_line() +
        scale_x_continuous(labels = as.numeric(pv_game$about.inning), breaks = pv_game$about.inning) +
        # xlim(min(velo_inn$about.inning),max(velo_inn$about.inning) ) + #ylim(0,5) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        labs(title = "Usage % by Inning (Not Splits Reactive) ", x = "Inning", y = "Usage (%)", color = " " ) + #, title = "Pitch Velocity by about.inning") +
        theme_bw() + theme(plot.title = element_text(size = 12, face = "bold", hjust = 0.5), axis.text = element_text(size = 8)) +
        theme(legend.text = element_text(size = 8), axis.title = element_text(size = 8))
    }
  }, width = 450, height = 450)
  
  
  
  output$pitch_location_plot2 <- renderPlot({
    
    
    if (input$SideInput != "All") {
      
    dataFilter <- reactive({
      Game %>%
        filter(Pitcher == input$PitcherInput,
               Date == input$GameInput,
               matchup.splits.pitcher == input$SideInput)
    }) 
    ggplot(data = dataFilter() , 
           aes(x = pitchData.coordinates.pX *(-1), y = pitchData.coordinates.pZ)) +
      xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot" ), subtitle = "Pitcher's View" )+
      #stat_density2d()+
      geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
      geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
      geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
      geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
      
      # Horizontal Lines (Bottom Inner, Top Inner)
      geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
      geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
      
      # Vertical Lines (Left Inner, Right Inner)
      geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
      geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
      #Plate
      geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
      geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
      geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
      geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
      geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
      geom_point(aes(colour = factor(details.type.code)), size = 4) +
      scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                    'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
      
      theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
      theme(axis.title = element_blank())  +
      theme(strip.text = element_text(size = 7, face = 'bold'),
            axis.text.x=element_blank(), #remove x axis labels
            axis.text.y=element_blank(),  #remove y axis labels
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank()) 
    } else{
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput,
                 Date == input$GameInput)
      }) 
      ggplot(data = dataFilter() , 
             aes(x = pitchData.coordinates.pX *(-1), y = pitchData.coordinates.pZ)) +
        xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot" ), subtitle = "Pitcher's View" )+
        #stat_density2d()+
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        #Plate
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        geom_point(aes(colour = factor(details.type.code)), size = 4) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        
        theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
        theme(axis.title = element_blank())  +
        theme(strip.text = element_text(size = 7, face = 'bold'),
              axis.text.x=element_blank(), #remove x axis labels
              axis.text.y=element_blank(),  #remove y axis labels
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())
    }
    
    
    }, width = 600, height = 450)
  
  
  output$pitch_location_Situation <- renderPlot({
    
    
    if (input$SideInput != "All") {
      
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput,
                 Date == input$GameInput,
                 matchup.splits.pitcher == input$SideInput)
      }) 
      ggplot(data = dataFilter() , 
             aes(x = pitchData.coordinates.pX *(-1), y = pitchData.coordinates.pZ)) +
        xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot By Situation" ), subtitle = "Pitcher's View" )+
        #stat_density2d()+
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        #Plate
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        geom_point(aes(colour = factor(details.type.code)), size = 4) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        
        theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
        theme(legend.position = "none" ,axis.title = element_blank())  +
        theme(strip.text = element_text(size = 7, face = 'bold'),
              axis.text.x=element_blank(), #remove x axis labels
              axis.text.y=element_blank(),  #remove y axis labels
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) + facet_wrap(~ matchup.splits.menOnBase)
    } else{
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput,
                 Date == input$GameInput)
      }) 
      ggplot(data = dataFilter() , 
             aes(x = pitchData.coordinates.pX *(-1), y = pitchData.coordinates.pZ)) +
        xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot By Situation" ), subtitle = "Pitcher's View" )+
        #stat_density2d()+
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        #Plate
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        geom_point(aes(colour = factor(details.type.code)), size = 4) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        
        theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
        theme(legend.position = "none" ,axis.title = element_blank())  +
        theme(strip.text = element_text(size = 7, face = 'bold'),
              axis.text.x=element_blank(), #remove x axis labels
              axis.text.y=element_blank(),  #remove y axis labels
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) + facet_wrap(~ matchup.splits.menOnBase)
    }
    
    
  }, width = 600, height = 450)
  
  
  output$pitch_location_Player <- renderPlot({
    
    
    if (input$SideInput != "All") {
      
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput,
                 Date == input$GameInput,
                 matchup.splits.pitcher == input$SideInput)
      }) 
      ggplot(data = dataFilter() , 
             aes(x = pitchData.coordinates.pX *(-1), y = pitchData.coordinates.pZ)) +
        xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot By Player" ), subtitle = "Pitcher's View" )+
        #stat_density2d()+
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        #Plate
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        geom_point(aes(colour = factor(details.type.code)), size = 4) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        
        theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
        theme(legend.position = "none" ,axis.title = element_blank())  +
        theme(strip.text = element_text(size = 7, face = 'bold'),
              axis.text.x=element_blank(), #remove x axis labels
              axis.text.y=element_blank(),  #remove y axis labels
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank()) + facet_wrap(~matchup.batter.fullName)
    } else{
      dataFilter <- reactive({
        Game %>%
          filter(Pitcher == input$PitcherInput,
                 Date == input$GameInput)
      }) 
      ggplot(data = dataFilter() , 
             aes(x = pitchData.coordinates.pX *(-1), y = pitchData.coordinates.pZ)) +
        xlim(-1.5,1.5) + ylim(0,4) + labs(color = "",title = paste("Pitch Plot By Player" ), subtitle = "Pitcher's View" )+
        #stat_density2d()+
        geom_segment(x = (Left), y = (Bottom), xend = (Right), yend = (Bottom)) +
        geom_segment(x = (Left), y = (Top), xend = (Right), yend = (Top)) +
        geom_segment(x = (Left), y = (Bottom), xend = (Left), yend = (Top)) +
        geom_segment(x = (Right), y = (Bottom), xend = (Right), yend = (Top)) +
        
        # Horizontal Lines (Bottom Inner, Top Inner)
        geom_segment(x = (Left), y = (Bottom + Height), xend = (Right), yend = (Bottom + Height)) +
        geom_segment(x = (Left), y = (Top - Height), xend = (Right), yend = (Top - Height)) +
        
        # Vertical Lines (Left Inner, Right Inner)
        geom_segment(x = (Left + Width), y = (Bottom), xend = (Left + Width), yend = (Top)) +
        geom_segment(x = (Right - Width), y = (Bottom), xend = (Right - Width), yend = (Top)) +
        #Plate
        geom_segment(x = (Left), y = (0), xend = (Right), yend = (0)) +
        geom_segment(x = (Left), y = (0), xend = (Left), yend = (4.25/12)) +
        geom_segment(x = (Left), y = (4.25/12), xend = (0), yend = (8.5/12)) +
        geom_segment(x = (Right), y = (4.25/12), xend = (Right), yend = (0)) +
        geom_segment(x = (0), y = (8.5/12), xend = (Right), yend = (4.25/12))+ 
        geom_point(aes(colour = factor(details.type.code)), size = 4) +
        scale_color_manual(values = c('FF' = 'red', 'CU' = 'darkgreen', 'SI' = '#f47b20',  'SL'='cornflowerblue',
                                      'FC' = 'gold',  'CH'='violet', 'FS' = 'black', "KC" = "pink", "ST" = "cyan", "SC"= "green", "KN"= "darkgrey", "SV"= "purple"))+
        
        theme_bw() + theme(plot.title = element_text(size = 11, face = "bold", hjust = 0.5)) +
        theme(legend.position = "none" ,axis.title = element_blank())  +
        theme(strip.text = element_text(size = 7, face = 'bold'),
              axis.text.x=element_blank(), #remove x axis labels
              axis.text.y=element_blank(),  #remove y axis labels
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank())+ facet_wrap(~matchup.batter.fullName)
    }
    
    
  }, width = 900, height = 600)
  
  
 
}

shinyApp(ui = ui, server = server)