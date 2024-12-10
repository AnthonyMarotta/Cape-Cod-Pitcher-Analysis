# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(shinythemes)
library(DT)
library(GeomMLBStadiums)
library(ggrepel)    


# Define UI
ui <- fluidPage(
  theme = shinytheme("cosmo"),  # Apply a theme
  tags$head(
    tags$style(HTML("
      .centered {
        text-align: center;
      }
      .custom-panel {
        border-radius: 15px;
        border: 1px solid #ccc;
        padding: 15px;
        background-color: #f9f9f9;
        box-shadow: 2px 2px 12px rgba(0, 0, 0, 0.1);
      }
      .shiny-output-error-validation {
        color: red;
      }
      .scroll-table {
        height: 400px;  /* Adjust height as needed */
        overflow-y: scroll;
      }
    "))
  ),
  titlePanel("Cape Cod Baseball League Pitching Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      class = "custom-panel",
      style = "text-align: center;",  # Center align content in sidebar
      selectInput("team", "Select Team", choices = NULL),
      selectInput("pitcher", "Select Pitcher", choices = NULL),
      tags$br(),  # Add a line break for spacing
      selectInput("pitchType", "Select Pitch Type", choices = NULL),  # Pitch type filter
      tags$br(),
      selectInput("batterSide", "Batter's Side", 
                  choices = c("All", "Left", "Right")),  # Add new input for Batter's Side
      tags$br(),
      dateRangeInput("dateRange", "Select Date Range",
                     start = "2024-06-01", end = Sys.Date()),  # Date range input
      tags$br(),
      tags$img(src = "https://images.ctfassets.net/iiozhi00a8lc/4852OqYux6VPlGNVkd78dI/718866b7153eda424e697d94a022a381/565.svg", 
               height = 200, width = 200, style = "display: block; margin: 0 auto;")  # Center the image
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pitch Location Map",
                 plotOutput("pitch_location_map", height = "400px", width = "400px")),
        tabPanel("Pitch Heatmap",
                 div(
                   plotOutput("pitch_heatmap", height = "475px", width = "350px"),
                   class = "scroll-table")),
        tabPanel("Release Point Chart",
                 plotOutput("release_chart", height = "400px", width = "400px")),
        tabPanel("Movement Chart",
                 plotOutput("movement_chart", height = "400px", width = "400px")),
        tabPanel("Hit Chart",
                 plotOutput("hit_chart", height = "400px", width = "400px")),
        tabPanel("Pitch Metrics",
                 div(DT::dataTableOutput("metrics_table"), class = "scroll-table")),  # Add tab for Metrics table
        tabPanel("All Pitches",
                 div(DT::dataTableOutput("all_pitches_table"), class = "scroll-table")),  # Add tab for All Pitches table
        tabPanel("Game Statistics",
                 div(DT::dataTableOutput("game_table"), class = "scroll-table")),
        tabPanel("Season Statistics",
                 div(DT::dataTableOutput("statistics_table"), class = "scroll-table")),
        tabPanel("Pitcher Hit Spray Chart",
                 plotOutput("pitcher_hit_chart", height = "400px", width = "400px"))# Add tab for calculated statistics
      ),
      class = "custom-panel"  # Move class attribute here
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  setwd("~/Downloads/CCBLRCodes")
  
  # Load the CSV data directly
  data <- reactive({
    read.csv("SeasonCCBL.csv")
  })
  
  # Mapping of original team names to new names
  team_name_mapping <- c("BRE_WHI" = "Brewster Whitecaps", "BOU_BRA" = "Bourne Braves", "CHA_ANG" = "Chatham Anglers", "YAR_RED" = "Yarmouth-Dennis Red Sox", "FAL_COM" = "Falmouth Commodores", "WAR_GAT" = "Wareham Gateman", "HAR_MAR" = "Harwich Mariners", "HYA_HAR" = "Hyannis Harbor Hawks", "ORL_FIR" = "Orleans Firebirds", "COT_KET" = "Cotuit Kettleers")
  
  # Update team choices based on data
  observe({
    if (!is.null(data())) {
      updated_data <- data() %>%
        mutate(PitcherTeam = ifelse(PitcherTeam %in% names(team_name_mapping), team_name_mapping[PitcherTeam], PitcherTeam))
      
      updateSelectInput(session, "team", choices = unique(updated_data$PitcherTeam))
    }
  })
  
  # Update pitcher and pitch type choices based on selected team
  observeEvent(input$team, {
    if (!is.null(data())) {
      filtered_data <- data() %>%
        mutate(PitcherTeam = ifelse(PitcherTeam %in% names(team_name_mapping), team_name_mapping[PitcherTeam], PitcherTeam)) %>%
        filter(PitcherTeam == input$team & !is.na(AutoPitchType) & AutoPitchType != "")
      
      # Combine Four-Seam and Sinker data under Fastball
      filtered_data <- filtered_data %>%
        mutate(AutoPitchType = ifelse(AutoPitchType == "Sinker" | AutoPitchType == "Four-Seam", "Fastball", AutoPitchType))
      
      # Update pitcher choices
      updateSelectInput(session, "pitcher", choices = unique(filtered_data$Pitcher))
      
      # Update pitch type choices
      updateSelectInput(session, "pitchType", choices = c("All", unique(filtered_data$AutoPitchType)))
      
      # Assign filtered data to reactive value
      assign("filtered_data", filtered_data, envir = .GlobalEnv)
    }
  })
  
  # Filter the data based on selected pitcher, pitch type, batter's side, and date range
  dataFilter <- reactive({
    req(input$pitcher, input$pitchType, input$batterSide, input$dateRange)
    filtered_data <- get("filtered_data", envir = .GlobalEnv)
    
    # Convert date column to Date type if necessary (replace 'DateColumn' with actual date column name)
    filtered_data <- filtered_data %>%
      mutate(Date = as.Date(Date, format = "%Y-%m-%d"))
    
    if (input$pitchType == "All") {
      if (input$batterSide != "All") {
        filtered_data <- filtered_data %>%
          filter(Pitcher == input$pitcher & BatterSide == input$batterSide &
                   Date >= input$dateRange[1] & Date <= input$dateRange[2])
      } else {
        filtered_data <- filtered_data %>%
          filter(Pitcher == input$pitcher & Date >= input$dateRange[1] & Date <= input$dateRange[2])
      }
    } else {
      if (input$batterSide != "All") {
        filtered_data <- filtered_data %>%
          filter(Pitcher == input$pitcher & AutoPitchType == input$pitchType & 
                   BatterSide == input$batterSide & Date >= input$dateRange[1] & Date <= input$dateRange[2])
      } else {
        filtered_data <- filtered_data %>%
          filter(Pitcher == input$pitcher & AutoPitchType == input$pitchType & 
                   Date >= input$dateRange[1] & Date <= input$dateRange[2])
      }
    }
    
    return(filtered_data)
  })
  
  # Filter data for the selected batter and create a hit chart
  output$pitcher_hit_chart <- renderPlot({
    req(input$pitcher)
    filtered_data <- dataFilter() %>%
      filter(!(PlayResult %in% c("Out", "Undefined", "FieldersChoice", "StolenBase", "CaughtStealing", "Sacrifice", "Error")) &
               !is.na(ExitSpeed)) %>%
      # Calculate hc_x and hc_y
      mutate(hc_x = sin(Bearing * pi / 180) * Distance,
             hc_y = cos(Bearing * pi / 180) * Distance)
    
    # Ensure PlayResult is a factor with correct levels
    filtered_data$PlayResult <- factor(filtered_data$PlayResult, levels = c("Single", "Double", "Triple", "HomeRun"))
    
    plot17 <- ggplot(filtered_data, aes(x = hc_x, y = hc_y)) + 
      geom_mlb_stadium(stadium_ids = 'dodgers',
                       stadium_transform_coords = TRUE, 
                       stadium_segments = 'all', 
                       linewidth = 0.5, 
                       color = 'black') + 
      theme_void() + 
      geom_point(aes(fill = PlayResult, color = PlayResult), 
                 shape = 21, 
                 colour = 'black', 
                 stroke = 0.5, 
                 size = 3,    # Adjust the size of points
                 alpha = 0.8) + 
      scale_fill_manual(values = c("Single" = "Blue",    # Red
                                   "Double" = "Red",   # Blue
                                   "Triple" = "Orange",
                                   "HomeRun" = "Green"),# Purple
                        breaks = c("Single", "Double", "Triple", "HomeRun"),
                        limits = c("Single", "Double", "Triple", "HomeRun")) +
      scale_color_manual(values = c("Single" = "black", "Double" = "black", "Triple" = "black", "HomeRun" = "black")) +  # Adjust point border colors
      theme(panel.background = element_rect(fill = 'white', colour = NA),  # Remove border by setting colour to NA
            plot.title = element_text(hjust = 0.5)) +
      coord_fixed() +
      labs(title = paste(input$batter, "Batted Ball Chart"),
           fill = 'Hit Type')
    
    # Add labels for ExitSpeed next to each point, rounded to one decimal and bold, with repulsion to prevent overlap
    plot17 <- plot17 +
      geom_text_repel(aes(label = paste(round(ExitSpeed, 1))), 
                      colour = "black", 
                      size = 3,   # Adjust the size of labels
                      box.padding = unit(0.25, "lines"),  # Padding around the label
                      point.padding = unit(1, "lines"),  # Minimum distance to points
                      segment.size = 0.2,  # Size of line segments
                      direction = "both",  # Direction of repulsion
                      force = 5,  # Strength of repulsion
                      fontface = "bold") +  # Make the text bold
      scale_color_manual(values = c("Single" = "black", "Double" = "black", "Triple" = "black", "HomeRun" = "black"))  # Adjust point border colors
    
    plot17
  })
  # Calculate average and max metrics including pitch count and usage percentage
  pitchMetrics <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    # Calculate pitch count and pitch usage percentage
    pitch_counts <- data_filtered %>%
      group_by(AutoPitchType) %>%
      summarise(PitchCount = n())
    
    total_pitches <- sum(pitch_counts$PitchCount, na.rm = TRUE)
    
    pitch_metrics <- data_filtered %>%
      group_by(AutoPitchType) %>%
      summarise(
        AvgVelocity = round(mean(RelSpeed, na.rm = TRUE), 1),
        MaxVelocity = round(max(RelSpeed, na.rm = TRUE), 1),
        AvgSpinRate = round(mean(SpinRate, na.rm = TRUE), 1),
        MaxSpinRate = round(max(SpinRate, na.rm = TRUE), 1),
        InducedVerticalBreak = round(mean(InducedVertBreak, na.rm = TRUE), 1),
        HorizontalBreak = round(mean(HorzBreak, na.rm = TRUE), 1),
        ReleaseHeight = round(mean(RelHeight, na.rm = TRUE), 1),
        ReleaseSide = round(mean(RelSide, na.rm = TRUE), 1),
        VerticalApproachAngle = round(mean(VertApprAngle, na.rm = TRUE), 1),
        Extension = round(mean(Extension, na.rm = TRUE), 1),
        AvgExitVelocity = round(mean(ExitSpeed, na.rm = TRUE), 1)
      ) %>%
      left_join(pitch_counts, by = "AutoPitchType") %>%
      mutate(
        PitchUsagePct = round(PitchCount / total_pitches * 100, 0)
      )
    
    return(pitch_metrics)
  })
  
  calculateStatistics <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    data_filtered <- data_filtered %>%
      mutate(
        InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
        Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
        Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0),
        ZSwing = ifelse(InStrikeZone == 1 & Swing == 1, 1, 0)
      )
    
    # Calculate first pitch strikes and total first pitches
    first_pitch_strikes <- data_filtered %>%
      filter(PitchofPA == 1 & PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "StrikeCalled", "StrikeSwinging"))
    
    first_pitch_strikes_count <- nrow(first_pitch_strikes)
    
    total_first_pitches <- data_filtered %>%
      filter(PitchofPA == 1)
    
    total_first_pitches_count <- nrow(total_first_pitches)
    
    first_pitch_strike_percentage <- round(first_pitch_strikes_count / total_first_pitches_count * 100, 1)
    
    # Filter out plays with result "Error" for EarnedRuns calculation
    earned_runs_filtered <- data_filtered %>%
      filter(PlayResult != "Error")
    
    earned_runs <- sum(earned_runs_filtered$RunsScored, na.rm = TRUE)
    
    statistics <- data_filtered %>%
      summarise(
        Pitches = n(),
        PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
        AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
        IP = round(sum((OutsOnPlay == "1" | KorBB == "Strikeout")) / 3, 2),
        EarnedRuns = earned_runs,
        ERA = round((EarnedRuns / sum((OutsOnPlay == "1" | KorBB == "Strikeout") / 3)) * 9, 2),
        H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
        `1B` = sum(PlayResult == "Single"),
        `2B` = sum(PlayResult == "Double"),
        `3B` = sum(PlayResult == "Triple"),
        HR = sum(PlayResult == "HomeRun"),
        SO = sum(KorBB == "Strikeout"),
        BB = sum(KorBB == "Walk"),
        HBP = sum(PitchCall == "HitByPitch"),
        Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")),
        Whiffs = sum(PitchCall == "StrikeSwinging"),
        Swing = sum(Swing),
        GroundBall = sum(TaggedHitType == "GroundBall"),
        FlyBall = sum(TaggedHitType == "FlyBall"),
        TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")),
        InStrikeZone = sum(InStrikeZone),
        Chase = sum(Chase),
        ZSwingCount = sum(ZSwing, na.rm = TRUE),
        "Swing Percentage" = round(mean(Swing, na.rm = TRUE) / (Pitches) * 100, 1),
        "Whiff Percentage" = round(sum(Whiffs, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
        "Strike Percentage" = round(sum(Strikes, na.rm = TRUE) / sum(Pitches, na.rm = TRUE) * 100, 2),
        "Strikeout Percentage" = round(sum(SO, na.rm = TRUE) / sum(PA, na.rm = TRUE) * 100, 1),
        "Walk Percentage" = round(sum(BB, na.rm = TRUE) / sum(AB, na.rm = TRUE) * 100, 1),
        "GroundBall Percentage" = round(sum(GroundBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "FlyBall Percentage" = round(sum(FlyBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        "Strike Zone Percentage" = round(sum(InStrikeZone, na.rm = TRUE) / nrow(data_filtered) * 100, 1),
        "Chase Percentage" = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE)*100, 1),
        ZSwingPercentage = round(sum(ZSwing, na.rm = TRUE) / sum(InStrikeZone, na.rm = TRUE) * 100, 1),
        "First Pitch Strike Count" = first_pitch_strikes_count,
        "First Pitch Strike Percentage" = first_pitch_strike_percentage
      )
    
    return(statistics)
  })
  calculategame <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    # Define a mapping of BatterTeam to OpposingTeam
    opposing_team_mapping <- c("BRE_WHI" = "Brewster Whitecaps", "BOU_BRA" = "Bourne Braves", "CHA_ANG" = "Chatham Anglers", "YAR_RED" = "Yarmouth-Dennis Red Sox", "FAL_COM" = "Falmouth Commodores", "WAR_GAT" = "Wareham Gateman", "HAR_MAR" = "Harwich Mariners", "HYA_HAR" = "Hyannis Harbor Hawks", "ORL_FIR" = "Orleans Firebirds", "COT_KET" = "Cotuit Kettleers"
    )
    # Filter out plays with result "Error" for EarnedRuns calculation
    earned_runs_filtered <- data_filtered %>%
      filter(PlayResult != "Error")
    
    
    data_filtered <- data_filtered %>%
      mutate(
        InStrikeZone = PlateLocSide >= -1 & PlateLocSide <= 1 & PlateLocHeight >= 1.40 & PlateLocHeight <= 3.6,
        Swing = PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "StrikeSwinging", "InPlay"),
        Chase = ifelse(InStrikeZone == 0 & Swing == 1, 1, 0),
        ZSwing = ifelse(InStrikeZone == 1 & Swing == 1, 1, 0),
        Date = as.Date(Date), # Ensure Date column is of Date type
        OpposingTeam = opposing_team_mapping[BatterTeam] # Add the OpposingTeam column
      )
    
    statistics <- data_filtered %>%
      group_by(Date, OpposingTeam) %>%  # Group by Date and OpposingTeam
      summarise(
        Pitches = n(),
        PA = sum(PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout", "Walk", "HitbyPitch")),
        AB = sum((PitchCall %in% c("InPlay") | KorBB %in% c("Strikeout")) & !KorBB %in% c("Walk", "HitbyPitch") & !PlayResult %in% c("Sacrifice")),
        IP = round(sum((OutsOnPlay == "1" | KorBB == "Strikeout")) / 3, 2),
        EarnedRuns = sum(ifelse(PlayResult != "Error", RunsScored, 0), na.rm = TRUE), # Calculate earned runs per game
        ERA = round((sum(ifelse(PlayResult != "Error", RunsScored, 0), na.rm = TRUE) / sum((OutsOnPlay == "1" | KorBB == "Strikeout") / 3)) * 9, 2), # Calculate ERA per game
        H = sum(PlayResult %in% c("Single", "Double", "Triple", "HomeRun")),
        `1B` = sum(PlayResult == "Single"),
        `2B` = sum(PlayResult == "Double"),
        `3B` = sum(PlayResult == "Triple"),
        HR = sum(PlayResult == "HomeRun"),
        SO = sum(KorBB == "Strikeout"),
        BB = sum(KorBB == "Walk"),
        HBP = sum(PitchCall == "HitByPitch"),
        Strikes = sum(PitchCall %in% c("StrikeCalled", "StrikeSwinging", "FoulBallNotFieldable", "FoulBallFieldable", "InPlay")),
        Whiffs = sum(PitchCall == "StrikeSwinging"),
        Swing = sum(Swing),
        GroundBall = sum(TaggedHitType == "GroundBall"),
        FlyBall = sum(TaggedHitType == "FlyBall"),
        TotalBallsInPlay = sum(TaggedHitType %in% c("GroundBall", "FlyBall", "Popup", "LineDrive")),
        InStrikeZone = sum(InStrikeZone),
        Chase = sum(Chase),
        ZSwingCount = sum(ZSwing, na.rm = TRUE),
        `Swing Percentage` = round(mean(Swing, na.rm = TRUE) / Pitches, 1),
        `Whiff Percentage` = round(sum(Whiffs, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
        `Strike Percentage` = round(sum(Strikes, na.rm = TRUE) / sum(Pitches, na.rm = TRUE), 2),
        `Strikeout Percentage` = round(sum(SO, na.rm = TRUE) / sum(PA, na.rm = TRUE) * 100, 1),
        `Walk Percentage` = round(sum(BB, na.rm = TRUE) / sum(AB, na.rm = TRUE) * 100, 1),
        `GroundBall Percentage` = round(sum(GroundBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        `FlyBall Percentage` = round(sum(FlyBall, na.rm = TRUE) / sum(TotalBallsInPlay, na.rm = TRUE) * 100, 1),
        `Strike Zone Percentage` = round(sum(InStrikeZone, na.rm = TRUE) / n() * 100, 1),
        `Chase Percentage` = round(sum(Chase, na.rm = TRUE) / sum(Swing, na.rm = TRUE) * 100, 1),
        `ZSwingPercentage` = round(sum(ZSwing, na.rm = TRUE) / sum(InStrikeZone, na.rm = TRUE) * 100, 1),
        `First Pitch Strikes` = sum(PitchofPA == 1 & PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "StrikeCalled", "StrikeSwinging")),
        `First Pitch Count` = sum(PitchofPA == 1),
        `First Pitch Strike Percentage` = round(sum(PitchofPA == 1 & PitchCall %in% c("FoulBallNotFieldable", "FoulBallFieldable", "InPlay", "StrikeCalled", "StrikeSwinging")) / sum(PitchofPA == 1) * 100, 1)
      )
    
    return(statistics)
  })
  
  
  
  # Plot pitch location map
  output$pitch_location_map <- renderPlot({
    req(dataFilter())
    ggplot(dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight, fill = AutoPitchType)) +
      geom_point(shape = 21, size = 3) +  # Adjust size as needed
      scale_fill_manual(values = c("red", "blue", "green", "yellow", "purple", "orange")) +
      labs(title = paste("Pitch Location Map for", input$pitcher),
           x = "Plate Location (Side)",
           y = "Plate Location (Height)",
           fill = "Pitch Type") +
      theme_minimal() +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4),
                fill = NA, color = "black", size = 1) +
      xlim(-1.8, 1.8) +
      ylim(1, 4)
  })
  
  # Pitch Heatmap chart
  output$pitch_heatmap <- renderPlot({
    req(dataFilter())
    ggplot(dataFilter(), aes(x = PlateLocSide, y = PlateLocHeight)) +
      stat_density_2d(aes(fill = ..density..), geom = 'raster', contour = FALSE) +
      scale_fill_gradientn(colours = c("blue", "white", "red")) +
      annotate("rect", xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4, fill = NA, color = "black", alpha = 0.1) +
      ylim(1, 4) + xlim(-1.8, 1.8) +
      theme_bw() + theme_classic() +
      xlab("Horizontal Pitch Location") +
      ylab("Vertical Pitch Location") +
      ggtitle("Pitch Location Heat Map", subtitle = "Pitcher's Perspective") +
      facet_wrap(~AutoPitchType, ncol = 2)
  }, height = function() {
    if (input$pitchType == "All") 650 else 475
  }, width = function() {
    if (input$pitchType == "All") 500 else 350
  })
  
  # Plot release chart
  output$release_chart <- renderPlot({
    req(dataFilter())
    ggplot(dataFilter(), aes(x = RelSide, y = RelHeight, color = AutoPitchType)) +
      geom_point(size = 3, na.rm = TRUE) +
      labs(x = "Horizontal Release Point", y = "Vertical Release Point", color = "Pitch Type", title = paste("Release Point Chart for", input$pitcher)) +
      xlim(-4, 4) + ylim(2, 7) +
      geom_segment(aes(x = 0, y = 2, xend = 0, yend = 7), size = 1, color = "grey55") +
      geom_segment(aes(x = -4, y = 4.5, xend = 4, yend = 4.5), size = 1, color = "grey55") +
      scale_color_manual(values = c("red", "blue", "green", "black", "purple")) +
      theme_bw() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  # Plot movement chart
  output$movement_chart <- renderPlot({
    req(dataFilter())
    ggplot(dataFilter(), aes(x = HorzBreak, y = InducedVertBreak, color = AutoPitchType)) +
      geom_point(size = 3, na.rm = TRUE) +
      labs(x = "Horizontal Movement (HB)", y = "Vertical Movement (IVB)", color = "Pitch Type", title = paste("Pitch Movement Chart for", input$pitcher)) +
      xlim(-30, 30) + ylim(-30, 30) +
      geom_segment(aes(x = 0, y = -30, xend = 0, yend = 30), size = 1, color = "grey55") +
      geom_segment(aes(x = -30, y = 0, xend = 30, yend = 0), size = 1, color = "grey55") +
      scale_color_manual(values = c("red", "blue", "black", "green", "orange")) +
      theme_bw() +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            legend.position = "bottom",
            legend.text = element_text(size = 12),
            axis.title = element_text(size = 14))
  })
  
  # Function to prepare data for Hit Chart
  hitChartData <- reactive({
    req(dataFilter())
    data_filtered <- dataFilter()
    
    # Filter out only relevant columns for Hit Chart and remove specific PlayResult values
    hit_data <- data_filtered %>%
      filter(!PlayResult %in% c("CaughtStealing", "FieldersChoice","Error", "StolenBase", "Sacrifice", "Undefined")) %>%
      select(PlateLocSide, PlateLocHeight, PlayResult, AutoPitchType)
    
    return(hit_data)
  })
  
  # Render Hit Chart
  output$hit_chart <- renderPlot({
    req(hitChartData())
    
    # Create a scatter plot for pitch locations with PlayResult and AutoPitchType
    ggplot(hitChartData(), aes(x = PlateLocSide, y = PlateLocHeight, color = PlayResult, shape = AutoPitchType)) +
      geom_point(size = 3, alpha = 0.7) +
      scale_color_manual(values = c("red", "blue", "green", "orange", "purple", "yellow", "black", "cyan", "magenta", "brown")) +
      labs(title = "Hit Chart",
           x = "Horizontal Pitch Location",
           y = "Vertical Pitch Location",
           color = "Play Result",
           shape = "Pitch Type") +
      theme_minimal() +
      geom_rect(aes(xmin = -1, xmax = 1, ymin = 1.6, ymax = 3.4),
                fill = NA, color = "black", size = 1) +
      xlim(-1.8, 1.8) +
      ylim(1, 4)
  })
  
  
  
  # Render all pitches table
  output$all_pitches_table <- renderTable({
    req(dataFilter())
    dataFilter() %>%
      mutate(PitchNumber = row_number()) %>%  # Add pitch number
      select(PitchNumber, AutoPitchType, RelSpeed, InducedVertBreak, HorzBreak, SpinRate, Extension, RelHeight, RelSide, VertApprAngle) %>%
      rename(
        `Pitch Number` = PitchNumber,
        `Pitch Type` = AutoPitchType,
        Velocity = RelSpeed,
        `Induced Vertical Break` = InducedVertBreak,
        `Horizontal Break` = HorzBreak,
        `Spin Rate` = SpinRate,
        Extension = Extension,
        `Release Height` = RelHeight,
        `Release Side` = RelSide,
        `Vertical Approach Angle` = VertApprAngle
      )
  })
  
  # Render metrics table
  output$metrics_table <- DT::renderDataTable({
    req(pitchMetrics())
    DT::datatable(
      pitchMetrics(),
      options = list(
        scrollY = "400px",  # Vertical scrolling height
        scrollX = TRUE,     # Enable horizontal scrolling
        paging = FALSE,     # Disable paging
        searching = FALSE,  # Disable search/filtering
        ordering = FALSE    # Disable ordering by clicking on column headers
      ),
      rownames = FALSE     # Remove row numbers (index column)
    )
  })
  
  output$all_pitches_table <- DT::renderDataTable({
    req(dataFilter())
    
    # Check if dataFilter() returns the expected data frame
    print(str(dataFilter()))
    
    # Mutate and process the data
    processed_data <- dataFilter() %>%
      mutate(PitchNumber = row_number(),  # Add pitch number
             Velocity = round(RelSpeed, 1),
             InducedVertBreak = round(InducedVertBreak, 1),
             HorzBreak = round(HorzBreak, 1),
             SpinRate = round(SpinRate, 1),
             Extension = round(Extension, 1),
             RelHeight = round(RelHeight, 1),
             RelSide = round(RelSide, 1),
             VertApprAngle = round(VertApprAngle, 1)) %>%
      select(PitchNumber, AutoPitchType, Velocity, InducedVertBreak, HorzBreak, SpinRate, Extension, RelHeight, RelSide, VertApprAngle) %>%
      rename(
        `Pitch Number` = PitchNumber,
        `Pitch Type` = AutoPitchType,
        `Velocity` = Velocity,
        `Induced Vertical Break` = InducedVertBreak,
        `Horizontal Break` = HorzBreak,
        `Spin Rate` = SpinRate,
        `Extension` = Extension,
        `Release Height` = RelHeight,
        `Release Side` = RelSide,
        `Vertical Approach Angle` = VertApprAngle
      )
    
    # Render the DataTable
    DT::datatable(
      processed_data,
      options = list(
        scrollY = "400px",  # Vertical scrolling height
        scrollX = TRUE,     # Enable horizontal scrolling
        paging = FALSE,     # Disable paging
        searching = FALSE,  # Disable search/filtering
        ordering = FALSE    # Disable ordering by clicking on column headers
      ),
      rownames = FALSE     # Remove row numbers (index column)
    )
  })
  
  
  
  # Render statistics table
  output$statistics_table <- DT::renderDataTable({
    req(calculateStatistics())
    DT::datatable(
      calculateStatistics(),
      options = list(
        scrollY = "400px",  # Vertical scrolling height
        scrollX = TRUE,     # Enable horizontal scrolling
        paging = FALSE,     # Disable paging
        searching = FALSE,  # Disable search/filtering
        ordering = FALSE    # Disable ordering by clicking on column headers
      ),
      rownames = FALSE     # Remove row numbers (index column)
    )
  })
  # Render games table
  output$game_table <- DT::renderDataTable({
    req(calculategame())
    DT::datatable(
      calculategame(),
      options = list(
        scrollY = "400px",  # Vertical scrolling height
        scrollX = TRUE,     # Enable horizontal scrolling
        paging = FALSE,     # Disable paging
        searching = FALSE,  # Disable search/filtering
        ordering = FALSE    # Disable ordering by clicking on column headers
      ),
      rownames = FALSE     # Remove row numbers (index column)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)