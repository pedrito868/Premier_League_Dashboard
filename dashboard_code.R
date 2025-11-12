library("shiny")
library("ggplot2")
library("plotly")
library("formatR")
library("stringr")
library("shinythemes")
library("dplyr")
library("shinydashboard")
library("readr")
library("readxl")
library("Hmisc")

######################### Data preparation ######################### 

# https://fbref.com/en/comps/9/2022-2023/stats/2022-2023-Premier-League-Stats - link do strony z której pobieramy dane
# Player Standard Stats 2022-2023 Premier League - tytuł tabeli z której pobieramy dane
# Dane zostały skopiowane do pliku Excela i zapisane w arkuszu "Sheet1"

Players <- read_excel("Players.xlsx")
#View(Players)

Players <- rename_with(Players, 
                       ~ gsub("\\...\\d+", "_p90", .), 
                       matches(c("Gls...27", "Ast...28", "G+A...29", "G-PK...30", 
                                 "xG...32", "xAG...33", "npxG...35", "npxG+xAG...36"))
)

Players <- rename_with(Players, 
                       ~ gsub("\\.{3}\\d+", "", .), 
                       matches(c("Gls...12", "Ast...13", "G+A...14", "G-PK...15", 
                                 "xG...20", "npxG...21", "xAG...22", "npxG+xAG...23"))
)


Players <- Players %>%
  rename_at("90s", ~"full_90s") %>%
  rename_at(14, ~"G+A") %>%
  rename_at(23, ~"npxG+xAG") %>%
  rename_at(29, ~"G+A_p90") %>%
  rename_at(31, ~"G+A-PK_p90") %>%
  rename_at(34, ~"xG+xAG_p90") %>%
  rename_at(36, ~"npxG+xAG_p90")


Players <- rename_all(Players, ~gsub("\\+", "_pl_", .))
Players <- rename_all(Players, ~gsub("-", "_mi_", .))

Players <- select(Players, -c(Rk, Matches))

Players <- Players %>%
  mutate(Nation = substr(Nation, nchar(Nation) - 2, nchar(Nation)))

Players <- Players %>%
  mutate(Pos = substr(Pos, 0, 2))

positions_order <- c("GK", "DF", "MF", "FW")
Players$Pos <- factor(Players$Pos, levels = positions_order)

Players$Min <- as.numeric(gsub(",", "", Players$Min))

Players$PrgC_p90 <- round(Players$PrgC / Players$full_90s, 2)
Players$PrgP_p90 <- round(Players$PrgP / Players$full_90s, 2)
Players$PrgR_p90 <- round(Players$PrgR / Players$full_90s, 2)


label(Players$Player) <- "Player Name"
label(Players$Nation) <- "Nationality"
label(Players$Pos) <- "Most played position"
label(Players$Squad) <- "Team name"
label(Players$Age) <- "Age"
label(Players$Born) <- "Birth Year"
label(Players$MP) <- "Matches Played"
label(Players$Starts) <- "Starts"
label(Players$Min) <- "Minutes Played"
label(Players$full_90s) <- "Full 90s"
label(Players$Gls) <- "Goals"
label(Players$Ast) <- "Assists"
label(Players$'G_pl_A') <- "Goals + Assists"
label(Players$'G_mi_PK') <- "Goals - Penalties"
label(Players$PK) <- "Penalty Kicks"
label(Players$PKatt) <- "Penalty Kicks Attempted"
label(Players$CrdY) <- "Yellow Cards"
label(Players$CrdR) <- "Red Cards"
label(Players$xG) <- "Expected Goals (xG)"
label(Players$npxG) <- "Non-Penalty Expected Goals (npxG)"
label(Players$xAG) <- "Expected Assists (xAG)"
label(Players$'npxG_pl_xAG') <- "Non-Penalty Expected Goals + Expected Assists"
label(Players$PrgC) <- "Progressive carries"
label(Players$PrgP) <- "Progressive Passes"
label(Players$PrgR) <- "Progressive Passes Received"
label(Players$Gls_p90) <- "Goals per 90 minutes"
label(Players$Ast_p90) <- "Assists per 90 minutes"
label(Players$'G_pl_A_p90') <- "Goals + Assists per 90 minutes"
label(Players$'G_mi_PK_p90') <- "Goals - Penalties per 90 minutes"
label(Players$'G_pl_A_mi_PK_p90') <- "Goals + Assists - Penalties per 90 minutes"
label(Players$xG_p90) <- "Expected Goals (xG) per 90 minutes"
label(Players$xAG_p90) <- "Expected Assists (xAG) per 90 minutes"
label(Players$'xG_pl_xAG_p90') <- "Expected Goals + Expected Assists per 90 minutes"
label(Players$npxG_p90) <- "Non-Penalty Expected Goals (npxG) per 90 minutes"
label(Players$'npxG_pl_xAG_p90') <- "Non-Penalty Expected Goals + Expected Assists per 90 minutes"
label(Players$PrgC_p90) <- "Progressive carries per 90 minutes"
label(Players$PrgP_p90) <- "Progressive Passes per 90 minutes"
label(Players$PrgR_p90) <- "Progressive Passes Received per 90 minutes"

Players_900min <- Players[Players$Min >= 900, ]

columns_to_percentiles <- c("Gls_p90", "Ast_p90", "xG_p90", "xAG_p90", "PrgC_p90", "PrgP_p90", "PrgR_p90")

for (column in columns_to_percentiles) {
  percentile_rank <- round(rank(Players_900min[[column]]) / length(Players_900min[[column]]) * 100)
  new_column_name <- paste0(column, "_perc")
  Players_900min[[new_column_name]] <- percentile_rank
}


label(Players_900min$Gls_p90_perc) <- "Percentile Rank of Goals per 90 minutes"
label(Players_900min$Ast_p90_perc) <- "Percentile Rank of Assists per 90 minutes"
label(Players_900min$xG_p90_perc) <- "Percentile Rank of Expected Goals (xG) per 90 minutes"
label(Players_900min$xAG_p90_perc) <- "Percentile Rank of Expected Assists (xAG) per 90 minutes"
label(Players_900min$PrgC_p90_perc) <- "Percentile Rank of Progressive carries per 90 minutes"
label(Players_900min$PrgP_p90_perc) <- "Percentile Rank of Progressive Passes per 90 minutes"
label(Players_900min$PrgR_p90_perc) <- "Percentile Rank of Progressive Passes Received per 90 minutes"

project_color <- '#177E89'

m <- list(
  l = 70,
  r = 70,
  b = 70,
  t = 70,
  pad = 2
)


######################### Shiny App ######################### 

#### Sidebar ####
sidebar <- dashboardSidebar(sidebarMenu(
  menuItem(
    "Individual Characteristics",
    icon = icon("dashboard"),
    tabName = "Players"
  ),
  menuItem(
    " Aggregated Characteristics",
    icon = icon("chart-bar"),
    tabName = "Aggregated"
  )
))


#### Body ####
body <- dashboardBody(tabItems(
  tabItem(
    tabName = "Players",
    h2("Individual Characteristics"),
    fluidRow(box(
      selectInput(
        "var3",
        label = "Select a player",
        choices = sort(Players_900min$Player),
        selected = "Kevin De Bruyne"
      )
    ),
    uiOutput("test")),
    fluidRow(
      box(
        title = "Player performance",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        plotOutput(outputId = "ploty", height = "600px")
      )
    ),
    fluidRow(
      valueBoxOutput("selected_var5"),
      valueBoxOutput("selected_var1"),
      valueBoxOutput("selected_var3"),
      valueBoxOutput("selected_var4"),
      valueBoxOutput("selected_var6"),
      valueBoxOutput("selected_var7")
    )
  ),
  tabItem(
    tabName = "Aggregated",
    h2("Aggregated Characteristics"),
    fluidRow(column(
      width = 6,
      box(
        title = "Player comparison",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        selectInput(
          "aggregated_var1",
          label = "Select variable x for comparison",
          choices = colnames(Players_900min),
          selected = "xG_p90"
        ),
        selectInput(
          "aggregated_var2",
          label = "Select variable y for comparison",
          choices = colnames(Players_900min),
          selected = "xAG_p90"
        ),
        actionButton("update_aggregated_plot", "Update Plot"),
        plotlyOutput("aggregated_plot", height = "620px", width = "100%")
      )
    ),
    column(
      width = 6,
      box(
        title = "Position comparison",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        width = 12,
        selectInput(
          "box_var",
          label = "Select variable for comparison",
          choices = colnames(Players_900min),
          selected = "xG_p90"
        ),
        actionButton("update_box_plot", "Update Plot"),
        plotlyOutput("box_plot", height = "700px", width = "100%") 
      )
    ))
  )
))


#### UI ####
ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "Premier League"),
                    sidebar,
                    body)


#### Server ####
server <- function(input, output) {
  selectedPlayerData <- reactive({
    Players_900min[Players_900min$Player == input$var3,]
  })
  
  plotData <- reactive({
    variables <-
      c(
        "Gls_p90_perc",
        "Ast_p90_perc",
        "xG_p90_perc",
        "xAG_p90_perc",
        "PrgC_p90_perc",
        "PrgP_p90_perc",
        "PrgR_p90_perc"
      )
    values <- as.numeric(selectedPlayerData()[, variables])
    
    data.frame(
      variable = c(
        "Goals",
        "Assists",
        "Expected Goals (xG)",
        "Expected Assists (xAG)",
        "Progressive carries",
        "Progressive Passes",
        "Progressive Passes Received"
      ),
      value = values
    )
  })
  
  
  #### Plot 1 ####
  output$ploty = renderPlot({
    ggplot(plotData()) +
      geom_hline(aes(yintercept = y),
                 data.frame(y = seq(0, 100, by = 25)),
                 color = "grey") +
      geom_col(
        aes(
          x = str_wrap(variable, 5),
          y = value,
          fill = project_color
        ),
        show.legend = FALSE,
        alpha = .7
      ) +
      scale_y_continuous(
        limits = c(-25, 100),
        expand = c(0, 0),
        breaks = seq(0, 100, by = 20)
      ) +
      annotate(
        x = 7,
        y = 4,
        label = "0",
        geom = "text",
        color = "gray12"
      ) +
      annotate(
        x = 7,
        y = 29,
        label = "25",
        geom = "text",
        color = "gray12"
      ) +
      annotate(
        x = 7,
        y = 54,
        label = "50",
        geom = "text",
        color = "gray12"
      ) +
      annotate(
        x = 7,
        y = 79,
        label = "75",
        geom = "text",
        color = "gray12"
      ) +
      annotate(
        x = 7,
        y = 100,
        label = "100",
        geom = "text",
        color = "gray12"
      ) +
      coord_polar() +
      labs(title = paste("Statistics' percentiles per 90 minutes - ", input$var3),) +
      theme(
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_text(
          color = "gray12",
          size = 12,
          face = "bold",
          hjust = 1
        ),
        text = element_text(color = "gray12"),
        plot.title = element_text(
          face = "bold",
          size = 25,
          hjust = 0.5
        ),
        panel.background = element_rect(fill = "white", color = "white"),
        panel.grid = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })
  
  
  #### Plot 2 ####
  aggregatedPlotData <- reactive({
    selected_var8 <- input$aggregated_var1
    selected_var9 <- input$aggregated_var2
    plot_ly(
      data = Players_900min,
      x = as.formula(paste0("~ ", selected_var8)),
      y = as.formula(paste0("~ ", selected_var9)),
      color = ~ Pos,
      type = "scatter",
      text = ~ paste("Player:", Player, "<br>Minutes played:", Min),
      size = ~ Min
    ) %>%
      layout(
        title = paste(
          label(Players_900min[, selected_var8]),
          "vs",
          label(Players_900min[, selected_var9])
        ),
        xaxis = list(title = label(Players_900min[, selected_var8])),
        yaxis = list(title = label(Players_900min[, selected_var9])),
        showlegend = T,
        font = list(size = 12),
        autosize = T,
        margin = m
      )
  })
  
  
  #### Plot 3 ####
  output$aggregated_plot = renderPlotly({
    aggregatedPlotData()
  })
  
  boxPlotData <- reactive({
    selected_var10 <- input$box_var
    plot_ly(
      data = Players_900min,
      x = ~ Pos,
      y = as.formula(paste0("~ ", selected_var10)),
      type = "box",
      boxpoints = "all",
      jitter = 0.3,
      marker = list(color = project_color),
      color = ~ Pos,
      line = list(color = project_color),
      text = Players_900min$Player
    ) %>%
      layout(
        title = paste(
          "Comparison of",
          label(Players_900min[, selected_var10]),
          "per",
          label(Players_900min$Pos)
        ),
        xaxis = list(title = label(Players_900min$Pos)),
        yaxis = list(title = label(Players_900min[, selected_var10])),
        showlegend = FALSE,
        font = list(size = 12),
        autosize = T,
        margin = m
      )
  })
  
  
  #### Figures ####
  output$box_plot = renderPlotly({
    boxPlotData()
  })
  
  output$selected_var5 <- renderValueBox({
    valueBox(
      paste0(sum(Players$Gls[which(Players$Player == input$var3)]), ""),
      "Goals scored" ,
      icon = icon("futbol", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$selected_var1 <- renderValueBox({
    valueBox(
      paste0(Players$Min[which(Players$Player == input$var3)], ""),
      "Minutes played",
      icon = icon("clock", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$selected_var3 <- renderValueBox({
    valueBox(
      paste0(Players$Born[which(Players$Player == input$var3)], ""),
      "Year of birth",
      icon = icon("birthday-cake", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$selected_var4 <- renderValueBox({
    valueBox(
      paste0(Players$Ast[which(Players$Player == input$var3)], ""),
      "Number of Assists",
      icon = icon("handshake", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$selected_var6 <- renderValueBox({
    valueBox(
      paste0(Players$Nation[which(Players$Player == input$var3)], ""),
      "Nationality",
      icon = icon("flag", lib = "font-awesome"),
      color = "blue"
    )
  })
  
  output$selected_var7 <- renderValueBox({
    valueBox(
      paste0(Players$Squad[which(Players$Player == input$var3)], ""),
      "Team",
      icon = icon("users", lib = "font-awesome"),
      color = "blue"
    )
  })
  
}

#### App execution ####
shinyApp(ui, server)

