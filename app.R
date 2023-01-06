# Import ------------------------------------------------------------------
library(shiny)
library(shinyWidgets)
library(DT)
library(RSQLite)
library(tidyverse)
library(ggplot2)

# Data Refresh ------------------------------------------------------------
db <- dbConnect(SQLite(), "app.db")
now <- Sys.time()
refresh <- TRUE
if (!is.na(match("last_refresh", dbListTables(db)))) {
  cat("last_refresh table exists", sep = "\n")
  # Refresh data if 6+ hours since last refresh
  hours.since.last.refresh <- round(
    (as.numeric(now) - dbReadTable(db, "last_refresh")$datetime) / 60 / 60,
    digits = 1
  )
  refresh <- hours.since.last.refresh > 6
  cat(paste(hours.since.last.refresh, "hours since last refresh"), sep = "\n")
} else {
  cat("last_refresh table does NOT exist", sep = "\n")
}
if (refresh) {
  cat("Refreshing data", sep = "\n")
  source("data.R")
  dbWriteTable(
    db,
    "last_refresh",
    data.frame(datetime = as.numeric(now)),
    overwrite = TRUE
  )
} else {
  cat("Skipping refresh", sep = "\n")
}

start.year <- 2020
end.year <- as.integer(format(now, "%Y"))

injuries.df <- dbReadTable(db, "injuries") %>%
  mutate(
    team = factor(team),
    date = as.Date(date, format = "%Y-%m-%d"),
    retrodate = as.Date(retrodate, format = "%Y-%m-%d"),
    returndate = as.Date(returndate, format = "%Y-%m-%d"),
    eligibledate = as.Date(eligibledate, format = "%Y-%m-%d")
  )
# TODO: use game logs to find player's teams during injury
#       (think Yanks trading for Bader while hurt)
teams.df <- dbReadTable(db, "teams")
team.colors <- deframe(
  teams.df %>%
    group_by(fangraphsAbbreviation) %>%
    summarise(color = last(primaryColor))
)

months <- seq(0:12)
months <- setNames(months, coalesce(month.abb[months], "N/A"))

position.groups <- list(
  "Pitchers" = c("SP", "RP"),
  "Catchers" = c("C"),
  "Infielders" = c("1B", "2B", "3B", "SS", "INF", "UTL"),
  "Outfielders" = c("OF", "UTL")
)

# UI ----------------------------------------------------------------------
ui <- fluidPage(
  titlePanel("MLB Injury Tracker"),
  sidebarLayout(
    sidebarPanel(
      dateRangeInput(
        inputId = "dateRange",
        "Time Frame",
        start = now,
        end = now,
        min = min(injuries.df$date),
        max = now,
        format = "M d, yyyy",
        separator = "to"
      ),
      sliderTextInput(
        inputId = "injuryMonth",
        label = "Injury Month",
        choices = names(months)[0:12],
        selected = names(months)[c(1, 12)]
      ),
      sliderTextInput(
        inputId = "returnMonth",
        label = "Return Month",
        choices = names(months),
        selected = names(months)[c(1, 13)]
      ),
      sliderInput(
        inputId = "injuryDuration",
        label = "Injury Duration (days)",
        min = 0,
        max = as.integer(max(injuries.df$injuryDuration, na.rm = TRUE)),
        value = c(
          0,
          as.integer(max(injuries.df$injuryDuration, na.rm = TRUE))
        )
      ),
      fluidRow(
        column(
          4,
          checkboxGroupInput(
            inputId = "posGroup",
            label = "Position Group",
            choices = names(position.groups),
            selected = names(position.groups)
          )
        )
      ),
      width = 3
    ),
    mainPanel(
      selectInput(
        inputId = "team",
        label = "Team",
        choices = c("All", sort(unique(teams.df$fangraphsAbbreviation))),
        selected = "All"
      ),
      dataTableOutput("dataTable"),
      br(),
      plotOutput(outputId = "teamInjuryCount")
    )
  )
)

# Server ------------------------------------------------------------------
server <- function(input, output) {
  filtered.df <- reactive({
    injuries.df %>%
      filter(
        # Time Frame
        (
          date <= as.Date(
            input$dateRange[length(input$dateRange)],
            format = "%Y-%m-%d"
          ) & (
            returndate >= as.Date(input$dateRange[1], format = "%Y-%m-%d") |
            (is.na(returndate) & season == end.year)
          )
        ) &
        # Injury Month
        between(
          as.integer(format(date, "%m")),
          months[input$injuryMonth[1]],
          months[input$injuryMonth[length(input$injuryMonth)]]
        ) &
        # Return Month
        between(
          coalesce(as.integer(format(returndate, "%m")), 13),
          coalesce(months[input$returnMonth[1]], 13),
          coalesce(months[input$returnMonth[length(input$returnMonth)]], 13)
        ) &
        # Injury Duration
        (
          coalesce(injuryDuration, 0) >= input$injuryDuration[1] &
          coalesce(injuryDuration, 0) <=
            input$injuryDuration[length(input$injuryDuration)]
        ) &
        # Position Group
        (
          position1 %in% unname(unlist(position.groups[input$posGroup])) |
          position2 %in% unname(unlist(position.groups[input$posGroup]))
        )
      )
  })
  # TODO: add other filters, such as position, team, injury month, return month,
  #       duration, body part, status, etc.
  # TODO: join with schedule data to see how many games were missed
  #       (MLB Stats API?)
  
  output$dataTable <- renderDataTable({
    datatable(
      filtered.df() %>%
        # Team
        filter(input$team == "All" | team == input$team) %>%
        select(playerName, team, position, date, injurySurgery, returndate,
               injuryDuration, latestUpdate),
      rownames = FALSE,
      colnames = c("Player", "Team", "Position", "Injury/Surgery Date",
                   "Injury or Surgery", "Return", "Duration (days)", "Status"),
      options = list(
        order = list(list(3, 'desc'))
      )
    )
  })
  
  output$teamInjuryCount <- renderPlot({
    ggplot(
      filtered.df() %>% count(team, .drop = FALSE),
      aes(x = sort(team), y = n, fill = team)
    ) +
      geom_bar(stat = "identity") +
      scale_x_discrete(limits = rev(names(team.colors))) +
      scale_fill_manual(values = unname(team.colors)) +
      coord_flip() +
      ggtitle("Injured Players by Team") +
      theme(
        plot.title = element_text(size = 20, face="bold", hjust = 0.5),
        text = element_text(size = 14)
      ) +
      xlab("") +
      ylab("Injured Players") +
      theme(legend.position = "none")
  })
}

# Run the application -----------------------------------------------------
shinyApp(ui = ui, server = server)
