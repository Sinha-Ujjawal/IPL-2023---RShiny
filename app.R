source("./ipl_stat_utils.R")

library(shiny)
library(data.table)
library(readODS)

download_file_from_drive <- function(fileid, fileext) {
  temp <- tempfile(fileext = fileext)
  url <- paste("https://drive.google.com/uc?id=", fileid, "&export=download", sep="")
  download.file(url, temp)
  temp
}

load_df_matches <- function(fileid) {
  ipl_ods_link <- download_file_from_drive(fileid = fileid, fileext = ".ods")
  df_matches <- read_ods(ipl_ods_link, sheet = "Matches") |> as.data.table()
  df_matches[, "Datestamp" := as.POSIXct(paste(`Date`, `Time`, sep=" "), format = "%d-%b-%y %H:%M:%S", tz = "IST")]
  df_matches[`Winner` == "Home", "Winner Team Name" := `Home Team`]
  df_matches[`Winner` == "Away", "Winner Team Name" := `Away Team`]
  df_matches[, "Time" := format(`Datestamp`, "%H:%M")]
  df_matches[order(`Datestamp`)]
}

my_date_formatter <- function(dt) format(dt, "%d-%b-%y")

ui <- fluidPage(
  tags$head(HTML("<title>IPL 2023</title>")),
  
  # Title
  titlePanel(textOutput(outputId = "wout_title")),
  ##
  
  # Matches left Markdown
  htmlOutput(outputId = "wout_match_left_md"),
  ##
  
  verticalLayout(
    # Scorecard
    h3("Scorecard"),
    sliderInput(
      inputId = "win_days_till",
      label = textOutput(outputId = "wout_win_days_till"),
      min = 1,
      max = 1,
      value = 1,
      step = 1
    ),
    dataTableOutput(outputId = "wout_scorecard_table"),
    ##
    
    # Last Completed Day Matches
    htmlOutput(outputId = "wout_last_completed_day_matches"),
    ##
    
    # Next Day Matches
    htmlOutput(outputId = "wout_next_day_matches"),
    ##
    
    # All Matches
    h3("All Matches"),
    dataTableOutput(outputId = "wout_all_matches_table")
    ##
  )
)

server <- function(input, output) {
  # Prepare Initial Data
  df_matches <- load_df_matches("1Q65lfqb1sCg6Sa84OpjWRY6dDH8poald")
  total_days <- nrow(df_matches)
  
  last_completed_match_day <- get_last_completed_match_day(df_matches)
  df_last_completed_matches <- df_matches[`Match Day` == last_completed_match_day]
  
  next_match_day <- last_completed_match_day + 1
  df_next_day_matches <- df_matches[`Match Day` == next_match_day]
  
  df_scorecards <- 1:last_completed_match_day |>
    lapply(function(days_till) build_scorecard_hist(df_matches, days_till)) |>
    rbindlist()
  ##
  
  rx_df_scorecard <- reactiveVal(df_scorecards[`Days Till` == last_completed_match_day])
  
  observeEvent(input$win_days_till, {
    rx_df_scorecard(df_scorecards[`Days Till` == input$win_days_till])
  })
  
  get_win_days_till_text <- reactive({
    df_scorecard <- rx_df_scorecard()
    if (nrow(df_scorecard) >= 1) {
      paste0("Days Till (", my_date_formatter(df_scorecard$`Date`[1]), ")")
    } else {
      "Days Till"
    }
  })
  
  # Title
  output$wout_title <- renderText({
    dt <- get_last_completed_match_date(df_matches)
    paste0("IPL ", year(dt), " (Last Updated On: ", my_date_formatter(dt), ")")
  })
  ##
  
  # Matches left Markdown
  output$wout_match_left_md <- renderUI(
    markdown(
      paste0("#### Matches Left: ", (total_days - last_completed_match_day), "/", total_days)
    )
  )
  ##
  
  # Scorecard
  updateSliderInput(
    inputId = "win_days_till",
    value = last_completed_match_day,
    max = last_completed_match_day
  )
  output$wout_win_days_till <- renderText(get_win_days_till_text())
  output$wout_scorecard_table <- renderDataTable(
    rx_df_scorecard()
    [
      ,
      list(
        `Rank`,
        `Team`,
        `Num Matches`,
        `Num Wins`,
        `Num Losses`,
        `NRR`,
        `Win Points`,
        `Last 5`
      )
    ]
  )
  ##
  
  # Last Completed Day Matches
  if (last_completed_match_day > 0) {
    output$wout_last_completed_day_matches <- renderUI({
      verticalLayout(
        h3({
          dt <- df_last_completed_matches$`Datestamp`[1]
          paste0("Last Completed Day (", my_date_formatter(dt), ") Match(s)")
        }),
        renderDataTable(df_last_completed_matches[
          ,
          list(
            `Day`,
            `Time`,
            `Home Team`,
            `Runs (Home)`,
            `Balls (Home)`,
            `Wickets Dropped (Home)`,
            `Away Team`,
            `Runs (Away)`,
            `Balls (Away)`,
            `Wickets Dropped (Away)`,
            "Winner" = `Winner Team Name`
          )
        ])
      )
    })
  }
  ##
  
  # Next Day Matches
  if (next_match_day < total_days) {
    output$wout_next_day_matches <- renderUI({
      verticalLayout(
        h3({
          dt <- df_next_day_matches$`Datestamp`[1]
          paste0("Next (", my_date_formatter(dt), ") Match(s)")
        }),
        renderDataTable(df_next_day_matches[
          ,
          list(
            `Day`,
            `Time`,
            `Home Team`,
            `Away Team`
          )
        ])
      )
    })
  }
  ##
  
  # All Matches
  output$wout_all_matches_table <- renderDataTable(df_matches[
    ,
    list(
      `Date`,
      `Day`,
      `Time`,
      `Home Team`,
      `Runs (Home)`,
      `Balls (Home)`,
      `Wickets Dropped (Home)`,
      `Away Team`,
      `Runs (Away)`,
      `Balls (Away)`,
      `Wickets Dropped (Away)`,
      "Winner" = `Winner Team Name`
    )
  ])
  ##
}

shinyApp(ui = ui, server = server)
