source("./ipl_stat_utils.R")

library(data.table)
library(ggplot2)
library(readODS)
library(shiny)
library(stringi)

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
  df_matches[, "Match#" := 1:.N]
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
    
    # Scorecard Bump Chart
    h3("Scorecard Bump Chart"),
    htmlOutput(outputId = "wout_scorecard_bump_chart_team_selector"),
    plotOutput(outputId = "wout_scorecard_bump_chart"),
    ##
    
    # Last Completed Day Matches
    htmlOutput(outputId = "wout_last_completed_day_matches"),
    ##
    
    # Next Day Matches
    htmlOutput(outputId = "wout_next_day_matches"),
    markdown(
      "***Note -***
      
        ***1. Win Ratio is the ratio of wins of a team, where the opponents of the other team were also involved***
        
        ***2. CNRR (Common Net Run Rate) is the net run rate of a team, where the opponents of the other team were also involved***
      "
    ),
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
  
  df_matches_till_date <- df_matches[!is.na(`Winner`)]
  
  last_completed_match_day <- get_last_completed_match_day(df_matches)
  df_last_completed_matches <- df_matches[`Match Day` == last_completed_match_day]
  
  next_match_day <- last_completed_match_day + 1
  df_next_day_matches <- df_matches[`Match Day` == next_match_day]
  
  home_teams <- df_next_day_matches$`Home Team`
  away_teams <- df_next_day_matches$`Away Team`
  
  df_next_day_matches$`Perf Comp` <- 1:length(home_teams) |>
    lapply(function(i) get_team_performance_comparison(df_matches_till_date, home_teams[[i]], away_teams[[i]]))
  
  df_next_day_matches$`Win Ratio (Home Team)` <- df_next_day_matches$`Perf Comp` |>
    lapply(function(perf_comp) perf_comp$win_ratio_team_a) |>
    unlist() |>
    round(2)
  
  df_next_day_matches$`Win Ratio (Away Team)` <- df_next_day_matches$`Perf Comp` |>
    lapply(function(perf_comp) perf_comp$win_ratio_team_b) |>
    unlist() |>
    round(2)
  
  df_next_day_matches$`CNRR (Home Team)` <- df_next_day_matches$`Perf Comp` |>
    lapply(function(perf_comp) perf_comp$nrr_wrt_common_opps_team_a) |>
    unlist() |>
    round(2)
  
  df_next_day_matches$`CNRR (Away Team)` <- df_next_day_matches$`Perf Comp` |>
    lapply(function(perf_comp) perf_comp$nrr_wrt_common_opps_team_b) |>
    unlist() |>
    round(2)
  
  df_scorecards <- 1:last_completed_match_day |>
    lapply(function(days_till) build_scorecard_hist(df_matches, days_till)) |>
    rbindlist()
  
  df_scorecards[, "Team Short Name" := strsplit(`Team`, split = " ") |> lapply(function(word) substr(word, 1, 1))]
  df_scorecards$`Team Short Name` <- df_scorecards$`Team Short Name` |>
    lapply(stri_flatten) |>
    unlist()
  
  distinct_teams <- unique(df_scorecards$`Team`) |> sort()
  distinct_num_teams <- distinct_teams |> length()
  
  df_scorecards[, "Inverse Rank" := -`Rank`]
  
  setindex(df_scorecards, `Days Till`)
  ##
  
  # Title
  output$wout_title <- renderText({
    dt <- get_last_completed_match_date(df_matches)
    paste0("IPL ", year(dt), " (Last Updated On: ", my_date_formatter(dt), ")")
  })
  ##
  
  # Matches left Markdown
  output$wout_match_left_md <- renderUI(
    markdown(
      paste0(
        "#### Matches Left: ",
        (total_days - last_completed_match_day),
        "/",
        total_days,
        " (",
        round(last_completed_match_day * 100 / total_days, 0),
        "% complete)"
      )
    )
  )
  ##
  
  # Scorecard
  rx_df_scorecard <- reactiveVal(df_scorecards[`Days Till` == last_completed_match_day])
  
  observeEvent(input$win_days_till, {
    rx_df_scorecard(df_scorecards[`Days Till` == input$win_days_till])
  })
  
  updateSliderInput(
    inputId = "win_days_till",
    value = last_completed_match_day,
    max = last_completed_match_day
  )
  output$wout_win_days_till <- renderText({
    df_scorecard <- rx_df_scorecard()
    if (nrow(df_scorecard) >= 1) {
      paste0("Days Till (", my_date_formatter(df_scorecard$`Date`[1]), ")")
    } else {
      "Days Till"
    }
  })
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
  
  # Scorecard Bump Chart
  rx_df_scorecard_bump_chart <- reactiveVal(df_scorecards)
  
  output$wout_scorecard_bump_chart_team_selector <- renderUI(
    selectInput(
      inputId = "win_scorecard_bump_chart_selected_teams",
      label = "Select Teams to Filter",
      choices = distinct_teams,
      selected = distinct_teams,
      multiple = TRUE
    )
  )
  observeEvent(input$win_scorecard_bump_chart_selected_teams, {
    selected_teams <- input$win_scorecard_bump_chart_selected_teams
    rx_df_scorecard_bump_chart(df_scorecards[`Team` %in% selected_teams])
  })
  output$wout_scorecard_bump_chart <- renderPlot({
    df_scorecard_bump_chart <- rx_df_scorecard_bump_chart()
    ggplot(data = df_scorecard_bump_chart) +
      geom_point(mapping = aes(x = `Days Till`, y = `Inverse Rank`)) +
      geom_line(mapping = aes(x = `Days Till`, y = `Inverse Rank`, color = `Team Short Name`), show.legend = FALSE) +
      geom_text(
        data = df_scorecard_bump_chart[`Days Till` == last_completed_match_day],
        mapping = aes(label = `Team Short Name`, color = `Team Short Name`, x = last_completed_match_day + 1, y = `Inverse Rank`),
        show.legend = FALSE
      ) +
      theme_classic()
  })
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
            `Match#`,
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
            `Match#`,
            `Day`,
            `Time`,
            `Home Team`,
            `Win Ratio (Home Team)`,
            `CNRR (Home Team)`,
            `Away Team`,
            `Win Ratio (Away Team)`,
            `CNRR (Away Team)`
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
      `Match#`,
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
      "Winner" = `Winner Team Name`,
      `Comments`
    )
  ])
  ##
}

shinyApp(ui = ui, server = server)
