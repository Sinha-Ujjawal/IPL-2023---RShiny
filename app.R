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
  df_matches <- (read_ods(ipl_ods_link, sheet = "Matches")
                 |> as.data.table())
  df_matches[, "Datestamp" := as.POSIXct(paste(`Date`, `Time`, sep=" "), format = "%d-%b-%y %H:%M:%S", tz = "IST")]
  df_matches[`Winner` == "Home", "Winner Team Name" := `Home Team`]
  df_matches[`Winner` == "Away", "Winner Team Name" := `Away Team`]
  df_matches[order(`Datestamp`)]
}

my_date_formatter <- function(dt) format(dt, "%d-%b-%y")

DF_MATCHES <- load_df_matches("1Q65lfqb1sCg6Sa84OpjWRY6dDH8poald")
TOTAL_DAYS <- nrow(DF_MATCHES)

LAST_COMPLETED_MATCH_DAY <- last_completed_match_day(DF_MATCHES)
DF_LAST_COMPLETED_MATCHES <- DF_MATCHES[`Match Day` == LAST_COMPLETED_MATCH_DAY]

NEXT_MATCH_DAY <- LAST_COMPLETED_MATCH_DAY + 1
DF_NEXT_DAY_MATCHES <- DF_MATCHES[`Match Day` == NEXT_MATCH_DAY]

ui <- fluidPage(
  tags$head(HTML("<title>IPL 2023</title>")),
  
  # Title
  titlePanel(textOutput(outputId = "wout_title")),
  ##
  
  markdown(paste0("#### Matches Left: ", (TOTAL_DAYS - LAST_COMPLETED_MATCH_DAY), "/", TOTAL_DAYS)),
  
  verticalLayout(
    # Scorecard
    sliderInput(
      inputId = "days_till",
      label = textOutput(outputId = "wout_days_till"),
      min = 1,
      max = LAST_COMPLETED_MATCH_DAY,
      value = LAST_COMPLETED_MATCH_DAY
    ),
    
    h3("Scorecard"),
    dataTableOutput(outputId = "wout_scorecard_table"),
    ##
    
    # Last Completed Day Matches
    {
      if (LAST_COMPLETED_MATCH_DAY > 0) {
        verticalLayout(
          h3({
            dt <- DF_LAST_COMPLETED_MATCHES$`Datestamp`[1]
            paste0("Last Completed Day (", my_date_formatter(dt), ") Match(s)")
          }),
          dataTableOutput(outputId = "wout_last_completed_day_match_table")
        )
      }
      else {
        HTML("")
      }
    },
    ##
    
    # Next Day Matches
    {
      if (NEXT_MATCH_DAY < TOTAL_DAYS) {
        verticalLayout(
          h3({
            dt <- DF_NEXT_DAY_MATCHES$`Datestamp`[1]
            paste0("Next (", my_date_formatter(dt), ") Match(s)")
          }),
          dataTableOutput(outputId = "wout_next_match_table")
        )
      }
      else {
        HTML("")
      }
    },
    ##
    
    # All Matches
    h3("All Matches"),
    dataTableOutput(outputId = "wout_all_matches_table")
    ##
  )
)

server <- function(input, output) {
  rx_df_scorecard <- reactiveVal(scorecard_hist(DF_MATCHES, 1))
  
  observeEvent(input$days_till, {
    if (input$days_till > LAST_COMPLETED_MATCH_DAY) {
      updateNumericInput(inputId = "days_till", value = LAST_COMPLETED_MATCH_DAY)
    } else {
      rx_df_scorecard(scorecard_hist(DF_MATCHES, input$days_till))
    }
  })
  
  get_title <- reactive({
    dt <- last_completed_match_date(DF_MATCHES)
    paste0("IPL ", year(dt), " (Last Updated On: ", my_date_formatter(dt), ")")
  })
  
  get_days_till_text <- reactive({
    df_scorecard <- rx_df_scorecard()
    if (nrow(df_scorecard) >= 1) {
      paste0("Days Till (", my_date_formatter(df_scorecard$`Date`[1]), ")")
    } else {
      "Days Till"
    }
  })
  
  # Title
  output$wout_title <- renderText(get_title())
  ##
  
  # Scorecard
  output$wout_days_till <- renderText(get_days_till_text())
  output$wout_scorecard_table <- renderDataTable(rx_df_scorecard())
  ##
  
  # Last Completed Day Matches
  output$wout_last_completed_day_match_table <- renderDataTable(DF_LAST_COMPLETED_MATCHES[
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
      "Winner" = if (`Winner` == "Home") {`Home Team`} else {`Away Team`} 
    )
  ])
  ##
  
  # Next Day Matches
  output$wout_next_match_table <- renderDataTable(DF_NEXT_DAY_MATCHES[
    ,
    list(
      `Day`,
      `Time`,
      `Home Team`,
      `Away Team`
    )
  ])
  ##
  
  # All Matches
  output$wout_all_matches_table <- renderDataTable(DF_MATCHES[
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
