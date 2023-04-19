library(data.table)

by_team_match_records <- function(df_matches) {
  (
    rbindlist(list(
      df_matches[
        ,
        list(
          "Match Day" = `Match Day`,
          "Datestamp" = `Datestamp`,
          "Team" = `Home Team`,
          "Won?" = (`Winner` == "Home"),
          "Runs" = `Runs (Home)`,
          "Balls" = `Balls (Home)`,
          "Wickets" = `Wickets Dropped (Home)`
        ),
      ],
      df_matches[
        ,
        list(
          "Match Day" = `Match Day`,
          "Datestamp" = `Datestamp`,
          "Team" = `Away Team`,
          "Won?" = (`Winner` == "Away"),
          "Runs" = `Runs (Home)`,
          "Balls" = `Balls (Home)`,
          "Wickets" = `Wickets Dropped (Home)`
        ),
      ]
    ))
    [!is.na(`Won?`)]
    [order(`Team`, `Datestamp`)]
  )
}

by_team_last_n_match_records <- function(df_matches, n) {
  df_by_team_match_records <- by_team_match_records(df_matches)
  setkey(df_by_team_match_records, `Team`)
  df_by_team_match_records[, rank := .N:1, by = list(`Team`)]
  df_by_team_match_records[rank <= n][order(-rank)][,!"rank"]
}

by_team_last_n <- function(df_matches, n) {
  df_by_team_last_n_match_records <- by_team_last_n_match_records(df_matches, n)
  df_by_team_last_n_match_records[`Won?` == TRUE, "WonStr" := "✔"]
  df_by_team_last_n_match_records[`Won?` == FALSE, "WonStr" := "✖"]
  col_expr <- parse(text = paste0("list(\"Last ", n, "\" = paste(`WonStr`, collapse=\"\"))"))
  (
    df_by_team_last_n_match_records
    [, eval(col_expr), by = `Team`]
  )
}

scorecard <- function(df_matches, lastn = 5, nrr_round_digits = 2) {
  df_win_stats <- rbindlist(list(
    df_matches[
      Winner == "Home",
      list(
        "Win Points" = .N * 2,
        "Num Matches" = .N,
        "Num Losses" = 0,
        "Runs For NRR" = sum(`Runs For NRR (Home)`),
        "Balls For NRR" = sum(`Balls For NRR (Home)`),
        "Opponent Runs For NRR" = sum(`Runs For NRR (Away)`),
        "Opponent Balls For NRR" = sum(`Balls For NRR (Away)`)
      ),
      by = list("Team" = `Home Team`)
    ],
    df_matches[
      Winner == "Away",
      list(
        "Win Points" = .N * 2,
        "Num Matches" = .N,
        "Num Losses" = 0,
        "Runs For NRR" = sum(`Runs For NRR (Away)`),
        "Balls For NRR" = sum(`Balls For NRR (Away)`),
        "Opponent Runs For NRR" = sum(`Runs For NRR (Home)`),
        "Opponent Balls For NRR" = sum(`Balls For NRR (Home)`)
      ),
      by = list("Team" = `Away Team`)
    ]
  ))
  
  df_loss_stats <- rbindlist(list(
    df_matches[
      Winner == "Away",
      list(
        "Win Points" = 0,
        "Num Matches" = .N,
        "Num Losses" = .N,
        "Runs For NRR" = sum(`Runs For NRR (Home)`),
        "Balls For NRR" = sum(`Balls For NRR (Home)`),
        "Opponent Runs For NRR" = sum(`Runs For NRR (Away)`),
        "Opponent Balls For NRR" = sum(`Balls For NRR (Away)`)
      ),
      by = list("Team" = `Home Team`)
    ],
    df_matches[
      Winner == "Home",
      list(
        "Win Points" = 0,
        "Num Matches" = .N,
        "Num Losses" = .N,
        "Runs For NRR" = sum(`Runs For NRR (Away)`),
        "Balls For NRR" = sum(`Balls For NRR (Away)`),
        "Opponent Runs For NRR" = sum(`Runs For NRR (Home)`),
        "Opponent Balls For NRR" = sum(`Balls For NRR (Home)`)
      ),
      by = list("Team" = `Away Team`)
    ]
  ))
  
  df_by_team_last_n <- by_team_last_n(df_matches, n = lastn)
  
  (
    merge.data.table(
      rbindlist(list(df_win_stats, df_loss_stats))[
        ,
        list(
          "Num Matches" = sum(`Num Matches`),
          "Num Wins" = sum(`Win Points`) %/% 2,
          "Num Losses" = sum(`Num Losses`),
          "NRR" = round(
            (
              (
                (sum(`Runs For NRR`) / sum(`Balls For NRR`))
                - (sum(`Opponent Runs For NRR`) / sum(`Opponent Balls For NRR`))
              )
              * 6
            ),
            digits = nrr_round_digits
          ),
          "Win Points" = sum(`Win Points`)
        ),
        by = `Team`
      ],
      df_by_team_last_n,
      by="Team"
    )
    [order(-`Win Points`, -`NRR`)]
  )
}

scorecard_hist <- function(df_matches, days_till, lastn = 5, nrr_round_digits = 2) {
  df_matches <- df_matches[`Match Day` <= days_till]
  dt <- last_completed_match_date(df_matches)
  df_ret <- scorecard(df_matches, lastn = lastn, nrr_round_digits = nrr_round_digits)
  df_ret[,`:=`("Days Till" = days_till, "Date" = dt)]
  df_ret
}

last_completed_match_date <- function(df_matches) {
  df_matches[!is.na(`Winner`), max(`Datestamp`)] |> as.IDate()
}

last_completed_match_day <- function(df_matches) {
  df_matches[!is.na(`Winner`), max(`Match Day`)]
}