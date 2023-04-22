library(data.table)

build_by_team_match_records <- function(df_matches) {
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

build_by_team_last_n_match_records <- function(df_matches, n) {
  df_by_team_match_records <- build_by_team_match_records(df_matches)
  setkey(df_by_team_match_records, `Team`)
  df_by_team_match_records[, rank := .N:1, by = list(`Team`)]
  df_by_team_match_records[rank <= n][order(-rank)][,!"rank"]
}

build_by_team_last_n <- function(df_matches, n) {
  df_by_team_last_n_match_records <- build_by_team_last_n_match_records(df_matches, n)
  df_by_team_last_n_match_records[`Won?` == TRUE, "WonStr" := "✔"]
  df_by_team_last_n_match_records[`Won?` == FALSE, "WonStr" := "✖"]
  col_expr <- parse(text = paste0("list(\"Last ", n, "\" = paste(`WonStr`, collapse=\"\"))"))
  (
    df_by_team_last_n_match_records
    [, eval(col_expr), by = `Team`]
  )
}

build_scorecard <- function(df_matches, lastn = 5, nrr_round_digits = 2) {
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
  
  df_by_team_last_n <- build_by_team_last_n(df_matches, n = lastn)
  
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
    [, "Rank" := 1:.N]
  )
}

build_scorecard_hist <- function(df_matches, days_till, lastn = 5, nrr_round_digits = 2) {
  df_matches <- df_matches[`Match Day` <= days_till]
  dt <- get_last_completed_match_date(df_matches)
  df_ret <- build_scorecard(df_matches, lastn = lastn, nrr_round_digits = nrr_round_digits)
  df_ret[,`:=`("Days Till" = days_till, "Date" = dt)]
  df_ret
}

get_last_completed_match_date <- function(df_matches) {
  df_matches[!is.na(`Winner`), max(`Datestamp`)] |> as.IDate()
}

get_last_completed_match_day <- function(df_matches) {
  df_matches[!is.na(`Winner`), max(`Match Day`)]
}

estimate_prob_wins <- function(df_matches_till_date, team_a, team_b) {
  build_matches_where_a_team_is_involved <- function(team) {
    rbindlist(list(
      df_matches_till_date
      [`Home Team` == team]
      [
        ,
        list(
          "Team" = `Home Team`,
          "Opponent Team" = `Away Team`,
          "Won?" = `Winner` == "Home"
        )
      ],
      df_matches_till_date
      [`Away Team` == team]
      [
        ,
        list(
          "Team" = `Away Team`,
          "Opponent Team" = `Home Team`,
          "Won?" = `Winner` == "Away"
        )
      ]
    ))
  }
  
  df_matches_where_team_a_is_involved <- build_matches_where_a_team_is_involved(team_a)
  df_matches_where_team_b_is_involved <- build_matches_where_a_team_is_involved(team_b)
  
  common_opponents <- intersect(
    df_matches_where_team_a_is_involved$`Opponent Team`,
    df_matches_where_team_b_is_involved$`Opponent Team`
  )
  
  prob_a_win <- {
    df_matches_where_team_a_is_involved_and_common_opponents_with_b <- (
      df_matches_where_team_a_is_involved
      [`Opponent Team` %in% common_opponents]
    )
    (
      nrow(df_matches_where_team_a_is_involved_and_common_opponents_with_b[`Won?` == TRUE])
      / nrow(df_matches_where_team_a_is_involved_and_common_opponents_with_b)
    )
  }
  
  prob_b_win <- {
    df_matches_where_team_b_is_involved_and_common_opponents_with_a <- (
      df_matches_where_team_b_is_involved
      [`Opponent Team` %in% common_opponents]
    )
    (
      nrow(df_matches_where_team_b_is_involved_and_common_opponents_with_a[`Won?` == TRUE])
      / nrow(df_matches_where_team_b_is_involved_and_common_opponents_with_a)
    )
  }
  c(prob_a_win, prob_b_win)
}
