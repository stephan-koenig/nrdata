#' Compute win rates for corp IDs over releases.
#'
#' This function loads a cleaned netrunner data set provided by the
#' nrdata package and computes the win rates for each corp ID against each
#' runner ID, and the corresponding matchup frequency for each release period.
#'
#' @return A data frame grouped by corp IDs, runner IDs, and release with the
#' corresponding corp win rate and matchup frequency.
#' @import dplyr
#'
#' @export
#'

get_win_rates <- function() {
  # CHECK IF NRDATA IS PRESENT, IF NOT...
  load("nrdata.RData")

  # Remove any entries with NA's for release.
  ### REMOVE AFTER DATA CLEANING IS FIXED!!!!
  nrdata <- nrdata %>%
    filter(!is.na(release))

  corps <- levels(nrdata$corp_id)
  runners <- levels(nrdata$runner_id)
  releases <- levels(nrdata$release)

  # Determine total number of games for each matchup during each release.
  matchup <- nrdata %>%
    group_by(corp_id, runner_id, release) %>%
    summarize(matchup_games = n()) %>%
    ungroup()

  # Calculate total number of games by release.
  release <- matchup %>%
    group_by(release) %>%
    summarize(release_games = sum(matchup_games)) %>%
    ungroup()

  # Determine corp wins for each matchup during each release.
  corp_wins <- nrdata %>%
    filter(winner == "corp") %>%
    group_by(corp_id, runner_id, release) %>%
    summarize(corp_wins = n()) %>%
    ungroup()

  combined <- full_join(matchup, corp_wins,
                        by = c("corp_id", "runner_id", "release"))

  combined <- full_join(combined, release, by = "release")

  # Replace corp_wins NA's with 0.
  combined <- combined %>%
    mutate(corp_wins = replace(corp_wins, which(is.na(corp_wins)), 0),
           corp_wins_percent = corp_wins/matchup_games * 100,
           matchup_freq = matchup_games/release_games * 100)

  # Ensure that a complete set of all corp, runner, release combinations are
  # represented.
  full_set <- tbl_df(expand.grid(corps, runners, releases))
  names(full_set) <- c("corp_id", "runner_id", "release")
  combined <- right_join(combined, full_set,
                         by = c("corp_id", "runner_id", "release"))
  save(combined, file = "combined.RData")
}
