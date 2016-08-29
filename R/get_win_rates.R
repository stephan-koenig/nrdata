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
  load("output/nrdata.RData")

  # Create a table with all possible combinations of corp, runner, and release.
  corps <- levels(nrdata$corp_id)
  runners <- levels(nrdata$runner_id)
  releases <- levels(nrdata$release)
  full_set <- tbl_df(expand.grid(corps, runners, releases))
  names(full_set) <- c("corp_id", "runner_id", "release")

  # Determine total number of games and corp win reate for each matchup during
  # each release.
  matchup <- nrdata %>%
    group_by(corp_id, runner_id, release) %>%
    summarize(matchup_games = n(),
              corp_win_rate = sum(winner == "corp") / n() * 100) %>%
    ungroup()

  # Calculate total number of games by release.
  release <- nrdata %>%
    group_by(release) %>%
    summarize(release_games = n())

  # Combine all derived variables.
  combined <- full_join(matchup, release, by = "release")

  # Replace corp_win_rate NA's with 0.
  combined <- combined %>%
    mutate(matchup_freq = matchup_games / release_games * 100)

  # Ensure that a complete set of all corp, runner, release combinations are
  # represented.
  combined <- right_join(combined, full_set,
                         by = c("corp_id", "runner_id", "release"))
  save(combined, file = "output/combined.RData")
}
