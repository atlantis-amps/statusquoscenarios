#' Plot Average Catch per Fishery Across Atlantis Scenarios
#'
#' Reads \code{AMPS_OUTCatchPerFishery.txt} from each supplied Atlantis output
#' folder, computes the mean total catch per fishery over the last
#' \code{n_last_years} years of each run, and returns a faceted bar plot
#' comparing scenarios side-by-side within each fishery panel.
#'
#' The file has one row per (time, fishery) combination.  The \code{Time}
#' column records elapsed simulation days; the underlying Atlantis model uses
#' \code{timesteps_per_year} five-day steps per year, so each annual output
#' row corresponds to \code{365} days.  The \code{Fishery} column contains the
#' fishery name, and every remaining column is the catch (tonnes) for one
#' functional group.  Total catch for a fishery is the row sum across all
#' group columns.
#'
#' @param folder.paths Character vector of paths to Atlantis output folders,
#'   each containing an \code{AMPS_OUTCatchPerFishery.txt} file.
#' @param scenario.names Character vector of human-readable scenario labels,
#'   in the same order as \code{folder.paths}.
#' @param n_last_years Integer. Number of final simulation years to average
#'   over. Default \code{5}.
#' @param timesteps_per_year Integer. Number of model timesteps per calendar
#'   year (used to convert the \code{Time} column from days to years).
#'   Default \code{73}.
#'
#' @return A \code{ggplot} object: a bar plot with one bar per scenario,
#'   faceted by fishery, coloured with the \code{"Zissou1"} palette from
#'   \pkg{wesanderson}.
#'
#' @examples
#' \dontrun{
#' folder.paths <- c(
#'   "/nfsdata/status_quo_atlantis_runs/Atlantis_base_2025_2020/outputFolder",
#'   "/nfsdata/status_quo_atlantis_runs/Atlantis_base_2025_2030/outputFolder",
#'   "/nfsdata/status_quo_atlantis_runs/Atlantis_base_2025_2040/outputFolder"
#' )
#' scenario.names <- c("Base scenario", "2030", "2040")
#' p <- plot_catch_per_fishery_scenarios(folder.paths, scenario.names)
#' print(p)
#' ggsave("catch_per_fishery_scenarios.pdf", p, width = 14, height = 10)
#' }
#'
#' @importFrom readr read_table cols col_double col_character
#' @importFrom dplyr mutate filter group_by summarise left_join
#' @importFrom tidyr pivot_longer
#' @importFrom magrittr %>%
#' @importFrom purrr map2_dfr
#' @importFrom ggplot2 ggplot aes geom_col facet_wrap labs scale_fill_manual
#'   theme_bw theme element_text
#' @importFrom wesanderson wes_palette
#' @export
plot_catch_per_fishery_scenarios <- function(folder.paths,
                                             scenario.names,
                                             n_last_years       = 5L,
                                             timesteps_per_year = 73L) {

  stopifnot(
    length(folder.paths) == length(scenario.names),
    all(file.exists(file.path(folder.paths, "AMPS_OUTCatchPerFishery.txt")))
  )

  days_per_year <- 365  # Atlantis Time column is always in days

  # ---- 1. Read all scenarios and compute annual total catch per fishery ------
  combined <- purrr::map2_dfr(
    folder.paths,
    scenario.names,
    function(folder, scenario) {

      fpath <- file.path(folder, "AMPS_OUTCatchPerFishery.txt")

      raw <- readr::read_table(
        fpath,
        col_types = readr::cols(
          Time    = readr::col_double(),
          Fishery = readr::col_character(),
          .default = readr::col_double()
        )
      )

      raw %>%
        dplyr::mutate(year = Time / days_per_year,
                      scenario = scenario) %>%
        tidyr::pivot_longer(
          cols      = -c(Time, Fishery, year, scenario),
          names_to  = "group",
          values_to = "catch"
        ) %>%
        dplyr::group_by(Time, Fishery, year, scenario) %>%
        dplyr::summarise(total_catch = sum(catch, na.rm = TRUE),
                         .groups = "drop")
    }
  )

  # ---- 2. Keep only the last n_last_years of each scenario ------------------
  cutoffs <- combined %>%
    dplyr::group_by(scenario) %>%
    dplyr::summarise(cutoff_year = max(year) - n_last_years, .groups = "drop")

  avg_catch <- combined %>%
    dplyr::left_join(cutoffs, by = "scenario") %>%
    dplyr::filter(year > cutoff_year) %>%
    dplyr::group_by(scenario, Fishery) %>%
    dplyr::summarise(mean_catch = mean(total_catch, na.rm = TRUE),
                     .groups = "drop") %>%
    dplyr::mutate(scenario = factor(scenario, levels = scenario.names))

  # ---- 3. Colours -----------------------------------------------------------
  pal <- wesanderson::wes_palette(
    "Zissou1",
    n    = length(scenario.names),
    type = "continuous"
  )
  names(pal) <- scenario.names

  # ---- 4. Plot --------------------------------------------------------------
  ggplot2::ggplot(avg_catch,
                  ggplot2::aes(x = scenario, y = mean_catch, fill = scenario)) +
    ggplot2::geom_col(width = 0.7) +
    ggplot2::facet_wrap(~ Fishery, scales = "free_y") +
    ggplot2::scale_fill_manual(values = pal) +
    ggplot2::labs(
      title = paste0("Mean catch per fishery \u2013 last ", n_last_years,
                     " years of simulation"),
      x     = NULL,
      y     = "Mean total catch (tonnes)",
      fill  = "Scenario"
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.text      = ggplot2::element_text(size = 8, face = "bold"),
      axis.text.x     = ggplot2::element_text(angle = 35, hjust = 1, size = 7),
      legend.position = "bottom"
    )
}
