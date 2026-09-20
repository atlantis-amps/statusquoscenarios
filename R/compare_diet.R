#' @title Extract and compare Atlantis diet output across scenarios
#'
#' @author Hem Nalini Morzaria Luna
#' @description
#' Reads \code{AMPS_OUTDietCheck.txt} from each scenario output folder,
#' averages diet proportions over the last \code{avg.years} of each run,
#' and produces stacked-bar comparison plots (one PNG per predator)
#' Call the function once per mirroring the pattern
#' used by \code{plot_biomass_heatmap}.
#'
#' @details
#' The Atlantis diet-check file is spatially aggregated (no Box column).
#' \code{atlantis.table} therefore organise the output
#' files and plot titles rather than filter rows of the diet data.
#' If box-level diet data becomes available (\code{AMPS_OUTBoxDietCheck.txt}),
#' the extraction step can be extended to use \code{atlantis.table$Box} for
#' true spatial subsetting.
#'
#' @param folder.paths   Character vector of output folder paths, one per
#'   scenario (e.g. lines 60-70 of statusquoscenarios.Rmd).
#' @param scenario.names Character vector of scenario labels, same length as
#'   \code{folder.paths}.
#' @param functionalgroup.file Full path to the functional-groups CSV
#'   (e.g. \code{here::here("data","PugetSoundAtlantisFunctionalGroups_2024_V1_SQ.csv")}).
#' @param diet.file       Diet check filename. Default
#'   \code{"AMPS_OUTDietCheck.txt"}.
#' @param predators.to.plot Character vector of predator \code{Name} or
#'   \code{Code} values to include.  \code{NULL} (default) plots all active
#'   predator groups.
#' @param threshold       Minimum summed diet proportion (across all scenarios
#'   and cohorts) for a prey item to appear in the plot.  Default \code{0.01}.
#' @param startyear       Model start year.  Default \code{2011}.
#' @param timeperiod      Model output frequency in days.  Default \code{73}.
#' @param avg.years       Number of years at the end of each run to average
#'   over.  Default \code{5}.
#' @param output.dir      Directory for saved PNG files.  Created if absent.
#'   Default \code{"output"}.
#'
#' @return Invisibly returns the combined (multi-scenario) averaged diet data
#'   frame, which can be passed to further analysis.
#' @export
#'
#' @examples
#' \dontrun{
#' folder.paths <- c(
#'   "/nfsdata/newMar2026_2011SQrun/outputFolder",
#'   "/nfsdata/status_quo_atlantis_runs/Atlantis_base_2025_2020/outputFolder",
#'   "/nfsdata/status_quo_atlantis_runs/Atlantis_base_2025_2030/outputFolder",
#'   "/nfsdata/status_quo_atlantis_runs/Atlantis_base_2025_2040/outputFolder"
#' )
#' scenario.names <- c("Base 2011", "2020", "2030", "2040")
#' fg.file <- here::here("data", "PugetSoundAtlantisFunctionalGroups_2024_V1_SQ.csv")
#'
#' }
#' }

compare_diet <- function(folder.paths,
                          scenario.names,
                          functionalgroup.file,
                          diet.file,
                          predators.to.plot = NULL,
                          threshold,
                          startyear,
                          timeperiod,
                          avg.years,
                          output.dir) {

  # ── Functional groups ──────────────────────────────────────────────────────
  fgrps <- readr::read_csv(here::here("data",functionalgroup.file), show_col_types = FALSE) %>%
    dplyr::select(Code, Name, longname, IsTurnedOn, GroupType, IsPredator) %>%
    dplyr::filter(IsTurnedOn == 1)

  prey.names <- fgrps %>%
    dplyr::select(Code, longname) %>%
    dplyr::rename(prey_code = Code, prey_longname = longname)

  # Active predator groups (mirrors get_groups logic from atlantisplotter)
  pred.groups <- fgrps %>%
    dplyr::filter(
      !GroupType %in% c("SM_PHY","CARRION","LAB_DET","PL_BACT",
                        "SED_BACT","PHYTOBEN","SEAGRASS"),
      IsPredator == 1
    )

  if (!is.null(predators.to.plot)) {
    pred.groups <- pred.groups %>%
      dplyr::filter(Name %in% predators.to.plot | Code %in% predators.to.plot)
  }

  # ── Read diet check files from all scenario folders ────────────────────────
  
  read_diet<- function(i, folder.paths){
    diet.path <- here::here(folder.paths[i], diet.file)
    if (!file.exists(diet.path)) {
      warning("Diet file not found, skipping: ", diet.path)
      return(NULL)
    }
    utils::read.table(diet.path, header = TRUE, sep = " ", as.is = TRUE) %>%
      dplyr::mutate(
        scenario = scenario.names[i],
        Year     = startyear + Time / 365
      )
  }
  
  folder.length <- 1:length(folder.paths)
  diet.raw <- lapply(folder.length, read_diet, folder.paths)
                     
  
  diet.all <- dplyr::bind_rows(Filter(Negate(is.null), diet.raw))

  if (nrow(diet.all) == 0) {
    warning("No diet data could be read from any folder.")
    return(invisible(diet.all))
  }

  # ── Average over the last avg.years of each scenario run ───────────────────
  # Find the last available year per scenario and retain records within the window
  diet.avg <- diet.all %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(max_year = max(Year)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(Year >= max_year - avg.years) %>%
    dplyr::select(-max_year)

  # Prey columns = everything after the fixed header columns
  fixed.cols <- c("Time","Predator","Cohort","Stock","Updated","scenario","Year")
  prey.cols  <- setdiff(names(diet.avg), fixed.cols)

  # Average diet proportions across timesteps within each scenario × cohort
  diet.mean <- diet.avg %>%
    dplyr::group_by(scenario, Predator, Cohort) %>%
    dplyr::summarise(
      dplyr::across(dplyr::all_of(prey.cols), ~ mean(.x, na.rm = TRUE)),
      .groups = "drop"
    ) %>%
    dplyr::mutate(scenario = factor(scenario, levels = scenario.names))

  # ── Prepare output directory ───────────────────────────────────────────────
  dir.create(output.dir, showWarnings = FALSE, recursive = TRUE)

  # ── Build a global prey-label palette so each prey item always gets the
  #    same colour regardless of which predator plot it appears in ───────────
  all.prey.labels <- diet.mean %>%
    dplyr::select(Predator, dplyr::all_of(prey.cols)) %>%
    tidyr::pivot_longer(
      cols      = dplyr::all_of(prey.cols),
      names_to  = "prey_code",
      values_to = "proportion"
    ) %>%
    dplyr::group_by(prey_code) %>%
    dplyr::summarise(total = sum(proportion, na.rm = TRUE), .groups = "drop") %>%
    dplyr::filter(total > threshold) %>%
    dplyr::left_join(prey.names, by = "prey_code") %>%
    dplyr::mutate(prey_label = dplyr::coalesce(prey_longname, prey_code)) %>%
    dplyr::pull(prey_label) %>%
    sort() %>%
    unique()

  global.n.prey  <- length(all.prey.labels)
  global.palette <- stats::setNames(
    grDevices::colorRampPalette(
      RColorBrewer::brewer.pal(min(9L, global.n.prey), "Set1")
    )(global.n.prey),
    all.prey.labels
  )

  # ── Plot one PNG per predator ──────────────────────────────────────────────
  for (pred.code in pred.groups$Code) {

    pred.longname <- pred.groups$longname[pred.groups$Code == pred.code]
    if (length(pred.longname) == 0) pred.longname <- pred.code

    sub.diet <- diet.mean %>%
      dplyr::filter(Predator == pred.code)

    if (nrow(sub.diet) == 0) next

    # Select prey above threshold (summed across all scenarios and cohorts)
    prey.totals  <- colSums(sub.diet[prey.cols], na.rm = TRUE)
    selec.prey   <- names(prey.totals[prey.totals > threshold])

    if (length(selec.prey) == 0) next

    # Pivot to long format and attach prey long names
    plot.data <- sub.diet %>%
      dplyr::select(scenario, Cohort, dplyr::all_of(selec.prey)) %>%
      tidyr::pivot_longer(
        cols      = dplyr::all_of(selec.prey),
        names_to  = "prey_code",
        values_to = "proportion"
      ) %>%
      dplyr::left_join(prey.names, by = "prey_code") %>%
      dplyr::mutate(
        prey_label = factor(
          dplyr::coalesce(prey_longname, prey_code),
          levels = all.prey.labels
        )
      )

    diet.plot <- ggplot2::ggplot(
      plot.data,
      ggplot2::aes(
        x    = scenario,
        y    = proportion * 100,
        fill = prey_label,
        color = prey_label
      )
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::scale_fill_manual(values  = global.palette, name = "Prey", drop = TRUE) +
      ggplot2::scale_color_manual(values = global.palette, name = "Prey", drop = TRUE) +
      ggplot2::facet_wrap(~ paste("Age", Cohort)) +
      ggplot2::labs(
        title    = paste("Diet of", pred.longname),
        subtitle = paste0("\u2014 average last ", avg.years, " years"),
        y        = "Diet proportion (%)",
        x        = "Scenario"
      ) +
      ggplot2::theme_minimal() +
      ggplot2::theme(
        axis.text.x    = ggplot2::element_text(angle = 45, hjust = 1),
        legend.position = "bottom"
      )

    filename <- paste0(
      "diet_", pred.code, ".png"
    )
    ggplot2::ggsave(
      filename = here::here(output.dir, filename),
      plot     = diet.plot,
      width    = 12, height = 8, dpi = 150
    )
  }

  invisible(diet.mean)
}
