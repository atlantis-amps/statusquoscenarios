#' Create Radar Charts for Biomass and Abundance Indicators
#'
#' Generates radar charts comparing multiple scenarios across ecological indicators
#' (biomass and abundance) by basin.
#'
#' @param vital.signs.data A data frame containing vital signs indicators with columns:
#'   \itemize{
#'     \item \code{basin}: Geographic basin identifier
#'     \item \code{scenario}: Scenario name/identifier
#'     \item \code{indicator_short}: Short name of the indicator
#'     \item \code{parameter}: Either "biomass" or "abundance"
#'     \item \code{value}: Numeric value for the indicator
#'   }
#' @param scenarios Character string specifying the scenarios being compared
#'   (used in output file naming).
#'
#' @details
#' This function creates normalized radar charts for each basin, with separate
#' panels for biomass and abundance indicators. Values are normalized to a 0-1
#' scale within each indicator and basin. The "decades" color palette from
#' psimfcolors is used to distinguish scenarios.
#'
#' Output files are saved to the output directory with naming convention:
#' \code{radar_[basin]_[scenarios].png}
#'
#' @return Invisibly returns NULL. Generates PNG files for each basin.
#'
#' @examples
#' \dontrun{
#'   data <- get_radar_data(output.files)
#'   plot_radar(vital.signs.data = data, scenarios = "2020-2040")
#' }
#'
#' @export
#'
#' @importFrom dplyr filter group_by mutate ungroup select if_else
#' @importFrom tidyr pivot_wider
#' @importFrom ggradar ggradar
#' @importFrom cowplot plot_grid theme_half_open
#' @importFrom ggplot2 ggplot theme element_text element_blank ggsave
#' @importFrom sysfonts font_add_google
#' @importFrom psimfcolors psimf_palette
#' @importFrom here here
plot_radar <- function(vital.signs.data, scenarios){


  vital.signs.data <- vital.signs.data %>%
    dplyr::filter(!is.na(basin))

  unique.basins <- unique(vital.signs.data$basin)

  # Initialize list to store convex hull areas
  convex_hull_areas <- list()
  
  radar.plot.data <- vital.signs.data %>%
    dplyr::group_by(indicator_short, basin, parameter) %>%
    dplyr::mutate(max_val = max(value)) %>%
    dplyr::mutate(corr_val = value/max_val) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value) %>% 
    dplyr::mutate(corr_val = dplyr::if_else(is.nan(corr_val),0,corr_val)) %>% 
    dplyr::mutate(indicator_short=gsub(" abundance","",indicator_short)) %>% 
    dplyr::mutate(indicator_short=dplyr::if_else(indicator_short=="Resident killer whales nums","Southern residents",
                                                 dplyr::if_else(indicator_short=="Spawning Pacific Herring","Spawning herring",indicator_short))) 
  
  #https://docs.google.com/spreadsheets/d/1PxZQwlBadcxI0G-Z7i8DMfrgsqAoWperfA76rXq4yIs/edit#gid=0
  #each column should be an indicator, first column are scenarios
  
  
  #Add nice font
  #download.file("https://github.com/ricardo-bion/ggtech/blob/master/Circular%20Air-Light%203.46.45%20PM.ttf", "~/Circular Air-Light 3.46.45 PM.ttf", method = "curl")
  
  #extrafont::font_import(pattern = 'Circular', prompt = FALSE)
  
  # Get named color vector for scenarios
  lcols_full <- psimfcolors::psimf_palette("decades")

  for(eachbasin in 1:length(unique.basins)){

    this_basin <- unique.basins[eachbasin]


    print(this_basin)

    radar.data <- radar.plot.data %>%
      dplyr::filter(basin == this_basin) %>%
      dplyr::ungroup()

    # Color for basins - subset named colors to only scenarios in this basin
    lcols <- lcols_full[names(lcols_full) %in% unique(radar.data$scenario)]
    
    sysfonts::font_add_google("Roboto", "roboto")
    
    
    radar.plot.biomass <- radar.data %>%
      dplyr::filter(parameter=="biomass") %>%
      dplyr::select(-max_val, -parameter, -basin) %>%
      tidyr::pivot_wider(names_from=indicator_short, values_from=corr_val) %>%
      dplyr::mutate(scenario=as.factor(scenario))

    # # Calculate convex hull areas for biomass by scenario
    # biomass_hull_areas <- radar.plot.biomass %>%
    #   dplyr::group_by(scenario) %>%
    #   dplyr::group_map(~ {
    #     # Get indicator columns (exclude scenario)
    #     indicator_cols <- setdiff(names(.x), "scenario")
    #     indicator_values <- as.matrix(.x[, indicator_cols])
    # 
    #     # Convert polar coordinates to Cartesian
    #     n_indicators <- length(indicator_cols)
    #     angles <- seq(0, 2*pi, length.out = n_indicators + 1)[1:n_indicators]
    # 
    #     # Convert each point from polar to Cartesian
    #     x_coords <- indicator_values[1,] * cos(angles)
    #     y_coords <- indicator_values[1,] * sin(angles)
    # 
    #     points_2d <- cbind(x_coords, y_coords)
    # 
    #     # Calculate maximum possible area (regular polygon with all values = 1)
    #     max_area <- (n_indicators * sin(2*pi / n_indicators)) / 2
    # 
    #     # Calculate convex hull if we have enough points
    #     if (nrow(points_2d) >= 3) {
    #       hull <- geometry::convhulln(points_2d)
    #       # Calculate area using the convex hull
    #       hull_points <- points_2d[c(hull[1,], hull[1,1]), ]
    #       area <- abs(sum(hull_points[-nrow(hull_points), 1] *
    #                       (hull_points[-1, 2] - hull_points[-nrow(hull_points) - 1, 2])) / 2)
    #       # Normalize area to [0, 1] range
    #       area_normalized <- area / max_area
    #     } else {
    #       area_normalized <- 0
    #     }
    # 
    #     data.frame(
    #       basin = this_basin,
    #       parameter = "biomass",
    #       scenario = unique(.x$scenario),
    #       convex_hull_area = area_normalized
    #     )
    #   }, .keep = TRUE) %>%
    #   dplyr::bind_rows() 
    
    biomass.radar <- ggradar::ggradar(radar.plot.biomass, background.circle.colour = "white",
                                      base.size = 12,
                                      axis.line.colour = "gray70",
                                      gridline.min.colour = "gray70",
                                      gridline.mid.colour = "gray70",
                                      gridline.max.colour = "gray70",
                                      axis.label.offset = 1.1,
                                      group.colours = lcols,
                                      values.radar = c(0,0.5,1),
                                      legend.position = "bottom",
                                      axis.label.size = 4,
                                      font.radar = "roboto",
                                      plot.title = "Biomass-based indicators") +
      ggplot2::theme(plot.title = ggplot2::element_text(size = 11)) +
      cowplot::theme_half_open() +
      ggplot2::theme(axis.line = ggplot2::element_blank(),
                     axis.text = ggplot2::element_blank(),
                     axis.ticks = ggplot2::element_blank(),
                     axis.title = ggplot2::element_blank()) +
      theme(legend.position = "bottom")
    
    radar.plot.abundance <- radar.data %>%
      dplyr::filter(parameter=="abundance") %>%
      dplyr::select(-max_val, -parameter, -basin) %>%
      tidyr::pivot_wider(names_from=indicator_short, values_from=corr_val) %>%
      dplyr::mutate(scenario=as.factor(scenario))

    # # Calculate convex hull areas for abundance by scenario
    # abundance_hull_areas <- radar.plot.abundance %>%
    #   dplyr::group_by(scenario) %>%
    #   dplyr::group_map(~ {
    #     # Get indicator columns (exclude scenario)
    #     indicator_cols <- setdiff(names(.x), "scenario")
    #     indicator_values <- as.matrix(.x[, indicator_cols])
    # 
    #     # Convert polar coordinates to Cartesian
    #     n_indicators <- length(indicator_cols)
    #     angles <- seq(0, 2*pi, length.out = n_indicators + 1)[1:n_indicators]
    # 
    #     # Convert each point from polar to Cartesian
    #     x_coords <- indicator_values[1,] * cos(angles)
    #     y_coords <- indicator_values[1,] * sin(angles)
    # 
    #     points_2d <- cbind(x_coords, y_coords)
    # 
    #     # Calculate maximum possible area (regular polygon with all values = 1)
    #     max_area <- (n_indicators * sin(2*pi / n_indicators)) / 2
    # 
    #     # Calculate convex hull if we have enough points
    #     if (nrow(points_2d) >= 3) {
    #       hull <- geometry::convhulln(points_2d)
    #       # Calculate area using the convex hull
    #       hull_points <- points_2d[c(hull[1,], hull[1,1]), ]
    #       area <- abs(sum(hull_points[-nrow(hull_points), 1] *
    #                       (hull_points[-1, 2] - hull_points[-nrow(hull_points) - 1, 2])) / 2)
    #       # Normalize area to [0, 1] range
    #       area_normalized <- area / max_area
    #     } else {
    #       area_normalized <- 0
    #     }
    # 
    #     data.frame(
    #       basin = this_basin,
    #       parameter = "abundance",
    #       scenario = unique(.x$scenario),
    #       convex_hull_area = area_normalized
    #     )
    #   }, .keep = TRUE) %>%
    #   dplyr::bind_rows() 
    # 
    abundance.radar <- ggradar::ggradar(radar.plot.abundance, background.circle.colour = "white",
                                      base.size = 12,
                                      axis.line.colour = "gray70",
                                      gridline.min.colour = "gray70",
                                      gridline.mid.colour = "gray70",
                                      gridline.max.colour = "gray70",
                                      axis.label.offset = 1.1,
                                      group.colours = lcols,
                                      values.radar = c(0,0.5,1),
                                      legend.position = "bottom",
                                      axis.label.size = 4,
                                      font.radar = "roboto",
                                      plot.title = "Abundance-based indicators"
                                      ) +
        ggplot2::theme(plot.title = ggplot2::element_text(size = 11)) +
      cowplot::theme_half_open() +
      ggplot2::theme(axis.line = ggplot2::element_blank(),
              axis.text = ggplot2::element_blank(),
              axis.ticks = ggplot2::element_blank(),
              axis.title = ggplot2::element_blank()) +
            theme(legend.position = "bottom")
    
    
    #  plot.extent.x.sf = 1.1,
    #  plot.extent.y.sf = 1.1)
    
    basin.grid <- cowplot::plot_grid(biomass.radar, abundance.radar, labels = c(this_basin, ''), ncol = 2, nrow = 1, label_size = 16)

   # cowplot::save_plot(here::here("output",paste0("radar_",this_basin,".png")), basin.grid, base_height = 8, base_width = 16)

    ggplot2::ggsave(paste0("radar_",this_basin,"_",scenarios, ".png"), basin.grid, path = here::here("output"), dpi=300, width=16, height=8)

    # Store convex hull areas for this basin
  #  convex_hull_areas[[eachbasin]] <- dplyr::bind_rows(biomass_hull_areas, abundance_hull_areas)

  }

  # Combine all convex hull areas into a single dataframe
  # convex_hull_summary <- dplyr::bind_rows(convex_hull_areas)
  # 
  # # Create bar plots for convex hull areas
  # # Plot by basin and parameter
  # biomass_hull_plot <- convex_hull_summary %>%
  #   dplyr::filter(parameter == "biomass") %>%
  #   ggplot2::ggplot(ggplot2::aes(x = basin, y = convex_hull_area, fill = scenario)) +
  #   ggplot2::geom_bar(stat = "identity", position = "dodge") +
  #   ggplot2::labs(
  #     title = "Convex Hull Area - Biomass Indicators",
  #     x = "Basin",
  #     y = "Convex Hull Area",
  #     fill = "Scenario"
  #   ) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
  #     plot.title = ggplot2::element_text(size = 14, face = "bold")
  #   )
  # 
  # abundance_hull_plot <- convex_hull_summary %>%
  #   dplyr::filter(parameter == "abundance") %>%
  #   ggplot2::ggplot(ggplot2::aes(x = basin, y = convex_hull_area, fill = scenario)) +
  #   ggplot2::geom_bar(stat = "identity", position = "dodge") +
  #   ggplot2::labs(
  #     title = "Convex Hull Area - Abundance Indicators",
  #     x = "Basin",
  #     y = "Convex Hull Area",
  #     fill = "Scenario"
  #   ) +
  #   ggplot2::theme_minimal() +
  #   ggplot2::theme(
  #     axis.text.x = ggplot2::element_text(angle = 45, hjust = 1),
  #     plot.title = ggplot2::element_text(size = 14, face = "bold")
  #   )
  # 
  # # Combine plots
  # hull_area_grid <- cowplot::plot_grid(biomass_hull_plot, abundance_hull_plot, ncol = 2, nrow = 1)
  # 
  # # Save combined bar plots
  # ggplot2::ggsave(
  #   paste0("convex_hull_areas_", scenarios, ".png"),
  #   hull_area_grid,
  #   path = here::here("output"),
  #   dpi = 300,
  #   width = 16,
  #   height = 6
  # )

  # # Save individual plots
  # ggplot2::ggsave(
  #   paste0("convex_hull_biomass_", scenarios, ".png"),
  #   biomass_hull_plot,
  #   path = here::here("output"),
  #   dpi = 300,
  #   width = 10,
  #   height = 6
  # )
  # 
  # ggplot2::ggsave(
  #   paste0("convex_hull_abundance_", scenarios, ".png"),
  #   abundance_hull_plot,
  #   path = here::here("output"),
  #   dpi = 300,
  #   width = 10,
  #   height = 6
  # )
  # 
  # #add phylopic icon to plot
  # #beaver_plot + add_phylopic(beaver_pic, alpha = 1, x = 10, y = 37.4, ysize = 10)
  # 
  # readr::write_csv(convex_hull_summary,here::here("output","convex_hull_summary.csv"))
}

