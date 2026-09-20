#' Combine All Basin Shapefiles
#'
#' Reads and combines all shapefile polygons from the data directory
#' (excluding those in subdirectories) into a single spatial object.
#'
#' @details
#' This function finds all .shp files directly in the data directory,
#' reads them, standardizes their coordinate reference systems (CRS),
#' and combines them into a single spatial data frame. Each polygon is
#' labeled with its source filename (without extension).
#'
#' @return An sf object containing combined basin polygons with a \code{basin}
#'   column identifying the source shapefile for each polygon.
#'
#' @importFrom sf st_read st_crs st_transform st_set_crs
#' @importFrom dplyr mutate bind_rows
#' @importFrom here here
#' @importFrom tools file_path_sans_ext
#'
#' @keywords internal
#'
combine_basin_shapefiles <- function() {
  data_dir <- here::here("data")
  
  # Get all .shp files directly in data directory (not in subdirectories)
  shp_files <- list.files(data_dir, pattern = "\\.shp$", full.names = TRUE, recursive = FALSE)
  
  # Read all shapefiles and standardize CRS
  shp_list <- lapply(shp_files, function(file) {
    sf::st_read(file, quiet = TRUE) %>%
      dplyr::mutate(basin = tools::file_path_sans_ext(basename(file)))
  })
  
  # Find the most common CRS (or use the first non-NA CRS)
  crs_values <- lapply(shp_list, sf::st_crs)
  crs_set <- unique(crs_values[!sapply(crs_values, is.na)])
  target_crs <- if (length(crs_set) > 0) crs_set[[1]] else NA
  
  # Transform all to the target CRS before combining
  combined_basins <- lapply(shp_list, function(x) {
    if (!is.na(sf::st_crs(x))) {
      sf::st_transform(x, crs = target_crs)
    } else {
      sf::st_set_crs(x, target_crs)
    }
  }) %>%
    dplyr::bind_rows()
  
  return(combined_basins)
}