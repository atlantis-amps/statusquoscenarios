#' Get Basin and Atlantis Intersection
#'
#' Combines all basin shapefiles and intersects them with Atlantis polygons.
#' Creates a visualization showing the overlap and returns intersection data.
#'
#' @param atlantis.dir Character string specifying the path to the Atlantis polygon file
#'   (relative to the data directory)
#'
#' @return A data frame containing the intersection of basin and Atlantis polygons
#'   with columns for basin name and Box ID
#'
#' @details
#' The function combines all basin shapefiles from the data directory,
#' intersects them with Atlantis polygons, and creates a visualization map
#' saved as "map_basins.jpg" showing the overlap for verification purposes.
#'
#' @export
#'
#' @importFrom sf st_read st_crs st_transform st_join st_intersects st_drop_geometry
#' @importFrom dplyr select rename arrange
#' @importFrom ggplot2 ggplot geom_sf geom_sf_label theme_bw ggsave aes labs theme element_text element_line
#' @importFrom ggspatial annotation_scale annotation_north_arrow north_arrow_fancy_orientations
#' @importFrom here here
#'

#' Combine All Basin Shapefiles
#'
#' Reads and combines all shapefile polygons from the data directory
#' (excluding those in subdirectories) into a single spatial object.
#'
#' @return An sf object containing combined basin polygons with a basin name column
#'
#' @details
#' This function finds all .shp files directly in the data directory,
#' reads them, and combines them into a single spatial data frame.
#' Each polygon is labeled with its source filename (without extension).
#'
#' @importFrom sf st_read
#' @importFrom here here
#' @importFrom dplyr bind_rows mutate
#'
#' @keywords internal
#'


get_basin <- function(basin.dir, atlantis.dir, output.dir){
  
  basin.pol <- sf::st_read(here::here("data",basin.dir)) %>% 
    dplyr::select(-CNT_NAME, -SUM_AREA, -WEB_GEOMET) %>% 
    dplyr::rename(basin=NAME)
  
  crs.basin <- sf::st_crs(basin.pol)

  # Load watersheds from combined shapefiles
  watersheds <- combine_basin_shapefiles() %>%
    sf::st_transform(crs = crs.basin)

  #read Atlantis polygon
  
  atlantis.pol <- sf::st_read(here::here("data",atlantis.dir)) %>% 
    dplyr::select(BOX_ID) %>% 
    dplyr::rename(Box=BOX_ID)
  
  atlantis.tr <- sf::st_transform(atlantis.pol, crs = crs.basin)
  
  
  #plot both layers to check overlap
  
  pal <- wesanderson::wes_palette(9, name = "Zissou1", type = "continuous")
  
  area.plot <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = basin.pol, ggplot2::aes(fill = basin), color = "white", alpha = 0.8) +
    ggplot2::geom_sf(data = watersheds, ggplot2::aes(color = basin), alpha = 0.3, linewidth = 0.3) +
    #    ggplot2::geom_sf_label(data = watersheds, ggplot2::aes(label = basin), size = 2.5, alpha = 0.8) +
    ggplot2::geom_sf(data = atlantis.tr, color="darkblue", size = 0.5, fill = NA) +
    #  ggplot2::scale_color_manual(values = c("Atlantis polygons" = "darkblue")) +
    #  ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::scale_fill_manual(values = pal)+
    
    ggplot2::guides(color="none") +
    ggplot2::labs(
      title = "Atlantis polygons in basins",
      fill = "Basin",
      color = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      panel.grid.major = ggplot2::element_line(color = gray(0.8), linewidth = 0.3)
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      style = ggspatial::north_arrow_fancy_orienteering()
    )
  
  ggplot2::ggsave("map_basins.jpg", area.plot, path = here::here(output.dir), dpi=300)   
  
  area.plot.box <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = basin.pol, ggplot2::aes(fill = basin), color = "white", alpha = 0.8) +
    ggplot2::geom_sf(data = watersheds, ggplot2::aes(color = basin), alpha = 0.3, linewidth = 0.3) +
#    ggplot2::geom_sf_label(data = watersheds, ggplot2::aes(label = basin), size = 2.5, alpha = 0.8) +
    ggplot2::geom_sf(data = atlantis.tr, color="darkblue", size = 0.5, fill = NA) +
    ggplot2::geom_sf_text(data = atlantis.tr, aes(label = Box)) + 
  #  ggplot2::scale_color_manual(values = c("Atlantis polygons" = "darkblue")) +
  #  ggplot2::scale_fill_brewer(palette = "Set1")+
    ggplot2::scale_fill_manual(values = pal)+
    
  ggplot2::guides(color="none") +
     ggplot2::labs(
      title = "Atlantis polygons in basins",
      fill = "Basin",
      color = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.title = ggplot2::element_text(size = 16, face = "bold", hjust = 0.5),
      legend.position = "right",
      panel.grid.major = ggplot2::element_line(color = gray(0.8), linewidth = 0.3)
    ) +
    ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
    ggspatial::annotation_north_arrow(
      location = "tr",
      style = ggspatial::north_arrow_fancy_orienteering()
    )
  
  ggplot2::ggsave("map_basins_box_no.jpg", area.plot.box, path = here::here(output.dir), dpi=300)   
  
  pols.joined <- sf::st_join(x = basin.pol, y = atlantis.tr, join = sf::st_intersects) %>% 
    dplyr::arrange(Box)
  
  atlantis.table <- sf::st_drop_geometry(pols.joined)

  # Calculate proportion of each Atlantis box's area that overlaps each basin
  atlantis.areas <- atlantis.tr %>%
    dplyr::mutate(box_area = sf::st_area(.))

  overlap <- sf::st_intersection(atlantis.areas, basin.pol) %>%
    dplyr::mutate(
      intersection_area = sf::st_area(.),
      proportion        = as.numeric(intersection_area / box_area)
    ) %>%
    sf::st_drop_geometry() %>%
    dplyr::select(Box, basin, proportion)

  #need to normalize areas across polygons because the basin layer has all islands as empty, which reduces the proportion per box
  prop.table <- overlap %>% dplyr::summarise(tot_proportion = sum(proportion), .by= c("Box"))
  
  corr.prop <- overlap %>% dplyr::left_join(prop.table) %>% 
    dplyr::mutate(corr_prop=proportion/tot_proportion) %>% 
    dplyr::select(Box, basin, corr_prop) %>% 
    dplyr::rename(proportion = corr_prop)
  
 # corr.prop %>% dplyr::summarise(tot_prop = sum(corr_prop), .by="Box")
    
  atlantis.table.prop <- atlantis.table %>%
    dplyr::left_join(corr.prop, by = c("Box", "basin"))

  return(atlantis.table.prop)
}
