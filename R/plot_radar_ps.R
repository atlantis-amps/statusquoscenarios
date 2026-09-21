#' Create Radar Charts for Puget Sound Vital Signs
#'
#' Generates side-by-side radar charts comparing biomass and abundance indicators
#' aggregated at the Puget Sound scale (rather than by basin).
#'
#' @param vital.signs.data A data frame containing vital signs indicators with columns:
#'   \itemize{
#'     \item \code{indicator_short}: Short name of the indicator
#'     \item \code{parameter}: Either "biomass" or "abundance"
#'     \item \code{scenario}: Scenario name/identifier
#'     \item \code{value}: Numeric value for the indicator
#'     \item \code{basin}: (optional, will be filtered for NA values)
#'   }
#' @param scenarios Character string specifying the scenarios being compared
#'   (used in output file naming).
#'
#' @details
#' This function aggregates indicators across basins and creates normalized radar
#' charts (0-1 scale) for both biomass and abundance parameters. Two radar charts
#' are placed side-by-side using cowplot. The "Darjeeling1" color palette from
#' wesanderson is used to distinguish scenarios. Output files are saved to the
#' output directory with naming convention: \code{radar_puget_sound_[scenarios].png}
#'
#' @return Invisibly returns NULL. Generates PNG file to the output directory.
#'
#' @examples
#' \dontrun{
#'   data <- get_radar_data(output.files)
#'   plot_radar_ps(vital.signs.data = data, scenarios = "2020-2040")
#' }
#'
#' @export
#'
#' @importFrom dplyr filter summarise group_by mutate ungroup select if_else
#' @importFrom tidyr pivot_wider
#' @importFrom ggradar ggradar
#' @importFrom cowplot plot_grid theme_half_open
#' @importFrom ggplot2 ggplot theme element_text element_blank ggsave
#' @importFrom sysfonts font_add_google
#' @importFrom wesanderson wes_palette
#' @importFrom here here
#'
plot_radar_ps <- function(vital.signs.data, scenarios){
  
  
  vital.signs.data <- vital.signs.data %>% 
    dplyr::filter(!is.na(basin))
  

    radar.plot.data <- vital.signs.data %>%
    dplyr::summarise(tot_value=sum(value), .by=c("indicator_short", "parameter", "scenario")) %>% 
    dplyr::group_by(indicator_short,parameter) %>%   
    dplyr::mutate(max_val = max(tot_value)) %>%
    dplyr::mutate(corr_val = tot_value/max_val) %>%
    dplyr::ungroup() %>%
    dplyr::select(-tot_value) %>% 
    dplyr::mutate(corr_val = dplyr::if_else(is.nan(corr_val),0,corr_val)) %>% 
    dplyr::mutate(indicator_short=gsub(" abundance","",indicator_short)) %>% 
    dplyr::mutate(indicator_short=dplyr::if_else(indicator_short=="Resident killer whales nums","Southern residents",
                                                 dplyr::if_else(indicator_short=="Spawning Pacific Herring","Spawning herring",indicator_short))) 
  
  #https://docs.google.com/spreadsheets/d/1PxZQwlBadcxI0G-Z7i8DMfrgsqAoWperfA76rXq4yIs/edit#gid=0
  #each column should be an indicator, first column are scenarios
  
  
  #Add nice font
  #download.file("https://github.com/ricardo-bion/ggtech/blob/master/Circular%20Air-Light%203.46.45%20PM.ttf", "~/Circular Air-Light 3.46.45 PM.ttf", method = "curl")
  
  #extrafont::font_import(pattern = 'Circular', prompt = FALSE)
  
 
    radar.data <- radar.plot.data %>%
      dplyr::ungroup() 
    
    # Color for basins
    # See for colors
    lcols_full <- psimfcolors::psimf_palette("decades")
    lcols <- lcols_full[names(lcols_full) %in% unique(radar.data$scenario)]
    
    sysfonts::font_add_google("Roboto", "roboto")
    
    
    radar.plot.biomass <- radar.data %>% 
      dplyr::filter(parameter=="biomass") %>% 
      dplyr::select(-max_val, -parameter) %>%  
      tidyr::pivot_wider(names_from=indicator_short, values_from=corr_val) %>%
      dplyr::mutate(scenario=as.factor(scenario)) 
    
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
      dplyr::select(-max_val, -parameter) %>%  
      tidyr::pivot_wider(names_from=indicator_short, values_from=corr_val) %>%
      dplyr::mutate(scenario=as.factor(scenario)) 
    
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
    
    basin.grid <- cowplot::plot_grid(biomass.radar, abundance.radar, labels = c("Puget Sound", ''), ncol = 2, nrow = 1, label_size = 16)
       
  #  cowplot::save_plot(here::here("output",paste0("radar_",this_basin,".png")), basin.grid, base_height = 8, base_width = 16)
       
    ggplot2::ggsave(paste0("radar_puget_sound_",scenarios,".png"), basin.grid, path = here::here("output"), dpi=300, width=16, height=8)
    

#add phylopic icon to plot
#beaver_plot + add_phylopic(beaver_pic, alpha = 1, x = 10, y = 37.4, ysize = 10)

}

