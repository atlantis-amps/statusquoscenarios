#' Plot Biomass Change Heatmaps by Basin and Guild
#'
#' Creates heatmaps showing relative biomass change across scenarios by guild
#' for a specific basin and target scenario year.
#'
#' @param this.basin Character. The basin name to filter and plot.
#' @param basin.data Data frame. Biomass data with columns:
#'   \itemize{
#'     \item \code{basin}: Basin identifier
#'     \item \code{scenario}: Scenario identifier
#'     \item \code{guild}: Functional group guild
#'     \item \code{rel_biomass}: Relative biomass change (%)
#'   }
#' @param output.dir Character. Output directory for saving plots.
#' @param target.sc Numeric. Target scenario year (e.g., 2040, 2090) for filtering
#'   and output naming.
#'
#' @details
#' This function creates two heatmaps:
#' - One filtered to exclude certain guilds (Shrimp & squid for 2040;
#'   Shrimp & squid and Forage fish for 2090)
#' - One showing all guilds
#'
#' Both plots use a diverging RdBu color palette centered at zero, with
#' publication-ready formatting. Plots are saved as JPEG files with 300 DPI.
#'
#' @return Invisibly returns NULL. Saves JPEG files to output directory.
#'
#' @export
#'
#' @importFrom dplyr filter mutate
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_distiller theme_minimal theme element_text element_blank element_rect labs ggsave
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#'   biomass <- read.csv("biomass_data.csv")
#'   plot_biomass_heatmap(
#'     this.basin = "Central Puget Sound",
#'     basin.data = biomass,
#'     output.dir = "output",
#'     target.sc = 2040
#'   )
#' }
plot_biomass_heatmap <- function(this.basin, basin.data, output.dir, target.sc) {

  
  plot.data <- basin.data %>% 
    dplyr::filter(basin==this.basin) %>% 
     dplyr::mutate(scenario=as.factor(scenario))
    
  
   
  if(target.sc==2040){

    heatmap.data <- plot.data %>% 
      dplyr::filter(!guild=="Shrimp & squid") 
    
  }
  
  if(target.sc==2090){
    
    heatmap.data <- plot.data %>% 
      dplyr::filter(!guild %in% c("Shrimp & squid","Forage fish")) 
    
  }
  
  max_val <- max(abs(heatmap.data$rel_biomass))
  
  
   heatmap.plot <- heatmap.data %>% 
     ggplot2::ggplot(ggplot2::aes(x = scenario, y = guild,
                                  fill = rel_biomass)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_distiller(palette = "RdBu", limits = c(-max_val, max_val)) + 
     
   # ggplot2::scale_fill_viridis_c(name = "Mean relative\nchange (%)", na.value = "lightgray", option="G", direction=-1) +
    #  ggplot2::scale_fill_gradient2(low = "darkblue", mid = "white", high = "lightblue", midpoint = 0) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 12),
      axis.text.y = ggplot2::element_text(size = 12),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid  = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white")
    ) +
    ggplot2::labs(x = "Decade", y = "Guild", title = this.basin, fill = "Mean relative\nchange (%)")

ggplot2::ggsave(paste0(this.basin,"_heatmap_biomass_",target.sc,".jpg"),  heatmap.plot, path = here::here(output.dir), dpi=300, width=8, height=9)   
  

max_val <- max(abs(plot.data$rel_biomass))

heatmap.plot.all <- ggplot2::ggplot(
  plot.data,
  ggplot2::aes(x = scenario, y = guild,
               fill = rel_biomass)
) +
  ggplot2::geom_tile() +
  ggplot2::scale_fill_distiller(palette = "RdBu", limits = c(-max_val, max_val)) + 
  #ggplot2::scale_fill_viridis_c(name = "Mean relative\nchange (%)", na.value = "lightgray", option="G", direction=-1) +
  #  ggplot2::scale_fill_gradient2(low = "darkblue", mid = "white", high = "lightblue", midpoint = 0) +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    axis.text.x = ggplot2::element_text(angle = 30, hjust = 1, size = 12),
    axis.text.y = ggplot2::element_text(size = 12),
    axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
    axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
    panel.grid  = ggplot2::element_blank(),
    panel.background = ggplot2::element_rect(fill = "white")
  ) +
  ggplot2::labs(x = "Decade", y = "Guild", title = this.basin, fill = "Mean relative\nchange (%)")

ggplot2::ggsave(paste0(this.basin,"_heatmap_biomass_all_",target.sc,".jpg"), heatmap.plot.all, path = here::here(output.dir), dpi=300, width=8, height=9)   

 
}
