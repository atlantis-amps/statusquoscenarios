#' Create Biomass Change Visualizations by Basin and Guild
#'
#' Processes Atlantis biomass output to calculate relative biomass changes
#' between a baseline and target scenario, aggregated by basin and guild.
#'
#' @param biomass.res Data frame. Biomass data with columns:
#'   \itemize{
#'     \item \code{biomass}: Numeric biomass values
#'     \item \code{species}: Species/group name
#'     \item \code{scenario}: Scenario identifier
#'     \item \code{time}: Time step
#'     \item \code{box}: Spatial box identifier
#'   }
#' @param polygon.table Data frame. Spatial definitions with columns:
#'   \itemize{
#'     \item \code{box}: Box identifier
#'     \item \code{proportion}: Proportion in that box
#'     \item \code{basin}: Basin name
#'   }
#' @param ndyn.pols Numeric vector. Box identifiers to exclude (non-dynamic polygons).
#' @param max.time Numeric. Maximum time step to include.
#' @param min.time Numeric. Minimum time step to include.
#' @param fgs Data frame. Functional groups metadata with columns:
#'   \itemize{
#'     \item \code{LongName}: Species name
#'     \item \code{code}: Group code
#'     \item \code{Large_group_alt3}: Guild classification
#'   }
#' @param target.sc Character. Target scenario name.
#' @param base.sc Character. Baseline scenario name for comparison.
#'
#' @details
#' This function:
#' 1. Filters biomass data to specified time range
#' 2. Joins with functional groups and spatial information
#' 3. Excludes non-dynamic polygons
#' 4. Calculates relative biomass change as (scenario / baseline - 1) × 100
#' 5. Aggregates by basin and guild
#'
#' Returns both basin-specific and Puget Sound-wide summaries.
#'
#' @return Data frame with relative biomass changes by scenario, basin, and guild.
#'
#' @importFrom dplyr mutate filter rename left_join if_else summarise
#'
#' @keywords internal
#'
biomass_plots <- function(biomass.res, polygon.table, ndyn.pols, max.time, min.time, fgs, target.sc, base.sc){
  
  #eliminated non dynamic polygons and subset the timesteps for 2045-2049
  
  biomass.res.time <- biomass.res %>% 
    dplyr::mutate(scenario = as.character(scenario)) %>% 
    dplyr::filter(time <= max.time) %>% 
    dplyr::filter(time >= min.time) 
  
  biomass.basin <- biomass.res.time %>% 
    dplyr::rename(LongName=species) %>% 
    dplyr::left_join(fgs) %>% 
    dplyr::rename(guild=Large_group_alt3) %>% 
    dplyr::left_join(polygon.table) %>% 
    dplyr::filter(!box %in% ndyn.pols) %>% 
    dplyr::mutate(prop_biomass = biomass * proportion) %>% 
    dplyr::summarise(tot_biomass=sum(biomass), .by = c("code","LongName","time","basin","scenario","guild")) 
    
 biomass.base <- biomass.basin %>% 
    dplyr::filter(scenario==base.sc) %>% 
    dplyr::rename(base_biomass = tot_biomass) %>% 
    dplyr::select(-scenario)
  
  biomass.sc <- biomass.basin %>% 
    dplyr::filter(!scenario==base.sc)
  
  #summary and plot by guild
  
  combined.data.guild <- dplyr::left_join(biomass.sc, biomass.base) %>%  
    dplyr::mutate(rel_change = tot_biomass/base_biomass) %>% 
    dplyr::mutate(rel_change= dplyr::if_else(is.nan(rel_change), 0, rel_change)) %>%   
    dplyr::summarise(sc_change=mean(rel_change), .by = c("scenario","basin", "guild")) %>% 
    dplyr::mutate(rel_biomass=(sc_change*100)) %>% 
    dplyr::mutate(rel_biomass=(rel_biomass-100)) 
    
  combined.all.guild <- dplyr::left_join(biomass.sc, biomass.base) %>%  
    dplyr::mutate(rel_change = tot_biomass/base_biomass) %>% 
    dplyr::mutate(rel_change= dplyr::if_else(is.nan(rel_change), 0, rel_change)) %>% 
    dplyr::mutate(basin = "Puget Sound") %>% 
    dplyr::summarise(sc_change=mean(rel_change), .by = c("scenario","basin", "guild")) %>% 
    dplyr::mutate(rel_biomass=(sc_change*100)) %>% 
    dplyr::mutate(rel_biomass=(rel_biomass-100)) 
    
  #check for BionIndx.txt
  
  #test.biomass <- dplyr::left_join(biomass.sc, biomass.base) %>% 
  #  dplyr::summarise(sum_biomass=sum(base_biomass), .by = c(scenario, t, Code))
  
  basin.data <- combined.data.guild %>% 
    dplyr::bind_rows(combined.all.guild) %>% 
   dplyr::mutate(guild=stringr::str_to_sentence(guild))
  
  readr::write_csv(basin.data, here::here("output","basin_guild_biomass.csv"))
  
  desired_order <- c("Puget Sound", "West Strait of Juan de Fuca", "East Strait of Juan de Fuca", "Admiralty Inlet","Strait of Georgia" , "Whidbey Basin", "San Juan Archipelago" , "Central Puget Sound", "Hood Canal", "South Puget Sound")
  basin.data$basin <- factor(basin.data$basin, levels = desired_order)
  
  desired_order <- c("Bacteria","Primary producers","Zooplankton","Jellies","Shrimp & squid","Benthos","Forage fish","Salmon","Piscivorous fish","Benthivorous fish", "Shark","Bird","Mammal", "Detritus")
  
  basin.data$guild <- factor(basin.data$guild, levels = desired_order)
  
  pal <- psimfcolors::psimf_palette("basin")
  
  #pal <- brewer.pal(length(unique(basin.data$basin)), "Set1")
  
  basin.plot <- basin.data %>%
    dplyr::filter(scenario == target.sc) %>% 
    dplyr::filter(!guild %in% c("Piscivorous fish", "Shrimp & squid")) %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x=basin, y=rel_biomass, fill=basin), stat="identity") +
    ggplot2::facet_wrap(. ~ guild) +
    ggplot2::labs(title=paste("Change in",target.sc, "relative to",base.sc), x= "", y="Mean % relative biomass change") +
    ggplot2::scale_fill_manual(values = pal)+
    ggplot2::guides(fill="none") +
    ggplot2::theme_minimal() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::theme(
      plot.title = element_text(size = 15, face = "bold"),
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid  = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white")
    )


  basin.high <- basin.data %>%
    dplyr::filter(scenario == target.sc) %>% 
    dplyr::filter(guild %in% c("Piscivorous fish", "Shrimp & squid")) %>% 
    droplevels()
  
  basin.high.plot <- basin.high %>% 
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x=basin, y=rel_biomass, fill=basin), stat="identity") +
    ggplot2::facet_wrap(. ~ guild, scales = "free_y") +
    ggplot2::labs(x= "Basin", y="Mean % relative biomass change") +
    ggplot2::scale_fill_manual(values = pal)+
    ggplot2::guides(fill="none") +
    ggplot2::theme_minimal() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      strip.text = ggplot2::element_text(size = 12, face = "bold"),
      panel.grid  = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white")
    )
  
  org.basin.plot <- basin.plot / basin.high.plot + patchwork::plot_layout(heights = c(4, 1)) +  patchwork::plot_annotation(tag_levels = 'A')
  
  ggplot2::ggsave(paste0("biomass_guild_change_",target.sc,".jpg"), org.basin.plot, path = here::here("output"), dpi=300, width=14, height=12)  
  
  
  #summary and plot by longname
  
  combined.data <- dplyr::left_join(biomass.sc, biomass.base) %>%  
    dplyr::mutate(rel_change = tot_biomass/base_biomass) %>% 
    dplyr::mutate(rel_change= dplyr::if_else(is.nan(rel_change), 0, rel_change)) %>%   
    dplyr::summarise(sc_change=mean(rel_change), .by = c("scenario","basin", "LongName","guild")) %>% 
    dplyr::mutate(rel_biomass=(sc_change*100)) %>% 
    dplyr::mutate(rel_biomass=(rel_biomass-100)) 
  
  combined.all <- dplyr::left_join(biomass.sc, biomass.base) %>%  
    dplyr::mutate(rel_change = tot_biomass/base_biomass) %>% 
    dplyr::mutate(rel_change= dplyr::if_else(is.nan(rel_change), 0, rel_change)) %>% 
    dplyr::mutate(basin = "Puget Sound") %>% 
    dplyr::summarise(sc_change=mean(rel_change), .by = c("scenario","basin", "LongName","guild")) %>% 
    dplyr::mutate(rel_biomass=(sc_change*100)) %>% 
    dplyr::mutate(rel_biomass=(rel_biomass-100)) 
  
  #check for BionIndx.txt
  
  #test.biomass <- dplyr::left_join(biomass.sc, biomass.base) %>% 
  #  dplyr::summarise(sum_biomass=sum(base_biomass), .by = c(scenario, t, Code))
  
  sp.data <- combined.data %>% 
    dplyr::bind_rows(combined.all)
  
  
  readr::write_csv(sp.data, here::here("output","basin_species_biomass.csv"))
  
  
  if(target.sc == 2040) {

    plot.data <- sp.data %>% 
      dplyr::filter(rel_biomass > 10 | rel_biomass < -10) %>% 
      dplyr::filter(!guild %in% c("Detritus","Bacteria")) %>% 
      dplyr::filter(scenario == target.sc) %>% 
      dplyr::mutate(LongName = dplyr::if_else(LongName=="Chum Hood Canal summer run Subyearling","Chum Hood Canal summer Subyrl.", LongName))
    
    
    
    desired_order <- c("Puget Sound", "West Strait of Juan de Fuca", "East Strait of Juan de Fuca", "Admiralty Inlet","Strait of Georgia" , "Whidbey Basin", "San Juan Archipelago" , "Central Puget Sound", "Hood Canal", "South Puget Sound")
    plot.data$basin <- factor(plot.data$basin, levels = desired_order)
    
    desired_order <- c("Small phytoplankton","Large phytoplankton","Macroalgae","Seagrass","Mesozooplankton","Gelatinous zooplankton","Benthic filter feeder","Shrimp","Squid","Pacific Herring Puget Sound","Pacific Herring Cherry Point","Small Planktivorous Fish", "Perch","Chum Hood Canal summer Subyrl.",
                       "Other salmonids","Chinook resident Hood canal", "Strait of Georgia salmonids","Pink Salmon Subyearling","Small demersal fish","Small-mouthed Flatfish","Piscivorous flatfish","Hake_Large gadoids","Large demersal predators","Harbor porpoise", "California sea lions")
    
    plot.data$LongName <- factor(plot.data$LongName, levels = desired_order)
    
    desired_order <- c("Primary producers","Zooplankton","Jellies","Shrimp & squid","Benthos","Forage fish","Salmon","Piscivorous Fish","Benthivorous Fish", "Shark","Bird","Mammal")
    
    plot.data$guild <- factor(plot.data$guild, levels = desired_order)
    
    rel.change <- "10"
  }
  
  if(target.sc == 2090) {
    
    plot.data <- sp.data %>% 
      dplyr::filter(rel_biomass > 25 | rel_biomass < -25) %>% 
      dplyr::filter(!guild %in% c("Detritus","Bacteria")) %>% 
      dplyr::filter(scenario == target.sc) %>% 
      dplyr::mutate(LongName = dplyr::if_else(LongName=="Chum Hood Canal summer run Subyearling","Chum Hood Canal summer Subyrl.", LongName))
    
    
    desired_order <- c("Puget Sound", "West Strait of Juan de Fuca", "East Strait of Juan de Fuca", "Admiralty Inlet","Strait of Georgia" , "Whidbey Basin", "San Juan Archipelago" , "Central Puget Sound", "Hood Canal", "South Puget Sound")
    plot.data$basin <- factor(plot.data$basin, levels = desired_order)
    
    desired_order <- c("Macroalgae","Seagrass","Small phytoplankton","Mesozooplankton",               
                       "Gelatinous zooplankton","Squid",                         
                       "Shrimp","Geoducks","Pacific Herring Puget Sound","Small Planktivorous Fish","Small demersal fish",
                       "Chinook resident Hood canal",
                       "Chum Hood Canal summer Subyrl.", "Pink Salmon Subyearling","Large demersal predators",      
                       "Demersal rockfish","Midwater rockfish",            
                       "Demersal vulnerable rockfish","Midwater vulnerable rockfish",  
                       "Piscivorous flatfish", "California sea lions","Steller sea lions",             
                       "Harbor porpoise")
    
    plot.data$LongName <- factor(plot.data$LongName, levels = desired_order)
    
    desired_order <- c("Primary producers","Zooplankton","Jellies","Shrimp & squid","Benthos","Forage fish","Salmon","Piscivorous Fish","Benthivorous Fish", "Mammal")
    
    plot.data$guild <- factor(plot.data$guild, levels = desired_order)
    
    rel.change <- "25"
    
  }
  
  
  
  pal <- c("darkblue", wesanderson::wes_palette(length(unique(plot.data$guild))-1, name = "FrenchDispatch", type = "continuous"))
  
  #pal <- brewer.pal(length(unique(basin.data$basin)), "Set1")
  
  sp.plot <- plot.data %>%
    ggplot2::ggplot() +
    ggplot2::geom_bar(ggplot2::aes(x=basin, y=rel_biomass, fill=guild), stat="identity") +
    ggplot2::facet_wrap(. ~ LongName) +
     ggplot2::scale_fill_manual(values = pal)+
 #   ggplot2::guides(fill="none") +
    ggplot2::labs(title=paste("Functional groups with > ±",rel.change,"% mean relative change in",target.sc), x= "Basin", y="Mean relative change in biomass", fill = "Guild") +
    ggplot2::theme_minimal() +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.3) +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, vjust = 0.5, hjust = 1)) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_text(size = 10),
      axis.title.x = ggplot2::element_text(size = 12, face = "bold"),
      axis.title.y = ggplot2::element_text(size = 12, face = "bold"),
      strip.text = ggplot2::element_text(face = "bold"),
      panel.grid  = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "white")
    )  +
    ggplot2::theme(legend.position = "bottom")
  


  ggplot2::ggsave(paste0("biomass_species_change_",target.sc,".jpg"), sp.plot, path = here::here("output"), dpi=300, width=12, height=9)
  
  
}