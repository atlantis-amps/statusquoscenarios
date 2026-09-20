#' Calculate Biomass by Basin and Functional Group for a Decade
#'
#' Extracts biomass data from Atlantis polygon output for a specified time period,
#' aggregates it by basin and functional group, and calculates decadal biomass.
#'
#' @param data.path Character. Path to the directory containing the data file.
#' @param data.name Character. Name of the biomass data file to read.
#' @param func.groups Data frame. Functional groups reference table with at least
#'   columns \code{func_group} and \code{longname}.
#' @param polygon.data Data frame. Atlantis polygon biomass data (typically loaded
#'   via \code{\link{data.table::fread}}).
#' @param this.decade Character or numeric. Label for the current decade being processed.
#'
#' @details
#' The function assumes specific timestep patterns in the output data:
#' 73 timesteps per year, extracting the last 5 years of the run.
#' It calculates mean biomass per box and functional group, then joins with
#' basin information via \code{atlantis.table} (must be in the calling environment).
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{basin}: Basin identifier
#'     \item \code{longname}: Functional group long name
#'     \item \code{tot_bio}: Total biomass by basin and group
#'     \item \code{decade}: The decade label passed in
#'   }
#'
#' @importFrom data.table fread
#' @importFrom dplyr filter pivot_longer group_by summarise left_join mutate
#' @importFrom tidyr pivot_longer
#'
#' @keywords internal
#'
get_decade <- function(data.path, data.name, func.groups, polygon.data, this.decade){
  
  polygon.data <- data.table::fread(paste0(data.path,"/",data.name))
  
  #timesteps in output * outout in a year * years
  #2015-2019
  end.run <- seq(from=(73*5*45), to=(73*5*50), by=73)
  
  
  biomass.data <- polygon.data %>% 
    dplyr::filter(Time %in% end.run) %>% 
    tidyr::pivot_longer(cols=BB:DC, names_to="func_group", values_to = "biomass") %>% 
    dplyr::group_by(Box, func_group) %>% 
    dplyr::summarise(mean_bio = mean(biomass)) %>% 
    dplyr::left_join(func.groups, by="func_group") 
  
  basin.bio <- biomass.data %>% 
    dplyr::left_join(atlantis.table, by="Box", relationship = "many-to-many") %>% 
    dplyr::group_by(basin, longname) %>% 
    dplyr::summarise(tot_bio = sum(mean_bio)) %>% 
    dplyr::mutate(decade=this.decade)
  
}
