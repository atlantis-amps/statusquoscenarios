#' Extract Weight-at-Age and Numbers Data from Atlantis Output
#'
#' Reads Atlantis NetCDF output and extracts weight-at-age and abundance numbers
#' for fisheries and predatory groups.
#'
#' @param this.path Character. Path to the scenario run folder.
#' @param max.time Numeric. Maximum time (in years) to extract.
#' @param min.time Numeric. Minimum time (in years) to extract.
#' @param outputfrequency Numeric. Output frequency (timestep) in days.
#' @param functionalgroup.file Character. Path to the functional groups CSV file.
#' @param this.output.nc Character. Filename of the NetCDF output file.
#' @param polygon.table Data frame. Spatial polygon/box definitions.
#'
#' @details
#' This function extracts weight-at-age and abundance data for fish, sharks,
#' birds, and mammals from Atlantis NetCDF output. The scenario identifier is
#' derived from the run folder path. Results are written to a CSV file in the
#' output directory.
#'
#' @return Invisibly returns NULL. Writes data to CSV file:
#'   \code{[scenario]_Nums_ResN_W.csv}
#'
#' @importFrom readr read_csv write_csv
#' @importFrom here here
#' @importFrom dplyr select filter mutate bind_rows
#' @importFrom RNetCDF open.nc read.nc
#'
#' @keywords internal
#'
get_wage_nums <- function(this.path, max.time, min.time, outputfrequency, functionalgroup.file, this.output.nc, polygon.table){
  
 
  # make detailed Biomass, biomass per plot, Rn Sn, and Nums plots
  
  this.sc <- this.path %>% 
    strsplit("/") %>% 
    unlist %>% 
    .[4] %>% 
    strsplit("_") %>% 
    unlist %>% 
    .[4]
  
  fg.list <- readr::read_csv(here::here(data.path,functionalgroup.file)) %>% 
    dplyr::select(Code, IsTurnedOn, GroupType, NumCohorts, Name, longname) %>% 
    dplyr::filter(!Code %in% c("DIN","DL"))
  
  func.groups <- fg.list %>% 
    dplyr::filter(IsTurnedOn==1) %>% 
    dplyr::filter(GroupType == "FISH" | GroupType == "SHARK" | GroupType == "BIRD"| GroupType == "MAMMAL") %>% 
    dplyr::select(Name) %>% .$Name
  
  
  #get abundance and numbers at age
  nc <- RNetCDF::open.nc(paste0(this.path,"/",this.output.nc))
  nc.data <- RNetCDF::read.nc(nc)
  
  maxtimestep <- (max.time*365)/outputfrequency
    
  group.atlantis.data <- lapply(func.groups, get_nc_basin, thisncfile = nc, fg.list, polygon.table, maxtimestep) %>% 
    dplyr::bind_rows() %>% 
    dplyr::mutate(scenario=this.sc)
  
  readr::write_csv(group.atlantis.data, here::here("output",paste0(this.sc,"_Nums_ResN_W.csv")))
  
}