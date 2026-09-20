#' Extract Vital Signs Indicators Data for Radar Plots
#'
#' Retrieves and processes vital signs indicators (biomass and abundance) from
#' Atlantis model output files and Google Sheets specifications.
#'
#' @param this.scenario Character. The name of the scenario to process.
#' @param file.name Character. The filename suffix for abundance/nums output files.
#' @param startyear Numeric. Starting year for the simulation.
#' @param runtime Numeric. Total runtime of the simulation in years.
#' @param outputfrequency Numeric. Output frequency (timestep) in days.
#' @param functionalgroup.file Character. Path to the functional groups CSV file.
#' @param this.output.nc Character. (Currently unused) Path to NetCDF output file.
#' @param atlantis.table (Currently unused) Atlantis model table.
#' @param folder.base Character. Base folder path containing scenario output folders.
#' @param biomass.paths Character. Path to the biomass data file.
#'
#' @details
#' This function fetches vital signs indicator definitions from a Google Sheet
#' (https://vitalsigns.pugetsoundinfo.wa.gov/) and combines them with abundance
#' and biomass data from Atlantis model output files. Results are aggregated by
#' indicator and basin.
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{indicator_short}: Short name of the indicator
#'     \item \code{basin}: Geographic basin
#'     \item \code{value}: Aggregated indicator value
#'     \item \code{scenario}: Scenario name
#'     \item \code{parameter}: Either "abundance" or "biomass"
#'   }
#'
#' @author Hem Nalini Morzaria-Luna
#'
#' @export
#'
#' @importFrom googlesheets4 gs4_deauth read_sheet
#' @importFrom readr read_csv
#' @importFrom data.table fread
#' @importFrom dplyr filter mutate summarise rename left_join if_else bind_rows
#' @importFrom tidyr separate_rows
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#'   vital_signs <- get_radar_data(
#'     this.scenario = "scenario_2020",
#'     file.name = "_Nums.csv",
#'     startyear = 2020,
#'     runtime = 20,
#'     outputfrequency = 5,
#'     functionalgroup.file = "data/functionalgroups.csv",
#'     folder.base = "output/",
#'     biomass.paths = "output/biomass_data.csv"
#'   )
#' }
get_radar_data <- function(this.scenario, file.name, startyear, runtime, outputfrequency, functionalgroup.file, this.output.nc, atlantis.table, folder.base, biomass.paths){
  
  
  # Authenticate with Google
  
  googlesheets4::gs4_deauth()
  
  vital.signs.sheet <- googlesheets4::read_sheet(ss="1PxZQwlBadcxI0G-Z7i8DMfrgsqAoWperfA76rXq4yIs", sheet = "vital_signs")
  
  this.folderpath <- paste0(folder.base, this.scenario)
  
  func.groups <- readr::read_csv(functionalgroup.file)
 
  parameter.names <- c("abundance","biomass")
  
  vital.signs.data <- list()
  
  for(thisparameter in 1:length(parameter.names)){
    
    eachparameter <- parameter.names[thisparameter]
    print(eachparameter)
    
    vital.signs.sel <- vital.signs.sheet %>% 
      dplyr::filter(output_type==eachparameter) %>%
      dplyr::mutate(ind_no=1:nrow(.)) %>% 
      tidyr::separate_rows(func_group, sep = "\n")
    
    
    if(eachparameter=="abundance"){
      
      
      this.data <- data.table::fread(here::here("output",paste0(this.scenario,file.name)))
      
      
         this.file <- this.data %>% dplyr::filter(variable_type=="Nums")
      
      
      mintimestep <- (min.time*365)/outputfrequency
        
      this.data <- this.file %>%
        dplyr::filter(time>=mintimestep) %>%
        dplyr::mutate(rel_value = dplyr::if_else(is.na(rel_value), 0, rel_value)) %>% 
        dplyr::summarise(abundance=sum(rel_value), .by=c("Code", "longname", "basin")) %>% 
        dplyr::rename(code=Code)
      
      vital.signs.param <- vital.signs.sel %>%
        dplyr::rename(code=func_group) %>%
        dplyr::left_join(this.data, by="code") %>%
        dplyr::summarise(value=sum(abundance), .by=c("indicator_short","basin")) %>%
        dplyr::mutate(scenario=this.scenario, parameter = eachparameter)
      
    }
    
    if(eachparameter=="biomass"){
      
      this.file <- data.table::fread(biomass.path)
      
      
      this.data <- this.file %>%
        dplyr::filter(scenario == this.scenario) %>% 
        dplyr::filter(time>=min.time) %>%
        dplyr::filter(time<=max.time) %>%
        dplyr::left_join(polygon.table) %>% 
        dplyr::rename(longname=species) %>% 
        dplyr::left_join(func.groups) %>% 
        dplyr::mutate(rel_value = biomass * proportion) %>% 
        dplyr::summarise(biomass=sum(rel_value), .by=c("Code","longname","basin")) %>% 
        dplyr::rename(code=Code)  
      
      vital.signs.param <- vital.signs.sel %>%
        dplyr::rename(code=func_group) %>%
        dplyr::left_join(this.data, by="code") %>%
        dplyr::summarise(value=sum(biomass), .by=c("indicator_short","basin")) %>%
        dplyr::mutate(scenario=this.scenario, parameter = eachparameter)
      
      
    }
    
    vital.signs.data[[thisparameter]] <- vital.signs.param
    
  
  }
  
  vital.signs <- vital.signs.data %>% dplyr::bind_rows()
  
  return(vital.signs)
}