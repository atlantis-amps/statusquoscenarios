#' Extract Spatial Biomass from Atlantis NetCDF Output
#'
#' Reads Atlantis NetCDF output files and calculates spatial biomass for all
#' functional groups and boxes.
#'
#' @param thisrun Character. Path to the scenario run folder.
#' @param boxes Data frame. Box definitions (currently unused).
#' @param prm_biol Character. Path to the biological parameter file.
#' @param func.groups Data frame. Functional groups table with at least columns:
#'   \itemize{
#'     \item \code{Name}: Group identifier
#'     \item \code{NumCohorts}: Number of age cohorts
#'   }
#' @param fgs Data frame. Functional groups specification from Atlantis.
#' @param init (Currently unused).
#' @param bps Data frame. Box properties specification.
#' @param nc_file Character. (Currently unused) NetCDF file path.
#' @param run_file Character. (Currently unused) Run parameters file path.
#'
#' @details
#' This function extracts numbers (Nums), structural nitrogen (StructN), and
#' reserve nitrogen (ResN) from the NetCDF output, calculates spatial biomass
#' using atlantistools, and adds scenario information derived from the run path.
#'
#' @return A data frame with spatial biomass data including:
#'   \itemize{
#'     \item \code{box}: Spatial box identifier
#'     \item \code{scenario}: Scenario year extracted from the run path
#'     \item Additional columns from atlantistools biomass calculations
#'   }
#'
#' @importFrom atlantistools get_conv_mgnbiot load_nc load_nc_physics calculate_biomass_spatial
#' @importFrom dplyr filter pull rename mutate
#'
#' @keywords internal
#'
get_nc_biomass <- function(thisrun, boxes, prm_biol, func.groups, fgs, init, bps, nc_file, run_file){
  
  print(thisrun)
  
    this_year <- thisrun %>% 
    strsplit("/") %>% 
    unlist() %>% 
    .[4] %>% 
    strsplit("_") %>% 
    unlist() %>% 
    .[4]
  
  bio_conv <- atlantistools::get_conv_mgnbiot(prm_biol = prm_biol)
  
  nc_gen <- file.path(thisrun,"outputFolder","AMPS_OUT.nc")
  prm_run <- file.path(thisrun, "PugetSound_run.prm")
  
  groups_age <- func.groups %>% 
    dplyr::filter(NumCohorts>1) %>% 
    dplyr::pull(Name)
  
  groups_rest <- func.groups %>% 
    dplyr::filter(NumCohorts==1) %>% 
    dplyr::pull(Name)
  
  
  nums <- atlantistools::load_nc(nc = nc_gen, bps = bps, fgs = fgs,
                                 select_groups = groups_age, select_variable = "Nums",
                                 prm_run = prm_run, bboxes = bboxes)
  sn <- atlantistools::load_nc(nc = nc_gen, bps = bps, fgs = fgs,
                               select_groups = groups_age, select_variable = "StructN",
                               prm_run = prm_run, bboxes = bboxes)
  rn <- atlantistools::load_nc(nc = nc_gen, bps = bps, fgs = fgs,
                               select_groups = groups_age, select_variable = "ResN",
                               prm_run = prm_run, bboxes = bboxes)
  n <- atlantistools::load_nc(nc = nc_gen, bps = bps, fgs = fgs,
                              select_groups = groups_rest, select_variable = "N",
                              prm_run = prm_run, bboxes = bboxes)
  vol <- atlantistools::load_nc_physics(nc = nc_gen, select_physics = c("volume", "dz"),
                                        prm_run = prm_run, bboxes = bboxes, aggregate_layers = FALSE)
  
  spatial.biomass <- atlantistools::calculate_biomass_spatial(nums = nums, sn = sn, rn = rn, n = n, vol_dz = vol,
                                                              bio_conv = bio_conv, bps = bps) %>% 
    dplyr::rename(box=polygon) %>% 
    dplyr::mutate(scenario=this_year)
  
  return(spatial.biomass)
}
  