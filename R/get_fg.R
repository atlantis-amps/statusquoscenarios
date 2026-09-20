#' Classify Functional Groups by Biomass Type
#'
#' Reads functional groups from a CSV file and categorizes them into biomass types
#' (vertebrate, plankton, 2D benthic, or other).
#'
#' @param func.groups Character. Path to the functional groups CSV file
#'   (relative to the data directory).
#'
#' @details
#' This function categorizes Atlantis functional groups based on their \code{GroupType}:
#' \itemize{
#'   \item \strong{Vertebrate}: FISH, SHARK, BIRD, MAMMAL
#'   \item \strong{Plankton}: PWN, CEP, LG_ZOO, MED_ZOO, SM_ZOO, LG_PHY, SM_PHY
#'   \item \strong{2D (Benthic)}: MOB_EP_OTHER, SED_EP_FF, SED_EP_OTHER, SEAGRASS, PHYTOBEN
#'   \item \strong{Other}: LG_INF, MICROPHTYBENTHOS, SED_BACT, PL_BACT, SM_INF, CARRION, LAB_DET, REF_DET
#' }
#'
#' @return A data frame combining all functional groups with an added \code{BiomassType}
#'   column indicating their classification.
#'
#' @importFrom readr read_csv
#' @importFrom here here
#' @importFrom dplyr filter mutate bind_rows
#'
#' @keywords internal
#'
get_fg <- function(func.groups){
  
  grps <- readr::read_csv(here::here("data",func.groups))
  
  # set up a functional group types table
  vertebrate_groups <- grps %>% dplyr::filter(GroupType%in%c("FISH","SHARK","BIRD","MAMMAL")) %>% dplyr::mutate(BiomassType="vertebrate")
  plankton_groups <- grps %>% dplyr::filter(GroupType %in% c("PWN",'CEP','LG_ZOO','MED_ZOO','SM_ZOO','LG_PHY','SM_PHY')) %>% 
    dplyr::mutate(BiomassType="plankton")
  bottom_groups <- grps %>% dplyr::filter(GroupType %in% c("MOB_EP_OTHER",'SED_EP_FF','SED_EP_OTHER','SEAGRASS','PHYTOBEN')) %>% 
    dplyr::mutate(BiomassType="2D")
  other_groups <- grps %>% dplyr::filter(GroupType %in% c("LG_INF","MICROPHTYBENTHOS","SED_BACT","PL_BACT","SM_INF","CARRION","LAB_DET","REF_DET"))%>% 
    dplyr::mutate(BiomassType="other")
  biomass_groups <- dplyr::bind_rows(vertebrate_groups,plankton_groups,bottom_groups,other_groups)
  
  return(biomass_groups)
  
}