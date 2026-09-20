#' Load and Process Atlantis NetCDF Output
#'
#' Reads an Atlantis NetCDF output file and extracts key dimensions,
#' volumes, areas, and functional group information for downstream analysis.
#'
#' @param data.path Character. Path to the directory containing the NetCDF file.
#' @param data.name Character. Filename of the NetCDF output file.
#' @param timeperiod Numeric. Output frequency (timestep) in days.
#'
#' @details
#' This function loads multiple components from an Atlantis NetCDF file:
#' \itemize{
#'   \item Coastline data (west coast North America)
#'   \item NetCDF tidync and ncdf4 objects
#'   \item Volume data for each layer and box
#'   \item Time dimension converted to years
#'   \item Box areas derived from deepest layer volumes
#'   \item Functional group grid dimensions
#' }
#'
#' @return A list containing:
#'   \itemize{
#'     \item \code{coaststates}: sf object with North American coastline
#'     \item \code{out}: tidync NetCDF object
#'     \item \code{this.nc}: ncdf4 NetCDF object
#'     \item \code{volumes}: Data frame of volume data by box and layer
#'     \item \code{tyrs}: Numeric vector of times converted to years
#'     \item \code{areas}: Data frame of box areas
#'     \item \code{fg_dimensions}: Data frame of functional group grid dimensions
#'   }
#'
#' @importFrom rnaturalearth ne_countries
#' @importFrom dplyr filter
#' @importFrom tictoc tic
#' @importFrom tidync tidync hyper_filter hyper_tibble hyper_grids
#' @importFrom ncdf4 nc_open ncvar_get
#' @importFrom purrr pluck map_df
#'
#' @keywords internal
#'
load_output <- function(data.path, data.name, timeperiod) {

  atlantis_outputs <- list()

  # load west cost land for mapping
  coaststates <- rnaturalearth::ne_countries(continent="North America",returnclass = 'sf') %>%
    dplyr::filter(name %in% c('Canada','United States','Mexico'))

  atlantis_outputs[['coaststates']] <- coaststates

  tictoc::tic("Loading files: ")
  # output file
  out_fl <- paste0(data.path, "/", data.name)
  out <- tidync::tidync(out_fl)
  this.nc <- ncdf4::nc_open(out_fl)

  atlantis_outputs[['out']] <- out
  atlantis_outputs[['this.nc']] <- this.nc

  # derived values for output
  # depths <- out %>% hyper_filter(t=t==0) %>% hyper_tibble(select_var="dz") %>% dplyr::select(-t)
  # glimpse(depths)

  # volumes of each layer
  volumes <- out %>%
    tidync::hyper_filter(t=t>timeperiod) %>% #tidync::hyper_filter(t=t==0)
    tidync::hyper_tibble(select_var="volume") %>%
    dplyr::select(-t)

  atlantis_outputs[['volumes']] <- volumes

  # time dimension
  ts <- ncdf4::ncvar_get(this.nc,varid = "t") %>%
    as.numeric
  tyrs <- ts/(60*60*24*timeperiod)

  atlantis_outputs[['tyrs']] <- tyrs

  # area of each box is the same as volume of the deepest depth layer, because the dz of that layer is 1
  areas <- volumes %>%
    dplyr::filter(z==max(z)) %>%
    dplyr::select(b,volume) %>% dplyr::rename(area=volume)

  atlantis_outputs[['areas']] <- areas

  # functional group dimensions
  fg_dimensions <- tidync::hyper_grids(out) %>%
    purrr::pluck("grid") %>%
    purrr::map_df(function(x){
      out %>% tidync::activate(x) %>% tidync::hyper_vars() %>%
        dplyr::mutate(grd=x)
    })

  atlantis_outputs[['fg_dimensions']] <- fg_dimensions

  return(atlantis_outputs)
}
