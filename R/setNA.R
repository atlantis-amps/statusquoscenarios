#' Set Matrix Values to NA Based on Polygon Range
#'
#' Sets specified polygons (rows/columns) to NA in a matrix, useful for masking
#' land areas or excluded regions in spatial Atlantis output.
#'
#' @param mat Numeric matrix or 3D array. The data to mask.
#' @param ini.pol Numeric. Starting polygon (column/row) index.
#' @param end.pol Numeric. Ending polygon (column/row) index.
#'
#' @details
#' This function handles both 2D matrices and 3D arrays:
#' \itemize{
#'   \item For 3D arrays: sets \code{mat[, c(1, ini.pol:end.pol), ]} to NA
#'   \item For 2D matrices: sets \code{mat[c(1, ini.pol:end.pol), ]} to NA
#' }
#' This is typically used to mask coastal polygons or other excluded areas from
#' Atlantis output before analysis or visualization.
#'
#' @return A matrix or array of the same dimensions as \code{mat} with specified
#'   indices set to NA.
#'
#' @examples
#' \dontrun{
#'   # Mask polygons 5 through 10 in a 3D array
#'   masked_array <- setNA(data_array, ini.pol = 5, end.pol = 10)
#' }
#'
#' @export
#'
setNA <- function(mat, ini.pol, end.pol) {
  mat2 <- mat
  if(length(dim(mat2))==3) mat2[,c(1,ini.pol:end.pol),]<-NA
  if(length(dim(mat2))==2) mat2[c(1,ini.pol:end.pol),] <- NA
  mat2
}
