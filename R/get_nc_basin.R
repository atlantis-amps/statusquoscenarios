#' Extract Age-Specific Weight and Abundance by Basin from Atlantis NetCDF
#'
#' Reads weight-at-age and abundance numbers from Atlantis NetCDF output files
#' for a single functional group and aggregates by spatial boxes (basins).
#'
#' @param eachgroup Character. Name of the functional group to extract.
#' @param thisncfile NetCDF file object opened with RNetCDF::open.nc().
#' @param fg.list Data frame. Functional groups with columns:
#'   \itemize{
#'     \item \code{Name}: Group identifier
#'     \item \code{NumCohorts}: Number of age cohorts
#'   }
#' @param polygon.table Data frame. Spatial box definitions with columns:
#'   \itemize{
#'     \item \code{box}: Box identifier
#'     \item \code{proportion}: Proportion of group in each box
#'   }
#' @param maxtimestep Numeric. Maximum timestep to extract from NetCDF.
#'
#' @details
#' This function extracts age-specific abundance (_Nums) and weight (_ResN converted to Wage)
#' for the specified group across all age cohorts and spatial boxes. Data are
#' averaged over depth layers and weighted by spatial proportion. Output includes
#' relative values accounting for the proportion of biomass in each box.
#'
#' Wage is calculated as: ResN * 20 * 5.7 * (3.65/2.65) / 1,000,000
#'
#' @return A data frame with columns:
#'   \itemize{
#'     \item \code{box}: Spatial box identifier
#'     \item \code{time}: Timestep identifier
#'     \item \code{value}: Numeric measurement value
#'     \item \code{age}: Age cohort
#'     \item \code{group}: Variable name with age suffix
#'     \item \code{variable_type}: Either "Nums" or "Wage"
#'     \item \code{name}: Functional group name
#'     \item \code{longname}: Full group name
#'     \item \code{rel_value}: Value weighted by spatial proportion
#'   }
#'
#' @importFrom RNetCDF var.get.nc
#' @importFrom dplyr filter mutate left_join bind_rows
#' @importFrom tidyr as_tibble pivot_longer
#'
#' @keywords internal
#'
get_nc_basin <- function(eachgroup,thisncfile, fg.list, polygon.table, maxtimestep){
  
  print(paste("Analyzing this group",eachgroup))
  
  this.sprow <- fg.list %>% 
    dplyr::filter(Name==eachgroup) 
  
  print(this.sprow)
  
  group.ages <- paste(eachgroup,1:this.sprow$NumCohorts,sep="")
  
  print(group.ages)
  
  #make names for nc variables
  
  varlist <- c("_Nums","_Wage")
  
  var.listdata <- list()
  
  for(eachvar in 1:length(varlist)){
    
    eachvarlist <- varlist[eachvar]
    
    print(eachvarlist)
    
    name.var <- paste(group.ages,eachvarlist,sep="")
    
    variable.type <- gsub("_","",eachvarlist)
    
    if(eachvarlist == "_Wage") {
      
      for(eachage in 1:length(name.var)) {
        
      
          eachvarlist = "_ResN"
          name.var <- paste(group.ages,eachvarlist,sep="")
          variable.type <- gsub("_","",eachvarlist)
          eachvarlist = "_Wage"
        
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- RNetCDF::var.get.nc(thisncfile, eachvariable)
          
          thisData<-thisData*20*5.7*(3.65/2.65)/1000000
          
          variable.type = "Wage"
    
        thisData[thisData==0]<-NA  # Replace 0's with NA
        thisData <- thisData[1:7,1:88,1:maxtimestep]
        thisDataMeanMg <-apply(thisData,c(2,3),mean,na.rm = TRUE) #Get mean size over time, averaging over depth
        #apply(thisData[1:7, 2:89, 1:thistimestep], c(2, 3), mean)
        
        fg.groups <- fg.list %>% 
          dplyr::mutate(name=Name)
           
          thisY <- thisDataMeanMg %>% 
            tidyr::as_tibble() %>% 
            dplyr::mutate(box=1:dplyr::n()) %>%
            tidyr::pivot_longer(cols= -box, names_to = "time") %>% 
              dplyr::mutate(time=gsub("V","",time), age=eachage, group=eachvariable,
                            variable_type=variable.type, name=eachgroup) %>% 
            dplyr::left_join(fg.groups) %>% 
            dplyr::left_join(polygon.table) %>% 
            dplyr::mutate(rel_value=value*proportion)
            
        
        listname <- paste0(thisY$name[1],"_",thisY$variable_type[1],"_",eachage)
        var.listdata[[listname]] <- thisY  }
        
      } else if (eachvarlist == "_Nums") {
          
      for(eachage in 1:length(name.var)) {
        
        eachvariable <- name.var[eachage]
        print(eachvariable)
        
        thisData <- RNetCDF::var.get.nc(thisncfile, eachvariable)
        thisData[thisData==0]<-NA  # Replace 0's with NA
        print(dim(thisData))
        thisData <- thisData[1:7,2:89,1:maxtimestep]
        thisDataMeanMg <-apply(thisData,c(2,3),mean,na.rm = TRUE) #Get mean size over time, averaging over depth
        
        fg.groups <- fg.list %>% 
          dplyr::mutate(name=Name)
        
       
        thisY <- thisDataMeanMg %>% 
          tidyr::as_tibble() %>% 
          dplyr::mutate(box=1:dplyr::n()) %>%
          tidyr::pivot_longer(cols= -box, names_to = "time") %>% 
          dplyr::mutate(time=gsub("V","",time), age=eachage, group=eachvariable,
                        variable_type=variable.type, name=eachgroup) %>% 
          dplyr::left_join(fg.groups) %>% 
          dplyr::left_join(polygon.table) %>% 
          dplyr::mutate(rel_value=value*proportion)
        
        
        var.listdata[[eachvariable]] <- thisY  
        
      }
      
      }
    
    
  }
  
  
  thissp.data <- var.listdata %>% 
    dplyr::bind_rows() 
  
  
  print(paste("Done with group",eachgroup))
  
  
  return(thissp.data)
}
