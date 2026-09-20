#' Plot Diet Composition across Time and Cohorts
#'
#' Creates stacked bar plots showing the diet composition of predator groups
#' across different time periods and age cohorts. Generates one plot per predator.
#'
#' @param dietsAll Data frame. Diet data with columns \code{Time}, \code{Predator},
#'   \code{Cohort}, and prey code columns.
#' @param FG_to_plot Character vector. Names of predator functional groups to plot.
#' @param threshold Numeric. Minimum summed diet proportion (across all cohorts
#'   and times) for a prey item to appear in the plot.
#' @param years Character or numeric. Years to filter for (typically from the diet
#'   data's Time column). If \code{"all"}, uses all unique times in the data.
#'
#' @details
#' This function requires \code{FG_tab} (functional groups reference table) and
#' \code{dietsAll} to be available in the calling environment. It converts diet
#' proportions to percentages and creates faceted bar plots by age cohort for
#' each predator group. Prey items are colored using the RColorBrewer "Set1" palette.
#'
#' @return Invisibly returns NULL. Prints plots to the active graphics device.
#'
#' @importFrom reshape2 melt
#' @importFrom RColorBrewer brewer.pal colorRampPalette
#' @importFrom dplyr filter
#' @importFrom ggplot2 ggplot aes geom_bar scale_fill_manual scale_colour_manual
#'   facet_wrap labs theme_bw
#'
#' @keywords internal
#'
library("reshape2")
library("RColorBrewer")

plot_Diets<-function(dietsAll, FG_to_plot, threshold, years){
  
  if(years=="all"){years<-2013+unique(dietsAll$Time)/365}
  
  for (fg in FG_to_plot){
  
  FG_code<-FG_tab$Code[FG_tab$Name==fg]
  
  dietsAll %>%
    filter(Predator==FG_code) -> subDiet
  
  selec_prey<-names(which(colSums(subDiet[6:ncol(subDiet)])>threshold)) 
  
  colourCount = length(selec_prey)
  getPalette = colorRampPalette(brewer.pal(9, "Set1"))
  
  subDiet %>%
    melt(id.vars = c("Time", "Predator", "Cohort"), measure.vars=selec_prey) %>%
    filter(Time%in%as.character(c((years-2013)*365))) %>%
    ggplot(aes(x=2013+(Time/365),y=value*100,fill=variable, color=variable))+
    geom_bar(stat="identity")+
    scale_fill_manual(values=getPalette(colourCount), name = "Prey", labels = FG_tab$LongName[FG_tab$Code%in%selec_prey])+
    scale_colour_manual(values=getPalette(colourCount),name = "Prey", labels = FG_tab$LongName[FG_tab$Code%in%selec_prey])+
    facet_wrap(~paste("Age",Cohort))+
    labs(title= paste("Diet of ",FG_tab$LongName[FG_tab$Name==fg]),
         y="Diet proportions (%)", x = "Years",fill = "Prey",
         color="Prey")+
    theme_bw()->dietplot
  
  plot(dietplot)
  
  }
  
}
