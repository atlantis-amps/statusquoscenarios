#' Plot Weight-at-Age and Abundance by Scenario
#'
#' Creates publication-ready plots of mean weight-at-age and abundance across
#' age classes and scenarios from Atlantis model output.
#'
#' @param wage.files A character vector of file paths to wage/abundance data files.
#' @param min.time Numeric. Minimum time (in years) to filter data.
#' @param max.time Numeric. Maximum time (in years) to filter data.
#' @param time.step Numeric. Time step used in the Atlantis model output.
#'
#' @details
#' This function reads multiple wage and abundance data files, calculates mean
#' weight-at-age and mean abundance by scenario and age group, and generates
#' multi-page plots suitable for publication. Plots use the "decades" color
#' palette from the psimfcolors package.
#'
#' Output plots are saved as JPEG files in the output directory:
#' - \code{wage_plot_page#.jpg}: Weight-at-age plots
#' - \code{nums_plot_page#.jpg}: Abundance plots
#'
#' @return Invisibly returns the last plot object created.
#'
#' @examples
#' \dontrun{
#'   wage_files <- list.files("data", pattern = "wage.*\\.csv", full.names = TRUE)
#'   plot_wage_nums(wage.files = wage_files, min.time = 0, max.time = 100, time.step = 5)
#' }
#'
#' @export
#'
#' @importFrom dplyr filter summarise mutate bind_rows if_else
#' @importFrom ggplot2 ggplot aes geom_line scale_color_manual labs theme_classic theme element_text ggsave
#' @importFrom ggforce facet_wrap_paginate n_pages
#' @importFrom stringr str_wrap
#' @importFrom data.table fread
#' @importFrom psimfcolors psimf_palette
#' @importFrom here here
plot_wage_nums <- function(wage.files, min.time, max.time, time.step){
  
  
  wage.nums.data <- list()
  
  for(eachfile in 1:length(wage.files)){
    
    min.timestep <- (min.time*365)/time.step
    max.timestep <- (max.time*365)/time.step
    
    
    this.file <- wage.files[eachfile]
    this.data <- data.table::fread(this.file)
    
    #subset time and Wage
    this.data.time <- this.data %>% 
      dplyr::filter(time >= min.timestep)
    #rel_value is value corrected by basin
    
    #average weight across age classes, time and all basins
    
     print(head(this.data.time))
    wage.nums.data[[eachfile]] <- this.data.time
  }
  
  wage.sc.data <- wage.nums.data %>% dplyr::bind_rows()
  
  mean.weight <- wage.sc.data %>%
    dplyr::filter(variable_type=="Wage") %>%
    dplyr::summarise(mean_w_kg = mean(value), .by=c("group","longname","age","scenario")) %>%
    dplyr::mutate(longname = stringr::str_wrap(longname, width = 15))
  
  wage.plot.base <-mean.weight %>%
    dplyr::mutate(scenario=as.factor(scenario)) %>%
    dplyr::mutate(age=as.factor(age)) %>%
    # Without transparency (left)
    ggplot2::ggplot(aes(x=age, y=mean_w_kg, group=scenario, color=scenario)) +
    ggplot2::geom_line(linewidth = 1) +
    ggplot2::scale_color_manual(values = psimfcolors::psimf_palette("decades", n = length(unique(mean.weight$scenario)))) +
    ggplot2::labs(x= "Age", y = "Mean weight-at-age (kg)", color = "Scenario") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 11),
      axis.title = ggplot2::element_text(size = 12, face = "bold"),
      legend.text = ggplot2::element_text(size = 11),
      legend.title = ggplot2::element_text(size = 12, face = "bold"),
      strip.text = ggplot2::element_text(size = 10, face = "bold")
    )
 #   ggplot2::theme_ipsum() +

  # Determine number of pages needed (ncol*nrow panels per page)
  n_pg <- ggforce::n_pages(
    wage.plot.base +
      ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free_y",
                                   ncol = 5, nrow = 3, page = 1)
  )

  # Save each page as a separate JPG
  for (pg in seq_len(n_pg)) {
    wage.plot <- wage.plot.base +
      ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                   ncol = 5, nrow = 3, page = pg)

    ggplot2::ggsave(
      filename = paste0("wage_plot_page", pg, ".jpg"),
      plot     = wage.plot,
      device   = "jpeg",
      path = here::here("output"),
      width    = 14,
      height   = 10,
      units    = "in",
      dpi      = 300
    )
  }

  invisible(wage.plot)
  
  #subset nums
  mean.nums <- wage.sc.data %>%
    dplyr::filter(variable_type=="Nums") %>% 
    dplyr::mutate(value = dplyr::if_else(is.na(value),0,value)) %>% 
    dplyr::summarise(tot_nums = sum(value), .by=c("group","longname","age","scenario", "time")) %>% 
    dplyr::summarise(mean_nums = sum(tot_nums), .by=c("group","longname","age","scenario"))
  
nums.plot.base <- mean.nums %>%
  dplyr::mutate(longname = stringr::str_wrap(longname, width = 15)) %>%
  dplyr::mutate(scenario=as.factor(scenario)) %>%
  dplyr::mutate(age=as.factor(age)) %>%
  # Without transparency (left)
  ggplot2::ggplot(aes(x=age, y=mean_nums, group=scenario, color=scenario)) +
#  ggplot2::geom_bar(stat="identity") +
  ggplot2::geom_line(linewidth = 1) +
  ggplot2::scale_color_manual(values = psimfcolors::psimf_palette("decades", n = length(unique(mean.nums$scenario)))) +
  ggplot2::labs(x= "Age", y = "Mean abundance", color = "Scenario") +
  ggplot2::theme_classic() +
  ggplot2::theme(
    axis.text = ggplot2::element_text(size = 11),
    axis.title = ggplot2::element_text(size = 12, face = "bold"),
    legend.text = ggplot2::element_text(size = 11),
    legend.title = ggplot2::element_text(size = 12, face = "bold"),
    strip.text = ggplot2::element_text(size = 10, face = "bold")
  )
#   ggplot2::theme_ipsum() +

# Determine number of pages needed (ncol*nrow panels per page)
n_pg <- ggforce::n_pages(
  nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 5, nrow = 3, page = 1)
)

# Save each page as a separate JPG
for (pg in seq_len(n_pg)) {
  nums.plot <- nums.plot.base +
    ggforce::facet_wrap_paginate(ggplot2::vars(longname), scales = "free",
                                 ncol = 5, nrow = 3, page = pg)
  
  ggplot2::ggsave(
    filename = paste0("nums_plot_page", pg, ".jpg"),
    plot     = nums.plot,
    device   = "jpeg",
    path = here::here("output"),
    width    = 14,
    height   = 10,
    units    = "in",
    dpi      = 300
  )
}

invisible(nums.plot)
}
