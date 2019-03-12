#' Selectivity Plotting
#'
#' \code{selectivity_plotter} takes a dataframe with one application year and returns
#' a lollipop plot of applicaitons and offers
#'
#' @param dfADM a dataframe exported by the \code{\link{adm_data_prep}} function
#' @param l.color list of colors for the dots; default is "#35618f"
#' @param theme_stripe custom theme object for plotting
#' @param v.AppYear the varaible defining the year of application; default value is
#' \code{AppYear}
#' @param v.Selectivity the variable name for the selectivity metric; default value
#' is \code{Selectivity}
#' @param colorVar the variable used to define the color and fill for the dots;
#' the default is \code{Graduate.Applciation.HEGIS}
#'
#' @return a ggplot object
#'
#'
#'@export
#'


selectivity_plotter <- function(dfADM, l.color = c("#35618f"), theme_stripe = NULL,
                                v.AppYear = AppYear, v.Selectivity = Selectivity,
                                colorVar = Graduate.Application.HEGIS){
  q.AppYear = enquo(v.AppYear)
  q.Selectivity = enquo(v.Selectivity)
  q.colorVar <- enquo(colorVar)

  maxAxis <- dfADM %>%
    ungroup() %>%
    summarise(maxvalue = max(!! q.Selectivity) *1.1) %>%
    unlist

plSelectivity <- dfADM %>%
  ggplot() +
  geom_segment(aes(x = !! q.AppYear, xend = !! q.AppYear, y = 0, yend = !! q.Selectivity)) +
  geom_point(aes( x = !! q.AppYear, y = !! q.Selectivity, fill = !! q.colorVar,
                  color = !! q.colorVar,
                  text = paste0("Application Year: ", !! q.AppYear,
                                "\nSelectivity: ", percent( !! q.Selectivity))),
             stat = 'identity', size = 5)+
  theme_minimal()+
  scale_fill_manual(values = l.color)+
  scale_color_manual(values = l.color)+
  theme(panel.grid.major.x = element_blank())+
  theme_stripe+
  scale_y_continuous(labels = percent_format(), limits = c(0, maxAxis)) +
  scale_x_continuous(breaks = int_breaks_x)


plSelectivity

}
