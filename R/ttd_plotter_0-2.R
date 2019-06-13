#' Time To Degree Plotter
#'
#' \code{ttd_plotter_2} takes a dataframe with one row per graduate
#' and returns a plot. If there are more than ten rows, a histogram
#' will be returned.
#'
#' @param dfTTD a dataframe imported from
#' @param ttdColor fill color for the histogram or density plot; default is a deep blue: #35618f
#'
#' @return a ggplot object, if there are 10 or more rows in the supplied dataframe, then the
#' object returned will be a histogram. If there are 5 or more rows, but fewer than 10, the
#' object will be kernal desnsity plot. If there are fewer than 5 rows, the object will be
#' a blank plot with red text stating 'Insufficient Observations'
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr ungroup summarise
#'
#'@export


ttd_plotter_2 <- function(dfTTD, ttdColor = "#35618f", ttdVar = Time.To.Degree..Yrs....22,
                          medColor = "black") {

 q.ttdvar <- enquo(ttdVar)

 medTTD <- dfTTD %>%
   ungroup() %>%
   summarise(medTTD = median( (!! q.ttdvar), na.rm=T)) %>%
   unlist()


  if (nrow(dfTTD) >= 10) {
    plTTD <- ggplot(dfTTD) +
      geom_histogram( aes(x = (!! q.ttdvar)), binwidth = 1/3, fill = ttdColor) +
      geom_vline( aes( xintercept = medTTD ), col = medColor) +
      stat_bin(aes(x = (!! q.ttdvar), label = "", text = paste0("Time to Degree: ",
        round(x, 1), " years", "\nCount: ", ..count..)),
        geom = "text", binwidth = 1/3) +
      scale_y_continuous(breaks = int_breaks_y) +
      scale_x_continuous(limits = c(0, NA))+
      theme_minimal() +
      theme(panel.grid.major.x = element_blank())

  } else {
    if (nrow(dfTTD) >= 5) {
      plTTD <- ggplot(dfTTD, aes(x = (!! q.ttdvar))) +
        geom_density(fill = ttdColor) +
        stat_density(aes(label = "", text = paste0("Time to Degree: ", round(x, 1),
          " years", "\nDensity: ", round(..density.., 2))), geom = "text") +
        theme_minimal() +
        theme(panel.grid.major.x = element_blank())
    } else {
      plTTD <- ggplot() + geom_text(aes(x = 0.5, y = 1,
        label = "INSUFFICENT OBSERVATIONS"), color = "red") +
        theme_minimal() +
        theme(axis.title = element_blank(), axis.text = element_blank(),
              axis.line = element_blank(),  panel.grid = element_blank(),
              panel.grid.major.x = element_blank())
    }
  }

  plTTD
}
