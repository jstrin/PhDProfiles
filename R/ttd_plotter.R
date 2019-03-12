#' Time To Degree Plotter
#'
#' \code{ttd_plotter} takes a dataframe with one row per graduate
#' and returns a plot. If there are more than ten rows, a histogram
#' will be
#'
#' @param dfTTD a dataframe imported from
#' @param ttdColor fill color for the histogram or density plot; default is a deep blue: #35618f
#'
#' @return a ggplot object, if there are 10 or more rows in the supplied dataframe, then the
#' object returned will be a histogram. If there are 5 or more rows, but fewer than 10, the
#' object will be kernal desnsity plot. If there are fewer than 5 rows, the object will be
#' a blank plot with red text stating 'Insufficient Observations'
#'
#'
#'@export


ttd_plotter <- function(dfTTD, ttdColor = "#35618f", ttdVar = Time.To.Degree..Yrs...22) {
 q.ttdvar <- enquo(ttdVar)

  if (nrow(dfTTD) >= 10) {
    plTTD <- ggplot(dfTTD, aes(x = (!! q.ttdvar))) +
      geom_histogram(binwidth = 1/3, fill = ttdColor) +
      stat_bin(aes(label = "", text = paste0("Time to Degree: ",
        round(x, 1), " years", "\nCount: ", ..count..)),
        geom = "text", binwidth = 1/3) +
      scale_y_continuous(breaks = int_breaks_y) +
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
