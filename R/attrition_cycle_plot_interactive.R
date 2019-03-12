#' Attrition Plot Interactive
#'
#' \code{attrition_cycle_plot_int} plotting funciton for the creation of cycle plots For an overivew of cycle plots,
#' please see this \href{https://www.perceptualedge.com/articles/guests/intro_to_cycle_plots.pdf}{article} in
#' Stephen Few's Perceptual Edge library.
#'
#' @param dfAttrition_long a attrition dataframe formatted by the \code{\link{attrition_data_prep}} function
#' @param l.hegis a vector of hegis codes to plot
#' @param ExitPath a character string indicating which exit path ("TOTAL", "With Masters", "Without Masters"
#' to plot )
#'
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr mutate filter
#' @import plotly
#'
#'
#'
#' @return A cycle plot attrtition data as a plotly object
#'
#' @export


attrition_cycle_plot_int <- function(dfAttrition_long, l.hegis = NULL, ExitPath = "TOTAL"){

# filter to hegis of interest
dfAttrition_long <- dfAttrition_long %>%
  filter(HEGIS %in% l.hegis)

AcadYear <- c(2003:2021)

exityr <- c( paste0("yr", c(1:11)))



Path <- c("With MA", "Without MA", "PhD", "Total")



dfBase <- expand.grid(AcadYear = AcadYear, exityr = exityr, Path = Path) %>%
  mutate(exityr2 = case_when(exityr %in% ("yr11") ~ "yr10plus",
                             !exityr %in% ("yr11") ~ as.character(exityr))) %>%
  mutate(exityr = as.character(exityr),
         exityr2 = as.character(exityr2),
         Path = as.character(Path))



dfPlot <- dfBase %>%
  full_join(., dfAttrition_long, by = c("exityr2" = "Year","AcadYear" =  "AcadYear", "Path" = "Path")) %>%
  mutate(Year = factor(exityr2, levels = c("yr1","yr2", "yr3", "yr4", "yr5", "yr6", "yr7",
                                           "yr8", "yr9", "yr10", "yr10plus"), ordered = T,
                       labels = c("1st",  "2nd", "3rd", "4th", "5th", "6th", "7th", "8th", "9th", "10th", "10 plus")))%>%
  mutate(xvalue = as.integer(Year) + AcadYear/10000) %>%
  mutate(xvalue = as.integer(factor(xvalue, ordered = T))) %>%
  mutate(x_tickValue = case_when(Cohort.Year %in% c("04-05", "09-10", "14-15") ~ xvalue,
                                 !Cohort.Year %in% c("04-05", "09-10", "14-15") ~ as.integer(NA)),
         x_tickLabel = case_when(Cohort.Year %in% c( "04-05", "09-10", "14-15") ~ Cohort.Year,
                                 !Cohort.Year %in% c("04-05", "09-10", "14-15") ~ "")) %>%
  mutate(tooltip = paste0("Cohort: ", Cohort.Year,
                          "\nCount: ", Count ,
                          "\nPercent: ", scales::percent(Per.Cohort)))







#Filter to single exit path and  create shared data

sdAttrition <- dfPlot %>%
  filter(Path %in% ExitPath) %>%
  crosstalk::SharedData$new(., ~Cohort.Year, group = "Entering Cohort")







plCycle <- plot_ly(sdAttrition,x = ~xvalue, y = ~Per.Cohort) %>%
  add_lines(text = ~tooltip, hoverinfo = "text",
            connectgaps = TRUE, color = ~Cohort.Year, colors = "white") %>%
  add_markers(text = ~tooltip, hoverinfo = "text") %>%
  add_lines( text = ~tooltip, hoverinfo ="text") %>%
  highlight(on = "plotly_click", persistent = TRUE, selectize = TRUE, dynamic = TRUE) %>%
  layout(showlegend = FALSE,
         margin = list(b=75),
         yaxis = list(range = c(0,1), tickformat="%", title="Percent of Cohort"),
         xaxis = list(ticktext = ~x_tickLabel,
                      tickmode = 'array',
                      tickvals = ~x_tickValue,
                      title = "Year in Program & Entering Cohort Year"),
         shapes = list(list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 16, x1 = 19, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 35, x1 = 38, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 54, x1 = 57, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 73, x1 = 76, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 92, x1 = 95, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 111, x1 = 114, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 130, x1 = 133, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 149, x1 = 152, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 168, x1 = 171, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"),
                       list(type = "rect",
                            fillcolor = "black", line = list(color = "black"), opacity = 0.3,
                            x0 = 187, x1 = 190, xref = "x",
                            y0 = 0, y1 = 1, yref = "y"))) %>%
  add_annotations( text = "Year 1",
                   x = 7, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 2",
                   x = 26, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 3",
                   x = 45, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 4",
                   x = 65, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 5",
                   x = 84, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 6",
                   x = 103, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 7",
                   x = 122, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 8",
                   x = 141, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 9",
                   x = 160, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 10",
                   x = 179, y = .95,
                   showarrow = FALSE) %>%
  add_annotations( text = "Year 10+",
                   x = 198, y = .95,
                   showarrow = FALSE)


plCycle
}
