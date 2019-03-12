#' Attrition Plot Interactive
#'
#' \code{attrition_plot_int} returns a plotly object
#'
#' @param dfAttrition_report a copy of the 'PhD Attrtion by Hegis Code
#' Template - AY[AY] thru [AY] - [date]' report imported from the xlsx
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select mutate filter group_by mutate_if summarise_if ungroup
#' @importFrom tidyr gather spread
#'
#'
#' @return A list of dataframes: one with one column with one row per program/cohort (\code{dfAttrition_wide}), and
#'  one with one row per program/cohort and exit pathway (\code{dfAttrition_long})
#'
