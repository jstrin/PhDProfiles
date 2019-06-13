#' Applications & Offers Plotting
#'
#' \code{apps_offers_plotter} takes a dataframe with one application year and returns
#' a line plot of applicaitons and offers
#'
#' @param dfADM a dataframe exported by the \code{\link{adm_data_prep}} function
#' @param l.appColors list of colors for the applications and offers lines; default is
#' c("#35618f", "#a0e3b7")
#' @param n.Ynudge a numeric value for adjusting the the placement of the line labels y
#' position, #' if not supplied, a value is calculated of 0.05 * max(# applicaitons in
#' latest two years)
#'
#' @return a ggplot object
#'
#'
#'@export
#'


apps_offers_plotter <- function(dfADM, l.appColors, n.Ynudge = NULL){


  max.year <- max(dfADM$AppYear)

  if(is.null(n.Ynudge)){
    n.Ynudge <- 0.08 * ( max( dfADM$Applications[dfADM$AppYear %in% c(max.year, max.year-1) ]))
  }

plApps <- dfADM %>%
  ungroup() %>%
  select(AppYear, Applications, Offers = Admitted) %>%
  gather(metric, value, Applications:Offers) %>%
  mutate(label = case_when(AppYear %in% max.year ~ as.character(metric),
                           TRUE ~ as.character(""))) %>%
  ggplot()+
  geom_line( aes( x = AppYear, y = value, color = metric))+
  geom_point(aes(x = AppYear, y = value, color = metric, text = paste0("Fall ", AppYear,
                                                                       "\n", metric, ": ", value)), alpha = 0)+
  geom_text(aes(x = AppYear, y = value, label = label, color = metric ),
            hjust = 0, nudge_x = -0.5, nudge_y = n.Ynudge)+
  theme_minimal()+
  scale_color_manual(breaks = c("Applications", "Offers"), values = l.appColors)+
  scale_y_continuous(limits = c(0, 1.10*max(dfADM$Applications)),
                     breaks = int_breaks_y)+
  scale_x_continuous(breaks = int_breaks_x)

plApps

}
