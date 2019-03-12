#' Attrition Data Preperation
#'
#' \code{attrition_data_prep} modifies the PhD Attrition by Hegis report
#' and returns a dataframe that is ready for plotting by calculatiing totals and
#' attrition rates
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
#'  Shared Variables:
#'
#' \itemize{
#' \item \code{Col} is the college;
#' \item \code{Hegis.Cd} is HEGIS code of the phd program;
#' \item \code{Hegis.Description} is the HEGIS description fo the phd program;
#' \item \code{Cohort.Year} is the academic year of the entering cohort e.g., 02-03 is the cohort that
#' began in Fall 2002;
#' \item \code{Cohort.Size} number of students in the entering cohort;
#'}
#'
#' \code{dfAttrition_wide} variables:
#' \itemize{
#' \item \code{Without.MA[__n]} number of students who left in a given year in the \code{n} + 1 year in program
#' without a Masters or PhD
#' \item \code{Stop.Out[__n]} number of students who left in a given year in the \code{n} + 1 year in program
#' but returned at a later date
#' \item\code{PhD.Degree[__n]} number of students who left in a given year in the \code{n} + 1 year in program
#' with a PhD
#' \item\code{Continuing[__n]} number of students who continued from year \code{n} + 1  to year \code{n} + 2
#' \item\code{Info.Unknown[__n]} number of students in  the \code{n} + 1 year in program that we were not
#' able to find information on
#' \item\code{Without.MA[__n]} number of students who left in a given year in the \code{n} + 1 year in program
#' without a Masters or PhD
#' \item\code{Without.MA_yr[n]} is the number of phd students who left by the end of year \code{n}
#' without a Masters degree or a PhD
#' \item\code{With.MA_yr[n]} is the number of phd students who left by the end of year \code{n}
#' with a Masters degree and without a PhD
#'  \item\code{Total_yr[n]} is the number of phd students who left by the end of year \code{n} without
#'  a PhD
#'}
#'
#'
#'\code{dfAttrition_long} variables:
#'\itemize{
#'\item\code{AcadYear} numeric variable indicating the academic year of the entering cohort (e.g., "02-03" would
#'have a  value of 2003)
#'\item\code{Pathyear} string/character variable where the first part indicates the path of exit (with MA,
#'without MA, total) and the second part indicates the year in program
#' \item\code{Count} numeric varialbe indicating the cumulative number of students who exited the program
#' in the given path by the year in program
#' \item\code{Year} string/character variable indicating the year in program
#' \item\code{Path} string/character variable indicating the path of exit
#' \item\code{Per.Cohort} string/character indicating the percentage of the cohort that exited the program
#' in the given path by the year in program
#'}
#'
#'
#'
#'

attrition_data_prep_parameterized <- function(dfAttrition_report,
                                college_var = Col,
                                hegiscd_var = Hegis.Cd,
                                hegisdesc_var = Hegis.Description,
                                cohort_var = Cohort.Year,
                                WoMA_var = Without.MA..8,
                                wMA_var = With.MA..9){

  q.Col = enquo( Col )
  q.hegis_code = enquo( hegiscd_var )
  q.hegis_desc = enquo( hegisdesc_var )
  q.cohort = enquo( cohort_var )
  q.wo_ma = enquo( WoMA_var )
  q.w_ma = enquo( wMA_var )



  # remove Totals, blanks and NAs for College
  # remove Approved Deferrals & recode GMS Hegis and HEgis description fields
  # select last 15 years
  # To account for recoding of GMS hegis, need to group and sum all values
  #  Create cummulative totals; define academic year for plotting purposes

  dfAttrition_wide <- dfAttrition_report %>%
    filter(!( !! q.Col ) %in% c("TOT", "", NA),
           !(!! q.hegis_code) %in% c("0010")) %>%
    mutate( !! q.hegis_desc := case_when(!! q.hegis_code %in% c("0477") ~ "Biochemistry",
                                          !! q.hegis_code %in% c("0478") ~ "Biophysics",
                                          !! q.hegis_code %in% c("0479") ~ "Genetics & Genomics",
                                          !! q.hegis_code %in% c("0482", "0487") ~ "Molec&Transltnl_Med",
                                          !! q.hegis_code %in% c("0480") ~ "Nutrition&Metabolism",
                                          !! q.hegis_code %in% c("1346") ~ "Oral Biology",
                                          !! q.hegis_code %in% c("0481") ~ "Microbiology",
                                          !! q.hegis_code %in% c("0483") ~ "Pathology",
                                          !! q.hegis_code %in% c("0484") ~ "Physiology",
                                          TRUE ~ !! q.hegis_desc),
            !! q.hegis_code := case_when(!! q.hegis_code %in% c("0477") ~ "0414",
                                 !! q.hegis_code %in% c("0478") ~ "0439",
                                 !! q.hegis_code %in% c("0479") ~ "0444",
                                 !! q.hegis_code %in% c("0482", "0487") ~ "0486",
                                 !! q.hegis_code %in% c("1346") ~ "1234",
                                 !! q.hegis_code %in% c("0481") ~ "0411",
                                 !! q.hegis_code %in% c("0483") ~ "0408",
                                 !! q.hegis_code %in% c("0484") ~ "0410",
                                 TRUE ~ !! q.hegis_code)) %>%
    filter(!! q.cohort %in% c("02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17")) %>%
    mutate(HEGIS = as.numeric(!! q.hegis_code)) %>%
    group_by(!! q.Col, !! q.hegis_code, !! q.hegis_desc, HEGIS,  !! q.cohort) %>%
    summarise_if(is.numeric, sum, na.rm=FALSE) %>%
    ungroup()%>%
    mutate(Without.MA_yr1 = !! q.wo_ma,
           Without.MA_yr2 = Without.MA__1 + Without.MA_yr1,
           Without.MA_yr3 = Without.MA__2 +  Without.MA_yr2,
           Without.MA_yr4 = Without.MA__3 + + Without.MA_yr3,
           Without.MA_yr5 = Without.MA__4 +  Without.MA_yr4,
           Without.MA_yr6 = Without.MA__5 +  Without.MA_yr5,
           Without.MA_yr7 = Without.MA__6 +  Without.MA_yr6,
           Without.MA_yr8 = Without.MA__7 +  Without.MA_yr7,
           Without.MA_yr9 = Without.MA__8 +  Without.MA_yr8,
           Without.MA_yr10 = Without.MA__9 +  Without.MA_yr9,
           Without.MA_yr10plus = Without.MA__10 +   Without.MA_yr10,
           With.MA_yr1 = With.MA,
           With.MA_yr2 = With.MA__1 + With.MA_yr1,
           With.MA_yr3 = With.MA__2 + With.MA_yr2,
           With.MA_yr4 = With.MA__3 + With.MA_yr3,
           With.MA_yr5 = With.MA__4 + With.MA_yr4,
           With.MA_yr6 = With.MA__5 + With.MA_yr5,
           With.MA_yr7 = With.MA__6 + With.MA_yr6,
           With.MA_yr8 = With.MA__7 + With.MA_yr7,
           With.MA_yr9 = With.MA__8 + With.MA_yr8,
           With.MA_yr10 = With.MA__9 + With.MA_yr9,
           With.MA_yr10plus = With.MA__10 + With.MA_yr10,
           Total_yr1 = Without.MA_yr1 + With.MA_yr1,
           Total_yr2 = Without.MA_yr2 + With.MA_yr2,
           Total_yr3 = Without.MA_yr3 + With.MA_yr3,
           Total_yr4 = Without.MA_yr4 + With.MA_yr4,
           Total_yr5 = Without.MA_yr5 + With.MA_yr5,
           Total_yr6 = Without.MA_yr6 + With.MA_yr6,
           Total_yr7 = Without.MA_yr7 + With.MA_yr7,
           Total_yr8 = Without.MA_yr8 + With.MA_yr8,
           Total_yr9 = Without.MA_yr9 + With.MA_yr9,
           Total_yr10 = Without.MA_yr10 + With.MA_yr10,
           Total_yr10plus = Without.MA_yr10plus + With.MA_yr10plus,
           AcadYear = 2000+ as.numeric(stringr::str_sub(!! q.cohort, start=4, end=5)))

  # Select Col, Hegis, Hegis description, cohort year, acad year, cohort size, and all cumulative sums
  # transform to long format (e.g., possible to group by pathway and year
  #  calculate rates

  dfAttrition_long <- dfAttrition_wide %>%
    select(!! q.Col, HEGIS, !! q.hegis_desc, !!q.cohort, AcadYear, Cohort.Size, With.MA_yr1,
           With.MA_yr2, With.MA_yr3, With.MA_yr4, With.MA_yr5, With.MA_yr6,
           With.MA_yr7, With.MA_yr8, With.MA_yr9,  With.MA_yr10, With.MA_yr10plus,
           Without.MA_yr1, Without.MA_yr2, Without.MA_yr3, Without.MA_yr4, Without.MA_yr5,
           Without.MA_yr6, Without.MA_yr7, Without.MA_yr8, Without.MA_yr9,
           Without.MA_yr10, Without.MA_yr10plus, Total_yr1, Total_yr2, Total_yr3,
           Total_yr4, Total_yr5, Total_yr6, Total_yr7, Total_yr8, Total_yr9,  Total_yr10,
           Total_yr10plus) %>%
    gather(Pathyear, Count, With.MA_yr1:Total_yr10plus) %>%
    mutate(Year = stringr::str_sub(Pathyear, start = stringr::str_locate(Pathyear, "_")[,1]+1),
           Path = case_when(grepl("Without.", Pathyear) ~ "Without Masters",
                            grepl("With.", Pathyear) ~ "With Masters",
                            grepl("Total", Pathyear) ~ "Total"),
           AcadYear = 2000+ as.numeric(stringr::str_sub(!! q.cohort, start=4, end=5))) %>%
    mutate(Per.Cohort = Count/Cohort.Size)

lsOut <- list(dfAttrition_wide = dfAttrition_wide, dfAttrition_long = dfAttrition_long)

lsOut
}
