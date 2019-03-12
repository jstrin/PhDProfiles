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
#' \item \code{Without.MA[..n]} number of students who left in a given year in the \code{n} + 1 year in program
#' without a Masters or PhD
#' \item \code{Stop.Out[..n]} number of students who left in a given year in the \code{n} + 1 year in program
#' but returned at a later date
#' \item\code{PhD.Degree[..n]} number of students who left in a given year in the \code{n} + 1 year in program
#' with a PhD
#' \item\code{Continuing[..n]} number of students who continued from year \code{n} + 1  to year \code{n} + 2
#' \item\code{Info.Unknown[..n]} number of students in  the \code{n} + 1 year in program that we were not
#' able to find information on
#' \item\code{Without.MA[..n]} number of students who left in a given year in the \code{n} + 1 year in program
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
#'@export
#'
#'

attrition_data_prep <- function(dfAttrition_report){


  # remove Totals, blanks and NAs for College
  # remove Approved Deferrals & recode GMS Hegis and HEgis description fields
  # select last 15 years
  # To account for recoding of GMS hegis, need to group and sum all values
  #  Create cummulative totals; define academic year for plotting purposes

  dfAttrition_wide <- dfAttrition_report %>%
    filter(!Col %in% c("TOT", "", NA),
           !Hegis.Cd %in% c("0010")) %>%
    mutate( Hegis.Description = case_when(Hegis.Cd %in% c("0477") ~ "Biochemistry",
                                          Hegis.Cd %in% c("0478") ~ "Biophysics",
                                          Hegis.Cd %in% c("0479") ~ "Genetics & Genomics",
                                          Hegis.Cd %in% c("0482", "0487") ~ "Molec&Transltnl_Med",
                                          Hegis.Cd %in% c("0480") ~ "Nutrition&Metabolism",
                                          Hegis.Cd %in% c("1346") ~ "Oral Biology",
                                          Hegis.Cd %in% c("0481") ~ "Microbiology",
                                          Hegis.Cd %in% c("0483") ~ "Pathology",
                                          Hegis.Cd %in% c("0484") ~ "Physiology",
                                          !Hegis.Cd %in% c("0477", "0483", "0478", "0479", "0482", "0480", "0481", "1346", "0484") ~ Hegis.Description),
            Hegis.Cd = case_when(Hegis.Cd %in% c("0477") ~ "0414",
                                 Hegis.Cd %in% c("0478") ~ "0439",
                                 Hegis.Cd %in% c("0479") ~ "0444",
                                 Hegis.Cd %in% c("0482", "0487") ~ "0486",
                                 Hegis.Cd %in% c("1346") ~ "1234",
                                 Hegis.Cd %in% c("0481") ~ "0411",
                                 Hegis.Cd %in% c("0483") ~ "0408",
                                 Hegis.Cd %in% c("0484") ~ "0410",
                                 !Hegis.Cd %in% c("0477", "0478", "0479", "0482", "0480", "0481", "1346", "0483", "0484") ~ Hegis.Cd)) %>%
    filter(Cohort.Year %in% c("02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16", "16-17")) %>%
    mutate(HEGIS = as.numeric(Hegis.Cd)) %>%
    group_by(Col, Hegis.Cd, Hegis.Description, HEGIS,  Cohort.Year) %>%
    summarise_if(is.numeric, sum, na.rm=FALSE) %>%
    ungroup()%>%
    mutate(Without.MA_yr1 = Without.MA..8,
           Without.MA_yr2 = Without.MA..15 + Without.MA_yr1,
           Without.MA_yr3 = Without.MA..22 +  Without.MA_yr2,
           Without.MA_yr4 = Without.MA..29 + Without.MA_yr3,
           Without.MA_yr5 = Without.MA..36 +  Without.MA_yr4,
           Without.MA_yr6 = Without.MA..43 +  Without.MA_yr5,
           Without.MA_yr7 = Without.MA..50 +  Without.MA_yr6,
           Without.MA_yr8 = Without.MA..57 +  Without.MA_yr7,
           Without.MA_yr9 = Without.MA..64 +  Without.MA_yr8,
           Without.MA_yr10 = Without.MA..71 +  Without.MA_yr9,
           Without.MA_yr10plus = Without.MA..78 +   Without.MA_yr10,
           With.MA_yr1 = With.MA..9,
           With.MA_yr2 = With.MA..16 + With.MA_yr1,
           With.MA_yr3 = With.MA..23 + With.MA_yr2,
           With.MA_yr4 = With.MA..30 + With.MA_yr3,
           With.MA_yr5 = With.MA..37 + With.MA_yr4,
           With.MA_yr6 = With.MA..44 + With.MA_yr5,
           With.MA_yr7 = With.MA..51 + With.MA_yr6,
           With.MA_yr8 = With.MA..58 + With.MA_yr7,
           With.MA_yr9 = With.MA..65 + With.MA_yr8,
           With.MA_yr10 = With.MA..72 + With.MA_yr9,
           With.MA_yr10plus = With.MA..79 + With.MA_yr10,
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
           AcadYear = 2000+ as.numeric(stringr::str_sub(Cohort.Year, start=4, end=5)))

  # Select Col, Hegis, Hegis description, cohort year, acad year, cohort size, and all cumulative sums
  # transform to long format (e.g., possible to group by pathway and year
  #  calculate rates

  dfAttrition_long <- dfAttrition_wide %>%
    select(Col, HEGIS, Hegis.Description, Cohort.Year, AcadYear, Cohort.Size, With.MA_yr1,
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
           AcadYear = 2000+ as.numeric(stringr::str_sub(Cohort.Year, start=4, end=5))) %>%
    mutate(Per.Cohort = Count/Cohort.Size)

lsOut <- list(dfAttrition_wide = dfAttrition_wide, dfAttrition_long = dfAttrition_long)

lsOut
}
