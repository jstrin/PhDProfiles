#' Completions Data Preperation
#'
#'
#' \code{comp_data_prep} modifies the PhD Completion by Hegis report
#' and returns a dataframe that is ready for plotting by calculatiing totals and
#' attrition rates
#'
#' @param dfCompletion_report a copy of the 'PhD Completion by Hegis Code
#' Template - AY[AY] thru [AY] - [date]' report imported from the xlsx
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select mutate filter group_by summarise ungroup select starts_with
#' @importFrom tidyr gather spread
#' @importFrom stringr str_replace_all
#'
#'
#' @return A data frame with one row per program/cohort and completion year. There is a
#' variable
#'
#' \itemize{
#' \item \code{Col} is the college;
#' \item \code{Hegis.Cd} is HEGIS code of the phd program;
#' \item \code{Hegis.Description} is the HEGIS description fo the phd program;
#' \item \code{Cohort.Year} is the academic year of the entering cohort e.g., 02-03 is the cohort that
#' began in Fall 2002;
#' \item \code{Cohort.Size} number of students in the entering cohort;
#' \item\code{AcadYear} numeric variable indicating the academic year of the entering cohort (e.g., "02-03" would
#'have a  value of 2003)
#' \item\code{Count} numeric varialbe indicating the cumulative number of students who have completed the program
#' \item\code{Year} string/character variable indicating the year in program
#' \item\code{Per.Cohort} string/character indicating the percentage of the cohort that completed the PhD
#' by the Year
#'}
#'
#'@export
#'
#'
#'

comp_data_prep <- function(dfCompletion_report){




# names(dfCompletion_report) <- str_replace_all(names(dfCompletion_report), " ", ".")
#
# names(dfCompletion_report)[c(7, 9:25,27)] <- paste0("X", names(dfCompletion_report)[c(7, 9:25,27)])
# dfCompletion_report <- dfCompletion_report %>%
#   select(-starts_with("XX"))


dfCompletion <- dfCompletion_report %>%
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
  filter(Cohort.Year %in% c("01-02", "02-03", "03-04", "04-05", "05-06", "06-07", "07-08", "08-09", "09-10", "10-11", "11-12", "12-13", "13-14", "14-15", "15-16")) %>%
  mutate(HEGIS = as.numeric(Hegis.Cd)) %>%
  mutate(Col = case_when(
    Col %in% c("GSM") ~ "QST",
    !Col %in% c("GSM") ~ Col)) %>%
  group_by(Hegis.Cd, Hegis.Description, HEGIS,  Cohort.Year) %>%
  summarise_if(is.numeric, sum, na.rm=FALSE) %>%
  mutate(yr3 = ..1.thru.3,
         yr4 = ...4 + yr3,
         yr5 = ...5 +  yr4,
         yr6 = ...6 +  yr5,
         yr7 = ...7 +  yr6,
         yr8 = ...8 +  yr7,
         yr9 = ...9 +  yr8,
         yr10 = ...10 +  yr9,
         yr10plus = Completed.program.after.more.than.10.years +  yr10) %>%
  select(-starts_with("...")) %>%
  gather(Year, Count, yr3:yr10plus)%>%
  mutate(Per.Cohort = Count/Cohort.Size,
        AcadYear = 2000+ as.numeric(str_sub(Cohort.Year, start=4, end=5)),
        Year = factor(Year, levels = c("yr3", "yr4", "yr5", "yr6", "yr7", "yr8", "yr9", "yr10", "yr10plus"),
                      ordered=T))

dfCompletion
}
