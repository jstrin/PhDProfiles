#' Career Outcomes Preprocess v 2
#'
#' \code{career_outcomes_preprocess2} reduces the file down to one record per graduate
#' by selecting the most recent primary position
#'
#' @param career_excel name and path of the excel workbook in which the career outcomes and
#' data dictionary are stored
#'
#' @importFrom magrittr "%>%"
#' @import readxl
#' @importFrom dplyr select mutate filter group_by mutate_if summarise_if ungroup
#' @importFrom tidyr gather spread
#' @importFrom forcats fct_inorder
#'
#'
#' @return a data frame with one row per graduate with indicators for
#'
#'@export


career_outcomes_preprocess2 <- function(career_excel = here::here("data/PhDCareer_Profiles.xlsx")){







dfCareer5 <- read_excel(career_excel, sheet = "five_year")
dfCareer10 <- read_excel(career_excel, sheet = "ten_year")

dfCareer <- bind_rows(dfCareer5, dfCareer10)


dfOutcomeHigh <- read_excel(career_excel, sheet = "OutcomeHigh")
dfEmployerType <- read_excel(career_excel, sheet = "EmployerType")
dfAcStage <-  read_excel(career_excel, sheet = "AcStage")
dfAcadJobTitle <- read_excel(career_excel, sheet = "AcadJobTitle")





dfPlot <- dfCareer %>%
  ungroup() %>%
  left_join(., dfOutcomeHigh) %>%
  select(-OutcomeHigh) %>%
  rename(OutcomeHigh = Description) %>%
  left_join(., dfEmployerType) %>%
  select(-EmployerType) %>%
  rename(EmployerType = Description) %>%
  left_join(., dfAcStage) %>%
  select(-AcStage) %>%
  rename(AcStage = Description) %>%
  left_join(., dfAcadJobTitle) %>%
  select(-AcadJobTitle) %>%
  rename(AcadJobTitle = Description)


dfPlot

}
