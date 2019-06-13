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


career_outcomes_preprocess2 <- function(career_excel){







dfCareer1 <- read_excel(career_excel) %>%
  mutate(GradYear = case_when(
    #!is.na(GradYear) ~ as.integer(GradYear),
    grad_date %in% c("Sep_2006", "Jan_2007", "May_2007") ~ as.integer(2007),
    grad_date %in% c("Sep_2011", "Jan_2012", "May_2012") ~ as.integer(2012) )) %>%
  mutate(NAICS = as.numeric(NAICS),
         StartMonth = as.character(StartMonth),
         ZIP = as.numeric(ZIP),
         Primary.Position = as.numeric(Primary.Position)) %>%
  mutate(reportingYr = 2017)


dfCareer2 <- read_excel(here::here("data/PhdCareerOutcomes_21Feb2019.xlsx"), sheet = "AY2008_AY2013") %>%
  mutate(grad_date = case_when(Semester %in% "Summer" & AcadYear %in% c(2008) ~ "Sep_2007",
                               Semester %in% "Summer" & AcadYear %in% c(2013) ~ "Sep_2012",
                               Semester %in% "Fall" & AcadYear %in% c(2008) ~ "Jan_2008",
                               Semester %in% "Fall" & AcadYear %in% c(2013) ~ "Jan_2013",
                               Semester %in% "Spring" & AcadYear %in% c(2008) ~ "May_2008",
                               Semester %in% "Spring" & AcadYear %in% c(2013) ~ "May_2013")) %>%
  mutate(GradYear = AcadYear) %>%
  mutate(PositionTiming = case_when(EndYr %in% c("Current") & GradYear %in% 2013 ~ "5 years out",
                                    EndYr %in% c("Current") & GradYear %in% 2008 ~ "10 years out")) %>%
  mutate(SourceDate = as.character(SourceDate)) %>%
  mutate(reportingYr = 2018) %>%
  mutate(GradYear = case_when(UID %in% c("U17062842") ~ 2013,
                              UID %in% c("U74673229") ~ 2008,
                              TRUE ~ GradYear))


# find any case where there is only one record, code as 5 years out/10 years out


dfCareer3 <- dfCareer1 %>%
  bind_rows(dfCareer2) %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(PositionTiming2 = case_when(dup == 1 & GradYear %in% c(2012, 2013) ~ "5 years out",
                                     dup == 1 & GradYear %in% c(2007, 2008) ~ "10 years out",
                                     !is.na(PositionTiming) ~ PositionTiming)) %>%
  group_by(PositionTiming2, UID) %>%
  mutate(dup2 = n(),
         maxPrimary = max(Primary.Position, na.rm =T),
         nPrimary = sum(Primary.Position, na.rm = T)) %>%
  ungroup() %>%
  group_by(UID) %>%
  mutate(dup = n(),
         maxPrimary2 = max(Primary.Position, na.rm =T),
         nPrimary2 = sum(Primary.Position, na.rm = T)) %>%
  ungroup() %>%
  mutate(keep = case_when(dup == 1 ~ 1,
                          !is.na(PositionTiming2) & Primary.Position==1 ~ 1,
                          !is.na(PositionTiming2) & nPrimary2 ==1 ~ 0,
                          !is.na(PositionTiming2) & maxPrimary2 != 1 ~ 1,
                          TRUE ~ 0))


t<- dfCareer3 %>% filter(keep ==0)
dfKept<- dfCareer3 %>% filter(keep ==1)
t<- t %>% mutate(kept = case_when(UID %in% dfKept$UID ~ 1, TRUE ~ 0) )
t2 <- t %>% filter(kept ==0)
dfKept %>% ungroup() %>% group_by(UID) %>% mutate(dup = n()) ->kept
kept <- kept %>% filter(dup >1)


dfCareer_deDup <- dfCareer3 %>%
  filter(keep == 1)

dfmissing <- dfCareer3 %>%
  filter(!UID %in% dfCareer_deDup$UID)


dfCareer_deDup <- dfCareer_deDup %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  ungroup()

dfCareer_deDup %>%
  filter(dup >1 ) %>%
  nrow()


dfCareer <- dfCareer_deDup %>%
  mutate(PositionTiming = PositionTiming2)

dfOutcomeHigh <- read_excel(career_excel, sheet = "OutcomeHigh")
dfEmployerType <- read_excel(career_excel, sheet = "EmployerType")
dfAcStage <-  read_excel(career_excel, sheet = "AcStage")
dfAcadJobTitle <- read_excel(career_excel, sheet = "AcadJobTitle")
dfNAICS <- read_excel(career_excel, sheet = "NAICS")
dfNAICS2 <- read_excel(career_excel, sheet = "NAICS_2digit") %>%
  mutate(NAICS2 = as.character(NAICS)) %>%
  select(-NAICS)

# ordering factors

dfEmployerType <- dfEmployerType %>%
  mutate(Description = forcats::fct_inorder(Description),
         EmployerShort = forcats::fct_inorder(EmployerShort),
         EmployerHigh = forcats::fct_inorder(EmployerHigh))

dfNAICS2 <- dfNAICS2 %>%
  mutate(DescriptionShort = forcats::fct_inorder(DescriptionShort),
         Description = forcats::fct_inorder(Description))





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
  rename(AcadJobTitle = Description) %>%
  left_join(., dfNAICS) %>%
  rename(Industry_detail = Description) %>%
  mutate(NAICS2 = stringr::str_sub(NAICS, 1,2)) %>%
  left_join(dfNAICS2) %>%
  rename(Industry_short = DescriptionShort,
         Industry = Description)


dfPlot_outcome <- dfPlot %>%
  group_by(PositionTiming) %>%
  mutate(PositionTiming_count = n()) %>%
  group_by(OutcomeHigh, PositionTiming) %>%
  summarise(outcome_count = n(),
            PositionTiming_count = max(PositionTiming_count)) %>%
  ungroup() %>%
  mutate(perPosition = outcome_count/PositionTiming_count)

dfPlot_Employer <- dfPlot %>%
  group_by(PositionTiming) %>%
  mutate(PositionTiming_count = n()) %>%
  group_by(EmployerHigh, PositionTiming) %>%
  summarise(outcome_count = n(),
            PositionTiming_count = max(PositionTiming_count)) %>%
  ungroup() %>%
  mutate(perPosition = outcome_count/PositionTiming_count)


dfCareerShared_plot
}
