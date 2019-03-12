#' Career Outcomes Preprocess
#'
#' \code{career_outcomes_preprocess} reduces the file down to one record per graduate
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


career_outcomes_preprocess <- function(career_excel){




dfCareer <- read_excel(career_excel) %>%
  mutate(GradYear = case_when(
    #!is.na(GradYear) ~ as.integer(GradYear),
    grad_date %in% c("Sep_2006", "Jan_2007", "May_2007") ~ as.integer(2007),
    grad_date %in% c("Sep_2011", "Jan_2012", "May_2012") ~ as.integer(2012) )) %>%
  mutate(PositionTiming = case_when(PositionAY %in% c(2017) & GradYear %in% 2012 ~ "5 years out",
                                    PositionAY %in% c(2017) & GradYear %in% 2007 ~ "10 years out"))

# find any case where there is only one record, and no position AY date, code as 5 years out/10 years out


dfCareer3 <- dfCareer %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(PositionTiming = case_when(!PositionAY %in%  c(-99) ~ PositionTiming,
                                    dup == 1 & GradYear %in% c(2012) ~ "5 years out",
                                    dup == 1 & GradYear %in% c(2007) ~ "10 years out"))

# for AAUDE, choose only 5 years out
# remove the ms/phd's that were added in as duplicates, filter down to only those with 5 years out
# remove secondary positions (primary == 0)


dfCareer_deDup <- dfCareer3 %>%
  filter(Primary.Position %in% c(1, NA)) %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(keep = case_when(dup == 1 ~ 1,
                          dup == 2 & Degree.Desc %in% c("MS/PHD") ~ 0,
                          dup == 2 & Degree.Desc %in% c("PHD") ~ 1,
                          dup == 2 & EndYr %in% c("Current") ~ 1)) %>%
  filter(keep == 1)


dfmissing <- dfCareer3 %>%
  filter(!UID %in% dfCareer_deDup$UID)

# after reviewing, all records had end dates prior to 2017, need to create "empty" rows indicating they were not found
dfForAdd <- dfmissing %>%
  mutate(Available = "N")

# repeat the above operations, adding in the formerly missing records

dfCareer_plot <- dfCareer %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(PositionTiming = case_when(!PositionAY %in%  c(-99) ~ PositionTiming,
                                    dup == 1 & GradYear %in% c(2012) ~ "5 years out",
                                    dup == 1 & GradYear %in% c(2007) ~ "10 years out")) %>%
  bind_rows(dfForAdd) %>%
  filter(Primary.Position %in% c(1, NA)) %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(keep = case_when(dup == 1 ~ 1,
                          dup == 2 & Degree.Desc %in% c("MS/PHD") ~ 0,
                          dup == 2 & Degree.Desc %in% c("PHD") ~ 1,
                          dup == 2 & EndYr %in% c("Current") ~ 1)) %>%
  filter(keep == 1) %>%
  filter(!duplicated(UID))%>%
  group_by(GradYear, Available, OutcomeHigh, EmployerType, JobFunction, AcStage, AcadJobTitle) %>%
  summarise(count = n()) %>%
  ungroup()



dfOutcomeHigh <- read_excel(career_excel, sheet = "OutcomeHigh")
dfEmployerType <- read_excel(career_excel, sheet = "EmployerType")
dfAcStage <-  read_excel(career_excel, sheet = "AcStage")
dfAcadJobTitle <- read_excel(career_excel, sheet = "AcadJobTitle")
dfNAICS <- read_excel(career_excel, sheet = "NAICS") %>%
  mutate(NAICS = as.character(NAICS))
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



dfCareerShared <- dfCareer %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(PositionTiming = case_when(!PositionAY %in%  c(-99) ~ PositionTiming,
                                    dup == 1 & GradYear %in% c(2012) ~ "5 years out",
                                    dup == 1 & GradYear %in% c(2007) ~ "10 years out")) %>%
  bind_rows(dfForAdd) %>%
  filter(Primary.Position %in% c(1, NA)) %>%
  group_by(UID) %>%
  mutate(dup = n()) %>%
  #  mutate(newest = row_number(PositionAY)) %>%
  ungroup() %>%
  mutate(keep = case_when(dup == 1 ~ 1,
                          dup == 2 & Degree.Desc %in% c("MS/PHD") ~ 0,
                          dup == 2 & Degree.Desc %in% c("PHD") ~ 1,
                          dup == 2 & EndYr %in% c("Current") ~ 1)) %>%
  filter(keep == 1) %>%
  filter(!duplicated(UID))


dfCareerShared_plot <- dfCareerShared %>%
  ungroup() %>%
  left_join(., dfOutcomeHigh) %>%
  select(-OutcomeHigh, -Description) %>%
  rename(OutcomeHigh = DescShort) %>%
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
  rename(Industry_detail = Description,
         Industry_det_short = DescriptionShort) %>%
  mutate(NAICS2 = str_sub(NAICS, 1,2)) %>%
  left_join(dfNAICS2) %>%
  rename(Industry_short = DescriptionShort,
         Industry = Description) %>%
  select(PositionTiming, HEGIS, OutcomeHigh, AcStage, AcCIP, EmployerType, EmployerShort, EmployerHigh, NAICS, Industry, Industry_short,
         Industry_detail, Industry_det_short, InstOrgName, DiscpKnowledge, JobFunction,JobTitle,
         AcadJobTitle) %>%
  mutate(count = 1)


dfCareerShared_plot
}
