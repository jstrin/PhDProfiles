#' Enrollment Data Preperation
#'
#' \code{enr_data_prep} takes an export from the ADW report E4.1
#' formatted with each row representing a unique combination of
#' year semester, Enrollment Hegis, College,
#' Domestic/Internatioanl Status, IPeds Race, Gender (Sex),
#' Class year, and Degree Continuation. Variable is count of
#' students. There is no degree level or degree program;
#' report should be filtered to only PhD enrollments prior to
#' export
#'
#' @param dfEnroll a dataframe imported from ADW report A2.2
#' @param Col character string college abbreviation
#' @param HEGIS hegis number; provided as a number or character string
#'  if there are fewer than 4 digits, a leading zero will be added
#' @param hegis_display character string with how the program should be displayed in
#' the final profile
#' @param enrollVar name of column containing the count of students Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Students}
#' @param domintVar name of column indicating doemstic or international status.
#' Provided either bare (unquoted) or in open quotes. Default value is
#' \code{Student.Domestic.International.Status}
#' @param IPEDSVar name of column indicating IPEDS Race Ethnicity classification
#' Provided either bare (unquoted) or in open quotes. Default value is
#' \code{Student.IPEDS.Race.Ethnicity}
#' @param sexVar name of column indicating student sex. Provided either bare
#' (unquoted) or in open quotes. Default value is \code{Student.Gender}
#' @param clyearVar name of column indicating year in program. Provided either
#' bare (unquotred) or in open quotes. Default value is \code{Class.Year.Category}
#' @param contVar name of column indicating continuation status. Provided either
#' bare (unquoted) or in open quotes.Default value is \code{Degree.Continuation.Category}
#' @param semYearVar name of column containing the  Semester and Year of admission.
#' Provided either bare (unquoted) or in open quotes. Default value is \code{Academic.Year...Semester}
#' @param colVar name of column containing the 3 character college abbreviation. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Enrollment.College}
#' @param HEGISVar name of column containing the HEGIS code. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{HEGIS.Code.1}
#' @param HEGISdescVar name of column containing the HEGIS description. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Primary.Enrollment.HEGIS}
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select mutate lag bind_rows case_when left_join
#' @importFrom tidyr spread
#' @import rlang
#'
#' @return A dataframe filtered to the college and hegis supplied with four additional columns
#' \code{Selectivity} is the number of admitted students divided by the total number of applicants;
#' \code{AppYear} is the calendar year of the semester of application;
#' \code{dApps} is the difference between current year and previous year apps;
#' \code{pApps} is the percentage change from the previous year to to the current year in applications
#'
#'
#'
#'@export
enr_data_prep <- function(  dfEnroll,
                             Col,
                             HEGIS,
                             hegis_display,
                             enrollVar = Students,
                             domintVar = `Student.Domestic.International.Status`,
                             IPEDSVar = `Student.IPEDS.Race.Ethnicity`,
                             sexVar = `Student.Gender`,
                             clyearVar = `Class.Year.Category`,
                             contVar = `Degree.Continuation.Category`,
                             semYearVar = `Academic.Year...Semester`,
                             colVar = `Enrollment.College`,
                             HEGISVar = `HEGIS.Code.1`,
                             HEGISdescVar = `Primary.Enrollment.HEGIS` ){

  # create quosures for all variables
  q.enrollVar <- enquo( enrollVar )
  q.domintVar <- enquo(domintVar)
  q.IPEDSVar <- enquo(IPEDSVar)
  q.sexVar <- enquo(sexVar)
  q.clyearVar <- enquo(clyearVar)
  q.contVar <- enquo(contVar)
  q.semYearVar <- enquo(semYearVar)
  q.colVar <- enquo(colVar)
  q.HEGISVar <- enquo(HEGISVar)
  q.HEGISdescVar <- enquo(HEGISdescVar)

  # create character strings for filters

  c.hegis <- purrr::map(HEGIS, function(iHEGIS){
    if( nchar( iHEGIS) == 3 ){
      paste0( "0", iHEGIS)
    }else{
      as.character( iHEGIS )
    }})


  # create AY variable:
  dfEnroll <- dfEnroll %>%
    mutate( AcadYear = as.numeric( str_sub( (!!  q.semYearVar ), -4 )) + 1)



  # for the program and for BU as a whole,
  # filter the data frame


  dfSex <- dfEnroll %>%
    mutate(progOut = case_when(( !! q.HEGISVar) %in% c.hegis ~ hegis_display,
                                TRUE ~ "BU PhD Students")) %>%
    group_by(progOut, ( !! q.sexVar), AcadYear) %>%
    summarise( nSex = sum(( !!q.enrollVar), na.rm = T)) %>%
    ungroup() %>%
    group_by(progOut, AcadYear) %>%
    mutate(pSex = nSex/sum(nSex, na.rm = T)) %>%
    ungroup() %>%
    mutate( progOut = factor(progOut, ordered = T, levels = c("BU PhD Students", hegis_display)))

  dfRace <- dfEnroll %>%
    mutate(progOut = case_when(( !! q.HEGISVar) %in% c.hegis ~ hegis_display,
                               TRUE ~ "BU PhD Students")) %>%
    group_by(progOut, ( !! q.IPEDSVar), AcadYear ) %>%
    summarise( nRace = sum(( !! q.enrollVar), na.rm = T )) %>%
    ungroup() %>%
    group_by(progOut, AcadYear) %>%
    mutate( pRace = nRace/sum(nRace, na.rm = T) ) %>%
    ungroup() %>%
    mutate( progOut = factor(progOut, ordered = T, levels = c("BU PhD Students", hegis_display)))


  l.out <- list(Sex = dfSex, Race = dfRace)
}
