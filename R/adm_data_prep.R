#' Admissions Data Preperation
#'
#' \code{adm_data_prep} takes an export from the ADW report A2.2
#' with each row representing a single year, semester, snapshot date,
#' and hegis; Variables are Applications, Admitted, Accepted Offers,
#' Declined Offers, Rejected, Registered, Matriculants.
#' There is no degree level or degree program;
#' Data should be filtered to PhD program apps prior to export
#'
#' @param dfAdm a dataframe imported from ADW report A2.2
#' @param Col character string college abbreviation
#' @param HEGIS hegis number; provided as a number or character string
#'  if there are fewer than 4 digits, a leading zero will be added
#' @param appVar name of column containing the count of applications. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Applications}
#' @param admitVar name of column containing the count of extedended offers. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Admitted}
#' @param AcAcVar name of column containing the count of mutual accepts (e.g. students
#' who accepted the offer of admission)  Provided either bare (unquoted) or in open quotes.
#' Default value is \code{Accepted.Offers}
#' @param matVar name of column containg the count of matriculated students. Provided
#' either bare (unquoted) or in open quotes. Default value is \code{Matriculated}
#' @param semYearVar name of column containing the  Semester and Year of admission.
#' Provided either bare (unquoted) or in open quotes. Default value is \code{Applied.To.Year.Semester}
#' @param colVar name of column containing the 3 character college abbreviation. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Graduate.Application.College}
#' @param HEGISVar name of column containing the HEGIS code. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{HEGIS.Code.1}
#' @param HEGISdescVar name of column containing the HEGIS description. Provided either
#' bare (unquoted) or in open quotes. Default value is \code{Graduate.Application.HEGIS}
#'
#'
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr select mutate lag
#' @import rlang
#'
#'
#' @return A dataframe filtered to the college and hegis supplied with four additional columns
#' \code{Selectivity} is the number of admitted students divided by the total number of applicants;
#' \code{AppYear} is the calendar year of the semester of application;
#' \code{dApps} is the difference between current year and previous year apps;
#' \code{pApps} is the percentage change from the previous year to to the current year in applications
#' \code{dEnroll} is the difference between the current year and the prvious year matriculants
#' \code{pEnroll} is the percentage change from the previous year to the current year in matriculants
#'
#'

#'@export
#'
adm_data_prep <- function(  dfAdm,
                            Col,
                            HEGIS,
                            appVar = Applications,
                            admitVar = `Admitted`,
                            AcAcVar = `Accepted.Offers`,
                            matVar = Matriculants,
                            semYearVar = `Applied.To.Year.Semester`,
                            colVar = `Graduate.Application.College`,
                            HEGISVar = `HEGIS.Code.1`,
                            HEGISdescVar = `Graduate.Application.HEGIS` ){

  # create quosures for all variables
  q.colVar <- enquo( colVar )
  q.Hegis <- enquo( HEGISVar )
  q.semYearVar <- enquo( semYearVar )
  q.appVar <- enquo( appVar )
  q.admitVar <- enquo( admitVar )
  q.AcAcVar <- enquo( AcAcVar )
  q.matVar <- enquo( matVar )

  # create character strings for filters

  c.hegis <-purrr::map(HEGIS, function(iHEGIS){
    if( nchar( iHEGIS) == 3 ){
       paste0( "0", iHEGIS)
    }else{
       as.character( iHEGIS )
    }})



  # filter the data frame
  # extract the applicaiton year,
  # calculate the selectivity


  dfOut <- dfAdm %>%
    filter( (!! q.colVar ) %in% Col,
            (!! q.Hegis ) %in% c.hegis) %>%
    mutate( AppYear = as.numeric( str_sub( (!!  q.semYearVar ), -4 )),
            Selectivity = (!!  q.admitVar )/(!!  q.appVar)) %>%
    mutate(dApps = (!! q.appVar) - lag((!! q.appVar), order_by = AppYear),
           dEnroll = (!! q.matVar) - lag((!! q.matVar), order_by = AppYear)) %>%
    mutate(pApps = dApps/lag((!! q.appVar), order_by = AppYear),
           pEnroll = dEnroll/lag((!! q.matVar), order_by = AppYear))


  dfOut
}
