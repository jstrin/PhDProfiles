% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/enr_data_prep.R
\name{enr_data_prep}
\alias{enr_data_prep}
\title{Enrollment Data Preperation}
\usage{
enr_data_prep(dfEnroll, Col, HEGIS, hegis_display, enrollVar = Students,
  domintVar = Student.Domestic.International.Status,
  IPEDSVar = Student.IPEDS.Race.Ethnicity, sexVar = Student.Gender,
  clyearVar = Class.Year.Category,
  contVar = Degree.Continuation.Category,
  semYearVar = Academic.Year...Semester, colVar = Enrollment.College,
  HEGISVar = HEGIS.Code.1, HEGISdescVar = Primary.Enrollment.HEGIS)
}
\arguments{
\item{dfEnroll}{a dataframe imported from ADW report A2.2}

\item{Col}{character string college abbreviation}

\item{HEGIS}{hegis number; provided as a number or character string
if there are fewer than 4 digits, a leading zero will be added}

\item{hegis_display}{character string with how the program should be displayed in
the final profile}

\item{enrollVar}{name of column containing the count of students Provided either
bare (unquoted) or in open quotes. Default value is \code{Students}}

\item{domintVar}{name of column indicating doemstic or international status.
Provided either bare (unquoted) or in open quotes. Default value is
\code{Student.Domestic.International.Status}}

\item{IPEDSVar}{name of column indicating IPEDS Race Ethnicity classification
Provided either bare (unquoted) or in open quotes. Default value is
\code{Student.IPEDS.Race.Ethnicity}}

\item{sexVar}{name of column indicating student sex. Provided either bare
(unquoted) or in open quotes. Default value is \code{Student.Gender}}

\item{clyearVar}{name of column indicating year in program. Provided either
bare (unquotred) or in open quotes. Default value is \code{Class.Year.Category}}

\item{contVar}{name of column indicating continuation status. Provided either
bare (unquoted) or in open quotes.Default value is \code{Degree.Continuation.Category}}

\item{semYearVar}{name of column containing the  Semester and Year of admission.
Provided either bare (unquoted) or in open quotes. Default value is \code{Academic.Year...Semester}}

\item{colVar}{name of column containing the 3 character college abbreviation. Provided either
bare (unquoted) or in open quotes. Default value is \code{Enrollment.College}}

\item{HEGISVar}{name of column containing the HEGIS code. Provided either
bare (unquoted) or in open quotes. Default value is \code{HEGIS.Code.1}}

\item{HEGISdescVar}{name of column containing the HEGIS description. Provided either
bare (unquoted) or in open quotes. Default value is \code{Primary.Enrollment.HEGIS}}
}
\value{
A dataframe filtered to the college and hegis supplied with four additional columns
\code{Selectivity} is the number of admitted students divided by the total number of applicants;
\code{AppYear} is the calendar year of the semester of application;
\code{dApps} is the difference between current year and previous year apps;
\code{pApps} is the percentage change from the previous year to to the current year in applications
}
\description{
\code{enr_data_prep} takes an export from the ADW report E4.1
formatted with each row representing a unique combination of
year semester, Enrollment Hegis, College,
Domestic/Internatioanl Status, IPeds Race, Gender (Sex),
Class year, and Degree Continuation. Variable is count of
students. There is no degree level or degree program;
report should be filtered to only PhD enrollments prior to
export
}
