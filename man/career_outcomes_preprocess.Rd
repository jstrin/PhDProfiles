% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/career_outcomes_preprocess.R
\name{career_outcomes_preprocess}
\alias{career_outcomes_preprocess}
\title{Career Outcomes Preprocess}
\usage{
career_outcomes_preprocess(career_excel)
}
\arguments{
\item{career_excel}{name and path of the excel workbook in which the career outcomes and
data dictionary are stored}
}
\value{
a data frame with one row per graduate with indicators for
}
\description{
\code{career_outcomes_preprocess} reduces the file down to one record per graduate
by selecting the most recent primary position
}
