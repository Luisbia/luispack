% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/report_outliers.R
\name{report_outliers}
\alias{report_outliers}
\title{Identify outliers based on the Z-score of annual change}
\usage{
report_outliers(data, z = 3, sto_sel, time_min)
}
\arguments{
\item{data}{a data frame with the original data.}

\item{z}{the factor to use, normally 2.5 or 3.}

\item{sto_sel}{possibly filter some transactions.}

\item{time_min}{possibly filter unrevised data.}
}
\value{
a data frame with the identified outliers
}
\description{
Identify outliers based on the Z-score of annual change
}
\examples{
df <- load_csv(
folder = "D:/data/REGACC/csv",
country_sel = "AT",
time_min = "2021-12-01")

report_outliers(df)

}
