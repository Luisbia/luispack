% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\docType{data}
\name{labels}
\alias{labels}
\title{Labels for National Accounts}
\format{
A data.frame
\describe{
\item{dictionary}{na_item, nace_r2, geo, asset10, coicop}
\item{code_name}{codes}
\item{labels}{labels of the codes}
}
}
\source{
\url{https://ec.europa.eu/eurostat}

labels<-    tibble(
dictionary = c("na_item", "nace_r2", "geo", "asset10", "coicop"),
data = list(
eurostat::get_eurostat_dic("na_item"),
eurostat::get_eurostat_dic("nace_r2"),
eurostat::get_eurostat_dic("geo"),
eurostat::get_eurostat_dic("asset10"),
eurostat::get_eurostat_dic("coicop")
)
) \%>\%
tidyr::unnest(cols=c(data)) \%>\%
rename(label=full_name)
}
\usage{
labels
}
\description{
A data frame with most common dictionaries of labels for National Accounts
}
\keyword{datasets}
