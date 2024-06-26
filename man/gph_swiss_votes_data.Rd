% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geo_map_swiss.R
\name{gph_swiss_votes_data}
\alias{gph_swiss_votes_data}
\title{Retrieve swiss vote data}
\usage{
gph_swiss_votes_data(
  geolevel = c("canton", "district", "municipality"),
  votedates,
  filter_id,
  language = "FR"
)
}
\arguments{
\item{geolevel}{One of "canton", "district" or "municipality"}

\item{votedates}{The date of the vote}

\item{language}{One of "DE", "FR", "IT" or "RM"}
}
\value{
A ggplot2 map
}
\description{
Retrieve swiss vote data
}
\examples{
gph_map_swiss_votes("canton", votedates = "2024-03-03", id = 6650)
}
\keyword{internal}
