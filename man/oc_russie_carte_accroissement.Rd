% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/map_oc_russie.R
\name{oc_russie_carte_accroissement}
\alias{oc_russie_carte_accroissement}
\alias{oc_russie_carte_hc_accroissement}
\title{Accroissement naturel en Russie}
\usage{
oc_russie_carte_accroissement(
  theme = ggplot2::theme_minimal(),
  barwidth = 40,
  greyscale = FALSE
)

oc_russie_carte_hc_accroissement()
}
\arguments{
\item{theme}{A ggplot2 theme}

\item{barwidth}{Width of legend bar}

\item{greyscale}{Boolean: whether to create a grey scale map}
}
\value{
A map
}
\description{
Une carte de l'accroissement naturel en 2023
au niveau régional en Russie.
}
\examples{
oc_russie_carte_accroissement()
oc_russie_carte_hc_accroissement()
}
\concept{oc russie mortalité}
