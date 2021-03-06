% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotTourn.R
\name{bracketLines}
\alias{bracketLines}
\title{Determines X, Y Coordinates for MM Bracket}
\usage{
bracketLines(games, playin)
}
\arguments{
\item{games:}{number of games to build a bracket for (32 is the default)}

\item{playin:}{not available yet}
}
\value{
a data.frame of the beginning and end x and y coordinates to draw bracket
}
\description{
Returns the beginning and end x,y coordinates for the line segments needed to
build a two sided bracket. This function will likely only be used internally.
}
