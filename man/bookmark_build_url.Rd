% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bookmark_module.R
\name{bookmark_build_url}
\alias{bookmark_build_url}
\title{Build a bookmarkable URL}
\usage{
bookmark_build_url(id, lang = NULL, widgets, map_viewstate, select_id)
}
\arguments{
\item{id}{<\code{character}> A string or numeric value representing the ID of the
selected page.}

\item{lang}{<\verb{¸character}> An optional string value representing the selected
language. Default is NULL.}

\item{widgets}{A named list representing the values of the Curbcut widgets present
on the page.}

\item{map_viewstate}{<\verb{named list}> An optional named list representing the
state of the map view. Usually \code{cc.map::get_map_viewstate("map")}.}

\item{select_id}{<\code{character}> An optional string or numeric value representing
the ID of the selected item.}
}
\value{
A character string representing the bookmarkable URL.
}
\description{
This function builds a URL that can be used to bookmark the state of any
module of Curbcut. The URL includes information about the selected region, ID,
language, widgets, map viewstate, elected ID, ... If the page is in the
module's table, it makes the value a numeric. If the region is in the regions
dictionary, it makes the region a numeric as well. It also makes the value
of pickers numeric if they are present in the \code{variables} table, all to shorten
the length of the URL.
}
