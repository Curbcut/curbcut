% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_data.R
\name{load_data}
\alias{load_data}
\title{Initialize Necessary Data in the Global Environment}
\usage{
load_data(
  data_folder = "data",
  pos = 1,
  site_name,
  site_url,
  stories_page,
  inst_prefix,
  mapbox_username,
  default_random_address,
  map_zoom,
  map_loc
)
}
\arguments{
\item{data_folder}{<\code{character}> Specifies the folder containing the data
files and databases to be loaded. Default is "data".}

\item{pos}{<\code{numeric}> The environment position to which data and variables
will be assigned. Default is 1, referring to the global environment.}

\item{site_name}{<\code{character}> Name of the site. Example: "Curbcut Montréal"}

\item{site_url}{<\code{character}> URL of the site. Example: "https://montreal.curbcut.ca"}

\item{stories_page}{<\code{character}> Name of the stories page. Example: "Montréal stories"}

\item{inst_prefix}{<\code{character}> Prefix for the instance, used for both database
schema and tileset prefixes. Example: "mtl"}

\item{mapbox_username}{<\code{character}> Mapbox username. Example: "curbcut"}

\item{default_random_address}{<\code{character}> Default address placeholder for
location lock. Example: "845 Sherbrooke Ouest, Montréal, Quebec"}

\item{map_zoom}{<\code{numeric}> Default map zoom level. Example: 9.9}

\item{map_loc}{<\code{list}> List containing latitude and longitude for map.
Example: c(lat = -73.70, lon = 45.53)}
}
\value{
NULL, but objects are loaded into the specified environment and
SQLite connections are established.
}
\description{
This function reads \code{.qs} and \code{.qsm} files located in the root of the data
folder specified by the \code{data_folder} argument. Additionally, it establishes
connections to postgres database. Finally, it sets default Mapbox
configurations and assigns other necessary variables to the global environment.
}
