# Source file: regdata.R
#
# GPL-3 License
#
# Copyright (C) 2019-2024 Victor Ordu.

#' Local Government Areas of Nigeria
#'
#' A dataset containing the 774 Local Government Areas of Nigeria.
#'
#' @format A data frame with 774 rows and 2 columns
#' \describe{
#'    \item{lga}{Local Government Area}
#'    \item{state}{State of the Federation}
#'    \item{gpz}{Geo-political zone}
#'    }
"lgas_nigeria"




#' States of Nigeria
#' 
#' A dataset of the 36 States and Federal Capital Territory of Nigeria.
#' 
#' @format A \code{data.frame} with 37 rows and 2 columns
#' \describe{
#'   \item{isocode}{ISO 3661 Alpha 2 code}
#'   \item{state}{Name of the State or Territory}
#'   \item{gpz}{Geo-political zone}
#' }
"states_nigeria"




#' Distance By Road of Nigerian State Capitals
#' 
#' A distance matrix with the road distance in kilometers for Nigeria's
#' State capitals.
#'
#' @format An object of class \code{dist}.
"ngdist"
