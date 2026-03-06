# Source file: regdata.R
#
# GPL-3 License
#
# Copyright (C) 2019-2026 Victor Ordu.

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




#' Road Distances Between Nigerian State Capitals
#'
#' A symmetric distance matrix containing approximate road (driving)
#' distances in kilometres between all 37 Nigerian State capitals
#' (including Abuja for the Federal Capital Territory).
#'
#' @format An object of class \code{dist} with 37 labels — one for each
#'   State capital. The labels are city names in title case (e.g.
#'   \code{"Lagos"}, \code{"Port Harcourt"}).
#'
#' @details
#' The matrix holds \eqn{37 \times 36 / 2 = 666}{37 * 36 / 2 = 666}
#' unique pairwise distances. Values are sourced from UNDP and represent
#' typical road travel; actual distances may vary with route choice,
#' road conditions, or detours.
#'
#' For a convenient way to look up individual distances, see
#' \code{\link{ng_distance}}.
#'
#' @examples
#' data(ngdist)
#' labels(ngdist)          # city names
#' as.matrix(ngdist)[1:5, 1:5]
#'
#' @seealso \code{\link{ng_distance}} for querying a single pair of cities.
"ngdist"
