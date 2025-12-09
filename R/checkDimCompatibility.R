# MDSvis - Copyright (C) <2025-2026>
# <Université catholique de Louvain (UCLouvain), Belgique>
#
#   Description and complete License: see LICENSE file.
#
# This program (MDSvis) is free software:
#   you can redistribute it and/or modify it under the terms of the GNU General
# Public License as published by the Free Software Foundation,
# either version 3 of the License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details (<http://www.gnu.org/licenses/>).
#

# checks compatibility of dimensions of the three input objects
#' @importFrom methods is
checkDimCompatibility <- function(
        mdsObj, pData, stats) {
    dims <- rep(NA, 3)
    names(dims) <- c("MDS", "phenodata", "stats")
    
    if (is(mdsObj, "MDS")) {
        dims[1] <- CytoMDS::nPoints(mdsObj)
    }
    
    if (!is.null(pData)) {
        dims[2] <- nrow(pData)
    }
    
    if (!is.null(stats)) {
        dims[3] <- nrow(stats[[1]])
    }
    
    dimCompatibility <- list(
            areMDSPdataCompatible = TRUE,
            areMDSStatsCompatible = TRUE)
    
    if (!is.na(dims[1])) {
        if (!is.na(dims[2]) && dims[1] != dims[2]) {
            dimCompatibility$areMDSPdataCompatible <- FALSE
        }
            
        if (!is.na(dims[3]) && dims[1] != dims[3]) {
            dimCompatibility$areMDSStatsCompatible <- FALSE
        }
            
    }
    dimCheck <- list(
        dims = dims,
        dimCompatibility = dimCompatibility
    )
    dimCheck
}
    
    
    