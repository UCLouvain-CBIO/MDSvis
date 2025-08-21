# MDSvis - Copyright (C) <2025>
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

library(CytoMDS)

# load objects from text fixtures
Krieg_mdsObj <- readRDS(test_path("fixtures", "Krieg_mdsObj.rds"))
Krieg_phenoData <- readRDS(test_path("fixtures", "Krieg_phenoData.rds"))
Krieg_chStats <- readRDS(test_path("fixtures", "Krieg_chStats.rds"))

test_that("checkDimCompatibility works", {
    
    # start with initial objects
    nSamples = nrow(Krieg_phenoData)
    
    ret <- checkDimCompatibility(
        mdsObj = Krieg_mdsObj, 
        pData = Krieg_phenoData, 
        stats = Krieg_chStats
    )
    
    expect_equal(length(ret$dims), 3)
    expect_equal(names(ret$dims), c("MDS", "phenodata", "stats"))
    expect_equal(unname(ret$dims[1]), nSamples)
    expect_equal(unname(ret$dims[2]), nSamples)
    expect_equal(unname(ret$dims[3]), nSamples)
    expect_type(ret$dimCompatibility, "list")
    expect_equal(length(ret$dimCompatibility), 2)
    expect_equal(
        names(ret$dimCompatibility), 
        c("areMDSPdataCompatible", "areMDSStatsCompatible"))
    expect_equal(ret$dimCompatibility[[1]], TRUE)
    expect_equal(ret$dimCompatibility[[2]], TRUE)
    
    # now perturb pData
    pDataWrong <- Krieg_phenoData[-1,]
    ret2 <- checkDimCompatibility(
        mdsObj = Krieg_mdsObj, 
        pData = pDataWrong, 
        stats = Krieg_chStats
    )    
    expect_equal(length(ret2$dims), 3)
    expect_equal(names(ret2$dims), c("MDS", "phenodata", "stats"))
    expect_equal(unname(ret2$dims[1]), nSamples)
    expect_equal(unname(ret2$dims[2]), nSamples-1)
    expect_equal(unname(ret2$dims[3]), nSamples)
    expect_type(ret2$dimCompatibility, "list")
    expect_equal(length(ret2$dimCompatibility), 2)
    expect_equal(
        names(ret2$dimCompatibility), 
        c("areMDSPdataCompatible", "areMDSStatsCompatible"))
    expect_equal(ret2$dimCompatibility[[1]], FALSE)
    expect_equal(ret2$dimCompatibility[[2]], TRUE)
    
    # now perturb stats
    statsWrong <- lapply(
        Krieg_chStats,
        FUN = function(x) x[-1,]
    )
    
    ret3 <- checkDimCompatibility(
        mdsObj = Krieg_mdsObj, 
        pData = Krieg_phenoData, 
        stats = statsWrong
    )
    
    expect_equal(length(ret3$dims), 3)
    expect_equal(names(ret3$dims), c("MDS", "phenodata", "stats"))
    expect_equal(unname(ret3$dims[1]), nSamples)
    expect_equal(unname(ret3$dims[2]), nSamples)
    expect_equal(unname(ret3$dims[3]), nSamples-1)
    expect_type(ret3$dimCompatibility, "list")
    expect_equal(length(ret3$dimCompatibility), 2)
    expect_equal(
        names(ret3$dimCompatibility), 
        c("areMDSPdataCompatible", "areMDSStatsCompatible"))
    expect_equal(ret3$dimCompatibility[[1]], TRUE)
    expect_equal(ret3$dimCompatibility[[2]], FALSE)
})

test_that("checkDimCompatibility with edge cases works", {
    dummyMDS <- 3
    dummyPData <- NULL
    dummyStats <- NULL
    
    ret <- checkDimCompatibility(
        mdsObj = dummyMDS, 
        pData = dummyPData, 
        stats = dummyStats
    )
    
    expect_equal(length(ret$dims), 3)
    expect_equal(names(ret$dims), c("MDS", "phenodata", "stats"))
    expect_equal(unname(ret$dims[1]), NA)
    expect_equal(unname(ret$dims[2]), NA)
    expect_equal(unname(ret$dims[3]), NA)
    expect_type(ret$dimCompatibility, "list")
    expect_equal(length(ret$dimCompatibility), 2)
    expect_equal(
        names(ret$dimCompatibility), 
        c("areMDSPdataCompatible", "areMDSStatsCompatible"))
    expect_equal(ret$dimCompatibility[[1]], TRUE)
    expect_equal(ret$dimCompatibility[[2]], TRUE)
})
