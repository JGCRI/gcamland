# sector.R

#' Sector_initCalc
#'
#' @details Initializations related to the sector
#'          Note: we are storing technology parameters in the Leafs for
#'          convenience, but are leaving the technology/leaf calculations
#'          separate as they are in the C++ code.
#' @param aLandAllocator Land allocator to perform initializations on
#' @param aPeriod Current model time period
#' @author KVC September 2017
Sector_initCalc <- function(aLandAllocator, aPeriod, aScenarioInfo){
  # Loop through the land allocator
  for(child in aLandAllocator$mChildren) {
    Subsector_initCalc(child, aPeriod, aScenarioInfo)
  }
}

#' Subsector_initCalc
#'
#' @details Initializations related to the subsector.
#'          Note: we are using this to create a flexible nesting.
#'          This essentially serves as the "Node" level.
#'          This is a departure from how the C++ code works.
#' @param aLandNode Land node to perform initializations on
#' @param aPeriod Current model time period
#' @author KVC September 2017
Subsector_initCalc <- function(aLandNode, aPeriod, aScenarioInfo){
  # loop through children
  for(child in aLandNode$mChildren) {
    if(class(child) == "LandNode") {
      Subsector_initCalc(child, aPeriod, aScenarioInfo)
    } else if (class(child) == "LandLeaf") {
      # If this is a leaf, call the AgProductionTechnology method
      AgProductionTechnology_initCalc(child, aPeriod, aScenarioInfo)
    }
  }
}

