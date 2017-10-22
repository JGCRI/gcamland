# sector.R

#' Sector_initCalc
#'
#' @details Initializations related to the technology.
#'          Note: we are storing technology parameters in the Leafs for
#'          convenience, but are leaving the technology/leaf calculations
#'          separate as they are in the C++ code.
#' @param aLandAllocator Land allocator to perform initializations on
#' @param aPeriod Current model time period
#' @author KVC September 2017
Sector_initCalc <- function( aLandAllocator, aPeriod ){
  # do any technology initializations
  # Note: C++ code calls subsector initialization, but I don't think we need that
  # TODO: make this more flexible to nesting structure
  for( child in aLandAllocator$mChildren ) {
    for ( leaf in child$mChildren ) {
      if (class(leaf) == "LandLeaf") {
        AgProductionTechnology_initCalc(leaf, aPeriod)
      }
    }
  }
}
