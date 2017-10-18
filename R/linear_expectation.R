# linear_expectation.R

#' LinearExpectation_calcExpectedYield
#'
#' @param aLandLeaf LandLeaf to calculate expected yield for
#' @param aPeriod Current model period
#' @details Calculate the expected yield for a LandLeaf using
#'          a linear extrapolation from recent history.
#' @author KVC October 2017
LinearExpectation_calcExpectedYield <- function(aLandLeaf, aPeriod) {
#   int currYear = scenario->getModeltime()->getper_to_yr( aPeriod );
#   int startYear = currYear - mNumYears;
#   std::vector<double> yields;
#   yields.resize( mNumYears );
#
#   std::vector<double> years;
#   years.resize( mNumYears );
#
#   int period = 0;
#   // Loop over all years and get the price
#   for ( int i = 0; i < mNumYears; i++ ) {
#     years[ i ] = startYear + i;
#     period = scenario->getModeltime()->getyr_to_per( years[ i ] );
#     int year = scenario->getModeltime()->getper_to_yr( period );
#
#     // KVC_AGLU: Send information about type of expectation from region to AgProductionTechnology
#     vector<FilterStep*> expectationFilterSteps = parseFilterString( string("period[YearFilter,IntEquals,")+util::toString(year)+string("]/yield") );
#     GCAMFusion<LinearExpectation> sendExpectationInfo( *this, expectationFilterSteps );
#     sendExpectationInfo.startFilter( mTechContainer );
#
#     yields[ i ] = mCurrYield;
#   }
#
#   std::pair<double, double> params;
#   params = util::linearRegression( years, yields );
#   double expectedYield = params.first + params.second * currYear;
#   expectedYield = max( 0.0, expectedYield ); // Do not allow negative values
#
#   return expectedYield;
}

#' LinearExpectation_calcExpectedPrice
#'
#' @param aLandLeaf LandLeaf to calculate expected price for
#' @param aPeriod Current model period
#' @details Calculate the expected price for a LandLeaf using
#'          a linear extrapolation from recent history.
#' @author KVC October 2017
LinearExpectation_calcExpectedPrice <- function(aLandLeaf, aPeriod){
#   int currYear = scenario->getModeltime()->getper_to_yr( aPeriod );
#   int startYear = currYear - mNumYears;
#   std::vector<double> prices;
#   prices.resize( mNumYears );
#
#   std::vector<double> years;
#   years.resize( mNumYears );
#
#
#   Marketplace* marketplace = scenario->getMarketplace();
#   int period = 0;
#   // Loop over all years and get the price
#   for ( int i = 0; i < mNumYears; i++ ) {
#     years[ i ] = startYear + i;
#
#     if ( years[ i ] > scenario->getModeltime()->getStartYear() ) {
#       period = scenario->getModeltime()->getyr_to_per( years[ i ] );
#       prices[ i ] = marketplace->getPrice( aProductName, aRegionName, period );
#       //cout << aRegionName << " " << aProductName << " " <<  aPeriod << ": year = " << i << ", price = " << prices[ i ] << endl;
#     }
#     else {
#       // KVC: Not sure what to do here. For now, just using the first year's price
#       prices[ i ] = marketplace->getPrice( aProductName, aRegionName, 0 );
#     }
#   }
#
#       std::pair<double, double> params;
#       params = util::linearRegression( years, prices );
#       double expectedPrice = params.first + params.second * currYear;
#       expectedPrice = max( 0.0, expectedPrice ); // Do not allow negative values
#       //cout << aRegionName << " " << aProductName << " " <<  aPeriod << ": " << expectedPrice << endl;
#
#       return expectedPrice;
}
