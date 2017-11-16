# Test calculations related to the logit

context("logit")

# Note: these tests ensure all calculations replicate a simple nest, with 2 leaves and 1 node
test_that("share weight calculation works", {
  # First, define expected values
  expectedShwt <- data.frame(name = c("Type1", "Type2"),
                             shwt = c(0.396850263, 0.264566842))

  # Next, calculate the values using gcamland
  # Create a choice function
  myFunction <- ChoiceFunction("relative-cost", 0.75)
  myFunction$mOutputCost <- 1

  # Calculate share weights for the two leafs
  calculatedShwt <- expectedShwt
  calculatedShwt$shwt[calculatedShwt$name == "Type1"] <- RelativeCostLogit_calcShareWeight(myFunction, 0.5, 1, 1)
  calculatedShwt$shwt[calculatedShwt$name == "Type2"] <- RelativeCostLogit_calcShareWeight(myFunction, 0.5, 1.5, 1)

  expect_equivalent(calculatedShwt, expectedShwt,
                      info = paste("RelativeCostLogit_calcShareWeight not producing expected values"))

})
