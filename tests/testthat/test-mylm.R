
#data load
data_characters <- iris_Rbeta
data_characters$character_species <- as.character(data_characters$Species)
data_characters$character_speciesb <- as.character(data_characters$Species)


#logrelative error
LRE <- function(x,c){
  if(c==0){return(round(-log(abs(x), base = 10)))}
  else{return(round(-log((abs(x-c)/c) , base = 10)))}
}



#Input Checks

test_that("subsetting is possible", {
  expect_error(mylm(Sepal.Length ~ Sepal.Width, iris_Rbeta, subset = c(1:(nrow(iris_Rbeta)+1))), "Subset stated is larger than the input dataset, please correct")
  expect_error(mylm(Sepal.Length ~ Sepal.Width, iris_Rbeta, subset = c("hello", "my", "name", "is", "slim")),"Non numeric vector input please correct")
  expect_error(mylm(Sepal.Length ~ Sepal.Width, iris_Rbeta, subset = list(1,2,3,4)),"Non numeric vector input please correct")
})

test_that("formula only contains names of variables present", {
  expect_error(mylm(fake.variable ~ Sepal.Length + Petal.Length, iris_Rbeta), "fake.variable are named in formula but not present in data, please check formula")
  expect_error(mylm(fake.variable ~ fakebeta + Petal.Length, iris_Rbeta), "fake.variable and fakebeta are named in formula but not present in data, please check formula")
})

test_that("subsetted data only contains usable data types", {
  expect_error(mylm(Petal.Length ~ Sepal.Length + character_species, data_characters), "character_species are not numeric, integer or factor variables, please reconsider")
  expect_error(mylm(Petal.Length ~ Sepal.Length + character_species + character_speciesb, data_characters), "character_species and character_speciesb are not numeric, integer or factor variables, please reconsider")
})

test_that("data characteristic warnings and errors work", {
  expect_warning(mylm(y ~ z, data_tests), "1 data points contain at least 1 null value and so will not be considered", ignore.case = TRUE)
  expect_warning(mylm(y ~ z, data_tests, subset= c(3:6)), "2 to 5 data points, technique may not be suitable", ignore.case = TRUE)
  expect_error(mylm(y ~ z, data_tests, subset= c(5:6)),"2 or fewer data points, technique not suitable" )
  expect_error(mylm(y ~ x, data_tests, subset= c(3:10)),"numeric predictor variable with 0 variance detected, please remove and try again")
  expect_error(mylm(y ~ x + b, data_tests, subset= c(3:10)),"numeric predictor variable with 0 variance detected, please remove and try again")
  expect_error(mylm(y ~ y + a, data_tests, subset= c(3:10)),"catagorical with only 1 factor detected, please remove and try again")
  expect_error(mylm(y ~ a, data_tests, subset= c(3:10)),"catagorical with only 1 factor detected, please remove and try again")
  expect_error(mylm(y ~ c1 + c2, data_tests), "Perfect corrolation detected between predictor variables, can not compute")
})


#Ouput Checks

test_that("returns correct type of values", {
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta), "list")
  expect_length(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta), 11)
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$call, "language")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$data, "list")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$yname, "character")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$coef, "double")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$sigma, "double")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$vcov, "double")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$npar, "integer")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$df.residual, "integer")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$residuals, "double")
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)$fitted.values, "double")
})


test_that("check return values matches (structural)", {
  x <- mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris_Rbeta)
  expect_equal(length(x$data),  3)
  expect_equal( length(x$coef),  3)
  expect_equal( x$npar, 3)
  expect_equal(length(x$data)^2, 9)
  expect_equal( length(x$vcov), 9)
  expect_equal(nrow(x$data) - x$npar, 147)
  expect_equal(x$df.residual, 147)
  expect_equal(length(x$residuals), 150)
  expect_equal(length(x$fitted.values), 150)
})


test_that("fitting process working", {
  x <- mylm(c1 ~ c2, data_tests)
  expect_true(LRE(x$coef[2], 1)>= 10)
  expect_true(LRE(x$coef[1], 0)>= 10)
})

