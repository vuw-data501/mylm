#Input Checks

data(iris)
data_characters <- iris
data_characters$character_species <- as.character(data_characters$Species)
data_characters$character_speciesb <- as.character(data_characters$Species)
data_tests<- data.frame(z = as.factor(c(NA, "hello", "hello", "hello", "orange", "orange", "orange", "orange", "orange", "orange")),
                        x = c(2,2,2,2,2,2,2,2,2,2),
                        y = c(0,1,2,3,4,5,6,8,9,10),
                        a = as.factor(c("hey", "hey", "hey", "hey", "hey","hey", "hey", "hey", "hey", "hey")),
                        b = c(2,3,4,5,6,7,8,9,10,11))


test_that("formula only contains names of variables present", {
  expect_error(mylm(fake.variable ~ Sepal.Length + Petal.Length, iris), "fake.variable are named in formula but not present in data, please check formula")
  expect_error(mylm(fake.variable ~ fakebeta + Petal.Length, iris), "fake.variable and fakebeta are named in formula but not present in data, please check formula")
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
})


#Ouput Checks

test_that("returns correct type of values", {
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris), "list")
  expect_length(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris), 11)
})







