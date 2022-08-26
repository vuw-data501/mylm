data(iris)

test_that("formula only contains names of variables present", {
  expect_error(mylm(fake.variable ~ Sepal.Length + Petal.Length, iris), "fake.variable are named in formula but not present in data, please check formula")
  expect_error(mylm(fake.variable ~ fakebeta + Petal.Length, iris), "fake.variable and fakebeta are named in formula but not present in data, please check formula")
})


test_that("returns correct type of values", {
  expect_type(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris), "list")
  expect_length(mylm(Petal.Width ~ Sepal.Length + Petal.Length, iris), 11)
})


#Variable(s) fake.variable is not in the dataset. Please select an alternative variable