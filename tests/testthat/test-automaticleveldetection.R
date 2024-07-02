test_that("sections can be detected", {
  type_labels <- c("A", "B", "C")
  expect_match(automaticleveldetection(type_labels , "en"), "section")
})

test_that("sections in lowercase can be detected", {
  type_labels <- c("a", "b", "c")
  expect_match(automaticleveldetection(type_labels , "en"), "section")
})

test_that("divisions can be detected", {
  type_labels <- c("Forestry and logging", "Fishing and aquaculture", "Mining of coal and lignite")
  expect_match(automaticleveldetection(type_labels , "en"), "division")
})

test_that("groups can be detected", {
  type_labels <- c("Processing and preserving of meat and production of meat products", "Manufacture of dairy products", "Manufacture of weapons and ammunition")
  expect_match(automaticleveldetection(type_labels , "en"), "group")
})

test_that("classes with the same names as groups can be detected", {
  type_labels <- c("Plant propagation", "Mixed farming", "Hunting, trapping and related service activities")
  expect_match(automaticleveldetection(type_labels , "en"), "class")
})

test_that("classes can be detected", {
  type_labels <- c("Growing of other non-perennial crops", "Growing of grapes", "
Manufacture of made-up textile articles, except apparel")
  expect_match(automaticleveldetection(type_labels , "en"), "class")
})

test_that("types with the same names as groups can be detected", {
  type_labels <- c("Mining of hard coal", "Mining of lignite", "Extraction of crude petroleum")
  expect_match(automaticleveldetection(type_labels , "en"), "type")
})

test_that("types with the same names as classes can be detected", {
  type_labels <- c("Growing of other non-perennial crops", "Growing of grapes", "Manufacture of cheese")
  expect_match(automaticleveldetection(type_labels , "en"), "type")
})

test_that("types can be detected", {
  type_labels <- c("Manufacture of fresh dairy products", "Manufacture of cheese", "Other milk processing")
  expect_match(automaticleveldetection(type_labels , "en"), "type")
})
