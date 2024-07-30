test_that("sections can be detected from code", {
  labels <- c("A", "B", "C")
  expect_match(automaticleveldetection(labels , "en"), "section")
})

test_that("sections in lowercase can be detected from code", {
  labels <- c("a", "b", "c")
  expect_match(automaticleveldetection(labels , "en"), "section")
})

test_that("some divisions can be detected from label", {
  labels <- c("Forestry and logging", "Fishing and aquaculture", "Mining of coal and lignite")
  expect_match(automaticleveldetection(labels , "en"), "division")
})

test_that("some groups can be detected from label", {
  labels <- c("Processing and preserving of meat and production of meat products", "Manufacture of dairy products", "Manufacture of weapons and ammunition")
  expect_match(automaticleveldetection(labels , "en"), "group")
})

test_that("classes with the same names as groups can be detected", {
  labels <- c("Preparation and spinning of textile fibres", "Weaving of textiles", "Sawmilling and planing of wood")
  expect_match(automaticleveldetection(labels , "en"), "class")
})

test_that("some classes can be detected from label", {
  labels <- c("Growing of other non-perennial crops", "Growing of grapes", "Manufacture of made-up textile articles, except apparel")
  expect_match(automaticleveldetection(labels , "en"), "class")
})

test_that("types with the same names as groups can be detected", {
  labels <- c("Mining of hard coal", "Mining of lignite", "Extraction of crude petroleum")
  expect_match(automaticleveldetection(labels , "en"), "type")
})

test_that("types with the same names as classes can be detected", {
  labels <- c("Growing of other non-perennial crops", "Growing of grapes", "Manufacture of cheese")
  expect_match(automaticleveldetection(labels , "en"), "type")
})

test_that("some types can be detected from label", {
  labels <- c("Manufacture of fresh dairy products", "Manufacture of cheese", "Other milk processing")
  expect_match(automaticleveldetection(labels , "en"), "type")
})

### Per language and level

## EN

test_that("english types can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$type), "name_en"]
  expect_match(automaticleveldetection(labels , "en"), "type")
})

test_that("english classes can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$class) & is.na(noga::lookup$type), "name_en"]
  expect_match(automaticleveldetection(labels , "en"), "class")
})

test_that("english groups can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$group) & is.na(noga::lookup$class), "name_en"]
  expect_match(automaticleveldetection(labels , "en"), "group")
})

test_that("english divisions can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$division) & is.na(noga::lookup$group), "name_en"]
  expect_match(automaticleveldetection(labels , "en"), "division")
})

test_that("english sections can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$section) & is.na(noga::lookup$division), "name_en"]
  expect_match(automaticleveldetection(labels , "en"), "section")
})

## DE

test_that("german types can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$type), "name_de"]
  expect_match(automaticleveldetection(labels , "de"), "type")
})

test_that("german classes can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$class) & is.na(noga::lookup$type), "name_de"]
  expect_match(automaticleveldetection(labels , "de"), "class")
})

test_that("german groups can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$group) & is.na(noga::lookup$class), "name_de"]
  expect_match(automaticleveldetection(labels , "de"), "group")
})

test_that("german divisions can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$division) & is.na(noga::lookup$group), "name_de"]
  expect_match(automaticleveldetection(labels , "de"), "division")
})

test_that("german sections can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$section) & is.na(noga::lookup$division), "name_de"]
  expect_match(automaticleveldetection(labels , "de"), "section")
})

## FR

test_that("french types can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$type), "name_fr"]
  expect_match(automaticleveldetection(labels , "fr"), "type")
})

test_that("french classes can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$class) & is.na(noga::lookup$type), "name_fr"]
  expect_match(automaticleveldetection(labels , "fr"), "class")
})

test_that("french groups can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$group) & is.na(noga::lookup$class), "name_fr"]
  expect_match(automaticleveldetection(labels , "fr"), "group")
})

test_that("french divisions can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$division) & is.na(noga::lookup$group), "name_fr"]
  expect_match(automaticleveldetection(labels , "fr"), "division")
})

test_that("french sections can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$section) & is.na(noga::lookup$division), "name_fr"]
  expect_match(automaticleveldetection(labels , "fr"), "section")
})

## IT

test_that("italian types can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$type), "name_it"]
  expect_match(automaticleveldetection(labels , "it"), "type")
})

test_that("italian classes can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$class) & is.na(noga::lookup$type), "name_it"]
  expect_match(automaticleveldetection(labels , "it"), "class")
})

test_that("italian groups can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$group) & is.na(noga::lookup$class), "name_it"]
  expect_match(automaticleveldetection(labels , "it"), "group")
})

test_that("italian divisions can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$division) & is.na(noga::lookup$group), "name_it"]
  expect_match(automaticleveldetection(labels , "it"), "division")
})

test_that("italian sections can be detected from label", {
  labels <- noga::lookup[!is.na(noga::lookup$section) & is.na(noga::lookup$division), "name_it"]
  expect_match(automaticleveldetection(labels , "it"), "section")
})

###
