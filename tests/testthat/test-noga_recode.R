test_that("auto recoding single digit numeric code to labels works", {

  skip("Auto recoding single digit numeric code is not implemented yet")

  codes <- c(1, 2, 3)

  num_to_labs <- function(language) noga_recode(
    codes,
    language,
    level = "auto",
    to = "auto"
  )

  labs_en <- c("Crop and animal production, hunting and related service activities",
               "Forestry and logging",
               "Fishing and aquaculture")
  labs_de <- c("Landwirtschaft, Jagd und damit verbundene Tätigkeiten",
               "Forstwirtschaft und Holzeinschlag",
               "Fischerei und Aquakultur")
  labs_fr <- c("Culture et production animale, chasse et services annexes",
               "Sylviculture et exploitation forestière",
               "Pêche et aquaculture")
  labs_it <- c("Produzioni vegetali e animali, caccia e servizi connessi",
               "Silvicoltura e utilizzo di aree forestali",
               "Pesca e acquicoltura")

  expect_equal(num_to_labs("en"), labs_en)
  expect_equal(num_to_labs("de"), labs_de)
  expect_equal(num_to_labs("fr"), labs_fr)
  expect_equal(num_to_labs("it"), labs_it)
})

test_that("auto recoding numeric code to labels works", {

  num_to_labs <- function(codes, language) noga_recode(
    codes,
    language,
    level = "auto",
    to = "auto"
  )

  ### Division
  cd_div <- c(10, 20, 30)

  labs_div_en <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_en"]
  labs_div_de <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_de"]
  labs_div_fr <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_fr"]
  labs_div_it <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_it"]


  expect_equal(num_to_labs(cd_div, "en"), labs_div_en)
  expect_equal(num_to_labs(cd_div, "de"), labs_div_de)
  expect_equal(num_to_labs(cd_div, "fr"), labs_div_fr)
  expect_equal(num_to_labs(cd_div, "it"), labs_div_it)

  ### Group
  cd_grp <- c(201, 431, 512)

  labs_grp_en <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_en"]
  labs_grp_de <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_de"]
  labs_grp_fr <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_fr"]
  labs_grp_it <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_it"]


  expect_equal(num_to_labs(cd_grp, "en"), labs_grp_en)
  expect_equal(num_to_labs(cd_grp, "de"), labs_grp_de)
  expect_equal(num_to_labs(cd_grp, "fr"), labs_grp_fr)
  expect_equal(num_to_labs(cd_grp, "it"), labs_grp_it)

  ### Class
  cd_cls <- c(1512, 3240, 5122)

  labs_cls_en <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_en"]
  labs_cls_de <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_de"]
  labs_cls_fr <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_fr"]
  labs_cls_it <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_it"]


  expect_equal(num_to_labs(cd_cls, "en"), labs_cls_en)
  expect_equal(num_to_labs(cd_cls, "de"), labs_cls_de)
  expect_equal(num_to_labs(cd_cls, "fr"), labs_cls_fr)
  expect_equal(num_to_labs(cd_cls, "it"), labs_cls_it)

  ### Type
  cd_type <- c(192000, 309100, 464202)

  labs_type_en <- lookup[(lookup$type %in% cd_type), "name_en"]
  labs_type_de <- lookup[(lookup$type %in% cd_type), "name_de"]
  labs_type_fr <- lookup[(lookup$type %in% cd_type), "name_fr"]
  labs_type_it <- lookup[(lookup$type %in% cd_type), "name_it"]


  expect_equal(num_to_labs(cd_type, "en"), labs_type_en)
  expect_equal(num_to_labs(cd_type, "de"), labs_type_de)
  expect_equal(num_to_labs(cd_type, "fr"), labs_type_fr)
  expect_equal(num_to_labs(cd_type, "it"), labs_type_it)
})

test_that("auto recoding numeric code as strings to labels works", {

  strg_code_to_labs <- function(codes, language) noga_recode(
    codes,
    language,
    level = "auto",
    to = "auto"
  )

  ### Division
  cd_div <- c("01", "20", "46")

  labs_div_en <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_en"]
  labs_div_de <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_de"]
  labs_div_fr <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_fr"]
  labs_div_it <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_it"]


  expect_equal(strg_code_to_labs(cd_div, "en"), labs_div_en)
  expect_equal(strg_code_to_labs(cd_div, "de"), labs_div_de)
  expect_equal(strg_code_to_labs(cd_div, "fr"), labs_div_fr)
  expect_equal(strg_code_to_labs(cd_div, "it"), labs_div_it)

  ### Group
  cd_grp <- c("011", "110", "512")

  labs_grp_en <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_en"]
  labs_grp_de <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_de"]
  labs_grp_fr <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_fr"]
  labs_grp_it <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_it"]


  expect_equal(strg_code_to_labs(cd_grp, "en"), labs_grp_en)
  expect_equal(strg_code_to_labs(cd_grp, "de"), labs_grp_de)
  expect_equal(strg_code_to_labs(cd_grp, "fr"), labs_grp_fr)
  expect_equal(strg_code_to_labs(cd_grp, "it"), labs_grp_it)

  ### Class
  cd_cls <- c("0112", "1310", "5122")

  labs_cls_en <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_en"]
  labs_cls_de <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_de"]
  labs_cls_fr <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_fr"]
  labs_cls_it <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_it"]


  expect_equal(strg_code_to_labs(cd_cls, "en"), labs_cls_en)
  expect_equal(strg_code_to_labs(cd_cls, "de"), labs_cls_de)
  expect_equal(strg_code_to_labs(cd_cls, "fr"), labs_cls_fr)
  expect_equal(strg_code_to_labs(cd_cls, "it"), labs_cls_it)

  ### Type
  cd_type <- c("011100", "309100", "464202")

  labs_type_en <- lookup[(lookup$type %in% cd_type), "name_en"]
  labs_type_de <- lookup[(lookup$type %in% cd_type), "name_de"]
  labs_type_fr <- lookup[(lookup$type %in% cd_type), "name_fr"]
  labs_type_it <- lookup[(lookup$type %in% cd_type), "name_it"]


  expect_equal(strg_code_to_labs(cd_type, "en"), labs_type_en)
  expect_equal(strg_code_to_labs(cd_type, "de"), labs_type_de)
  expect_equal(strg_code_to_labs(cd_type, "fr"), labs_type_fr)
  expect_equal(strg_code_to_labs(cd_type, "it"), labs_type_it)
})

test_that("auto recoding labels to codes works", {

  labs_to_codes <- function(labels, language) noga_recode(
    labels,
    language,
    level = "auto",
    to = "auto"
  )

  ### Division
  cd_div <- c("01", "20", "46")

  labs_div_en <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_en"]
  labs_div_de <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_de"]
  labs_div_fr <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_fr"]
  labs_div_it <- lookup[(lookup$division %in% cd_div & is.na(lookup$group)), "name_it"]


  expect_equal(labs_to_codes(labs_div_en, "en"), cd_div)
  expect_equal(labs_to_codes(labs_div_de, "de"), cd_div)
  expect_equal(labs_to_codes(labs_div_fr, "fr"), cd_div)
  expect_equal(labs_to_codes(labs_div_it, "it"), cd_div)

  ### Group
  cd_grp <- c("011", "110", "512")

  labs_grp_en <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_en"]
  labs_grp_de <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_de"]
  labs_grp_fr <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_fr"]
  labs_grp_it <- lookup[(lookup$group %in% cd_grp & is.na(lookup$class)), "name_it"]


  expect_equal(labs_to_codes(labs_grp_en, "en"), cd_grp)
  expect_equal(labs_to_codes(labs_grp_de, "de"), cd_grp)
  expect_equal(labs_to_codes(labs_grp_fr, "fr"), cd_grp)
  expect_equal(labs_to_codes(labs_grp_it, "it"), cd_grp)

  ### Class
  # Only classes without shared names with types
  cd_cls <- c("1051", "1082", "1413")

  labs_cls_en <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_en"]
  labs_cls_de <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_de"]
  labs_cls_fr <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_fr"]
  labs_cls_it <- lookup[(lookup$class %in% cd_cls & is.na(lookup$type)), "name_it"]


  expect_equal(labs_to_codes(labs_cls_en, "en"), cd_cls)
  expect_equal(labs_to_codes(labs_cls_de, "de"), cd_cls)
  expect_equal(labs_to_codes(labs_cls_fr, "fr"), cd_cls)
  expect_equal(labs_to_codes(labs_cls_it, "it"), cd_cls)

  ### Type
  cd_type <- c("011100", "309100", "464202")

  labs_type_en <- lookup[(lookup$type %in% cd_type), "name_en"]
  labs_type_de <- lookup[(lookup$type %in% cd_type), "name_de"]
  labs_type_fr <- lookup[(lookup$type %in% cd_type), "name_fr"]
  labs_type_it <- lookup[(lookup$type %in% cd_type), "name_it"]


  expect_equal(labs_to_codes(labs_type_en, "en"), cd_type)
  expect_equal(labs_to_codes(labs_type_de, "de"), cd_type)
  expect_equal(labs_to_codes(labs_type_fr, "fr"), cd_type)
  expect_equal(labs_to_codes(labs_type_it, "it"), cd_type)
})
