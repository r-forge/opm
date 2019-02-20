

library(testthat)
context("Testing the auxiliary functions of the pfn package")


## new_name
test_that("conversion of vectors of file names works", {

  expect_equal(new_name(letters, FALSE), letters)
  expect_equal(new_name(letters, TRUE), letters)
  expect_equal(new_name(LETTERS, FALSE), LETTERS)
  expect_equal(new_name(LETTERS, TRUE), letters)

})


## new_name
test_that("conversion of Germanic-language file names works", {

  # German "scharfes S"
  expect_equal(new_name("Stra\u{00DF}e ", FALSE), "Strasse")
  expect_equal(new_name("Stra\u{00DF}e ", TRUE), "strasse")
  expect_equal(new_name(" SO\u{1E9E}E", FALSE), "SOSSE")
  expect_equal(new_name(" SO\u{1E9E}E", TRUE), "sosse")

  # Eth character
  expect_equal(new_name("\u{00D0}\u{00F0}", FALSE), "Dd")
  expect_equal(new_name("\u{00D0}\u{00F0}", TRUE), "dd")

  # German umlauts (and few other things)
  for (name in names(UMLAUT_REPLACEMENT)) {
    wanted <- gsub("_", "", UMLAUT_REPLACEMENT[[name]], FALSE, FALSE, TRUE)
    expect_equal(new_name(name, FALSE), wanted)
  }

})


## new_name
test_that("conversion of French file names works", {

  # French ligatures
  expect_equal(new_name("Le b\u{0153}uf ", FALSE), "Le_boeuf")
  expect_equal(new_name("Le b\u{0153}uf ", TRUE), "le_boeuf")
  expect_equal(new_name("LE B\u{0152}UF ", FALSE), "LE_BOEUF")
  expect_equal(new_name("LE B\u{0152}UF ", TRUE), "le_boeuf")

  # Latin/French/English ligatures
  expect_equal(new_name("encyclop\u{00E6}dia ", FALSE), "encyclopaedia")
  expect_equal(new_name("ENCYCLOP\u{00C6}DIA ", FALSE), "ENCYCLOPAEDIA")

})


## new_name
test_that("conversion of miscellaneous file names works", {

  # Polish
  expect_equal(new_name(" Witold  Lutos\u{0142}awski ", FALSE),
    "Witold_Lutoslawski")
  expect_equal(new_name(" Witold  Lutos\u{0141}awski ", FALSE),
    "Witold_LutosLawski")

  # Turkish
  expect_equal(new_name("Diyarbak\u{0131}r", FALSE), "Diyarbakir")
  expect_equal(new_name("\u{0130}stanbul", FALSE), "Istanbul")

})


## new_name
test_that("conversion of file names with dashes works", {

  dashes <- c("\u{2012}", "\u{2013}", "\u{2014}", "\u{2015}")
  for (dash in dashes) {
    expect_equal(new_name(paste0("x", dash, "y"), FALSE), "x-y")
    # leading dash yield a leading hyphen, which must get removed
    expect_equal(new_name(paste0(dash, "x", dash, "y"), FALSE), "x-y")
  }

})


## new_name
test_that("conversion of compound file names works", {

  input <- "Weird - unexpected -- data from somewhere (?) "
  wanted <- "Weird-unexpected-data_from_somewhere_maybe"
  expect_equal(new_name(input, FALSE), wanted)
  expect_equal(new_name(input, TRUE), tolower(wanted))

  input <- "- Fritz' + Anna's + my file #89 "
  wanted <- "Fritz_plus_Anna_s_plus_my_file_No_89"
  expect_equal(new_name(input, FALSE), wanted)
  expect_equal(new_name(input, TRUE), tolower(wanted))

  for (name in names(ASCII_REPLACEMENT)) {
    wanted <- gsub("_", "", ASCII_REPLACEMENT[[name]], FALSE, FALSE, TRUE)
    expect_equal(new_name(name, FALSE), wanted)
  }

})

