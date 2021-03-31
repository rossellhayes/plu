test_that("is_capital", {
  expect_equal(is_capital("A"), TRUE)
  expect_equal(is_capital("a"), FALSE)
  expect_equal(is_capital("?"), NA)
  expect_equal(is_capital(1),   NA)
  expect_equal(is_capital(""),  NA)
})

test_that("is_capital strict", {
  expect_equal(is_capital("A", strict = TRUE), TRUE)
  expect_equal(is_capital("a", strict = TRUE), FALSE)
  expect_equal(is_capital("?", strict = TRUE), FALSE)
  expect_equal(is_capital(1,   strict = TRUE), FALSE)
  expect_equal(is_capital("",  strict = TRUE), FALSE)
})

test_that("vectorized is_capital", {
  expect_equal(is_capital(LETTERS), rep(TRUE,  26))
  expect_equal(is_capital(letters), rep(FALSE, 26))
  expect_equal(is_capital(1:26),    rep(NA,    26))
  expect_equal(
    is_capital(
      vapply(as.raw(c(1:64, 91:96, 123:126)), rawToChar, character(1))
    ),
    rep(NA, 74)
  )
})

test_that("multicharacter is_capital", {
  expect_equal(is_capital(c("aa", "AA", "!!")), c(FALSE, TRUE, NA))
  expect_equal(is_capital(c("Aa", "A!", "a!")), c(NA, NA, NA))
  expect_equal(
    is_capital(c("a", "A", "!", "aa", "AA", "!!")), rep(c(FALSE, TRUE, NA), 2)
  )
})

# test_that("multilingual is_capital", {
#   skip_on_cran()
#
#   # Non-ASCII Latin
#   expect_equal(is_capital("É"), TRUE)
#   expect_equal(is_capital("é"), FALSE)
#   # expect_equal(is_capital("ẞ"), TRUE)
#   # expect_equal(is_capital("ß"), FALSE)
#
#   # Greek
#   expect_equal(is_capital("Δ"), TRUE)
#   expect_equal(is_capital("\u3b4"), FALSE)
#
#   # Cyrillic
#   expect_equal(is_capital("Д"), TRUE)
#   expect_equal(is_capital("д"), FALSE)
#
#   # Coptic
#   expect_equal(is_capital("Ⲇ"), TRUE)
#   expect_equal(is_capital("ⲇ"), FALSE)
#
#   # Armenian
#   expect_equal(is_capital("Դ"), TRUE)
#   expect_equal(is_capital("դ"), FALSE)
#
#   # Adlam
#   # expect_equal(is_capital("𞤁"), TRUE)
#   # expect_equal(is_capital("𞤣"), FALSE)
#
#   # Warang Citi
#   # expect_equal(is_capital("𑢴"), TRUE)
#   # expect_equal(is_capital("𑣔"), FALSE)
#
#   # Cherokee
#   # expect_equal(is_capital("Ꮣ"), TRUE)
#   # expect_equal(is_capital("ꮣ"), FALSE)
#
#   # Osage
#   # expect_equal(is_capital("𐓈"), TRUE)
#   # expect_equal(is_capital("𐓰"), FALSE)
#
#   # Deseret
#   # expect_equal(is_capital("𐐔"), TRUE)
#   # expect_equal(is_capital("𐐼"), FALSE)
#
#   # Unicameral scripts
#   expect_equal(is_capital("ا"), NA) # Arabic
#   expect_equal(is_capital("汉"), NA) # Chinese (Simplified)
#   expect_equal(is_capital("დ"), NA) # Georgian
#   expect_equal(is_capital("א"), NA) # Hebrew
#   expect_equal(is_capital("日"), NA) # Japanese
#   expect_equal(is_capital("ಕ"), NA) # Kannada
#   expect_equal(is_capital("ㄱ"), NA) # Korean
#   expect_equal(is_capital("த"), NA) # Tamil
#   expect_equal(is_capital("త"), NA) # Telugu
# })

test_that("is_capitalized", {
  expect_equal(is_capitalized("A word"), TRUE)
  expect_equal(is_capitalized("a word"), FALSE)
  expect_equal(is_capitalized("a Word"), FALSE)

  expect_equal(is_capitalized(" A word"), TRUE)
  expect_equal(is_capitalized(" a word"), FALSE)
  expect_equal(is_capitalized(" a Word"), FALSE)

  expect_equal(is_capitalized("! A word"), TRUE)
  expect_equal(is_capitalized("! a word"), FALSE)
  expect_equal(is_capitalized("! a Word"), FALSE)

  expect_equal(is_capitalized("..."), NA)
  expect_equal(is_capitalized(""),    NA)
})

test_that("vectorized is_capitalized", {
  expect_equal(is_capitalized(c("A word", " A word", "!A word")), rep(TRUE, 3))
  expect_equal(is_capitalized(c("a word", " a word", "!a word")), rep(FALSE, 3))

  expect_equal(is_capitalized(c("A word", "a word", "...")), c(TRUE, FALSE, NA))
})

test_that("multilingual is_capitalized", {
  skip_on_cran()
  skip_on_os("linux")

  expect_equal(is_capitalized("множественный"), FALSE)
  expect_equal(is_capitalized("Множественный"), TRUE)
  expect_equal(is_capitalized("複数"),        NA)
  expect_equal(is_capitalized("複数 plural"), NA)
})

test_that("capitalize", {
  expect_equal(capitalize("A word"), "A word")
  expect_equal(capitalize("a word"), "A word")
  expect_equal(capitalize("a Word"), "A Word")

  expect_equal(capitalize(" A word"), " A word")
  expect_equal(capitalize(" a word"), " A word")
  expect_equal(capitalize(" a Word"), " A Word")

  expect_equal(capitalize("! A word"), "! A word")
  expect_equal(capitalize("! a word"), "! A word")
  expect_equal(capitalize("! a Word"), "! A Word")

  expect_equal(capitalize("..."), "...")
})

test_that("vectorized capitalize", {
  expect_equal(
    capitalize(c("A word", " A word", "!A word")),
    c("A word", " A word", "!A word")
  )
  expect_equal(
    capitalize(c("a word", " a word", "!a word")),
    c("A word", " A word", "!A word")
  )

  expect_equal(
    capitalize(c("A word", "a word", "...")), c("A word", "A word", "...")
  )
})

test_that("multilingual capitalize", {
  skip_on_cran()
  skip_on_os("linux")

  expect_equal(capitalize("множественный"), "Множественный")
  expect_equal(capitalize("Множественный"), "Множественный")
  expect_equal(capitalize("複数"),        "複数")
  expect_equal(capitalize("複数 plural"), "複数 plural")
})

test_that("lgl_collapse", {
  expect_equal(lgl_collapse(TRUE),  TRUE)
  expect_equal(lgl_collapse(FALSE), FALSE)
  expect_equal(lgl_collapse(NA),    NA)

  expect_equal(lgl_collapse(c(TRUE, TRUE, TRUE)),    TRUE)
  expect_equal(lgl_collapse(c(FALSE, FALSE, FALSE)), FALSE)
  expect_equal(lgl_collapse(c(NA, NA, NA)),          NA)

  expect_equal(lgl_collapse(c(TRUE, TRUE, FALSE)),  NA)
  expect_equal(lgl_collapse(c(FALSE, FALSE, TRUE)), NA)
  expect_equal(lgl_collapse(c(TRUE, TRUE, NA)),     NA)
  expect_equal(lgl_collapse(c(FALSE, FALSE, NA)),   NA)
  expect_equal(lgl_collapse(c(TRUE, FALSE, NA)),    NA)
})
