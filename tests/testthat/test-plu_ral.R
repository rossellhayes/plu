one   <- integer(1)
two   <- integer(2)
fifty <- integer(50)

test_that("pl", {
  expect_equal(plu_ral("word", pl = FALSE), "word")
  expect_equal(plu_ral("word", pl = TRUE), "words")
})

test_that("n", {
  expect_equal(plu_ral("word", n = 1), "word")
  expect_equal(plu_ral("word", n = -1), "word")
  expect_equal(plu_ral("word", n = 2), "words")
  expect_equal(plu_ral("word", n = 0), "words")
  expect_equal(plu_ral("word", n = 0.5), "words")
})

test_that("vector", {
  expect_equal(plu_ral("word", one), "word")
  expect_equal(plu_ral("word", two), "words")
})

test_that("spaces", {
  expect_equal(plu_ral("word "), "words ")
  expect_equal(plu_ral(" word"), " words")
  expect_equal(plu_ral(" word "), " words ")
  expect_equal(plu_ral("a word"), "words")
  expect_equal(plu_ral("a word "), "words ")
  expect_equal(plu_ral(" a word"), " words")
  expect_equal(plu_ral(" a word "), " words ")
})

test_that("vector", {
  expect_equal(
    plu_ral(c("word", "phrase", "sentence")),
    c("words", "phrases", "sentences")
  )

  expect_equal(
    plu_ral(c("person", "cactus", "attorney {general}")),
    c("people", "cacti", "attorneys general")
  )

  expect_equal(
    plu_ral(c("A sentence.", "A sentence? A sentence. A sentence!")),
    c("Sentences.", "Sentences? Sentences. Sentences!")
  )

  expect_equal(
    plu_ral(c(w = "word", p = "phrase", "sentence")),
    c(w = "words", p = "phrases", "sentences")
  )
})

test_that("punctuation", {
  expect_equal(plu_ral("This is a word."), "These are words.")
  expect_equal(plu_ral("This isn't a word."), "These aren't words.")
  expect_equal(plu_ral("Is he a low-life?"), "Are they low-lifes?")
  expect_equal(
    plu_ral("A Hispanic/Latino is a voter!"), "Hispanics/Latinos are voters!"
  )
})

test_that("invariant", {
  expect_equal(plu_ral("{invariant} variant", one), "invariant variant")
  expect_equal(plu_ral("{invariant} variant", two), "invariant variants")
})

test_that("irregulars", {
  expect_equal(plu_ral("formula", two), "formulas")
  expect_equal(plu_ral("formula", two, irregulars = "conservative"), "formulae")
  expect_equal(plu_ral("person", two), "people")
  expect_equal(plu_ral("person", two, irregulars = "liberal"), "persons")
})

test_that("pipe works", {
  expect_equal(plu_ral("{single|plural} number", one), "single number")
  expect_equal(plu_ral("{single|plural} number", two), "plural numbers")
  expect_equal(
    plu_ral("{one|many} {single|plural} number", two), "many plural numbers"
  )
  expect_equal(plu_ral("{single|dual|plural} number", one), "single number")
  expect_equal(plu_ral("{single|dual|plural} number", two), "dual numbers")
  expect_equal(plu_ral("{single|dual|plural} number", fifty), "plural numbers")

  expect_equal(plu_ral("number{1|2}",   one),   "number1")
  expect_equal(plu_ral("number{|2}",    one),   "number")
  expect_equal(plu_ral("number{1|2}",   two),   "number2")
  expect_equal(plu_ral("number{1|2|3}", fifty), "number3")
})

test_that("number", {
  expect_equal(plu_ral("n number", one), "1 number")
  expect_equal(plu_ral("n number", fifty), "50 numbers")
  expect_equal(plu_ral("n number", one, replace_n = FALSE), "n number")
  expect_equal(plu_ral("n number", fifty, replace_n = FALSE), "ns numbers")
  expect_equal(
    plu_ral("n number", integer(10000), n_fn = format, big.mark = ","),
    "10,000 numbers"
  )
  expect_equal(plu_ral("{the|both|all n} number", fifty), "all 50 numbers")
})

test_that("capitalization", {
  expect_equal(plu_ral("A word"), "Words")
  expect_equal(plu_ral("! A word"), "! Words")
  expect_equal(plu_ral("A test! A test! A test!"), "Tests! Tests! Tests!")
})

test_that("non-words", {
  expect_equal(plu_ral(c("", "-", "...", "?!")), c("", "-", "...", "?!"))
})

test_that("early return", {
  expect_equal(plu_ral(character(0)), character(0))
})

test_that("errors", {
  expect_error(plu_ral(integer(1)))
  expect_error(plu_ral("word", n = character(1)))
  expect_error(plu_ral("word", n = numeric(2)))
  expect_error(plu_ral("word", pl = NA))
  expect_error(plu_ral("word", pl = numeric(1)))
  expect_error(plu_ral("word", pl = logical(2)))
  expect_error(plu_ral("word", replace_n = NA))
  expect_error(plu_ral("word", replace_n = numeric(1)))
  expect_error(plu_ral("word", replace_n = logical(2)))

  expect_error(plu_ral("word", n_fn = plu_not_real_fun),    "plu_not_real_fun")
  expect_error(plu_ral("word", n_fn = "plu_not_real_fun"),  "plu_not_real_fun")
  expect_error(plu_ral("word", n_fn = plu::not_real_fun),   "plu::not_real_fun")
  expect_error(plu_ral("word", n_fn = "plu::not_real_fun"), "plu::not_real_fun")
})
