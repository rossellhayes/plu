capitalize(c("word", "a whole phrase"))
capitalize("preserving MIXED Case")
capitalize("... word")

is_capital(c("a", "A", "!"))
is_capital(c("aa", "AA", "!!"))
is_capital("Aa")

is_capitalized(c("a word", "A word", "a Word"))
is_capitalized("... A word")
is_capitalized("...")
