ingredients <- c("sugar", "spice", "everything nice")
plu::stick(ingredients)

plu::stick(ingredients, fn = toupper)
plu::stick(ingredients, fn = sQuote, q = FALSE)

arguments <- names(formals(plu::stick))
plu::stick(arguments, usethis::ui_code)

plu::stick(ingredients, conj = "or")

plu::stick(ingredients, conj_all = TRUE)

creed <- c("snow", "rain", "heat", "gloom of night")
plu::stick(creed, conj = "nor", conj_all = TRUE)

dedication <- c("my parents", "Ayn Rand", "God")
plu::stick(dedication)
plu::stick(dedication, oxford = TRUE)
plu::stick(dedication, oxford = FALSE)
