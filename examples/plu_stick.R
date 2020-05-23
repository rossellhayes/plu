ingredients <- c("sugar", "spice", "everything nice")
plu::stick(ingredients)

plu::stick(ingredients, fn = toupper)
message(plu::stick(names(formals(plu::stick)), fn = usethis::ui_code))

plu::stick(ingredients, conj = "or")

plu::stick(ingredients, syndeton = "all")

plu::stick(ingredients, sep = "/", syndeton = "none")

creed <- c("snow", "rain", "heat", "gloom of night")
plu::stick(creed, conj = "nor", syndeton = "all")

dedication <- c("my parents", "Ayn Rand", "God")
plu::stick(dedication)
plu::stick(dedication, oxford = TRUE)
plu::stick(dedication, oxford = FALSE)
