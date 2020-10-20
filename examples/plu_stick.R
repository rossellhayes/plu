ingredients <- c("sugar", "spice", "everything nice")
plu::stick(ingredients)
plu::stick(ingredients, conj = " or ")

# `fn` applies a function to each element
plu::stick(ingredients, fn = toupper)
plu::stick(names(formals(plu::stick)), fn = encodeString, quote = "`")

# When `conj` is `NULL`, `sep` is used between all elements
plu::stick(ingredients, sep = " and ", conj = NULL)
plu::stick(ingredients, sep = "/",     conj = NULL)

# `oxford` is optional for English, but should be `FALSE` for most languages
ingredientes <- c("azúcar", "flores", "muchos colores")
plu::stick(ingredientes, conj = " y ", oxford = FALSE)

# When `conj` is `NULL`, `sep` is used between all elements
creed <- c("snow", "rain", "heat", "gloom of night")
plu::stick(creed, sep = " nor ", conj = NULL)

# Oxford commas are only added when there are three or more elements
plu::stick(letters[1:3], oxford = TRUE)
plu::stick(letters[1:2], oxford = TRUE)
